use serena_core::tools::{Tool, ToolContext};
use serde_json::Value;
use tracing::instrument;
use walkdir::WalkDir;
use ignore::gitignore::Gitignore;
use globset::{Glob, GlobSetBuilder};
use regex::Regex;

pub struct FindFile;

#[async_trait::async_trait]
impl Tool for FindFile {
    fn name(&self) -> &'static str {
        "find_file"
    }

    fn description(&self) -> &'static str {
        "Find files by name using glob or regex patterns with optional filtering"
    }

    fn schema(&self) -> Value {
        serde_json::json!({
            "type": "object",
            "required": ["path"],
            "properties": {
                "path": {"type": "string", "description": "Directory path to search in"},
                "pattern": {"type": "string", "description": "Glob or regex pattern to match filenames"},
                "pattern_type": {"type": "string", "enum": ["glob", "regex"], "default": "glob", "description": "Type of pattern matching"},
                "recursive": {"type": "boolean", "default": true, "description": "Search recursively in subdirectories"},
                "ignore_vcs": {"type": "boolean", "default": true, "description": "Respect .gitignore rules"},
                "case_sensitive": {"type": "boolean", "default": true, "description": "Case sensitive pattern matching"},
                "max_results": {"type": "integer", "default": 1000, "description": "Maximum number of results to return"}
            }
        })
    }
    
    #[instrument(skip_all, fields(path = tracing::field::Empty, pattern = tracing::field::Empty, matches_count = tracing::field::Empty))]
    async fn run(&self, input: Value, ctx: ToolContext) -> serena_core::error::Result<Value> {
        let path = input
            .get("path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing path"))?;
        
        let pattern = input.get("pattern").and_then(|v| v.as_str());
        let pattern_type = input.get("pattern_type").and_then(|v| v.as_str()).unwrap_or("glob");
        let recursive = input.get("recursive").and_then(|v| v.as_bool()).unwrap_or(true);
        let ignore_vcs = input.get("ignore_vcs").and_then(|v| v.as_bool()).unwrap_or(true);
        let case_sensitive = input.get("case_sensitive").and_then(|v| v.as_bool()).unwrap_or(true);
        let max_results = input.get("max_results").and_then(|v| v.as_u64()).unwrap_or(1000) as usize;

        tracing::Span::current().record("path", path);
        if let Some(p) = pattern {
            tracing::Span::current().record("pattern", p);
        }

        let abs_path = ctx.cfg.workspace_root.join(path);
        
        if !abs_path.exists() {
            return Err(serena_core::error::CoreError::validation(
                format!("Path does not exist: {}", path)
            ));
        }
        
        if !abs_path.is_dir() {
            return Err(serena_core::error::CoreError::validation(
                format!("Path is not a directory: {}", path)
            ));
        }

        // Prepare pattern matcher
        let glob_set = if let Some(pattern_str) = pattern {
            if pattern_type == "glob" {
                let mut builder = GlobSetBuilder::new();
                let glob_pattern = if case_sensitive {
                    pattern_str.to_string()
                } else {
                    // For case insensitive glob, we'll handle it differently
                    pattern_str.to_string()
                };
                if let Ok(glob) = Glob::new(&glob_pattern) {
                    builder.add(glob);
                }
                builder.build().ok()
            } else {
                None
            }
        } else {
            None
        };

        let regex = if let Some(pattern_str) = pattern {
            if pattern_type == "regex" {
                let regex_pattern = if case_sensitive {
                    pattern_str
                } else {
                    &format!("(?i){}", pattern_str)
                };
                Regex::new(regex_pattern)
                    .map_err(|e| serena_core::error::CoreError::validation(e.to_string()))?
                    .into()
            } else {
                None
            }
        } else {
            None
        };

        // Load gitignore rules
        let gitignore = if ignore_vcs {
            Gitignore::new(&abs_path.join(".gitignore")).0
        } else {
            Gitignore::empty()
        };

        let walker = if recursive {
            WalkDir::new(&abs_path)
        } else {
            WalkDir::new(&abs_path).max_depth(1)
        };

        let mut matches = Vec::new();

        for entry in walker.into_iter().filter_map(|e| e.ok()) {
            if matches.len() >= max_results {
                break;
            }

            let entry_path = entry.path();
            
            // Skip directories unless we're looking for them specifically
            if !entry.file_type().is_file() {
                continue;
            }

            // Apply gitignore rules
            if ignore_vcs {
                let relative_path = entry_path.strip_prefix(&abs_path).unwrap_or(entry_path);
                if gitignore.matched(relative_path, false).is_ignore() {
                    continue;
                }
            }

            let filename = entry_path.file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("");

            // Apply pattern matching
            let pattern_matches = if let Some(ref glob_set) = glob_set {
                if case_sensitive {
                    glob_set.is_match(filename)
                } else {
                    // Case insensitive glob matching
                    glob_set.is_match(&filename.to_lowercase())
                }
            } else if let Some(ref regex) = regex {
                regex.is_match(filename)
            } else {
                // No pattern specified, match everything
                true
            };

            if pattern_matches {
                let relative_path = entry_path.strip_prefix(&ctx.cfg.workspace_root)
                    .unwrap_or(entry_path);
                
                matches.push(serde_json::json!({
                    "name": filename,
                    "path": relative_path.to_string_lossy(),
                    "directory": entry_path.parent()
                        .and_then(|p| p.strip_prefix(&ctx.cfg.workspace_root).ok())
                        .map(|p| p.to_string_lossy())
                        .unwrap_or_else(|| "".into())
                }));
            }
        }

        let matches_count = matches.len();
        tracing::Span::current().record("matches_count", matches_count);
        tracing::info!(path = %path, pattern = ?pattern, matches_count, "find_file completed");

        Ok(serde_json::json!({
            "matches": matches,
            "count": matches_count,
            "pattern": pattern,
            "pattern_type": pattern_type,
            "truncated": matches_count >= max_results
        }))
    }
}