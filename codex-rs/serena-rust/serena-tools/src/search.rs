use regex::Regex;
use serena_core::tools::{Tool, ToolContext};
use serde_json::Value;
use std::path::Path;
use tokio::fs;
use tracing::instrument;
use walkdir::WalkDir;
use ignore::gitignore::Gitignore;

pub struct Search;

#[async_trait::async_trait]
impl Tool for Search {
    fn name(&self) -> &'static str {
        "search_for_pattern"
    }
    fn description(&self) -> &'static str {
        "Search for regex patterns in files and directories with context lines"
    }
    fn schema(&self) -> Value {
        serde_json::json!({
            "type":"object",
            "required":["path","pattern"],
            "properties":{
                "path":{"type":"string", "description": "File or directory path to search"},
                "pattern":{"type":"string", "description": "Regular expression pattern to search for"},
                "recursive":{"type":"boolean", "default": true, "description": "Search recursively in directories"},
                "glob":{"type":"array", "items": {"type": "string"}, "description": "Glob patterns to filter files"},
                "ignore_vcs":{"type":"boolean", "default": true, "description": "Respect .gitignore rules"},
                "context_before":{"type":"integer", "default": 2, "description": "Lines of context before match"},
                "context_after":{"type":"integer", "default": 2, "description": "Lines of context after match"}
            }
        })
    }
    #[instrument(skip_all, fields(path = tracing::field::Empty, pattern = tracing::field::Empty, matches_count = tracing::field::Empty))]
    async fn run(&self, input: Value, ctx: ToolContext) -> serena_core::error::Result<Value> {
        let path = input
            .get("path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing path"))?;
        let pattern = input
            .get("pattern")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing pattern"))?;
        
        let recursive = input.get("recursive").and_then(|v| v.as_bool()).unwrap_or(true);
        let glob_patterns = input.get("glob").and_then(|v| v.as_array()).map(|arr| {
            arr.iter().filter_map(|v| v.as_str()).map(String::from).collect::<Vec<_>>()
        });
        let ignore_vcs = input.get("ignore_vcs").and_then(|v| v.as_bool()).unwrap_or(true);
        let context_before = input.get("context_before").and_then(|v| v.as_u64()).unwrap_or(2) as usize;
        let context_after = input.get("context_after").and_then(|v| v.as_u64()).unwrap_or(2) as usize;

        tracing::Span::current().record("path", path);
        tracing::Span::current().record("pattern", pattern);

        let re = Regex::new(pattern)
            .map_err(|e| serena_core::error::CoreError::validation(e.to_string()))?;
        
        let abs_path = ctx.cfg.workspace_root.join(path);
        let mut all_matches = Vec::new();

        if abs_path.is_file() {
            // Single file search
            if let Ok(matches) = search_file(&abs_path, &re, context_before, context_after).await {
                all_matches.extend(matches);
            }
        } else if abs_path.is_dir() {
            // Directory search
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

            for entry in walker.into_iter().filter_map(|e| e.ok()) {
                if !entry.file_type().is_file() {
                    continue;
                }

                let entry_path = entry.path();
                
                // Apply gitignore rules
                if ignore_vcs {
                    let relative_path = entry_path.strip_prefix(&abs_path).unwrap_or(entry_path);
                    if gitignore.matched(relative_path, false).is_ignore() {
                        continue;
                    }
                }

                // Apply glob patterns if specified
                if let Some(ref globs) = glob_patterns {
                    let filename = entry_path.file_name()
                        .and_then(|n| n.to_str())
                        .unwrap_or("");
                    
                    let matches_glob = globs.iter().any(|glob| {
                        globset::Glob::new(glob)
                            .map(|g| g.compile_matcher().is_match(filename))
                            .unwrap_or(false)
                    });
                    
                    if !matches_glob {
                        continue;
                    }
                }

                // Skip binary files (basic heuristic)
                if is_likely_binary(entry_path).await {
                    continue;
                }

                if let Ok(matches) = search_file(entry_path, &re, context_before, context_after).await {
                    all_matches.extend(matches);
                }
            }
        } else {
            return Err(serena_core::error::CoreError::validation(
                format!("Path does not exist: {}", path)
            ));
        }

        let matches_count = all_matches.len();
        tracing::Span::current().record("matches_count", matches_count);
        tracing::info!(path = %path, pattern = %pattern, matches_count, "search completed");
        
        Ok(serde_json::json!({ 
            "matches": all_matches,
            "total_matches": matches_count
        }))
    }
}

async fn search_file(path: &Path, re: &Regex, context_before: usize, context_after: usize) -> Result<Vec<Value>, std::io::Error> {
    let content = fs::read_to_string(path).await?;
    let lines: Vec<&str> = content.lines().collect();
    let mut matches = Vec::new();
    
    for (line_num, line) in lines.iter().enumerate() {
        for mat in re.find_iter(line) {
            let line_number = line_num + 1; // 1-based line numbers
            let column = mat.start() + 1; // 1-based column numbers
            
            // Get context lines
            let context_start = line_num.saturating_sub(context_before);
            let context_end = (line_num + context_after + 1).min(lines.len());
            
            let context_before_lines: Vec<String> = lines[context_start..line_num]
                .iter()
                .map(|s| s.to_string())
                .collect();
            
            let context_after_lines: Vec<String> = lines[line_num + 1..context_end]
                .iter()
                .map(|s| s.to_string())
                .collect();
            
            matches.push(serde_json::json!({
                "file": path.to_string_lossy(),
                "line": line_number,
                "column": column,
                "match": mat.as_str(),
                "line_text": line,
                "context_before": context_before_lines,
                "context_after": context_after_lines
            }));
        }
    }
    
    Ok(matches)
}

async fn is_likely_binary(path: &Path) -> bool {
    // Simple heuristic: check first 512 bytes for null bytes
    match fs::read(path).await {
        Ok(bytes) => {
            let sample_size = bytes.len().min(512);
            bytes[..sample_size].contains(&0)
        }
        Err(_) => true, // If we can't read it, assume it's binary
    }
}
