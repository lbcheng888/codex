use serena_core::tools::{Tool, ToolContext};
use serde_json::Value;
use tracing::instrument;
use walkdir::WalkDir;
use ignore::gitignore::Gitignore;
use globset::{Glob, GlobSetBuilder};

pub struct ListDir;

#[async_trait::async_trait]
impl Tool for ListDir {
    fn name(&self) -> &'static str {
        "list_dir"
    }

    fn description(&self) -> &'static str {
        "List directory contents with optional filtering, recursion, and file type information"
    }

    fn schema(&self) -> Value {
        serde_json::json!({
            "type": "object",
            "required": ["path"],
            "properties": {
                "path": {"type": "string", "description": "Directory path to list"},
                "recursive": {"type": "boolean", "default": false, "description": "List contents recursively"},
                "glob": {"type": "array", "items": {"type": "string"}, "description": "Glob patterns to filter files"},
                "ignore_vcs": {"type": "boolean", "default": true, "description": "Respect .gitignore rules"},
                "include_size": {"type": "boolean", "default": false, "description": "Include file sizes"},
                "include_type": {"type": "boolean", "default": true, "description": "Include file/directory type"}
            }
        })
    }
    
    #[instrument(skip_all, fields(path = tracing::field::Empty, items_count = tracing::field::Empty))]
    async fn run(&self, input: Value, ctx: ToolContext) -> serena_core::error::Result<Value> {
        let path = input
            .get("path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing path"))?;
        
        let recursive = input.get("recursive").and_then(|v| v.as_bool()).unwrap_or(false);
        let glob_patterns = input.get("glob").and_then(|v| v.as_array()).map(|arr| {
            arr.iter().filter_map(|v| v.as_str()).map(String::from).collect::<Vec<_>>()
        });
        let ignore_vcs = input.get("ignore_vcs").and_then(|v| v.as_bool()).unwrap_or(true);
        let include_size = input.get("include_size").and_then(|v| v.as_bool()).unwrap_or(false);
        let include_type = input.get("include_type").and_then(|v| v.as_bool()).unwrap_or(true);

        tracing::Span::current().record("path", path);

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

        // Build glob set if patterns provided
        let glob_set = if let Some(ref patterns) = glob_patterns {
            let mut builder = GlobSetBuilder::new();
            for pattern in patterns {
                if let Ok(glob) = Glob::new(pattern) {
                    builder.add(glob);
                }
            }
            builder.build().ok()
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

        let mut items = Vec::new();

        for entry in walker.into_iter().filter_map(|e| e.ok()) {
            let entry_path = entry.path();
            
            // Skip the root directory itself
            if entry_path == abs_path {
                continue;
            }

            // Apply gitignore rules
            if ignore_vcs {
                let relative_path = entry_path.strip_prefix(&abs_path).unwrap_or(entry_path);
                if gitignore.matched(relative_path, entry.file_type().is_dir()).is_ignore() {
                    continue;
                }
            }

            // Apply glob patterns if specified
            if let Some(ref glob_set) = glob_set {
                let filename = entry_path.file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("");
                
                if !glob_set.is_match(filename) {
                    continue;
                }
            }

            let mut item = serde_json::json!({
                "name": entry_path.file_name().and_then(|n| n.to_str()).unwrap_or(""),
                "path": entry_path.strip_prefix(&ctx.cfg.workspace_root)
                    .unwrap_or(entry_path)
                    .to_string_lossy()
            });

            if include_type {
                let file_type = if entry.file_type().is_dir() {
                    "directory"
                } else if entry.file_type().is_file() {
                    "file"
                } else {
                    "other"
                };
                item["type"] = Value::String(file_type.to_string());
            }

            if include_size && entry.file_type().is_file() {
                if let Ok(metadata) = entry.metadata() {
                    item["size"] = Value::Number(metadata.len().into());
                }
            }

            items.push(item);
        }

        let items_count = items.len();
        tracing::Span::current().record("items_count", items_count);
        tracing::info!(path = %path, items_count, recursive, "list_dir completed");

        Ok(serde_json::json!({
            "items": items,
            "count": items_count,
            "path": path
        }))
    }
}