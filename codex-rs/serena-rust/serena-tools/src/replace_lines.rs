use serena_core::tools::{Tool, ToolContext};
use serde_json::Value;
use std::path::Path;
use tokio::fs;
use tracing::instrument;

pub struct ReplaceLines;

#[async_trait::async_trait]
impl Tool for ReplaceLines {
    fn name(&self) -> &'static str {
        "replace_lines"
    }
    
    fn schema(&self) -> Value {
        serde_json::json!({
            "type": "object",
            "required": ["path", "start_line", "content"],
            "properties": {
                "path": {"type": "string", "description": "File path to edit"},
                "start_line": {"type": "integer", "minimum": 1, "description": "Starting line number (1-based)"},
                "end_line": {"type": "integer", "minimum": 1, "description": "Ending line number (1-based), defaults to start_line"},
                "content": {"type": "string", "description": "New content to replace the specified lines"},
                "backup": {"type": "boolean", "default": false, "description": "Create a backup file before editing"}
            }
        })
    }
    
    #[instrument(skip_all, fields(path = tracing::field::Empty, start_line = tracing::field::Empty, end_line = tracing::field::Empty, lines_replaced = tracing::field::Empty))]
    async fn run(&self, input: Value, ctx: ToolContext) -> serena_core::error::Result<Value> {
        let path = input
            .get("path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing path"))?;
        
        let start_line = input
            .get("start_line")
            .and_then(|v| v.as_u64())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing start_line"))? as usize;
        
        let end_line = input
            .get("end_line")
            .and_then(|v| v.as_u64())
            .map(|v| v as usize)
            .unwrap_or(start_line);
        
        let content = input
            .get("content")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing content"))?;
        
        let backup = input.get("backup").and_then(|v| v.as_bool()).unwrap_or(false);

        tracing::Span::current().record("path", path);
        tracing::Span::current().record("start_line", start_line);
        tracing::Span::current().record("end_line", end_line);

        if start_line < 1 {
            return Err(serena_core::error::CoreError::validation("start_line must be >= 1"));
        }
        
        if end_line < start_line {
            return Err(serena_core::error::CoreError::validation("end_line must be >= start_line"));
        }

        let abs_path = ctx.cfg.workspace_root.join(path);
        
        if !abs_path.exists() {
            return Err(serena_core::error::CoreError::validation(
                format!("File does not exist: {}", path)
            ));
        }
        
        if !abs_path.is_file() {
            return Err(serena_core::error::CoreError::validation(
                format!("Path is not a file: {}", path)
            ));
        }

        // Read original content
        let original_content = fs::read_to_string(&abs_path).await?;
        let lines: Vec<&str> = original_content.lines().collect();
        
        // Check bounds
        if start_line > lines.len() {
            return Err(serena_core::error::CoreError::validation(
                format!("start_line {} is beyond file length {}", start_line, lines.len())
            ));
        }
        
        let actual_end_line = end_line.min(lines.len());
        
        // Create backup if requested
        if backup {
            let backup_path = format!("{}.bak", abs_path.to_string_lossy());
            fs::copy(&abs_path, backup_path).await?;
        }

        // Replace lines (convert to 0-based indexing)
        let start_idx = start_line - 1;
        let end_idx = actual_end_line; // exclusive
        
        let mut new_lines = Vec::new();
        new_lines.extend_from_slice(&lines[..start_idx]);
        
        // Add new content lines
        let new_content_lines: Vec<&str> = content.lines().collect();
        new_lines.extend_from_slice(&new_content_lines);
        
        new_lines.extend_from_slice(&lines[end_idx..]);
        
        let new_file_content = new_lines.join("\n");
        let lines_replaced = end_idx - start_idx;
        let lines_added = new_content_lines.len();
        
        // Write atomically
        write_file_atomically(&abs_path, &new_file_content).await?;
        
        tracing::Span::current().record("lines_replaced", lines_replaced);
        tracing::info!(path = %path, start_line, end_line = actual_end_line, lines_replaced, lines_added, "replace_lines completed");

        Ok(serde_json::json!({
            "lines_replaced": lines_replaced,
            "lines_added": lines_added,
            "original_line_count": lines.len(),
            "new_line_count": new_lines.len(),
            "start_line": start_line,
            "end_line": actual_end_line,
            "backup_created": backup
        }))
    }
}

async fn write_file_atomically(path: &Path, content: &str) -> std::io::Result<()> {
    use tokio::fs;
    
    let dir = path.parent().unwrap_or_else(|| Path::new("."));
    let temp_file = dir.join(format!(".tmp_{}", uuid::Uuid::new_v4()));
    
    // Write to temporary file
    fs::write(&temp_file, content).await?;
    
    // Atomic rename
    fs::rename(&temp_file, path).await?;
    
    Ok(())
}