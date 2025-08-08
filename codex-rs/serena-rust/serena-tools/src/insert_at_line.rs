use serena_core::tools::{Tool, ToolContext};
use serde_json::Value;
use std::path::Path;
use tokio::fs;
use tracing::instrument;

pub struct InsertAtLine;

#[async_trait::async_trait]
impl Tool for InsertAtLine {
    fn name(&self) -> &'static str {
        "insert_at_line"
    }
    
    fn schema(&self) -> Value {
        serde_json::json!({
            "type": "object",
            "required": ["path", "line", "content"],
            "properties": {
                "path": {"type": "string", "description": "File path to edit"},
                "line": {"type": "integer", "minimum": 1, "description": "Line number (1-based) to insert at"},
                "content": {"type": "string", "description": "Content to insert"},
                "position": {"type": "string", "enum": ["before", "after"], "default": "before", "description": "Insert before or after the specified line"},
                "backup": {"type": "boolean", "default": false, "description": "Create a backup file before editing"}
            }
        })
    }
    
    #[instrument(skip_all, fields(path = tracing::field::Empty, line = tracing::field::Empty, position = tracing::field::Empty, lines_added = tracing::field::Empty))]
    async fn run(&self, input: Value, ctx: ToolContext) -> serena_core::error::Result<Value> {
        let path = input
            .get("path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing path"))?;
        
        let line = input
            .get("line")
            .and_then(|v| v.as_u64())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing line"))? as usize;
        
        let content = input
            .get("content")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing content"))?;
        
        let position = input
            .get("position")
            .and_then(|v| v.as_str())
            .unwrap_or("before");
        
        let backup = input.get("backup").and_then(|v| v.as_bool()).unwrap_or(false);

        tracing::Span::current().record("path", path);
        tracing::Span::current().record("line", line);
        tracing::Span::current().record("position", position);

        if line < 1 {
            return Err(serena_core::error::CoreError::validation("line must be >= 1"));
        }
        
        if position != "before" && position != "after" {
            return Err(serena_core::error::CoreError::validation("position must be 'before' or 'after'"));
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
        
        // Check bounds - allow inserting at end of file (line > lines.len())
        if line > lines.len() + 1 {
            return Err(serena_core::error::CoreError::validation(
                format!("line {} is too far beyond file length {}", line, lines.len())
            ));
        }
        
        // Create backup if requested
        if backup {
            let backup_path = format!("{}.bak", abs_path.to_string_lossy());
            fs::copy(&abs_path, backup_path).await?;
        }

        // Calculate insertion index (0-based)
        let insert_idx = if position == "before" {
            line - 1
        } else {
            // after
            line
        };
        
        let mut new_lines = Vec::new();
        
        // Add lines before insertion point
        if insert_idx <= lines.len() {
            new_lines.extend_from_slice(&lines[..insert_idx]);
        } else {
            new_lines.extend_from_slice(&lines);
        }
        
        // Add new content lines
        let new_content_lines: Vec<&str> = content.lines().collect();
        new_lines.extend_from_slice(&new_content_lines);
        
        // Add remaining lines
        if insert_idx < lines.len() {
            new_lines.extend_from_slice(&lines[insert_idx..]);
        }
        
        let new_file_content = new_lines.join("\n");
        let lines_added = new_content_lines.len();
        
        // Write atomically
        write_file_atomically(&abs_path, &new_file_content).await?;
        
        tracing::Span::current().record("lines_added", lines_added);
        tracing::info!(path = %path, line, position, lines_added, "insert_at_line completed");

        Ok(serde_json::json!({
            "lines_added": lines_added,
            "original_line_count": lines.len(),
            "new_line_count": new_lines.len(),
            "insert_line": line,
            "position": position,
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