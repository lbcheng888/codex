use regex::RegexBuilder;
use serena_core::tools::{Tool, ToolContext};
use serde_json::Value;
use std::path::Path;
use tokio::fs;
use tracing::instrument;

pub struct Replace;

#[async_trait::async_trait]
impl Tool for Replace {
    fn name(&self) -> &'static str {
        "search_and_replace"
    }
    fn description(&self) -> &'static str {
        "Search and replace text in files using regular expressions with capture groups"
    }
    fn schema(&self) -> Value {
        serde_json::json!({
            "type":"object",
            "required":["path","search","replace"],
            "properties":{
                "path":{"type":"string", "description": "File path to perform replacement in"},
                "search":{"type":"string", "description": "Regular expression pattern to search for"},
                "replace":{"type":"string", "description": "Replacement string (supports capture groups)"},
                "multiline":{"type":"boolean", "default": false, "description": "Enable multiline mode (. matches newlines)"},
                "case_insensitive":{"type":"boolean", "default": false, "description": "Case insensitive matching"},
                "global":{"type":"boolean", "default": true, "description": "Replace all occurrences (false = replace only first)"}
            }
        })
    }
    #[instrument(skip_all, fields(path = tracing::field::Empty, search = tracing::field::Empty, replace = tracing::field::Empty, replacements_count = tracing::field::Empty, bytes_written = tracing::field::Empty))]
    async fn run(&self, input: Value, ctx: ToolContext) -> serena_core::error::Result<Value> {
        let path = input
            .get("path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing path"))?;
        let search = input
            .get("search")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing search"))?;
        let replace_str = input
            .get("replace")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing replace"))?;

        let multiline = input.get("multiline").and_then(|v| v.as_bool()).unwrap_or(false);
        let case_insensitive = input.get("case_insensitive").and_then(|v| v.as_bool()).unwrap_or(false);
        let global = input.get("global").and_then(|v| v.as_bool()).unwrap_or(true);

        tracing::Span::current().record("path", path);
        tracing::Span::current().record("search", search);
        tracing::Span::current().record("replace", replace_str);

        let abs_path = ctx.cfg.workspace_root.join(path);
        
        // Check if file exists
        if !abs_path.exists() {
            return Err(serena_core::error::CoreError::validation(
                format!("File does not exist: {}", path)
            ));
        }

        // Read original content
        let original_content = fs::read_to_string(&abs_path).await?;
        
        // Build regex with appropriate flags
        let re = RegexBuilder::new(search)
            .multi_line(multiline)
            .case_insensitive(case_insensitive)
            .dot_matches_new_line(multiline)
            .build()
            .map_err(|e| serena_core::error::CoreError::validation(e.to_string()))?;

        // Count actual matches before replacement
        let replacements_count = re.find_iter(&original_content).count();
        
        // Perform replacement
        let new_content = if global {
            re.replace_all(&original_content, replace_str).to_string()
        } else {
            // Replace only the first occurrence
            re.replace(&original_content, replace_str).to_string()
        };

        let changed = original_content != new_content;
        let bytes_written = new_content.len();

        if changed {
            // Atomic write: write to temp file first, then rename
            match write_file_atomically(&abs_path, &new_content).await {
                Ok(_) => {
                    tracing::Span::current().record("replacements_count", replacements_count);
                    tracing::Span::current().record("bytes_written", bytes_written);
                    tracing::info!(path = %path, search = %search, replace = %replace_str, replacements_count, bytes_written, changed, "replace completed successfully");
                }
                Err(e) => {
                    tracing::error!(path = %path, search = %search, replace = %replace_str, error.message = %e, "replace failed during atomic write");
                    return Err(e.into());
                }
            }
        } else {
            tracing::info!(path = %path, search = %search, replace = %replace_str, replacements_count = 0, "no changes made");
        }

        Ok(serde_json::json!({
            "replaced_count": if global { replacements_count } else { replacements_count.min(1) },
            "bytes_written": bytes_written,
            "changed": changed,
            "original_size": original_content.len(),
            "new_size": new_content.len()
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
