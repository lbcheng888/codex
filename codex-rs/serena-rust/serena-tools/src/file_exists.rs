use serena_core::tools::{Tool, ToolContext};
use serde_json::Value;
use tokio::fs;
use tracing::instrument;

pub struct FileExists;

#[async_trait::async_trait]
impl Tool for FileExists {
    fn name(&self) -> &'static str { "file_exists" }
    fn description(&self) -> &'static str { "Check whether a path exists in the workspace" }
    fn schema(&self) -> Value {
        serde_json::json!({
            "type": "object",
            "required": ["path"],
            "properties": {
                "path": {"type": "string"}
            }
        })
    }
    #[instrument(skip_all, fields(path))]
    async fn run(&self, input: Value, ctx: ToolContext) -> serena_core::error::Result<Value> {
        let path = input.get("path").and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing path"))?;
        let abs = ctx.cfg.workspace_root.join(path);
        match fs::metadata(&abs).await {
            Ok(md) => Ok(serde_json::json!({ "exists": true, "is_dir": md.is_dir() })),
            Err(_) => Ok(serde_json::json!({ "exists": false, "is_dir": false })),
        }
    }
}
