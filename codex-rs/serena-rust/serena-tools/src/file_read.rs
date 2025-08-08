use serena_core::tools::{Tool, ToolContext};
use serde_json::Value;
use tokio::fs;
use tracing::{instrument};
pub struct FileRead;

/// 防止路径逃逸：将相对路径拼接到root并进行父目录处理，确保不越界
fn safe_join(root: &std::path::Path, rel: &str) -> Result<std::path::PathBuf, String> {
    use std::path::{Component, PathBuf};
    let mut out = PathBuf::from(root);
    for comp in std::path::Path::new(rel).components() {
        match comp {
            Component::Prefix(_) | Component::RootDir => return Err("absolute path not allowed".into()),
            Component::CurDir => {},
            Component::ParentDir => {
                if !out.pop() {
                    return Err("path escapes workspace root".into());
                }
            }
            Component::Normal(seg) => out.push(seg),
        }
    }
    // 确保仍在 root 下
    if !out.starts_with(root) {
        return Err("path escapes workspace root".into());
    }
    Ok(out)
}

#[async_trait::async_trait]
impl Tool for FileRead {
    fn name(&self) -> &'static str {
        "file_read"
    }
    fn description(&self) -> &'static str {
        "Read the contents of a file from the workspace"
    }
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
        use serena_core::error::CoreError;
        use serena_core::observability::audit_log;
        if !ctx.cfg.allow_fs_read {
            audit_log("fs_read_denied", "n/a", false, None);
            return Err(CoreError::permission_denied("file_read disabled by policy"));
        }
        let path = input
            .get("path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing path"))?;
        let abs = safe_join(&ctx.cfg.workspace_root, path).map_err(|e| CoreError::validation(e))?;
        let data = fs::read(&abs).await?;
        let text = String::from_utf8_lossy(&data).to_string();
        audit_log("fs_read", &abs.to_string_lossy(), true, None);
        Ok(serde_json::json!({ "content": text }))
        }
}