use serena_core::tools::{Tool, ToolContext};
use serde_json::Value;
use tokio::{fs, io::AsyncWriteExt};
use tracing::instrument;

pub struct FileWrite;

/// 防止路径逃逸：与 file_read 共享实现（复制一份以避免跨文件依赖）
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
    if !out.starts_with(root) {
        return Err("path escapes workspace root".into());
    }
    Ok(out)
}

#[async_trait::async_trait]
impl Tool for FileWrite {
    fn name(&self) -> &'static str {
        "file_write"
    }
    fn description(&self) -> &'static str {
        "Write content to a file in the workspace, creating directories as needed"
    }
    fn schema(&self) -> Value {
        serde_json::json!({
            "type": "object",
            "required": ["path", "content"],
            "properties": {
                "path": {"type": "string"},
                "content": {"type": "string"}
            }
        })
    }
    #[instrument(skip_all, fields(path = tracing::field::Empty, bytes_written = tracing::field::Empty))]
    async fn run(&self, input: Value, ctx: ToolContext) -> serena_core::error::Result<Value> {
        use serena_core::error::CoreError;
        use serena_core::observability::audit_log;
        if !ctx.cfg.allow_fs_write {
            audit_log("fs_write_denied", "n/a", false, None);
            return Err(CoreError::permission_denied("file_write disabled by policy"));
        }
        let path = input
            .get("path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing path"))?;
        let content = input
            .get("content")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing content"))?;
        let abs = safe_join(&ctx.cfg.workspace_root, path).map_err(|e| CoreError::validation(e))?;
        if let Some(parent) = abs.parent() {
            fs::create_dir_all(parent).await?;
        }
        let mut f = fs::File::create(&abs).await?;
        f.write_all(content.as_bytes()).await?;
        let bytes_written = content.len();
        
        tracing::info!(path = %abs.to_string_lossy(), bytes_written, "file_write ok");
        audit_log("fs_write", &abs.to_string_lossy(), true, None);
        Ok(serde_json::json!({
            "wrote": true,
            "bytes": bytes_written
        }))
    }
}