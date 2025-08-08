use serde_json::{json, Value};
use serena_core::tools::{Tool, ToolContext};
use serena_core::error::{Result, CoreError};
use std::process::Stdio;
use std::time::Duration;
use tokio::process::Command as TokioCommand;
use tokio::time::timeout;
use tracing::debug;

/// Tool for executing shell commands
pub struct ExecuteShellCommand;

/// 与文件工具共享的安全拼接
fn safe_join(root: &std::path::Path, rel_or_abs: &str) -> std::result::Result<std::path::PathBuf, String> {
    use std::path::{Component, Path, PathBuf};
    let rel_path = if Path::new(rel_or_abs).is_absolute() {
        // 拒绝绝对路径，改为相对根目录
        Path::new(rel_or_abs)
            .strip_prefix("/").unwrap_or(Path::new(""))
            .to_path_buf()
    } else {
        PathBuf::from(rel_or_abs)
    };
    let mut out = PathBuf::from(root);
    for comp in rel_path.components() {
        match comp {
            Component::Prefix(_) | Component::RootDir => return Err("absolute path not allowed".into()),
            Component::CurDir => {},
            Component::ParentDir => { if !out.pop() { return Err("path escapes workspace root".into()); } }
            Component::Normal(seg) => out.push(seg),
        }
    }
    if !out.starts_with(root) { return Err("path escapes workspace root".into()); }
    Ok(out)
}

#[async_trait::async_trait]
impl Tool for ExecuteShellCommand {
    fn name(&self) -> &'static str {
        "execute_shell_command"
    }

    fn description(&self) -> &'static str {
        "Execute a shell command in the workspace directory and return the output"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "required": ["command"],
            "properties": {
                "command": {
                    "type": "string",
                    "description": "The shell command to execute"
                },
                "timeout_seconds": {
                    "type": "integer",
                    "description": "Maximum time to wait for command completion (default: 30)",
                    "default": 30
                },
                "working_directory": {
                    "type": "string",
                    "description": "Working directory for the command (default: workspace root)"
                },
                "capture_stderr": {
                    "type": "boolean",
                    "description": "Whether to capture stderr output (default: true)",
                    "default": true
                }
            }
        })
    }

    async fn run(&self, input: Value, ctx: ToolContext) -> Result<Value> {
        use serena_core::observability::audit_log;
        use serena_core::error::CoreError;
        if !ctx.cfg.allow_process_execution {
            audit_log("process_exec_denied", "execute_shell_command", false, None);
            return Err(CoreError::permission_denied("process execution disabled by policy"));
        }

        let command = input["command"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("command is required".to_string()))?;

        // 校验白名单：按程序名（管道前第一个token）
        let program = command.split_whitespace().next().unwrap_or("");
        if !ctx.cfg.process_whitelist.is_empty() && !ctx.cfg.process_whitelist.iter().any(|p| p == program) {
            audit_log("process_exec_denied", program, false, Some(&json!({"reason":"not in whitelist"})));
            return Err(CoreError::permission_denied(format!("program '{}' not allowed by whitelist", program)));
        }

        let timeout_secs = input["timeout_seconds"].as_u64().unwrap_or(30);
        let requested_wd = input["working_directory"].as_str().map(|s| s.to_string());
        let working_dir = if ctx.cfg.sandbox_mode {
            // 沙箱模式下强制工作目录为 workspace_root
            ctx.cfg.workspace_root.to_string_lossy().to_string()
        } else {
            requested_wd.unwrap_or_else(|| ctx.cfg.workspace_root.to_string_lossy().to_string())
        };
        // 校验目录不越界
        let wd_path = safe_join(&ctx.cfg.workspace_root, &working_dir).map_err(|e| CoreError::validation(e))?;

        let capture_stderr = input["capture_stderr"].as_bool().unwrap_or(true);

        // 隐私脱敏：在日志中避免原始命令明文
        let (log_command, log_working_dir) = if ctx.cfg.privacy_redact {
            let salt = ctx.cfg.privacy_salt.as_deref();
            let cmd_hash = serena_core::observability::redact(command, salt);
            let wd_short = wd_path
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or(".")
                .to_string();
            (format!("hash:{}", cmd_hash), wd_short)
        } else {
            (command.to_string(), wd_path.to_string_lossy().to_string())
        };
        debug!(target = "command", command = %log_command, working_dir = %log_working_dir, "executing shell command");

        let mut cmd = TokioCommand::new("sh");
        cmd.arg("-c")
           .arg(command)
           .current_dir(&wd_path)
           .stdout(Stdio::piped());

        if capture_stderr {
            cmd.stderr(Stdio::piped());
        } else {
            cmd.stderr(Stdio::null());
        }

        let child = cmd.spawn()
            .map_err(|e| CoreError::io(format!("Failed to spawn command: {}", e)))?;

        let output = timeout(Duration::from_secs(timeout_secs), child.wait_with_output())
            .await
            .map_err(|_| CoreError::timeout(format!("Command timed out after {} seconds", timeout_secs), timeout_secs * 1000))?
            .map_err(|e| CoreError::io(format!("Failed to execute command: {}", e)))?;

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        let exit_code = output.status.code().unwrap_or(-1);

        let success = output.status.success();
        audit_log("process_exec", program, success, Some(&json!({"exit_code": exit_code})));

        Ok(json!({
            "stdout": stdout,
            "stderr": stderr,
            "exit_code": exit_code,
            "success": success,
            "command": command,
            "working_directory": wd_path
        }))
    }
}

/// Tool for restarting language server
pub struct RestartLanguageServer;

#[async_trait::async_trait]
impl Tool for RestartLanguageServer {
    fn name(&self) -> &'static str {
        "restart_language_server"
    }

    fn description(&self) -> &'static str {
        "Restart the language server for better code analysis (may be necessary if it hangs)"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {
                "language": {
                    "type": "string",
                    "description": "Specific language server to restart (optional)"
                }
            }
        })
    }

    async fn run(&self, input: Value, _ctx: ToolContext) -> Result<Value> {
        let language = input["language"].as_str();
        
        debug!(target = "lsp", language = ?language, "restarting language server");

        // For now, return a placeholder response since LSP manager integration is not complete
        if let Some(lang) = language {
            Ok(json!({
                "message": format!("Would restart language server for {}", lang),
                "language": lang,
                "status": "placeholder - LSP integration pending"
            }))
        } else {
            Ok(json!({
                "message": "Would restart all language servers",
                "status": "placeholder - LSP integration pending"
            }))
        }
    }
}
