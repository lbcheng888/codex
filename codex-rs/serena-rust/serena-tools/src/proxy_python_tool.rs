use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::process::Stdio;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::process::Command;

use serena_core::tools::{Tool, ToolContext};
use serena_core::error::Result;

#[derive(Debug, Clone)]
pub struct ProxyPythonTool;

#[derive(Debug, Serialize, Deserialize)]
struct ProxyPythonArgs {
    /// 要执行的命令（例如 "python3" 或 绝对路径）
    command: String,
    /// 传给命令的参数列表（例如 ["script.py", "--flag"]）
    #[serde(default)]
    args: Vec<String>,
    /// 作为 stdin 传入子进程的 JSON 值
    #[serde(default)]
    input: Value,
    /// 可选：工作目录（若不提供则沿用当前进程 cwd）
    #[serde(default)]
    cwd: Option<String>,
    /// 可选：环境变量表（字符串映射）
    #[serde(default)]
    env: Option<serde_json::Map<String, Value>>,
}

#[async_trait]
impl Tool for ProxyPythonTool {
    fn name(&self) -> &'static str {
        "proxy_python_tool"
    }

    fn description(&self) -> &'static str {
        "Run a Python (or arbitrary) subprocess: executes command + args; writes input JSON to stdin; reads stdout and parses JSON result (fallback to text)."
    }

    fn schema(&self) -> Value {
        // 与 list_mcp_format 对齐，确保 object 且含 required
        json!({
            "type": "object",
            "properties": {
                "command": { "type": "string", "description": "Executable to run (e.g., python3)" },
                "args": {
                    "type": "array",
                    "items": { "type": "string" },
                    "description": "Arguments for the executable"
                },
                "input": {
                    "description": "JSON value to pass to child process via stdin",
                    "oneOf": [
                        { "type": "object" },
                        { "type": "array" },
                        { "type": "string" },
                        { "type": "number" },
                        { "type": "boolean" },
                        { "type": "null" }
                    ]
                },
                "cwd": { "type": "string", "description": "Optional working directory" },
                "env": {
                    "type": "object",
                    "additionalProperties": {
                        "oneOf": [
                            { "type": "string" },
                            { "type": "number" },
                            { "type": "boolean" },
                            { "type": "null" }
                        ]
                    },
                    "description": "Optional environment variables map"
                }
            },
            "required": ["command"]
        })
    }

    async fn run(&self, args: Value, _ctx: ToolContext) -> Result<Value> {
        use serena_core::error::CoreError;

        let parsed: ProxyPythonArgs = serde_json::from_value(args)
            .map_err(|e| CoreError::validation(format!("invalid arguments: {}", e)))?;

        // 构建命令
        let mut cmd = Command::new(&parsed.command);
        cmd.args(&parsed.args);
        cmd.stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        if let Some(cwd) = &parsed.cwd {
            cmd.current_dir(cwd);
        }
        if let Some(env_map) = &parsed.env {
            for (k, v) in env_map {
                let val = if v.is_string() {
                    v.as_str().unwrap().to_string()
                } else {
                    v.to_string()
                };
                cmd.env(k, val);
            }
        }

        let mut child = cmd
            .spawn()
            .map_err(|e| CoreError::internal(format!("failed to spawn process: {}", e)))?;

        // 写入 stdin
        if let Some(mut stdin) = child.stdin.take() {
            let mut payload = serde_json::to_vec(&parsed.input)
                .map_err(|e| CoreError::validation(format!("failed to serialize input to JSON: {}", e)))?;
            payload.push(b'\n'); // 换行便于子进程逐行读取
            if let Err(e) = stdin.write_all(&payload).await {
                return Err(CoreError::internal(format!("failed to write stdin: {}", e)));
            }
        }

        // 读取 stdout 与 stderr
        let mut stdout = child
            .stdout
            .take()
            .ok_or_else(|| CoreError::internal(String::from("missing stdout from child process")))?;
        let mut stderr = child
            .stderr
            .take()
            .ok_or_else(|| CoreError::internal(String::from("missing stderr from child process")))?;

        let mut out_buf = Vec::new();
        let mut err_buf = Vec::new();

        let read_out = async { stdout.read_to_end(&mut out_buf).await };
        let read_err = async { stderr.read_to_end(&mut err_buf).await };

        let (_ro, _re) = tokio::join!(read_out, read_err);

        // 等待进程退出
        let status = child
            .wait()
            .await
            .map_err(|e| CoreError::internal(format!("failed to wait child: {}", e)))?;

        // stderr 内容用于调试或错误返回
        let stderr_text = String::from_utf8_lossy(&err_buf).to_string();

        // 优先尝试解析 stdout 为 JSON
        let content_value = match serde_json::from_slice::<Value>(&out_buf) {
            Ok(json_val) => json_val,
            Err(_) => {
                // 非 JSON 输出时，包装为文本
                Value::String(String::from_utf8_lossy(&out_buf).to_string())
            }
        };

        if !status.success() {
            return Err(CoreError::internal(format!(
                "child process exited with status {}: {}",
                status.code().unwrap_or(-1),
                stderr_text
            )));
        }

        // 直接返回 content 结构；上层会保持该结构
        Ok(json!({
            "content": [{
                "type": "json",
                "data": content_value
            }],
            "stderr": if stderr_text.is_empty() { Value::Null } else { Value::String(stderr_text) }
        }))
    }
}