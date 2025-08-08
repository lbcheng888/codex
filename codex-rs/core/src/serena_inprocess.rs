use std::collections::HashMap;
use std::sync::{Arc, OnceLock};
use std::time::Duration;

use mcp_types::{CallToolResult, ContentBlock, TextContent, Tool, ToolInputSchema};
use serde_json::Value;
use serena_core::{tools::{ToolContext, ToolRegistry}, RuntimeConfig};
use serena_tools::*;
use crate::interfaces::ToolInvoker;
use crate::types::{CancelToken, StreamingSink, ToolSpec};
use async_trait::async_trait;

const SERVER_NAME: &str = "serena";
const NAME_DELIM: &str = "__"; // Keep consistent with McpConnectionManager

pub struct SerenaInProcessManager {
    reg: ToolRegistry,
    ctx: ToolContext,
}

// 全局并发控制：按工具名维度的信号量缓存
static SEM_MAP: OnceLock<parking_lot::RwLock<HashMap<String, Arc<tokio::sync::Semaphore>>>> = OnceLock::new();

impl SerenaInProcessManager {
    pub fn new(workspace_root: std::path::PathBuf) -> Self {
        let mut cfg = RuntimeConfig::default();
        cfg.workspace_root = workspace_root;
        let cfg = Arc::new(cfg);

        let ctx = ToolRegistry::default_context(cfg.clone());

        let reg = ToolRegistry::new();
        // Register tools (mirror serena-mcp server registry)
        reg.register(FileRead);
        reg.register(FileWrite);
        reg.register(Search);
        reg.register(FileExists);
        reg.register(Replace);
        reg.register(ListDir);
        reg.register(FileList);
        reg.register(FindFile);
        reg.register(DeleteLines);
        reg.register(ReplaceLines);
        reg.register(InsertAtLine);
        // User/memory
        reg.register(UserCreate);
        reg.register(UserGet);
        reg.register(ReadMemory);
        reg.register(WriteMemory);
        reg.register(ListMemories);
        reg.register(DeleteMemory);
        // Symbol/LSP tools
        reg.register(GetSymbolsOverview);
        reg.register(FindSymbol);
        reg.register(FindReferencingSymbols);
        reg.register(ReplaceSymbolBody);
        reg.register(InsertAfterSymbol);
        reg.register(InsertBeforeSymbol);
        reg.register(RestartLanguageServerAll);
        reg.register(RestartLanguageServerLanguage);
        // Config tools
        reg.register(ActivateProject);
        reg.register(SwitchModes);
        reg.register(CheckOnboardingPerformed);
        reg.register(Onboarding);
        reg.register(RemoveProject);
        reg.register(GetCurrentConfig);
        // Deterministic testing utility tool
        reg.register(SleepTool);

        Self { reg, ctx }
    }

    pub fn list_all_tools(&self) -> HashMap<String, Tool> {
        let mut out = HashMap::new();
        for (name, schema_val) in self.reg.list() {
            // Best effort to populate ToolInputSchema; leave properties/required as-is
            let input_schema = ToolInputSchema {
                properties: schema_val
                    .as_object()
                    .and_then(|o| o.get("properties").cloned()),
                required: schema_val
                    .as_object()
                    .and_then(|o| o.get("required"))
                    .and_then(|v| serde_json::from_value::<Vec<String>>(v.clone()).ok()),
                r#type: "object".to_string(),
            };
            let tool = Tool {
                annotations: None,
                description: None, // ToolRegistry::list() does not expose description
                input_schema,
                name: name.clone(),
                output_schema: None,
                title: None,
            };
            let fq_name = format!("{}{}{}", SERVER_NAME, NAME_DELIM, name);
            out.insert(fq_name, tool);
        }
        out
    }

    pub fn parse_tool_name(&self, tool_name: &str) -> Option<(String, String)> {
        // Expect format: "serena__<tool>"
        if let Some(rest) = tool_name.strip_prefix(SERVER_NAME) {
            if let Some(tool) = rest.strip_prefix(NAME_DELIM) {
                if !tool.is_empty() {
                    return Some((SERVER_NAME.to_string(), tool.to_string()));
                }
            }
        }
        None
    }

    pub async fn call_tool(
        &self,
        _server: &str,
        tool: &str,
        arguments: Option<Value>,
        timeout: Option<Duration>,
    ) -> anyhow::Result<CallToolResult> {
        let args = arguments.unwrap_or(Value::Null);
        let timeout_ms = timeout
            .map(|d| d.as_millis() as u64)
            .unwrap_or(30_000);

        // 获取或创建该工具的信号量（并发限制）
        let sem = {
            let map_lock = SEM_MAP.get_or_init(|| parking_lot::RwLock::new(HashMap::new()));
            if let Some(s) = map_lock.read().get(tool) {
                s.clone()
            } else {
                let mut w = map_lock.write();
                // 从环境读取并发：SERENA_MCP_TOOL_CONCURRENCY__<tool> 或 SERENA_MCP_MAX_CONCURRENCY
                let per_tool_key = format!("SERENA_MCP_TOOL_CONCURRENCY__{}", tool.to_uppercase());
                let max_key = "SERENA_MCP_MAX_CONCURRENCY".to_string();
                let permits = std::env::var(&per_tool_key)
                    .ok()
                    .and_then(|v| v.parse::<usize>().ok())
                    .or_else(|| std::env::var(&max_key).ok().and_then(|v| v.parse::<usize>().ok()))
                    .unwrap_or(8);
                let s = Arc::new(tokio::sync::Semaphore::new(permits.max(1)));
                w.insert(tool.to_string(), s.clone());
                s
            }
        };

        // 获取许可
        let _permit = sem.acquire_owned().await?;

        let res = self
            .reg
            .run_with_timeout(tool, args, self.ctx.clone(), timeout_ms)
            .await;
        match res {
            Ok(v) => Ok(Self::value_to_call_tool_result(v)),
            Err(e) => Ok(CallToolResult {
                content: vec![ContentBlock::TextContent(TextContent {
                    annotations: None,
                    // 模拟 JSON-RPC 错误码对齐（文本中体现 code 与 message）
                    text: format!("code=-32000 message={} tool={} timeout_ms={}", e.to_string(), tool, timeout_ms),
                    r#type: "text".to_string(),
                })],
                is_error: Some(true),
                structured_content: None,
            }),
        }
    }

    fn value_to_call_tool_result(v: Value) -> CallToolResult {
        // For now, encode the JSON as text. This is sufficient for Codex to surface tool outputs.
        let text = match v {
            Value::String(s) => s,
            _ => v.to_string(),
        };
        CallToolResult {
            content: vec![ContentBlock::TextContent(TextContent {
                annotations: None,
                text,
                r#type: "text".to_string(),
            })],
            is_error: None,
            structured_content: None,
        }
    }
}

#[async_trait]
impl ToolInvoker for SerenaInProcessManager {
    fn list_all_tools(&self) -> std::collections::HashMap<String, mcp_types::Tool> {
        SerenaInProcessManager::list_all_tools(self)
    }
    fn parse_tool_name(&self, tool_name: &str) -> Option<(String, String)> {
        SerenaInProcessManager::parse_tool_name(self, tool_name)
    }
    async fn call_tool(
        &self,
        server: &str,
        tool: &str,
        arguments: Option<serde_json::Value>,
        timeout: Option<std::time::Duration>,
    ) -> anyhow::Result<mcp_types::CallToolResult> {
        SerenaInProcessManager::call_tool(self, server, tool, arguments, timeout).await
    }

    fn list_specs(&self) -> std::collections::HashMap<String, ToolSpec> {
        let mut out = std::collections::HashMap::new();
        for (name, schema_val) in self.reg.list() {
            let spec = ToolSpec {
                name: format!("{}{}{}", SERVER_NAME, NAME_DELIM, name),
                description: None,
                input_schema: schema_val.clone(),
                output_schema: None,
                title: None,
            };
            out.insert(spec.name.clone(), spec);
        }
        out
    }

    async fn call_tool_stream(
        &self,
        server: &str,
        tool: &str,
        arguments: Option<serde_json::Value>,
        sink: &dyn StreamingSink,
        cancel: &CancelToken,
        deadline: Option<std::time::Instant>,
    ) -> anyhow::Result<mcp_types::CallToolResult> {
        // 支持取消与超时：在独立任务中执行，select 监听取消/截止时间，必要时 abort
        if cancel.is_cancelled() {
            return Ok(mcp_types::CallToolResult { content: vec![], is_error: Some(true), structured_content: None });
        }
        let timeout = deadline.map(|d| d.saturating_duration_since(std::time::Instant::now()));
        let server_s = server.to_string();
        let tool_s = tool.to_string();
        let args = arguments.clone();
        let handle = tokio::spawn(async move {
            SerenaInProcessManager::call_tool(self, &server_s, &tool_s, args, timeout).await
        });

        let mut cancelled = false;
        let res = tokio::select! {
            r = handle => {
                match r {
                    Ok(inner) => inner,
                    Err(e) => Err(anyhow::anyhow!(e)),
                }
            }
            _ = async {
                // 轮询取消或截止
                loop {
                    if cancel.is_cancelled() { break; }
                    if let Some(dl) = deadline { if std::time::Instant::now() >= dl { break; } }
                    tokio::time::sleep(std::time::Duration::from_millis(20)).await;
                }
            } => {
                cancelled = true;
                handle.abort();
                Err(anyhow::anyhow!("tool call cancelled or timed out"))
            }
        };

        match res {
            Ok(res) => {
                for block in &res.content {
                    if let mcp_types::ContentBlock::TextContent(t) = block { sink.on_token(&t.text); }
                }
                sink.on_close();
                Ok(res)
            }
            Err(e) => {
                sink.on_close();
                Ok(mcp_types::CallToolResult {
                    content: vec![mcp_types::ContentBlock::TextContent(mcp_types::TextContent {
                        annotations: None,
                        text: if cancelled { "err: cancelled".to_string() } else { format!("err: {}", e) },
                        r#type: "text".to_string(),
                    })],
                    is_error: Some(true),
                    structured_content: None,
                })
            }
        }
    }
}

