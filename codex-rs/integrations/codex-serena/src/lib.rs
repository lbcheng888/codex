//! codex-serena 适配层：实现 codex-core 暴露的稳定接口（阶段 A/B 渐进集成）。

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Result;
use async_trait::async_trait;
use mcp_types::{CallToolResult, ContentBlock, TextContent, Tool, ToolInputSchema};
use serde_json::Value;

// serena-core 在所有子能力中都会用到
#[cfg(any(feature = "tools", feature = "lsp", feature = "agent"))]
use serena_core::{tools::{ToolContext, ToolRegistry}, RuntimeConfig};

// 仅当启用 tools 特性时引入 serena-tools
#[cfg(feature = "tools")]
use serena_tools::*;

use codex_core::interfaces::{ToolInvoker, LspFacade, AgentEngine, ModelClient as ModelClientTrait, ContextStore};
use codex_core::protocol::ResponseInputItem;

const SERVER_NAME: &str = "serena";
const NAME_DELIM: &str = "__"; // 与 codex-core 中保持一致

#[cfg(feature = "tools")]
pub struct SerenaToolInvoker {
    reg: ToolRegistry,
    ctx: ToolContext,
}

#[cfg(feature = "tools")]
impl SerenaToolInvoker {
    pub fn new(workspace_root: std::path::PathBuf) -> Self {
        let mut cfg = RuntimeConfig::default();
        cfg.workspace_root = workspace_root;
        let cfg = Arc::new(cfg);

        let ctx = ToolRegistry::default_context(cfg.clone());
        let reg = ToolRegistry::new();

        // 与 serena-mcp 的注册列表保持对齐
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
        // Testing utility
        reg.register(SleepTool);

        Self { reg, ctx }
    }
}

#[cfg(feature = "tools")]
#[async_trait]
impl ToolInvoker for SerenaToolInvoker {
    fn list_all_tools(&self) -> HashMap<String, Tool> {
        let mut out = HashMap::new();
        for (name, schema_val) in self.reg.list() {
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
                description: None,
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

    fn parse_tool_name(&self, tool_name: &str) -> Option<(String, String)> {
        if let Some(rest) = tool_name.strip_prefix(SERVER_NAME) {
            if let Some(tool) = rest.strip_prefix(NAME_DELIM) {
                if !tool.is_empty() {
                    return Some((SERVER_NAME.to_string(), tool.to_string()));
                }
            }
        }
        None
    }

    async fn call_tool(
        &self,
        _server: &str,
        tool: &str,
        arguments: Option<Value>,
        timeout: Option<Duration>,
    ) -> Result<CallToolResult> {
        let args = arguments.unwrap_or(Value::Null);
        let timeout_ms = timeout.map(|d| d.as_millis() as u64).unwrap_or(30_000);
        let res = self.reg.run_with_timeout(tool, args, self.ctx.clone(), timeout_ms).await;
        Ok(match res {
            Ok(v) => value_to_call_tool_result(v),
            Err(e) => CallToolResult {
                content: vec![mcp_types::ContentBlock::TextContent(TextContent {
                    annotations: None,
                    text: format!("err: {}", e.to_string()),
                    r#type: "text".to_string(),
                })],
                is_error: Some(true),
                structured_content: None,
            },
        })
    }

    async fn call_tool_stream(
        &self,
        _server: &str,
        tool: &str,
        arguments: Option<Value>,
        sink: &dyn codex_core::types::StreamingSink,
        cancel: &codex_core::types::CancelToken,
        deadline: Option<std::time::Instant>,
    ) -> Result<CallToolResult> {
        // 简单实现：直接走同步调用，但在返回前将文本写入 sink。
        // Serena 工具通常是短操作，此处满足阶段 A 需要。
        let res = self.call_tool(SERVER_NAME, tool, arguments, deadline.map(|d| d.saturating_duration_since(std::time::Instant::now()))).await?;
        for block in &res.content {
            if let mcp_types::ContentBlock::TextContent(t) = block {
                sink.on_token(&t.text);
            }
        }
        sink.on_close();
        Ok(res)
    }
}

fn value_to_call_tool_result(v: Value) -> CallToolResult {
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

// 其他接口占位实现，按特性开关导出
pub struct SerenaLspFacade;
#[async_trait]
impl LspFacade for SerenaLspFacade {}

/// SerenaAgentEngine：复用 codex 的模型客户端与 Serena 工具适配层，
/// 将函数调用优先路由到 Serena in-process 工具；同时通过 ContextStore 提供最小状态持久化能力。
pub struct SerenaAgentEngine {
    model_client: Arc<dyn ModelClientTrait>,
    tool_invoker: Arc<dyn ToolInvoker>,
    context: Arc<dyn ContextStore>,
}

impl SerenaAgentEngine {
    pub fn new(
        model_client: Arc<dyn ModelClientTrait>,
        tool_invoker: Arc<dyn ToolInvoker>,
        context: Arc<dyn ContextStore>,
    ) -> Self {
        Self { model_client, tool_invoker, context }
    }
}

#[async_trait]
impl AgentEngine for SerenaAgentEngine {
    async fn handle_function_call(
        &self,
        name: String,
        arguments: String,
        call_id: String,
    ) -> ResponseInputItem {
        // 若为 Serena 工具（serena__tool 格式），通过 Serena 适配层执行
        if let Some((_server, tool)) = self.tool_invoker.parse_tool_name(&name) {
            // 传递原始 JSON 字符串参数
            let args: Value = serde_json::from_str(&arguments).unwrap_or(Value::Null);
            let timeout = None;
            let result = self
                .tool_invoker
                .call_tool(SERVER_NAME, &tool, Some(args), timeout)
                .await;
            return ResponseInputItem::McpToolCallOutput {
                call_id,
                result: result.map_err(|e| e.to_string()),
            };
        }

        // 否则返回不支持，交由上层回退
        ResponseInputItem::FunctionCallOutput {
            call_id,
            output: codex_core::models::FunctionCallOutputPayload {
                content: format!("unsupported call: {}", name),
                success: None,
            },
        }
    }

    async fn plan(&self, goal: String) -> Result<Value> {
        // 将目标简单存入上下文，返回一个占位计划（阶段 A 最小实现）
        self.context.save("serena.plan", Value::String(goal)).await.ok();
        Ok(serde_json::json!({ "engine": "serena", "status": "planned" }))
    }

    async fn step(&self, state: Value) -> Result<Value> {
        // 记录状态，返回回显（阶段 A 最小实现）
        self.context.save("serena.state", state.clone()).await.ok();
        Ok(serde_json::json!({ "engine": "serena", "status": "stepped", "state": state }))
    }

    async fn observe(&self, feedback: Value) -> Result<()> {
        self.context.save("serena.feedback", feedback).await
    }

    async fn resume(&self, snapshot: Value) -> Result<()> {
        // 从快照恢复（热/冷启动均可通过外部传入 snapshot 完成）
        self.context.save("serena.snapshot", snapshot).await
    }
}

/// SerenaModelClient 作为占位，实际复用 codex ModelClient 通过 new() 注入。
pub struct SerenaModelClient;
impl ModelClientTrait for SerenaModelClient {}

/// SerenaContextStore：将会话状态落盘到简单的内存/文件实现由上层提供；此处保留适配实现。
pub struct SerenaContextStore;
#[async_trait]
impl ContextStore for SerenaContextStore {}
