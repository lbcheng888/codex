//! codex-serena 适配层：实现 codex-core 暴露的稳定接口（阶段 A/B 渐进集成）。

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Result;
use async_trait::async_trait;
use mcp_types::{CallToolResult, ContentBlock, TextContent, Tool, ToolInputSchema};
use serde_json::Value;
use serena_core::{tools::{ToolContext, ToolRegistry}, RuntimeConfig};
use serena_tools::*;

use codex_core::interfaces::{ToolInvoker, LspFacade, AgentEngine, ModelClient as ModelClientTrait, ContextStore};
use codex_core::protocol::ResponseInputItem;

const SERVER_NAME: &str = "serena";
const NAME_DELIM: &str = "__"; // 与 codex-core 中保持一致

pub struct SerenaToolInvoker {
    reg: ToolRegistry,
    ctx: ToolContext,
}

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

// 其他接口占位实现，后续阶段按需填充
pub struct SerenaLspFacade;
#[async_trait]
impl LspFacade for SerenaLspFacade {}

pub struct SerenaAgentEngine;
#[async_trait]
impl AgentEngine for SerenaAgentEngine {
    async fn handle_function_call(
        &self,
        _name: String,
        _arguments: String,
        call_id: String,
    ) -> ResponseInputItem {
        ResponseInputItem::FunctionCallOutput {
            call_id,
            output: codex_core::models::FunctionCallOutputPayload {
                content: "not implemented".to_string(),
                success: None,
            },
        }
    }
}

pub struct SerenaModelClient;
impl ModelClientTrait for SerenaModelClient {}

pub struct SerenaContextStore;
#[async_trait]
impl ContextStore for SerenaContextStore {}

