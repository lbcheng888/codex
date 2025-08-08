//! 稳定接口层：定义 codex-core 与外部集成的抽象边界。
//! 这些接口由适配层（例如 codex-serena）实现，以实现单向依赖。

use std::collections::HashMap;
use std::time::{Duration, Instant};

use anyhow::Result;
use async_trait::async_trait;
use mcp_types::{CallToolResult, Tool};
use serde_json::Value;

use crate::models::ResponseInputItem;
use crate::types::{CancelToken, StreamingSink, ToolSpec};

/// 工具调用抽象：封装工具注册可见性、调用、取消、流式输出
#[async_trait]
pub trait ToolInvoker: Send + Sync {
    /// 列出所有可见工具（向上层提供稳定描述）
    fn list_all_tools(&self) -> HashMap<String, Tool>;

    /// 以稳定 ToolSpec 描述（与上面 Tool 兼容，供非 OpenAI 场景）
    fn list_specs(&self) -> HashMap<String, ToolSpec> { HashMap::new() }

    /// 解析函数名为 (server, tool_name)。若不符合该 invoker 的命名规范则返回 None
    fn parse_tool_name(&self, tool_name: &str) -> Option<(String, String)>;

    /// 一次性调用（非流式）。若提供 timeout 则为后端的总超时上限。
    async fn call_tool(
        &self,
        server: &str,
        tool: &str,
        arguments: Option<Value>,
        timeout: Option<Duration>,
    ) -> Result<CallToolResult>;

    /// 流式调用：将文本 token/事件推送到 sink。可通过 cancel 取消，或到达 deadline 自动结束。
    async fn call_tool_stream(
        &self,
        server: &str,
        tool: &str,
        arguments: Option<Value>,
        sink: &dyn StreamingSink,
        cancel: &CancelToken,
        deadline: Option<Instant>,
    ) -> Result<CallToolResult>;
}

/// LSP 外观接口：抽象语言服务相关能力。
/// 为避免对具体 LSP/Serena 类型的耦合，既提供结构化编辑类型，也允许 JSON 直通。
#[async_trait]
pub trait LspFacade: Send + Sync {
    async fn restart_all(&self) -> Result<()> { Ok(()) }
    async fn restart_language(&self, _language: &str) -> Result<()> { Ok(()) }

    // 查询相关（保持 JSON 以便透传）
    async fn get_symbols_overview(&self, _path: &str) -> Result<Value> { Ok(Value::Null) }
    async fn find_symbol(&self, _name_path: &str, _relative_path: Option<&str>) -> Result<Value> { Ok(Value::Null) }
    async fn find_referencing_symbols(&self, _name_path: &str, _relative_path: &str) -> Result<Value> { Ok(Value::Null) }

    // 编辑相关（提供结构化 WorkspaceEdit 返回值）
    async fn apply_workspace_edit_json(&self, _edit: Value) -> Result<Value> { Ok(Value::Null) }
}

/// Agent 引擎抽象：在统一接口下执行 plan/step，支持观察反馈与中断恢复。
#[async_trait]
pub trait AgentEngine: Send + Sync {
    /// 处理一次函数调用，返回应回给模型的 ResponseInputItem。
    async fn handle_function_call(
        &self,
        name: String,
        arguments: String,
        call_id: String,
    ) -> ResponseInputItem;

    /// 生成计划
    async fn plan(&self, _goal: String) -> Result<Value> { Ok(Value::Null) }
    /// 执行一步
    async fn step(&self, _state: Value) -> Result<Value> { Ok(Value::Null) }
    /// 观察反馈（如用户/环境反馈）
    async fn observe(&self, _feedback: Value) -> Result<()> { Ok(()) }
    /// 从中断状态恢复
    async fn resume(&self, _snapshot: Value) -> Result<()> { Ok(()) }
}

/// 模型客户端抽象：封装与大模型提供方交互的能力（统一到 codex-rs 的流式输出）。
pub trait ModelClient: Send + Sync {
    /// 用于决定断线重试策略的最大重试次数
    fn stream_max_retries(&self) -> usize { 0 }

    /// 流式对话/补全接口：将 token 送入 sink，通过 cancel/deadline 控制生命周期。
    fn stream_chat(
        &self,
        _input: Value,
        _sink: &dyn crate::types::StreamingSink,
        _cancel: &crate::types::CancelToken,
        _deadline: Option<std::time::Instant>,
    ) -> Result<()> { Ok(()) }
}

/// 上下文存储：对会话状态、文件快照、索引引用进行抽象。
#[async_trait]
pub trait ContextStore: Send + Sync {
    // 会话状态
    async fn save(&self, _key: &str, _value: Value) -> Result<()> { Ok(()) }
    async fn load(&self, _key: &str) -> Result<Option<Value>> { Ok(None) }

    // 文件快照（可选实现）
    async fn save_file_snapshot(&self, _path: &str, _content: &str) -> Result<()> { Ok(()) }
    async fn load_file_snapshot(&self, _path: &str) -> Result<Option<String>> { Ok(None) }

    // 索引引用（可选实现）
    async fn put_index_ref(&self, _ns: &str, _key: &str, _value: Value) -> Result<()> { Ok(()) }
    async fn get_index_ref(&self, _ns: &str, _key: &str) -> Result<Option<Value>> { Ok(None) }
}
