//! codex-core 核心可复用类型与流式/取消抽象
//! 注意：该模块仅定义稳定类型与trait，避免依赖具体后端实现。

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::sync::Arc;
use std::time::{Duration, Instant};
use std::sync::atomic::{AtomicBool, Ordering};

// -------------------------------
// 基础编辑与诊断类型
// -------------------------------

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct TextPosition {
    pub line: u32,      // 0-based
    pub character: u32, // 0-based utf16 or utf8 由调用方约定
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct TextRange {
    pub start: TextPosition,
    pub end: TextPosition,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct CodeEdit {
    pub file_path: String,
    pub range: Option<TextRange>,
    pub new_text: String,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct WorkspaceEdit {
    pub changes: Vec<CodeEdit>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Information,
    Hint,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Diagnostic {
    pub file_path: String,
    pub range: Option<TextRange>,
    pub severity: Option<DiagnosticSeverity>,
    pub code: Option<String>,
    pub message: String,
    pub source: Option<String>,
}

// -------------------------------
// 工具调用抽象类型
// -------------------------------

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ToolSpec {
    pub name: String,
    pub description: Option<String>,
    /// JSON Schema for input
    pub input_schema: Value,
    /// 可选输出模式描述
    pub output_schema: Option<Value>,
    pub title: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ToolCall {
    pub id: String,
    pub name: String,      // fq name
    pub arguments: Value,  // object or null
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ToolResult {
    pub id: String,
    pub is_error: bool,
    /// 文本化输出（如存在）
    pub text: Option<String>,
    /// 结构化输出（与 text 二选一或并存，取决于工具）
    pub structured: Option<Value>,
}

// -------------------------------
// 流式输出与取消
// -------------------------------

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum StreamEventKind {
    Heartbeat,
    ToolStarted { name: String },
    ToolProgress { name: String, progress: Option<u8> },
    ToolCompleted { name: String, ok: bool },
    ModelToken, // 通用 token 事件由 on_token 传递，此枚举仅用于语义标记
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct StreamEvent {
    pub kind: StreamEventKind,
    pub at: InstantSerde,
    pub meta: Option<Value>,
}

/// 统一的流接收器，既可用于模型 token 流，也可用于工具输出流。
pub trait StreamingSink: Send + Sync {
    /// 推送一个 UTF-8 token 段
    fn on_token(&self, token: &str);
    /// 推送结构化事件（心跳、进度等）
    fn on_event(&self, event: StreamEvent);
    /// 结束回调（成功/失败由调用方额外事件表征）
    fn on_close(&self);
}

/// 取消令牌，包含截止时间；提供显式取消与超时语义。
#[derive(Clone, Debug)]
pub struct CancelToken {
    cancelled: Arc<AtomicBool>,
    deadline: Option<Instant>,
}

impl CancelToken {
    pub fn new(deadline: Option<Instant>) -> Self {
        Self { cancelled: Arc::new(AtomicBool::new(false)), deadline }
    }
    pub fn cancelled() -> Self { Self::new(None).with_cancelled(true) }
    pub fn with_cancelled(mut self, v: bool) -> Self { self.cancelled.store(v, Ordering::SeqCst); self }
    pub fn cancel(&self) { self.cancelled.store(true, Ordering::SeqCst); }
    pub fn is_cancelled(&self) -> bool { self.cancelled.load(Ordering::SeqCst) }
    pub fn deadline(&self) -> Option<Instant> { self.deadline }
    pub fn with_deadline(mut self, deadline: Instant) -> Self { self.deadline = Some(deadline); self }
    pub fn time_left(&self) -> Option<Duration> {
        self.deadline.map(|d| d.saturating_duration_since(Instant::now()))
    }
    pub fn is_expired(&self) -> bool {
        match self.deadline { Some(d) => Instant::now() >= d, None => false }
    }
}

// -------------------------------
// 时间序列化帮助（Instant 无法直接序列化）
// -------------------------------

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct InstantSerde(#[serde(with = "instant_as_millis")] pub Instant);

mod instant_as_millis {
    use super::*;
    use serde::{Deserializer, Serializer};

    pub fn serialize<S: Serializer>(t: &Instant, s: S) -> Result<S::Ok, S::Error> {
        let now = Instant::now();
        let ms = now.duration_since(*t).as_millis() as i64;
        // 记录相对当前时刻的负偏移（ms），避免依赖系统时间源
        // 存储为负值表示过去的时间点
        (-ms).serialize(s)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Instant, D::Error> {
        let rel_ms: i64 = Deserialize::deserialize(d)?;
        let now = Instant::now();
        if rel_ms <= 0 {
            Ok(now - Duration::from_millis(rel_ms.unsigned_abs() as u64))
        } else {
            Ok(now + Duration::from_millis(rel_ms as u64))
        }
    }
}
