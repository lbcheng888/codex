use reqwest::StatusCode;
use serde::{Deserialize, Serialize};
use serde_json;
use std::io;
use std::time::Duration;
use thiserror::Error;
use tokio::task::JoinError;

pub type Result<T> = std::result::Result<T, CodexErr>;

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq)]
#[non_exhaustive]
pub enum CodexErrorCode {
    // Codex core
    StreamDisconnected,
    Timeout,
    SpawnFailed,
    Interrupted,
    HttpUnexpectedStatus,
    UsageLimitReached,
    UsageNotIncluded,
    InternalServerError,
    RetryLimit,
    InternalAgentDied,
    // Sandbox
    SandboxDenied,
    SandboxTimeout,
    SandboxSignal,
    SandboxLandlockRestrict,
    // Serena mapped
    SerenaConfig,
    SerenaIo,
    SerenaProtocol,
    SerenaTool,
    SerenaValidation,
    SerenaNotFound,
    SerenaLanguageServer,
    SerenaTimeout,
    SerenaSerialization,
    SerenaAuthentication,
    SerenaPermission,
    SerenaInternal,
    // External passthrough
    Io,
    Reqwest,
    Json,
    EnvVar,
    Unknown,
}

#[derive(Error, Debug)]
pub enum SandboxErr {
    /// Error from sandbox execution
    #[error("sandbox denied exec error, exit code: {0}, stdout: {1}, stderr: {2}")]
    Denied(i32, String, String),

    /// Error from linux seccomp filter setup
    #[cfg(target_os = "linux")]
    #[error("seccomp setup error")]
    SeccompInstall(#[from] seccompiler::Error),

    /// Error from linux seccomp backend
    #[cfg(target_os = "linux")]
    #[error("seccomp backend error")]
    SeccompBackend(#[from] seccompiler::BackendError),

    /// Command timed out
    #[error("command timed out")]
    Timeout,

    /// Command was killed by a signal
    #[error("command was killed by a signal")]
    Signal(i32),

    /// Error from linux landlock
    #[error("Landlock was not able to fully enforce all sandbox rules")]
    LandlockRestrict,
}

#[derive(Error, Debug)]
pub enum CodexErr {
    /// Returned by ResponsesClient when the SSE stream disconnects or errors out **after** the HTTP
    /// handshake has succeeded but **before** it finished emitting `response.completed`.
    ///
    /// The Session loop treats this as a transient error and will automatically retry the turn.
    #[error("stream disconnected before completion: {0}")]
    Stream(String),

    /// Returned by run_command_stream when the spawned child process timed out (10s).
    #[error("timeout waiting for child process to exit")]
    Timeout,

    /// Returned by run_command_stream when the child could not be spawned (its stdout/stderr pipes
    /// could not be captured). Analogous to the previous `CodexError::Spawn` variant.
    #[error("spawn failed: child stdout/stderr not captured")]
    Spawn,

    /// Returned by run_command_stream when the user pressed Ctrl‑C (SIGINT). Session uses this to
    /// surface a polite FunctionCallOutput back to the model instead of crashing the CLI.
    #[error("interrupted (Ctrl-C)")]
    Interrupted,

    /// Unexpected HTTP status code.
    #[error("unexpected status {0}: {1}")]
    UnexpectedStatus(StatusCode, String),

    #[error("Usage limit has been reached")]
    UsageLimitReached,

    #[error("Usage not included with the plan")]
    UsageNotIncluded,

    #[error(
        "We’re currently experiencing high demand, which may cause temporary errors. We’re adding capacity in East and West Europe to restore normal service."
    )]
    InternalServerError,

    /// Retry limit exceeded.
    #[error("exceeded retry limit, last status: {0}")]
    RetryLimit(StatusCode),

    /// Agent loop died unexpectedly
    #[error("internal error; agent loop died unexpectedly")]
    InternalAgentDied,

    /// Sandbox error
    #[error("sandbox error: {0}")]
    Sandbox(#[from] SandboxErr),

    #[error("codex-linux-sandbox was required but not provided")]
    LandlockSandboxExecutableNotProvided,

    // -----------------------------------------------------------------
    // Automatic conversions for common external error types
    // -----------------------------------------------------------------
    #[error(transparent)]
    Io(#[from] io::Error),

    #[error(transparent)]
    Reqwest(#[from] reqwest::Error),

    #[error(transparent)]
    Json(#[from] serde_json::Error),

    #[cfg(target_os = "linux")]
    #[error(transparent)]
    LandlockRuleset(#[from] landlock::RulesetError),

    #[cfg(target_os = "linux")]
    #[error(transparent)]
    LandlockPathFd(#[from] landlock::PathFdError),

    #[error(transparent)]
    TokioJoin(#[from] JoinError),

    #[error("{0}")]
    EnvVar(EnvVarError),

    /// Serena 核心错误映射（标准化）
    #[error("serena core error: {0}")]
    Serena(#[from] serena_core::error::CoreError),
}

#[derive(Debug)]
pub struct EnvVarError {
    /// Name of the environment variable that is missing.
    pub var: String,

    /// Optional instructions to help the user get a valid value for the
    /// variable and set it.
    pub instructions: Option<String>,
}

impl std::fmt::Display for EnvVarError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Missing environment variable: `{}`.", self.var)?;
        if let Some(instructions) = &self.instructions {
            write!(f, " {instructions}")?;
        }
        Ok(())
    }
}

impl CodexErr {
    /// Minimal shim so that existing `e.downcast_ref::<CodexErr>()` checks continue to compile
    /// after replacing `anyhow::Error` in the return signature. This mirrors the behavior of
    /// `anyhow::Error::downcast_ref` but works directly on our concrete enum.
    pub fn downcast_ref<T: std::any::Any>(&self) -> Option<&T> {
        (self as &dyn std::any::Any).downcast_ref::<T>()
    }

    /// 标准化错误码
    pub fn code(&self) -> CodexErrorCode {
        match self {
            CodexErr::Stream(_) => CodexErrorCode::StreamDisconnected,
            CodexErr::Timeout => CodexErrorCode::Timeout,
            CodexErr::Spawn => CodexErrorCode::SpawnFailed,
            CodexErr::Interrupted => CodexErrorCode::Interrupted,
            CodexErr::UnexpectedStatus(_, _) => CodexErrorCode::HttpUnexpectedStatus,
            CodexErr::UsageLimitReached => CodexErrorCode::UsageLimitReached,
            CodexErr::UsageNotIncluded => CodexErrorCode::UsageNotIncluded,
            CodexErr::InternalServerError => CodexErrorCode::InternalServerError,
            CodexErr::RetryLimit(_) => CodexErrorCode::RetryLimit,
            CodexErr::InternalAgentDied => CodexErrorCode::InternalAgentDied,
            CodexErr::Sandbox(SandboxErr::Denied(_, _, _)) => CodexErrorCode::SandboxDenied,
            CodexErr::Sandbox(SandboxErr::Timeout) => CodexErrorCode::SandboxTimeout,
            CodexErr::Sandbox(SandboxErr::Signal(_)) => CodexErrorCode::SandboxSignal,
            CodexErr::Sandbox(SandboxErr::LandlockRestrict) => CodexErrorCode::SandboxLandlockRestrict,
            CodexErr::LandlockSandboxExecutableNotProvided => CodexErrorCode::SandboxLandlockRestrict,
            CodexErr::Io(_) => CodexErrorCode::Io,
            CodexErr::Reqwest(_) => CodexErrorCode::Reqwest,
            CodexErr::Json(_) => CodexErrorCode::Json,
            CodexErr::TokioJoin(_) => CodexErrorCode::InternalServerError,
            CodexErr::EnvVar(_) => CodexErrorCode::EnvVar,
            CodexErr::Serena(se) => match se {
                serena_core::error::CoreError::Config { .. } => CodexErrorCode::SerenaConfig,
                serena_core::error::CoreError::Io { .. } => CodexErrorCode::SerenaIo,
                serena_core::error::CoreError::Protocol { .. } => CodexErrorCode::SerenaProtocol,
                serena_core::error::CoreError::Tool { .. } => CodexErrorCode::SerenaTool,
                serena_core::error::CoreError::Validation { .. } => CodexErrorCode::SerenaValidation,
                serena_core::error::CoreError::NotFound { .. } => CodexErrorCode::SerenaNotFound,
                serena_core::error::CoreError::LanguageServer { .. } => CodexErrorCode::SerenaLanguageServer,
                serena_core::error::CoreError::Timeout { .. } => CodexErrorCode::SerenaTimeout,
                serena_core::error::CoreError::Serialization { .. } => CodexErrorCode::SerenaSerialization,
                serena_core::error::CoreError::Authentication { .. } => CodexErrorCode::SerenaAuthentication,
                serena_core::error::CoreError::Permission { .. } => CodexErrorCode::SerenaPermission,
                serena_core::error::CoreError::Internal { .. } => CodexErrorCode::SerenaInternal,
            },
        }
    }

    /// 标记是否建议重试（幂等调用前提下）
    pub fn is_retryable(&self) -> bool {
        match self {
            CodexErr::Stream(_) => true,
            CodexErr::Timeout => true,
            CodexErr::Interrupted => false,
            CodexErr::UnexpectedStatus(status, _body) => {
                *status == StatusCode::TOO_MANY_REQUESTS || status.is_server_error()
            }
            CodexErr::UsageLimitReached | CodexErr::UsageNotIncluded => false,
            CodexErr::InternalServerError => true,
            CodexErr::RetryLimit(_) => false,
            CodexErr::InternalAgentDied => true,
            CodexErr::Sandbox(SandboxErr::Timeout) => true,
            CodexErr::Sandbox(SandboxErr::Signal(_)) => true,
            CodexErr::Sandbox(SandboxErr::Denied(..)) => false,
            CodexErr::Sandbox(SandboxErr::LandlockRestrict) => false,
            CodexErr::LandlockSandboxExecutableNotProvided => false,
            CodexErr::Io(_) => true,
            CodexErr::Reqwest(e) => e.is_timeout() || e.is_connect() || e.is_request()
                || e.is_status() && e.status().map(|s| s.is_server_error()).unwrap_or(false),
            CodexErr::Json(_) => false,
            CodexErr::TokioJoin(_) => true,
            CodexErr::EnvVar(_) => false,
            CodexErr::Serena(se) => se.is_recoverable(),
        }
    }

    /// 提供标准化上下文（供日志/遥测/上层 UI 使用）
    pub fn context(&self) -> serde_json::Value {
        use serde_json::json;
        match self {
            CodexErr::UnexpectedStatus(status, body) => json!({
                "status": status.as_u16(),
                "body": body,
            }),
            CodexErr::RetryLimit(status) => json!({ "status": status.as_u16() }),
            CodexErr::Sandbox(SandboxErr::Denied(code, stdout, stderr)) => json!({
                "exit_code": code,
                "stdout": stdout,
                "stderr": stderr,
            }),
            CodexErr::Sandbox(SandboxErr::Signal(sig)) => json!({ "signal": sig }),
            CodexErr::Serena(err) => serde_json::to_value(err).unwrap_or_else(|_| json!({"err": err.to_string()})),
            CodexErr::Reqwest(e) => json!({
                "reqwest": e.to_string(),
                "is_timeout": e.is_timeout(),
            }),
            CodexErr::EnvVar(e) => json!({ "var": e.var, "instructions": e.instructions }),
            other => json!({ "message": other.to_string() }),
        }
    }

    /// 可选给出建议的下一次重试延迟（含指数退避+抖动），用于调用方自定义策略
    pub fn retry_delay_hint(&self, attempt: u64) -> Option<Duration> {
        if self.is_retryable() { Some(crate::util::backoff(attempt)) } else { None }
    }
}

pub fn get_error_message_ui(e: &CodexErr) -> String {
    match e {
        CodexErr::Sandbox(SandboxErr::Denied(_, _, stderr)) => stderr.to_string(),
        _ => e.to_string(),
    }
}
