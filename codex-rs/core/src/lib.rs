//! Root of the `codex-core` library.

// Prevent accidental direct writes to stdout/stderr in library code. All
// user-visible output must go through the appropriate abstraction (e.g.,
// the TUI or the tracing stack).
#![deny(clippy::print_stdout, clippy::print_stderr)]

mod apply_patch;
mod bash;
mod chat_completions;
mod client;
mod client_common;
pub mod codex;
pub use codex::Codex;
pub use codex::CodexSpawnOk;
pub mod codex_wrapper;
pub mod config;
pub mod config_profile;
pub mod config_types;
mod conversation_history;
pub mod error;
pub mod exec;
pub mod exec_env;
mod flags;
pub mod git_info;
mod is_safe_command;
mod mcp_connection_manager;
mod mcp_tool_call;
mod message_history;
mod model_provider_info;
pub use model_provider_info::BUILT_IN_OSS_MODEL_PROVIDER_ID;
pub use model_provider_info::ModelProviderInfo;
pub use model_provider_info::WireApi;
pub use model_provider_info::built_in_model_providers;
pub use model_provider_info::create_oss_provider_with_base_url;
pub mod model_family;
mod models;
mod openai_model_info;
mod openai_tools;
pub mod plan_tool;
mod project_doc;
pub mod protocol;
mod rollout;
pub(crate) mod safety;
pub mod seatbelt;
pub mod shell;
pub mod spawn;
pub mod turn_diff_tracker;
mod user_notification;
pub mod util;
pub mod audit;
pub mod external_decider;
pub use apply_patch::CODEX_APPLY_PATCH_ARG1;
pub use safety::get_platform_sandbox;
/// 统一错误别名，供外部稳定引用
pub use crate::error::CodexErr as CodexError;

// 稳定接口层（供适配层实现）
pub mod interfaces;
/// 稳定核心类型（流式、取消、编辑、诊断等）
pub mod types;
/// 通用重试/恢复策略实现
pub mod retry;

/// Serena 可选接线工厂
pub mod serena_wiring;

// Serena in-process adapter
mod serena_inprocess;
