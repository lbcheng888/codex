//! Types used to define the fields of [`crate::config::Config`].

// Note this file should generally be restricted to simple struct/enum
// definitions that do not contain business logic.

use std::collections::HashMap;
use std::path::PathBuf;
use strum_macros::Display;
use wildmatch::WildMatchPattern;

use serde::Deserialize;
use serde::Serialize;

/// 外部决策器（本地大模型）配置的 TOML 形式
#[derive(Deserialize, Debug, Clone, PartialEq, Default)]
pub struct ExternalDeciderConfigToml {
    pub enabled: Option<bool>,
    pub model_id: Option<String>,      // 例如 "gpt-oss-20b"
    pub device: Option<String>,        // "metal" | "cpu"
    pub precision: Option<String>,     // "bf16" | "f16" | "f32"
    pub max_tokens: Option<u32>,
    pub temperature: Option<f32>,
    pub top_p: Option<f32>,
    pub audit_log: Option<String>,     // JSONL 输出文件路径
    pub model_path: Option<String>,    // 本地 safetensors 路径
    pub tokenizer_path: Option<String>,// 本地 tokenizer.json 路径
    pub concurrency: Option<u32>,      // 并发度（默认 1）
}

/// 解析后的外部决策器配置
#[derive(Debug, Clone, PartialEq)]
pub struct ExternalDeciderConfig {
    pub enabled: bool,
    pub model_id: String,
    pub device: String,
    pub precision: String,
    pub max_tokens: u32,
    pub temperature: f32,
    pub top_p: f32,
    pub audit_log: String,
    pub model_path: Option<String>,
    pub tokenizer_path: Option<String>,
    pub concurrency: u32,
}

impl Default for ExternalDeciderConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            model_id: "gpt-oss-20b".into(),
            device: "metal".into(),
            precision: "bf16".into(),
            max_tokens: 512,
            temperature: 0.2,
            top_p: 0.9,
            audit_log: "~/.codex/audit/external_decisions.jsonl".into(),
            model_path: None,
            tokenizer_path: None,
            concurrency: 1,
        }
    }
}

impl ExternalDeciderConfig {
    pub fn resolve(from: Option<ExternalDeciderConfigToml>) -> Self {
        use std::env;
        let base = from.unwrap_or_default();
        let mut cfg = ExternalDeciderConfig::default();

        // 文件配置
        if let Some(v) = base.enabled { cfg.enabled = v; }
        if let Some(v) = base.model_id { cfg.model_id = v; }
        if let Some(v) = base.device { cfg.device = v; }
        if let Some(v) = base.precision { cfg.precision = v; }
        if let Some(v) = base.max_tokens { cfg.max_tokens = v; }
        if let Some(v) = base.temperature { cfg.temperature = v; }
        if let Some(v) = base.top_p { cfg.top_p = v; }
        if let Some(v) = base.audit_log { cfg.audit_log = v; }
        if let Some(v) = base.model_path { cfg.model_path = Some(v); }
        if let Some(v) = base.tokenizer_path { cfg.tokenizer_path = Some(v); }
        if let Some(v) = base.concurrency { cfg.concurrency = v.max(1); }

        // 环境变量覆盖（EXTERNAL_DECIDER_*）
        if let Ok(v) = env::var("EXTERNAL_DECIDER_ENABLED") {
            if let Ok(p) = v.parse::<bool>() { cfg.enabled = p; }
        }
        if let Ok(v) = env::var("EXTERNAL_DECIDER_MODEL_ID") { cfg.model_id = v; }
        if let Ok(v) = env::var("EXTERNAL_DECIDER_DEVICE") { cfg.device = v; }
        if let Ok(v) = env::var("EXTERNAL_DECIDER_PRECISION") { cfg.precision = v; }
        if let Ok(v) = env::var("EXTERNAL_DECIDER_MAX_TOKENS") {
            if let Ok(p) = v.parse::<u32>() { cfg.max_tokens = p; }
        }
        if let Ok(v) = env::var("EXTERNAL_DECIDER_TEMPERATURE") {
            if let Ok(p) = v.parse::<f32>() { cfg.temperature = p; }
        }
        if let Ok(v) = env::var("EXTERNAL_DECIDER_TOP_P") {
            if let Ok(p) = v.parse::<f32>() { cfg.top_p = p; }
        }
        if let Ok(v) = env::var("EXTERNAL_DECIDER_AUDIT_LOG") { cfg.audit_log = v; }
        if let Ok(v) = env::var("EXTERNAL_DECIDER_MODEL_PATH") { cfg.model_path = Some(v); }
        if let Ok(v) = env::var("EXTERNAL_DECIDER_TOKENIZER_PATH") { cfg.tokenizer_path = Some(v); }
        if let Ok(v) = env::var("EXTERNAL_DECIDER_CONCURRENCY") { if let Ok(p) = v.parse::<u32>() { cfg.concurrency = p.max(1); } }

        cfg
    }
}

/// Serena 相关配置（来自 codex.toml/config.toml 的 [serena] 节）。
#[derive(Deserialize, Debug, Clone, PartialEq, Default)]
pub struct SerenaConfigToml {
    /// 总开关：启用/禁用 serena 集成
    pub enabled: Option<bool>,
    /// 子开关：serena-tools
    pub tools: Option<bool>,
    /// 子开关：serena-lsp
    pub lsp: Option<bool>,
    /// 子开关：serena-agent
    pub agent: Option<bool>,

    /// 并发度限制（用于本地执行/工具运行等）
    pub concurrency: Option<usize>,
    /// 请求超时（毫秒）
    pub timeout_ms: Option<u64>,
    /// 日志级别（trace|debug|info|warn|error）
    pub log_level: Option<String>,
}

/// 解析后的 Serena 配置（合并默认值与环境变量覆盖）。
#[derive(Debug, Clone, PartialEq)]
pub struct SerenaConfig {
    pub enabled: bool,
    pub tools: bool,
    pub lsp: bool,
    pub agent: bool,
    pub concurrency: usize,
    pub timeout_ms: u64,
    pub log_level: String,
}

impl Default for SerenaConfig {
    fn default() -> Self {
        Self {
            // 与 integrations/codex-serena 的默认 feature 保持一致（serena_native 默认开启）
            enabled: true,
            tools: true,
            lsp: true,
            agent: true,
            // 与 serena-core RuntimeConfig 的默认值对齐
            concurrency: 8,
            timeout_ms: 30_000,
            log_level: "info".to_string(),
        }
    }
}

impl SerenaConfig {
    /// 基于可选的 toml 值与环境变量构建最终配置。
    /// 环境变量优先级高于文件内配置。
    pub fn resolve(from: Option<SerenaConfigToml>) -> Self {
        use std::env;
        let base = from.unwrap_or_default();
        let mut cfg = SerenaConfig::default();

        // 1) 先应用文件内配置
        if let Some(v) = base.enabled { cfg.enabled = v; }
        if let Some(v) = base.tools { cfg.tools = v; }
        if let Some(v) = base.lsp { cfg.lsp = v; }
        if let Some(v) = base.agent { cfg.agent = v; }
        if let Some(v) = base.concurrency { cfg.concurrency = v; }
        if let Some(v) = base.timeout_ms { cfg.timeout_ms = v; }
        if let Some(v) = base.log_level { cfg.log_level = v; }

        // 2) 再应用环境变量覆盖（便于 CI 与调试）
        // 使用 SERENA_* 前缀，命名保持直观
        if let Ok(v) = env::var("SERENA_ENABLED") {
            if let Ok(parsed) = v.parse::<bool>() { cfg.enabled = parsed; }
        }
        if let Ok(v) = env::var("SERENA_TOOLS_ENABLED") {
            if let Ok(parsed) = v.parse::<bool>() { cfg.tools = parsed; }
        }
        if let Ok(v) = env::var("SERENA_LSP_ENABLED") {
            if let Ok(parsed) = v.parse::<bool>() { cfg.lsp = parsed; }
        }
        if let Ok(v) = env::var("SERENA_AGENT_ENABLED") {
            if let Ok(parsed) = v.parse::<bool>() { cfg.agent = parsed; }
        }
        if let Ok(v) = env::var("SERENA_CONCURRENCY") {
            if let Ok(parsed) = v.parse::<usize>() { cfg.concurrency = parsed; }
        }
        if let Ok(v) = env::var("SERENA_TIMEOUT_MS") {
            if let Ok(parsed) = v.parse::<u64>() { cfg.timeout_ms = parsed; }
        }
        if let Ok(v) = env::var("SERENA_LOG_LEVEL") { cfg.log_level = v; }

        cfg
    }
}

/// Graphiti（ContextStore 持久化）配置 T O M L 形式
#[derive(Deserialize, Debug, Clone, PartialEq, Default)]
pub struct GraphitiConfigToml {
    /// 数据库路径（可相对，可绝对）。相对路径将基于 CODEX_HOME 解析。
    pub db_path: Option<String>,
    /// 命名空间（用于隔离不同项目/会话）
    pub namespace: Option<String>,
}

/// 解析后的 Graphiti 配置
#[derive(Debug, Clone, PartialEq)]
pub struct GraphitiConfig {
    pub db_path: PathBuf,
    pub namespace: String,
}

impl GraphitiConfig {
    pub fn resolve(base: Option<GraphitiConfigToml>, codex_home: &PathBuf) -> Self {
        use std::env;
        let base = base.unwrap_or_default();
        // 默认值
        let mut db_path = {
            let mut p = codex_home.clone();
            p.push("graphiti.db");
            p
        };
        let mut namespace = "codex_default".to_string();

        // 1) 应用文件配置
        if let Some(v) = base.db_path.as_ref() {
            let p = PathBuf::from(v);
            db_path = if p.is_absolute() { p } else {
                let mut c = codex_home.clone();
                c.push(p);
                c
            };
        }
        if let Some(v) = base.namespace { namespace = v; }

        // 2) 环境变量覆盖
        if let Ok(v) = env::var("GRAPHITI_DB_PATH") {
            let p = PathBuf::from(&v);
            db_path = if p.is_absolute() { p } else {
                let mut c = codex_home.clone();
                c.push(p);
                c
            };
        }
        if let Ok(v) = env::var("GRAPHITI_NAMESPACE") { namespace = v; }

        Self { db_path, namespace }
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
pub struct McpServerConfig {
    pub command: String,

    #[serde(default)]
    pub args: Vec<String>,

    #[serde(default)]
    pub env: Option<HashMap<String, String>>,
}

#[derive(Deserialize, Debug, Copy, Clone, PartialEq)]
pub enum UriBasedFileOpener {
    #[serde(rename = "vscode")]
    VsCode,

    #[serde(rename = "vscode-insiders")]
    VsCodeInsiders,

    #[serde(rename = "windsurf")]
    Windsurf,

    #[serde(rename = "cursor")]
    Cursor,

    /// Option to disable the URI-based file opener.
    #[serde(rename = "none")]
    None,
}

impl UriBasedFileOpener {
    pub fn get_scheme(&self) -> Option<&str> {
        match self {
            UriBasedFileOpener::VsCode => Some("vscode"),
            UriBasedFileOpener::VsCodeInsiders => Some("vscode-insiders"),
            UriBasedFileOpener::Windsurf => Some("windsurf"),
            UriBasedFileOpener::Cursor => Some("cursor"),
            UriBasedFileOpener::None => None,
        }
    }
}

/// Settings that govern if and what will be written to `~/.codex/history.jsonl`.
#[derive(Deserialize, Debug, Clone, PartialEq, Default)]
pub struct History {
    /// If true, history entries will not be written to disk.
    pub persistence: HistoryPersistence,

    /// If set, the maximum size of the history file in bytes.
    /// TODO(mbolin): Not currently honored.
    pub max_bytes: Option<usize>,
}

#[derive(Deserialize, Debug, Copy, Clone, PartialEq, Default)]
#[serde(rename_all = "kebab-case")]
pub enum HistoryPersistence {
    /// Save all history entries to disk.
    #[default]
    SaveAll,
    /// Do not write history to disk.
    None,
}

/// Collection of settings that are specific to the TUI.
#[derive(Deserialize, Debug, Clone, PartialEq, Default)]
pub struct Tui {}

#[derive(Deserialize, Debug, Clone, Copy, PartialEq, Default, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum SandboxMode {
    #[serde(rename = "read-only")]
    ReadOnly,

    #[serde(rename = "workspace-write")]
    WorkspaceWrite,

    #[serde(rename = "danger-full-access")]
    #[default]
    DangerFullAccess,
}

#[derive(Deserialize, Debug, Clone, PartialEq, Default)]
pub struct SandboxWorkspaceWrite {
    #[serde(default)]
    pub writable_roots: Vec<PathBuf>,
    #[serde(default)]
    pub network_access: bool,
    #[serde(default)]
    pub exclude_tmpdir_env_var: bool,
    #[serde(default)]
    pub exclude_slash_tmp: bool,
}

#[derive(Deserialize, Debug, Clone, PartialEq, Default)]
#[serde(rename_all = "kebab-case")]
pub enum ShellEnvironmentPolicyInherit {
    /// "Core" environment variables for the platform. On UNIX, this would
    /// include HOME, LOGNAME, PATH, SHELL, and USER, among others.
    Core,

    /// Inherits the full environment from the parent process.
    #[default]
    All,

    /// Do not inherit any environment variables from the parent process.
    None,
}

/// Policy for building the `env` when spawning a process via either the
/// `shell` or `local_shell` tool.
#[derive(Deserialize, Debug, Clone, PartialEq, Default)]
pub struct ShellEnvironmentPolicyToml {
    pub inherit: Option<ShellEnvironmentPolicyInherit>,

    pub ignore_default_excludes: Option<bool>,

    /// List of regular expressions.
    pub exclude: Option<Vec<String>>,

    pub r#set: Option<HashMap<String, String>>,

    /// List of regular expressions.
    pub include_only: Option<Vec<String>>,

    pub experimental_use_profile: Option<bool>,
}

pub type EnvironmentVariablePattern = WildMatchPattern<'*', '?'>;

/// Deriving the `env` based on this policy works as follows:
/// 1. Create an initial map based on the `inherit` policy.
/// 2. If `ignore_default_excludes` is false, filter the map using the default
///    exclude pattern(s), which are: `"*KEY*"` and `"*TOKEN*"`.
/// 3. If `exclude` is not empty, filter the map using the provided patterns.
/// 4. Insert any entries from `r#set` into the map.
/// 5. If non-empty, filter the map using the `include_only` patterns.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct ShellEnvironmentPolicy {
    /// Starting point when building the environment.
    pub inherit: ShellEnvironmentPolicyInherit,

    /// True to skip the check to exclude default environment variables that
    /// contain "KEY" or "TOKEN" in their name.
    pub ignore_default_excludes: bool,

    /// Environment variable names to exclude from the environment.
    pub exclude: Vec<EnvironmentVariablePattern>,

    /// (key, value) pairs to insert in the environment.
    pub r#set: HashMap<String, String>,

    /// Environment variable names to retain in the environment.
    pub include_only: Vec<EnvironmentVariablePattern>,

    /// If true, the shell profile will be used to run the command.
    pub use_profile: bool,
}

impl From<ShellEnvironmentPolicyToml> for ShellEnvironmentPolicy {
    fn from(toml: ShellEnvironmentPolicyToml) -> Self {
        // Default to inheriting the full environment when not specified.
        let inherit = toml.inherit.unwrap_or(ShellEnvironmentPolicyInherit::All);
        let ignore_default_excludes = toml.ignore_default_excludes.unwrap_or(false);
        let exclude = toml
            .exclude
            .unwrap_or_default()
            .into_iter()
            .map(|s| EnvironmentVariablePattern::new_case_insensitive(&s))
            .collect();
        let r#set = toml.r#set.unwrap_or_default();
        let include_only = toml
            .include_only
            .unwrap_or_default()
            .into_iter()
            .map(|s| EnvironmentVariablePattern::new_case_insensitive(&s))
            .collect();
        let use_profile = toml.experimental_use_profile.unwrap_or(false);

        Self {
            inherit,
            ignore_default_excludes,
            exclude,
            r#set,
            include_only,
            use_profile,
        }
    }
}

/// See https://platform.openai.com/docs/guides/reasoning?api-mode=responses#get-started-with-reasoning
#[derive(Debug, Serialize, Deserialize, Default, Clone, Copy, PartialEq, Eq, Display)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
pub enum ReasoningEffort {
    Low,
    #[default]
    Medium,
    High,
    /// Option to disable reasoning.
    None,
}

/// A summary of the reasoning performed by the model. This can be useful for
/// debugging and understanding the model's reasoning process.
/// See https://platform.openai.com/docs/guides/reasoning?api-mode=responses#reasoning-summaries
#[derive(Debug, Serialize, Deserialize, Default, Clone, Copy, PartialEq, Eq, Display)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
pub enum ReasoningSummary {
    #[default]
    Auto,
    Concise,
    Detailed,
    /// Option to disable reasoning summaries.
    None,
}
