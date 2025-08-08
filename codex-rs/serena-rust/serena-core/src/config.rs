use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::{env, fs, path::PathBuf, time::SystemTime};
use crate::error::{CoreError, Result};

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct RuntimeConfig {
    pub workspace_root: PathBuf,
    pub transport: Transport,
    pub log_level: String,
    pub metrics_port: Option<u16>,
    pub concurrency_limit: usize,
    pub request_timeout_ms: u64,
    /// Log format: "text" or "json"
    pub log_format: String,
    /// Enable hot reload of configuration
    pub hot_reload: bool,
    /// Configuration file path for hot reload
    pub config_file: Option<PathBuf>,
    /// Environment prefix for environment variable overrides
    pub env_prefix: String,
    /// Maximum file size for uploads/processing (in bytes)
    pub max_file_size: u64,
    /// Enable development mode features
    pub dev_mode: bool,

    // ---- Tool/LSP resource limits ----
    /// Max concurrent tool executions
    pub tools_max_concurrency: usize,
    /// Default timeout for tool execution (ms)
    pub tool_default_timeout_ms: u64,
    /// Soft memory cap for tools (MB); enforcement is best-effort/observer-only for now
    pub tools_memory_limit_mb: Option<u64>,
    /// LSP request timeout (ms)
    pub lsp_request_timeout_ms: u64,
    /// Global max concurrent LSP requests across all languages
    pub lsp_total_concurrency: usize,
    /// Per-language max concurrent LSP requests
    pub lsp_per_language_concurrency: usize,
    /// Soft memory cap for LSP subprocesses (MB); enforcement TBD
    pub lsp_memory_limit_mb: Option<u64>,

    // ---- Security & permissions ----
    /// Enable sandbox mode: deny process exec by default; restrict working dir to workspace root; enforce path isolation
    pub sandbox_mode: bool,
    /// Allow file read operations
    pub allow_fs_read: bool,
    /// Allow file write operations
    pub allow_fs_write: bool,
    /// Allow process execution (further restricted by whitelist)
    pub allow_process_execution: bool,
    /// Whitelist of allowed external commands (program names)
    pub process_whitelist: Vec<String>,

    // ---- Observability toggles ----
    /// Enable OpenTelemetry export (trace/log/metrics). Can be toggled via env.
    pub otel_enable: bool,
    /// OTLP endpoint (http/proto). Example: http://127.0.0.1:4318
    pub otel_endpoint: Option<String>,
    /// Sampling ratio for traces, 0.0..=1.0
    pub otel_sample_ratio: f64,
    /// Deployment environment tag, e.g., "dev", "ci", "prod"
    pub env: Option<String>,
    /// Enable privacy redaction for events/logs
    pub privacy_redact: bool,
    /// Salt used for hashing identifiers when redacting
    pub privacy_salt: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum Transport {
    Stdio,
    // Sse, // reserved for future
}

impl Default for RuntimeConfig {
    fn default() -> Self {
        Self {
            workspace_root: std::env::current_dir().unwrap_or_else(|_| ".".into()),
            transport: Transport::Stdio,
            log_level: "info".into(),
            metrics_port: None,
            concurrency_limit: 8,
            request_timeout_ms: 30_000,
            log_format: "text".into(),
            hot_reload: false,
            config_file: None,
            env_prefix: "SERENA".into(),
            max_file_size: 10 * 1024 * 1024, // 10MB
            dev_mode: false,

            // Resource limits defaults
            tools_max_concurrency: 32,
            tool_default_timeout_ms: 60_000,
            tools_memory_limit_mb: None,
            lsp_request_timeout_ms: 20_000,
            lsp_total_concurrency: 16,
            lsp_per_language_concurrency: 8,
            lsp_memory_limit_mb: None,

            // Security defaults: safe by default
            sandbox_mode: true,
            allow_fs_read: true,
            allow_fs_write: true,
            allow_process_execution: false,
            process_whitelist: vec![],

            otel_enable: false,
            otel_endpoint: None,
            otel_sample_ratio: 0.1,
            env: None,
            privacy_redact: true,
            privacy_salt: None,
        }
    }
}

impl RuntimeConfig {
    /// Load configuration from file with environment variable overrides
    pub fn load_from_file(path: &PathBuf) -> Result<Self> {
        let content = fs::read_to_string(path)
            .map_err(|e| CoreError::io_with_path(format!("Failed to read config file: {}", e), path.display().to_string()))?;

        let mut config: Self = if path
            .extension()
            .and_then(|s| s.to_str())
            .map(|e| e.eq_ignore_ascii_case("yaml") || e.eq_ignore_ascii_case("yml"))
            .unwrap_or(false)
        {
            serde_yaml::from_str(&content)?
        } else {
            serde_json::from_str(&content)?
        };

        // Apply environment variable overrides
        config.apply_env_overrides()?;

        // Validate configuration
        config.validate()?;

        // Store config file path for hot reload
        config.config_file = Some(path.clone());

        Ok(config)
    }

    /// Load configuration with defaults and environment overrides
    pub fn load_with_defaults() -> Result<Self> {
        let mut config = Self::default();
        config.apply_env_overrides()?;
        config.validate()?;
        Ok(config)
    }

    /// Apply environment variable overrides
    pub fn apply_env_overrides(&mut self) -> Result<()> {
        let prefix = &self.env_prefix;

        if let Ok(val) = env::var(format!("{}_WORKSPACE_ROOT", prefix)) {
            self.workspace_root = PathBuf::from(val);
        }

        if let Ok(val) = env::var(format!("{}_LOG_LEVEL", prefix)) {
            self.log_level = val;
        }

        if let Ok(val) = env::var(format!("{}_LOG_FORMAT", prefix)) {
            self.log_format = val;
        }

        if let Ok(val) = env::var(format!("{}_METRICS_PORT", prefix)) {
            self.metrics_port = Some(val.parse()
                .map_err(|_| CoreError::config(format!("Invalid metrics port: {}", val)))?);
        }

        if let Ok(val) = env::var(format!("{}_CONCURRENCY_LIMIT", prefix)) {
            self.concurrency_limit = val.parse()
                .map_err(|_| CoreError::config(format!("Invalid concurrency limit: {}", val)))?;
        }

        if let Ok(val) = env::var(format!("{}_REQUEST_TIMEOUT_MS", prefix)) {
            self.request_timeout_ms = val.parse()
                .map_err(|_| CoreError::config(format!("Invalid request timeout: {}", val)))?;
        }

        if let Ok(val) = env::var(format!("{}_MAX_FILE_SIZE", prefix)) {
            self.max_file_size = val.parse()
                .map_err(|_| CoreError::config(format!("Invalid max file size: {}", val)))?;
        }

        // ---- Tools/LSP resource limits ----
        if let Ok(val) = env::var(format!("{}_TOOLS_MAX_CONCURRENCY", prefix)) {
            self.tools_max_concurrency = val.parse()
                .map_err(|_| CoreError::config(format!("Invalid tools max concurrency: {}", val)))?;
        }
        if let Ok(val) = env::var(format!("{}_TOOL_DEFAULT_TIMEOUT_MS", prefix)) {
            self.tool_default_timeout_ms = val.parse()
                .map_err(|_| CoreError::config(format!("Invalid tool default timeout: {}", val)))?;
        }
        if let Ok(val) = env::var(format!("{}_TOOLS_MEMORY_LIMIT_MB", prefix)) {
            self.tools_memory_limit_mb = Some(val.parse()
                .map_err(|_| CoreError::config(format!("Invalid tools memory limit MB: {}", val)))?);
        }
        if let Ok(val) = env::var(format!("{}_LSP_REQUEST_TIMEOUT_MS", prefix)) {
            self.lsp_request_timeout_ms = val.parse()
                .map_err(|_| CoreError::config(format!("Invalid LSP request timeout: {}", val)))?;
        }
        if let Ok(val) = env::var(format!("{}_LSP_TOTAL_CONCURRENCY", prefix)) {
            self.lsp_total_concurrency = val.parse()
                .map_err(|_| CoreError::config(format!("Invalid LSP total concurrency: {}", val)))?;
        }
        if let Ok(val) = env::var(format!("{}_LSP_PER_LANGUAGE_CONCURRENCY", prefix)) {
            self.lsp_per_language_concurrency = val.parse()
                .map_err(|_| CoreError::config(format!("Invalid LSP per-language concurrency: {}", val)))?;
        }
        if let Ok(val) = env::var(format!("{}_LSP_MEMORY_LIMIT_MB", prefix)) {
            self.lsp_memory_limit_mb = Some(val.parse()
                .map_err(|_| CoreError::config(format!("Invalid LSP memory limit MB: {}", val)))?);
        }

        if let Ok(val) = env::var(format!("{}_DEV_MODE", prefix)) {
            self.dev_mode = val.parse()
                .map_err(|_| CoreError::config(format!("Invalid dev mode flag: {}", val)))?;
        }

        if let Ok(val) = env::var(format!("{}_HOT_RELOAD", prefix)) {
            self.hot_reload = val.parse()
                .map_err(|_| CoreError::config(format!("Invalid hot reload flag: {}", val)))?;
        }

        // ---- Security & permissions ----
        if let Ok(val) = env::var(format!("{}_SANDBOX_MODE", prefix)) {
            self.sandbox_mode = val.parse().map_err(|_| CoreError::config(format!("Invalid sandbox flag: {}", val)))?;
        }
        if let Ok(val) = env::var(format!("{}_ALLOW_FS_READ", prefix)) {
            self.allow_fs_read = val.parse().map_err(|_| CoreError::config(format!("Invalid allow fs read: {}", val)))?;
        }
        if let Ok(val) = env::var(format!("{}_ALLOW_FS_WRITE", prefix)) {
            self.allow_fs_write = val.parse().map_err(|_| CoreError::config(format!("Invalid allow fs write: {}", val)))?;
        }
        if let Ok(val) = env::var(format!("{}_ALLOW_PROCESS_EXECUTION", prefix)) {
            self.allow_process_execution = val.parse().map_err(|_| CoreError::config(format!("Invalid allow process execution: {}", val)))?;
        }
        if let Ok(val) = env::var(format!("{}_PROCESS_WHITELIST", prefix)) { // comma-separated
            self.process_whitelist = val.split(',').map(|s| s.trim().to_string()).filter(|s| !s.is_empty()).collect();
        }

        // ---- Observability ----
        if let Ok(val) = env::var(format!("{}_OTEL_ENABLE", prefix)) {
            self.otel_enable = val.parse()
                .map_err(|_| CoreError::config(format!("Invalid otel enable flag: {}", val)))?;
        }
        if let Ok(val) = env::var(format!("{}_OTEL_ENDPOINT", prefix)) {
            self.otel_endpoint = Some(val);
        }
        if let Ok(val) = env::var(format!("{}_OTEL_SAMPLE_RATIO", prefix)) {
            self.otel_sample_ratio = val.parse()
                .map_err(|_| CoreError::config(format!("Invalid otel sample ratio: {}", val)))?;
        }
        if let Ok(val) = env::var(format!("{}_ENV", prefix)) {
            self.env = Some(val);
        }
        if let Ok(val) = env::var(format!("{}_PRIVACY_REDACT", prefix)) {
            self.privacy_redact = val.parse()
                .map_err(|_| CoreError::config(format!("Invalid privacy redact flag: {}", val)))?;
        }
        if let Ok(val) = env::var(format!("{}_PRIVACY_SALT", prefix)) {
            self.privacy_salt = Some(val);
        }

        Ok(())
    }

    /// Validate configuration values
    pub fn validate(&self) -> Result<()> {
        // Validate log level
        match self.log_level.to_lowercase().as_str() {
            "trace" | "debug" | "info" | "warn" | "error" => {},
            _ => return Err(CoreError::config(format!("Invalid log level: {}", self.log_level))),
        }

        // Validate log format
        match self.log_format.to_lowercase().as_str() {
            "text" | "json" => {},
            _ => return Err(CoreError::config(format!("Invalid log format: {}", self.log_format))),
        }

        // Validate concurrency limit
        if self.concurrency_limit == 0 {
            return Err(CoreError::config("Concurrency limit must be greater than 0".to_string()));
        }

        if self.concurrency_limit > 1000 {
            return Err(CoreError::config("Concurrency limit too high (max 1000)".to_string()));
        }

        // Validate request timeout
        if self.request_timeout_ms == 0 {
            return Err(CoreError::config("Request timeout must be greater than 0".to_string()));
        }

        // Validate tool/LSP specific limits
        if self.tools_max_concurrency == 0 {
            return Err(CoreError::config("Tools max concurrency must be greater than 0".to_string()));
        }
        if self.tool_default_timeout_ms == 0 || self.tool_default_timeout_ms > 600_000 {
            return Err(CoreError::config("Tool default timeout must be in 1..=600000 ms".to_string()));
        }
        if self.lsp_request_timeout_ms == 0 || self.lsp_request_timeout_ms > 300_000 {
            return Err(CoreError::config("LSP request timeout must be in 1..=300000 ms".to_string()));
        }
        if self.lsp_total_concurrency == 0 || self.lsp_total_concurrency > 1024 {
            return Err(CoreError::config("LSP total concurrency must be in 1..=1024".to_string()));
        }
        if self.lsp_per_language_concurrency == 0 || self.lsp_per_language_concurrency > 256 {
            return Err(CoreError::config("LSP per-language concurrency must be in 1..=256".to_string()));
        }

        if self.request_timeout_ms > 300_000 { // 5 minutes max
            return Err(CoreError::config("Request timeout too high (max 5 minutes)".to_string()));
        }

        // Validate workspace root exists
        if !self.workspace_root.exists() {
            return Err(CoreError::config(format!("Workspace root does not exist: {}", self.workspace_root.display())));
        }

        // Validate security
        if !self.allow_process_execution && !self.process_whitelist.is_empty() {
            // Warning-like validation: whitelist ignored if execution disabled
        }

        // Validate max file size
        if self.max_file_size > 1024 * 1024 * 1024 { // 1GB max
            return Err(CoreError::config("Max file size too large (max 1GB)".to_string()));
        }

        Ok(())
    }

    /// Check if configuration file has been modified (for hot reload)
    pub fn file_modified_since(&self, since: SystemTime) -> Result<bool> {
        if let Some(config_file) = &self.config_file {
            let metadata = fs::metadata(config_file)
                .map_err(|e| CoreError::io_with_path(format!("Failed to read config file metadata: {}", e), config_file.display().to_string()))?;

            let modified = metadata.modified()
                .map_err(|e| CoreError::io(format!("Failed to get modification time: {}", e)))?;

            Ok(modified > since)
        } else {
            Ok(false)
        }
    }

    /// Reload configuration from file if it has been modified
    pub fn reload_if_modified(&mut self, since: SystemTime) -> Result<bool> {
        if self.hot_reload && self.file_modified_since(since)? {
            if let Some(config_file) = &self.config_file {
                let new_config = Self::load_from_file(config_file)?;
                *self = new_config;
                return Ok(true);
            }
        }
        Ok(false)
    }
}