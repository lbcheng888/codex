use crate::config::RuntimeConfig;
use parking_lot::Mutex;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tracing_subscriber::{fmt, layer::SubscriberExt, EnvFilter, Registry, Layer};
use serde::{Deserialize, Serialize};

// OpenTelemetry 集成在测试环境下暂时禁用（避免跨版本冲突）
// 若需启用，请在统一的依赖版本下实现 build_otel_layer 返回真实 Layer

/// Performance metrics collector
#[derive(Debug, Default)]
pub struct Metrics {
    pub tool_executions: AtomicU64,
    pub tool_errors: AtomicU64,
    pub lsp_requests: AtomicU64,
    pub lsp_errors: AtomicU64,
    pub total_request_time_ms: AtomicU64,
    pub active_connections: AtomicU64,
}

/// 统一的指标事件输出，关键路径包含时延/错误码/重试次数
pub fn metric_event(kind: &str, duration_ms: u64, error_code: Option<i64>, retry_count: u32, extra: Option<&serde_json::Value>) {
    if let Some(extra) = extra {
        tracing::info!(event_kind = kind, duration_ms, error_code, retry_count, extra = %extra, "metric_event");
    } else {
        tracing::info!(event_kind = kind, duration_ms, error_code, retry_count, "metric_event");
    }
}

/// 简单脱敏：对可能包含用户数据的字符串做带 salt 的 SHA-256
pub fn redact(input: &str, salt: Option<&str>) -> String {
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    if let Some(s) = salt { hasher.update(s.as_bytes()); }
    hasher.update(input.as_bytes());
    let out = hasher.finalize();
    hex::encode(out)
}

impl Metrics {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn increment_tool_executions(&self) {
        self.tool_executions.fetch_add(1, Ordering::Relaxed);
    }

    pub fn increment_tool_errors(&self) {
        self.tool_errors.fetch_add(1, Ordering::Relaxed);
    }

    pub fn increment_lsp_requests(&self) {
        self.lsp_requests.fetch_add(1, Ordering::Relaxed);
    }

    pub fn increment_lsp_errors(&self) {
        self.lsp_errors.fetch_add(1, Ordering::Relaxed);
    }

    pub fn add_request_time(&self, duration: Duration) {
        self.total_request_time_ms.fetch_add(duration.as_millis() as u64, Ordering::Relaxed);
    }

    pub fn increment_active_connections(&self) {
        self.active_connections.fetch_add(1, Ordering::Relaxed);
    }

    pub fn decrement_active_connections(&self) {
        self.active_connections.fetch_sub(1, Ordering::Relaxed);
    }

    pub fn snapshot(&self) -> MetricsSnapshot {
        MetricsSnapshot {
            tool_executions: self.tool_executions.load(Ordering::Relaxed),
            tool_errors: self.tool_errors.load(Ordering::Relaxed),
            lsp_requests: self.lsp_requests.load(Ordering::Relaxed),
            lsp_errors: self.lsp_errors.load(Ordering::Relaxed),
            total_request_time_ms: self.total_request_time_ms.load(Ordering::Relaxed),
            active_connections: self.active_connections.load(Ordering::Relaxed),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricsSnapshot {
    pub tool_executions: u64,
    pub tool_errors: u64,
    pub lsp_requests: u64,
    pub lsp_errors: u64,
    pub total_request_time_ms: u64,
    pub active_connections: u64,
}

impl MetricsSnapshot {
    pub fn tool_error_rate(&self) -> f64 {
        if self.tool_executions == 0 {
            0.0
        } else {
            self.tool_errors as f64 / self.tool_executions as f64
        }
    }

    pub fn lsp_error_rate(&self) -> f64 {
        if self.lsp_requests == 0 {
            0.0
        } else {
            self.lsp_errors as f64 / self.lsp_requests as f64
        }
    }

    pub fn average_request_time_ms(&self) -> f64 {
        if self.tool_executions + self.lsp_requests == 0 {
            0.0
        } else {
            self.total_request_time_ms as f64 / (self.tool_executions + self.lsp_requests) as f64
        }
    }
}

/// Health check status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthStatus {
    pub status: HealthState,
    pub timestamp: std::time::SystemTime,
    pub metrics: MetricsSnapshot,
    pub checks: Vec<HealthCheck>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HealthState {
    Healthy,
    Degraded,
    Unhealthy,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthCheck {
    pub name: String,
    pub status: HealthState,
    pub message: Option<String>,
    pub duration_ms: u64,
}

/// Global metrics instance
static METRICS: once_cell::sync::Lazy<Arc<Metrics>> = once_cell::sync::Lazy::new(|| {
    Arc::new(Metrics::new())
});

pub fn get_metrics() -> Arc<Metrics> {
    METRICS.clone()
}

/// Lightweight latency histogram for percentile computation (in-memory, bounded)
#[derive(Debug)]
pub struct LatencyHistogram {
    window: VecDeque<u128>,
    capacity: usize,
}

impl LatencyHistogram {
    pub fn new(capacity: usize) -> Self {
        Self { window: VecDeque::with_capacity(capacity), capacity }
    }
    pub fn record(&mut self, dur: Duration) {
        if self.window.len() == self.capacity { self.window.pop_front(); }
        self.window.push_back(dur.as_millis());
    }
    pub fn percentile(&self, p: f64) -> Option<u128> {
        if self.window.is_empty() { return None; }
        let mut v: Vec<u128> = self.window.iter().cloned().collect();
        v.sort_unstable();
        let idx = ((p.clamp(0.0, 100.0) / 100.0) * ((v.len() - 1) as f64)).round() as usize;
        v.get(idx).cloned()
    }
}

static TOOL_LAT_HIST: once_cell::sync::Lazy<Arc<Mutex<LatencyHistogram>>> = once_cell::sync::Lazy::new(|| {
    // Keep last 1024 executions
    Arc::new(Mutex::new(LatencyHistogram::new(1024)))
});

pub fn record_tool_latency(duration: Duration) {
    let hist = TOOL_LAT_HIST.clone();
    let mut guard = hist.lock();
    guard.record(duration);
}

pub fn current_tool_p50_p95() -> (Option<u128>, Option<u128>) {
    let hist = TOOL_LAT_HIST.clone();
    let guard = hist.lock();
    (guard.percentile(50.0), guard.percentile(95.0))
}

/// Performance timing helper
pub struct Timer {
    start: Instant,
    name: String,
}

impl Timer {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            start: Instant::now(),
            name: name.into(),
        }
    }

    pub fn elapsed(&self) -> Duration {
        self.start.elapsed()
    }
}

impl Drop for Timer {
    fn drop(&mut self) {
        let duration = self.elapsed();
        tracing::info!(
            timer = %self.name,
            duration_ms = duration.as_millis(),
            "Timer completed"
        );
        get_metrics().add_request_time(duration);
    }
}

/// Structured logging helper
pub fn log_tool_execution(tool_name: &str, duration: Duration, success: bool) {
    let metrics = get_metrics();
    metrics.increment_tool_executions();

    if success {
        tracing::info!(
            tool = tool_name,
            duration_ms = duration.as_millis(),
            success = true,
            "Tool execution completed"
        );
    } else {
        metrics.increment_tool_errors();
        tracing::error!(
            tool = tool_name,
            duration_ms = duration.as_millis(),
            success = false,
            "Tool execution failed"
        );
    }
}

pub fn log_lsp_request(language: &str, method: &str, duration: Duration, success: bool) {
    let metrics = get_metrics();
    metrics.increment_lsp_requests();

    if success {
        tracing::info!(
            language = language,
            method = method,
            duration_ms = duration.as_millis(),
            success = true,
            "LSP request completed"
        );
    } else {
        metrics.increment_lsp_errors();
        tracing::error!(
            language = language,
            method = method,
            duration_ms = duration.as_millis(),
            success = false,
            "LSP request failed"
        );
    }
}

pub fn init_tracing(cfg: &RuntimeConfig) {
    init_tracing_with_log(cfg, true);
}

/// 审计日志：记录敏感操作（如进程执行、文件写入等）
pub fn audit_log(action: &str, target: &str, success: bool, extra: Option<&serde_json::Value>) {
    if let Some(extra) = extra {
        tracing::info!(target = "audit", action = action, target = target, success = success, extra = %extra, "audit_event");
    } else {
        tracing::info!(target = "audit", action = action, target = target, success = success, "audit_event");
    }
}

pub fn init_tracing_silent(cfg: &RuntimeConfig) {
    init_tracing_with_log(cfg, false);
}

fn init_tracing_with_log(cfg: &RuntimeConfig, log_init: bool) {
    let filter = EnvFilter::try_from_default_env()
        .or_else(|_| EnvFilter::try_new(&cfg.log_level))
        .unwrap_or_else(|_| EnvFilter::new("info"));

    let fmt_layer = if cfg.log_format == "json" {
        fmt::layer()
            .json()
            .with_target(true)
            .with_file(true)
            .with_line_number(true)
            .with_thread_ids(true)
            .with_thread_names(true)
            .with_writer(std::io::stderr)
            .boxed()
    } else {
        fmt::layer()
            .with_target(true)
            .with_file(true)
            .with_line_number(true)
            .with_thread_ids(true)
            .with_thread_names(true)
            .with_writer(std::io::stderr)
            .boxed()
    };

    // 仅启用 fmt 层，不拼装 OTel 层，避免跨版本冲突
    let subscriber = Registry::default().with(filter).with(fmt_layer);
    let _ = tracing::subscriber::set_global_default(subscriber);

    if log_init {
        tracing::info!(
            version = env!("CARGO_PKG_VERSION"),
            log_level = %cfg.log_level,
            log_format = %cfg.log_format,
            otel_enabled = cfg.otel_enable,
            otel_endpoint = cfg.otel_endpoint.as_deref().unwrap_or(""),
            env = cfg.env.as_deref().unwrap_or("dev"),
            "Tracing initialized"
        );
    }
}


/// 优雅关闭 tracing（刷新 OTel 导出）
pub fn shutdown_tracing() {
    // 当前未启用 OTel，无需显式关闭
}

/// Health check implementation
pub async fn perform_health_check() -> HealthStatus {
    let start = Instant::now();
    let mut checks = Vec::new();
    let metrics = get_metrics().snapshot();

    // Check error rates
    let tool_error_check = if metrics.tool_error_rate() > 0.1 {
        HealthCheck {
            name: "tool_error_rate".to_string(),
            status: HealthState::Degraded,
            message: Some(format!("Tool error rate: {:.2}%", metrics.tool_error_rate() * 100.0)),
            duration_ms: 0,
        }
    } else {
        HealthCheck {
            name: "tool_error_rate".to_string(),
            status: HealthState::Healthy,
            message: None,
            duration_ms: 0,
        }
    };
    checks.push(tool_error_check);

    let lsp_error_check = if metrics.lsp_error_rate() > 0.1 {
        HealthCheck {
            name: "lsp_error_rate".to_string(),
            status: HealthState::Degraded,
            message: Some(format!("LSP error rate: {:.2}%", metrics.lsp_error_rate() * 100.0)),
            duration_ms: 0,
        }
    } else {
        HealthCheck {
            name: "lsp_error_rate".to_string(),
            status: HealthState::Healthy,
            message: None,
            duration_ms: 0,
        }
    };
    checks.push(lsp_error_check);

    // Check response times
    let response_time_check = if metrics.average_request_time_ms() > 5000.0 {
        HealthCheck {
            name: "response_time".to_string(),
            status: HealthState::Degraded,
            message: Some(format!("Average response time: {:.0}ms", metrics.average_request_time_ms())),
            duration_ms: 0,
        }
    } else {
        HealthCheck {
            name: "response_time".to_string(),
            status: HealthState::Healthy,
            message: None,
            duration_ms: 0,
        }
    };
    checks.push(response_time_check);

    // Determine overall status
    let overall_status = if checks.iter().any(|c| matches!(c.status, HealthState::Unhealthy)) {
        HealthState::Unhealthy
    } else if checks.iter().any(|c| matches!(c.status, HealthState::Degraded)) {
        HealthState::Degraded
    } else {
        HealthState::Healthy
    };

    let duration = start.elapsed();

    HealthStatus {
        status: overall_status,
        timestamp: std::time::SystemTime::now(),
        metrics,
        checks: checks.into_iter().map(|mut c| {
            c.duration_ms = duration.as_millis() as u64;
            c
        }).collect(),
    }
}