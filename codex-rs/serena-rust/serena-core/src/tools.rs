use crate::config::RuntimeConfig;
use crate::error::{CoreError, Result};
use crate::observability::{current_tool_p50_p95, log_tool_execution, record_tool_latency};
use crate::storage::{InMemoryStorage, Storage};
use parking_lot::RwLock;
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Semaphore;
use tokio::time::{timeout, Duration};
use tracing::instrument;

/// 对外稳定的错误分类，便于调用方（MCP 服务器）进行精准映射
#[derive(Clone, Copy, Debug)]
pub enum CoreErrorKind {
    Timeout,
    NotFound,
    Invalid,
    Internal,
}

/// 为 CoreError 提供轻量分类方法（不依赖内部细节，基于消息匹配作为后备策略）
/// 注意：真正的 CoreError 若已有结构化枚举，可在该 impl 中直接 match 对应分支
impl CoreError {
    pub fn kind(&self) -> CoreErrorKind {
        let msg = self.to_string();
        if msg.contains("timed out") || msg.contains("timeout") {
            CoreErrorKind::Timeout
        } else if msg.contains("not found") {
            CoreErrorKind::NotFound
        } else if msg.contains("invalid") || msg.contains("Invalid") {
            CoreErrorKind::Invalid
        } else {
            CoreErrorKind::Internal
        }
    }
}

#[derive(Clone)]
pub struct ToolContext {
    pub cfg: Arc<RuntimeConfig>,
    pub storage: Arc<dyn Storage>,
}

#[async_trait::async_trait]
pub trait Tool: Send + Sync {
    fn name(&self) -> &'static str;
    fn description(&self) -> &'static str {
        "No description available"
    }
    fn schema(&self) -> Value {
        serde_json::json!({})
    }
    async fn run(&self, input: Value, ctx: ToolContext) -> Result<Value>;
}

#[derive(Clone)]
pub struct ToolRegistry {
    inner: Arc<RwLock<HashMap<String, Arc<dyn Tool>>>>,
    schema_cache: Arc<RwLock<HashMap<String, Value>>>,
    // preserve insertion order for optional ordered listing
    order: Arc<RwLock<Vec<String>>>,
    // concurrency control for tool executions
    semaphore: Arc<Semaphore>,
}

impl ToolRegistry {
    pub fn new() -> Self {
        let max = std::env::var("SERENA_TOOLS_MAX_CONCURRENCY")
            .ok()
            .and_then(|s| s.parse::<usize>().ok())
            .filter(|&v| v > 0)
            .unwrap_or(32);
        Self {
            inner: Arc::new(RwLock::new(HashMap::new())),
            schema_cache: Arc::new(RwLock::new(HashMap::new())),
            order: Arc::new(RwLock::new(Vec::new())),
            semaphore: Arc::new(Semaphore::new(max)),
        }
    }

    /// 使用运行时配置创建注册表，支持并发上限配置
    pub fn from_config(cfg: &RuntimeConfig) -> Self {
        let max = cfg.tools_max_concurrency.max(1);
        Self {
            inner: Arc::new(RwLock::new(HashMap::new())),
            schema_cache: Arc::new(RwLock::new(HashMap::new())),
            order: Arc::new(RwLock::new(Vec::new())),
            semaphore: Arc::new(Semaphore::new(max)),
        }
    }
    
    pub fn register<T: Tool + 'static>(&self, tool: T) {
        let name = tool.name().to_string();
        let schema = tool.schema();
        let tool_arc = Arc::new(tool);
        
        {
            let mut tools = self.inner.write();
            tools.insert(name.clone(), tool_arc);
        }
        
        {
            let mut cache = self.schema_cache.write();
            cache.insert(name.clone(), schema);
        }

        {
            let mut order = self.order.write();
            if !order.contains(&name) {
                order.push(name);
            }
        }
    }
    
    pub fn list(&self) -> Vec<(String, Value)> {
        let cache = self.schema_cache.read();
        cache.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
    }

    /// List tools in MCP format
    ///
    /// Python/Rust 对齐要求：
    /// - 同时导出 "parameters" 与 "inputSchema" 两个键，保持客户端兼容性
    /// - 若 schema 缺失 required 字段，则补齐为空数组，避免严格客户端报错
    pub fn list_mcp_format(&self) -> Vec<Value> {
        let tools = self.inner.read();
        let cache = self.schema_cache.read();
        let order = self.order.read();

        // allow env-controlled ordering: alpha (default) or insertion
        let sort_mode = std::env::var("SERENA_MCP_TOOLS_ORDER").unwrap_or_else(|_| "alpha".to_string());
        let names: Vec<String> = if sort_mode.eq_ignore_ascii_case("insertion") {
            order.clone()
        } else {
            let mut v: Vec<String> = tools.keys().cloned().collect();
            v.sort();
            v
        };

        let mut out = Vec::with_capacity(names.len());
        for name in names {
            if let Some(tool) = tools.get(&name) {
                if let Some(schema) = cache.get(&name) {
                    // ensure object schema with required for compatibility
                    let mut schema_with_required = schema.clone();
                    if let Some(obj) = schema_with_required.as_object_mut() {
                        if !obj.contains_key("type") {
                            obj.insert("type".to_string(), Value::String("object".to_string()));
                        }
                        obj.entry("properties").or_insert_with(|| Value::Object(serde_json::Map::new()));
                        let is_object = obj
                            .get("type")
                            .and_then(|t| t.as_str())
                            .map(|t| t == "object")
                            .unwrap_or(false);
                        if is_object && !obj.contains_key("required") {
                            obj.insert("required".to_string(), Value::Array(vec![]));
                        }
                    } else {
                        // replace non-object schema with minimal object schema
                        schema_with_required = serde_json::json!({
                            "type": "object",
                            "properties": {},
                            "required": []
                        });
                    }

                    out.push(serde_json::json!({
                        "name": name,
                        "description": tool.description(),
                        // 兼容键：多数 Rust 端历史实现
                        "inputSchema": schema_with_required,
                        // 兼容键：多数 Python/pydantic 风格
                        "parameters": schema_with_required
                    }));
                }
            }
        }

        out
    }

    #[instrument(skip(self, ctx, input), fields(tool = %name, timeout_ms = timeout_ms))]
    pub async fn run_with_timeout(
        &self,
        name: &str,
        input: Value,
        ctx: ToolContext,
        timeout_ms: u64,
    ) -> Result<Value> {
        // Acquire concurrency permit for isolation and rate limiting
        let _permit = self.semaphore.acquire().await.map_err(|_| CoreError::internal("semaphore closed"))?;

        let tool = {
            let g = self.inner.read();
            g.get(name).cloned()
        }
        .ok_or_else(|| CoreError::not_found(format!("tool {name}")))?;

        let start = std::time::Instant::now();
        let fut = tool.run(input, ctx);
        let res = timeout(Duration::from_millis(timeout_ms), fut).await;
        let duration = start.elapsed();
        // record metrics and percentiles
        record_tool_latency(duration);
        match res {
            Ok(Ok(v)) => {
                log_tool_execution(name, duration, true);
                if let (Some(p50), Some(p95)) = current_tool_p50_p95() {
                    tracing::info!(tool = name, duration_ms = duration.as_millis(), p50_ms = p50, p95_ms = p95, "tool_latency_update");
                } else {
                    tracing::info!(tool = name, duration_ms = duration.as_millis(), "tool_latency_recorded");
                }
                Ok(v)
            }
            Ok(Err(e)) => {
                log_tool_execution(name, duration, false);
                if let (Some(p50), Some(p95)) = current_tool_p50_p95() {
                    tracing::error!(tool = name, duration_ms = duration.as_millis(), p50_ms = p50, p95_ms = p95, error = %e, "tool_failed");
                } else {
                    tracing::error!(tool = name, duration_ms = duration.as_millis(), error = %e, "tool_failed");
                }
                Err(e)
            }
            Err(_) => {
                log_tool_execution(name, duration, false);
                if let (Some(p50), Some(p95)) = current_tool_p50_p95() {
                    tracing::error!(tool = name, duration_ms = duration.as_millis(), p50_ms = p50, p95_ms = p95, "tool_timeout");
                } else {
                    tracing::error!(tool = name, duration_ms = duration.as_millis(), "tool_timeout");
                }
                Err(CoreError::timeout(format!("Tool {name} timed out"), timeout_ms))
            }
        }
    }

    /// 使用配置默认超时运行工具
    pub async fn run_default_timeout(
        &self,
        name: &str,
        input: Value,
        ctx: ToolContext,
    ) -> Result<Value> {
        let timeout_ms = ctx.cfg.tool_default_timeout_ms;
        self.run_with_timeout(name, input, ctx, timeout_ms).await
    }

    pub fn default_context(cfg: Arc<RuntimeConfig>) -> ToolContext {
        ToolContext {
            cfg,
            storage: Arc::new(InMemoryStorage::default()),
        }
    }
}
