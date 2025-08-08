use serde_json::Value;
use serena_core::{tools::ToolContext, tools::ToolRegistry, RuntimeConfig};
use std::sync::Arc;
use tracing::{debug, error, info, instrument, warn, Span};

use crate::jsonrpc::{Request, Response};

pub struct McpOptions {
    pub ready_line: bool,
    pub transport: Option<String>,
    pub project_root: Option<String>,
}

impl Default for McpOptions {
    fn default() -> Self {
        Self {
            ready_line: false,  // 禁用ready消息，符合标准MCP协议
            transport: None,
            project_root: None,
        }
    }
}

pub struct McpServer {
    cfg: Arc<RuntimeConfig>,
    reg: ToolRegistry,
    #[allow(dead_code)]
    opts: McpOptions,
}

impl McpServer {
    pub fn new(cfg: Arc<RuntimeConfig>, reg: ToolRegistry, opts: McpOptions) -> Self {
        Self { cfg, reg, opts }
    }

    #[instrument(skip(self), fields(transport = "stdio"))]
    pub async fn run_stdio(self) -> anyhow::Result<()> {
        // 根据环境变量决定是否启用 Content-Length 分帧（默认启用，兼容要求更高的客户端）
        // SERENA_MCP_CONTENT_LENGTH=0 可显式关闭以回退到换行分包
        let use_content_length = std::env::var("SERENA_MCP_CONTENT_LENGTH")
            .ok()
            .and_then(|v| v.parse::<u8>().ok())
            .map(|v| v != 0)
            .unwrap_or(false); // default to JSON Lines framing for compatibility

        if use_content_length {
            self.run_stdio_sync_framed()
        } else {
            self.run_stdio_sync()
        }
    }

    fn run_stdio_sync(self) -> anyhow::Result<()> {
        let ctx = ToolRegistry::default_context(self.cfg.clone());
        let cfg = self.cfg.clone();
        let reg = self.reg.clone();

        // 使用同步stdio，与Python版本保持一致
        use std::io::{self, BufRead, Write};
        let stdin = io::stdin();
        let mut stdout = io::stdout();

        // ready line - 简单的JSON对象，不是JSON-RPC格式
        if self.opts.ready_line {
            writeln!(stdout, r#"{{"ready": true}}"#)?;
            stdout.flush()?;
        }

        // 同步处理stdin，与Python版本保持一致
        let stdin_reader = stdin.lock();
        for line in stdin_reader.lines() {
            let line = line?;
            if line.trim().is_empty() {
                debug!(target = "mcp", "skip empty line");
                continue; // 静默跳过空行，不返回错误
            }

            // 避免记录原始请求内容，按配置脱敏
            if cfg.privacy_redact {
                let hash = serena_core::observability::redact(&line, cfg.privacy_salt.as_deref());
                debug!(target = "mcp", recv_len = line.len(), recv_hash = %hash, "received line (redacted)");
            } else {
                debug!(target = "mcp", recv_len = line.len(), "received line");
            }

            // Parse JSON-RPC request
            let req: Request = match serde_json::from_str(&line) {
                Ok(r) => r,
                Err(e) => {
                    warn!(target = "mcp", %e, "parse error");
                    let err = Response::err(Value::Null, -32700, "parse error", None);
                    let err_json = serde_json::to_string(&err)?;
                    writeln!(stdout, "{}", err_json)?;
                    stdout.flush()?;
                    continue;
                }
            };

            // Handle request synchronously
            let id = req.id.clone().unwrap_or(Value::Null);
            let method = req.method.clone();
            let _params = req.params;

            // Skip response for notifications (no id) and initialized
            let should_respond = req.id.is_some() && method != "initialized";

            let ctx_instance = ToolContext { cfg: cfg.clone(), storage: ctx.storage.clone() };

            // 创建同步版本的请求处理
            let resp = Self::handle_request_sync(id, method, _params, ctx_instance, &reg);

            if should_respond {
                let resp_json = serde_json::to_string(&resp)?;
                writeln!(stdout, "{}", resp_json)?;
                stdout.flush()?;
            }
        }

        Ok(())
    }

    // 基于 Content-Length 的分帧 I/O（兼容部分严格客户端）
    fn run_stdio_sync_framed(self) -> anyhow::Result<()> {
        use std::io::{self, Read, Write};
        let ctx = ToolRegistry::default_context(self.cfg.clone());
        let cfg = self.cfg.clone();
        let reg = self.reg.clone();

        let mut stdin = io::stdin().lock();
        let mut stdout = io::stdout();

        if self.opts.ready_line {
            let ready = br#"{"ready": true}"#;
            write!(stdout, "Content-Length: {}\r\n\r\n", ready.len())?;
            stdout.write_all(ready)?;
            stdout.flush()?;
        }

        loop {
            // 读取头部直到空行
            // 使用逐字节读取避免混合缓冲问题
            let mut buf = [0u8; 1];
            let mut last4 = [0u8; 4];
            let mut header_bytes: Vec<u8> = Vec::with_capacity(128);
            // 读取直到 \r\n\r\n
            loop {
                let n = stdin.read(&mut buf)?;
                if n == 0 {
                    return Ok(()); // EOF
                }
                header_bytes.push(buf[0]);
                last4.rotate_left(1);
                last4[3] = buf[0];
                if &last4 == b"\r\n\r\n" {
                    break;
                }
                // 防御：头部超长也提前失败
                if header_bytes.len() > 16 * 1024 {
                    let err = Response::err(Value::Null, -32700, "header too large", None);
                    let json = serde_json::to_string(&err)?;
                    write!(stdout, "Content-Length: {}\r\n\r\n", json.len())?;
                    stdout.write_all(json.as_bytes())?;
                    stdout.flush()?;
                    return Ok(());
                }
            }
            let header = String::from_utf8_lossy(&header_bytes).into_owned();

            // 解析 Content-Length
            let mut content_length: Option<usize> = None;
            for line in header.split("\r\n") {
                if let Some(v) = line.strip_prefix("Content-Length:") {
                    content_length = v.trim().parse::<usize>().ok();
                    break;
                }
            }
            let len = match content_length {
                Some(v) => v,
                None => {
                    let err = Response::err(Value::Null, -32700, "missing Content-Length", None);
                    let json = serde_json::to_string(&err)?;
                    write!(stdout, "Content-Length: {}\r\n\r\n", json.len())?;
                    stdout.write_all(json.as_bytes())?;
                    stdout.flush()?;
                    continue;
                }
            };

            // 读取指定长度的正文
            let mut body = vec![0u8; len];
            let mut read_total = 0usize;
            while read_total < len {
                let n = stdin.read(&mut body[read_total..])?;
                if n == 0 {
                    break;
                }
                read_total += n;
            }
            if read_total != len {
                let err = Response::err(Value::Null, -32700, "incomplete body", None);
                let json = serde_json::to_string(&err)?;
                write!(stdout, "Content-Length: {}\r\n\r\n", json.len())?;
                stdout.write_all(json.as_bytes())?;
                stdout.flush()?;
                continue;
            }

            // 解析 JSON-RPC
            let req: Request = match serde_json::from_slice(&body) {
                Ok(r) => r,
                Err(e) => {
                    warn!(target = "mcp", %e, "parse error (framed)");
                    let err = Response::err(Value::Null, -32700, "parse error", None);
                    let json = serde_json::to_string(&err)?;
                    write!(stdout, "Content-Length: {}\r\n\r\n", json.len())?;
                    stdout.write_all(json.as_bytes())?;
                    stdout.flush()?;
                    continue;
                }
            };

            let id = req.id.clone().unwrap_or(Value::Null);
            let method = req.method.clone();
            let _params = req.params;

            let should_respond = req.id.is_some() && method != "initialized";
            let ctx_instance = ToolContext { cfg: cfg.clone(), storage: ctx.storage.clone() };

            let resp = Self::handle_request_sync(id, method, _params, ctx_instance, &reg);
            if should_respond {
                let json = serde_json::to_string(&resp)?;
                write!(stdout, "Content-Length: {}\r\n\r\n", json.len())?;
                stdout.write_all(json.as_bytes())?;
                stdout.flush()?;
            }
        }
    }

    fn handle_request_sync(id: Value, method: String, params: Value, _ctx: ToolContext, reg: &ToolRegistry) -> Response {
        match method.as_str() {
            "ping" => Response::ok(id, serde_json::json!({"pong": true})),
            "initialize" => {
                debug!(target = "mcp", "initialize");
                // 允许通过环境变量覆盖 serverInfo 与 instructions，以对齐 Python/FastMCP 行为
                let server_name = std::env::var("SERENA_MCP_SERVERINFO_NAME").unwrap_or_else(|_| "Serena-Rust".to_string());
                let server_version = std::env::var("SERENA_MCP_SERVERINFO_VERSION").unwrap_or_else(|_| env!("CARGO_PKG_VERSION").to_string());
                let instructions_default = "You are a professional coding agent concerned with one particular codebase. You have access to semantic coding tools on which you rely heavily for all your work, as well as collection of memory files containing general information about the codebase. You operate in a resource-efficient and intelligent manner, always keeping in mind to not read or generate content that is not needed for the task at hand.\n\nWhen reading code in order to answer a user question or task, you should try reading only the necessary code. Some tasks may require you to understand the architecture of large parts of the codebase, while for others, it may be enough to read a small set of symbols or a single file. Generally, you should avoid reading entire files unless it is absolutely necessary, instead relying on intelligent step-by-step acquisition of information. However, if you already read a file, it does not make sense to further analyse it with the symbolic tools (except for the `find_referencing_symbols` tool), as you already have the information.".to_string();
                let instructions = if let Ok(path) = std::env::var("SERENA_MCP_INSTRUCTIONS_FILE") {
                    std::fs::read_to_string(path).unwrap_or_else(|_| instructions_default.clone())
                } else {
                    std::env::var("SERENA_MCP_INSTRUCTIONS").unwrap_or(instructions_default)
                };

                Response::ok(id, serde_json::json!({
                    "protocolVersion": "2024-11-05",
                    "capabilities": {
                        "experimental": {},
                        "prompts": { "listChanged": false },
                        "resources": { "subscribe": false, "listChanged": false },
                        "tools": { "listChanged": false }
                    },
                    "serverInfo": { "name": server_name, "version": server_version },
                    "instructions": instructions
                }))
            },
            "tools/list" => {
                // 返回真实的工具列表
                let tools = reg.list_mcp_format();
                Response::ok(id, serde_json::json!({
                    "tools": tools
                }))
            },
            "prompts/list" => {
                // 对齐能力声明，返回空列表
                Response::ok(id, serde_json::json!({ "prompts": [] }))
            },
            "resources/list" => {
                // 对齐能力声明，返回空列表
                Response::ok(id, serde_json::json!({ "resources": [] }))
            },
            "tools/call" => {
                // 将同步分发改为桥接异步执行，复用下方 async 版本 handle_request 的健壮实现
                // 这样可以获得超时、并发控制、错误规范化与统一日志
                let ctx_clone = _ctx.clone();
                let reg_clone = reg.clone();
                // 在当前线程阻塞等待异步处理完成（同步 stdio 循环约束）
                let resp = tokio::runtime::Handle::current().block_on(async move {
                    handle_request(&reg_clone, ctx_clone, "tools/call".to_string(), params, id.clone()).await
                });
                return resp;
            },
            _ => Response::err(id, -32601, &format!("Method not found: {}", method), None)
        }
    }
}

#[allow(dead_code)]
#[instrument(skip(reg, ctx, params), fields(method = %method))]
async fn handle_request(
    reg: &ToolRegistry,
    ctx: ToolContext,
    method: String,
    params: Value,
    id: Value,
) -> Response {
    match method.as_str() {
        "initialize" => {
            debug!(target = "mcp", "initialize");
            let server_name = std::env::var("SERENA_MCP_SERVERINFO_NAME").unwrap_or_else(|_| "Serena-Rust".to_string());
            let server_version = std::env::var("SERENA_MCP_SERVERINFO_VERSION").unwrap_or_else(|_| env!("CARGO_PKG_VERSION").to_string());
            let instructions_default = "You are a professional coding agent concerned with one particular codebase. You have \naccess to semantic coding tools on which you rely heavily for all your work, as well as collection of memory \nfiles containing general information about the codebase. You operate in a resource-efficient and intelligent manner, always\nkeeping in mind to not read or generate content that is not needed for the task at hand.\n\nWhen reading code in order to answer a user question or task, you should try reading only the necessary code. \nSome tasks may require you to understand the architecture of large parts of the codebase, while for others,\nit may be enough to read a small set of symbols or a single file.\nGenerally, you should avoid reading entire files unless it is absolutely necessary, instead relying on\nintelligent step-by-step acquisition of information. However, if you already read a file, it does not make\nsense to further analyse it with the symbolic tools (except for the `find_referencing_symbols` tool), \nas you already have the information.\nYou can achieve the intelligent reading of code by using the symbolic tools for getting an overview of symbols and\nthe relations between them, and then only reading the bodies of symbols that are necessary to answer the question \nor complete the task. \nYou can use the standard tools like list_dir, find_file and search_for_pattern if you need to.\nWhen tools allow it, you pass the `relative_path` parameter to restrict the search to a specific file or directory.\nFor some tools, `relative_path` can only be a file path, so make sure to properly read the tool descriptions.\nSymbols are identified by their `name_path and `relative_path`, see the description of the `find_symbol` tool for more details\non how the `name_path` matches symbols.\nYou can get information about available symbols by using the `get_symbols_overview` tool for finding top-level symbols in a file\nor directory, or by using `find_symbol` if you already know the symbol's name path. You generally try to read as little code as possible\nwhile still solving your task, meaning you only read the bodies when you need to, and after you have found the symbol you want to edit.".to_string();
            let instructions = if let Ok(path) = std::env::var("SERENA_MCP_INSTRUCTIONS_FILE") {
                std::fs::read_to_string(path).unwrap_or_else(|_| instructions_default.clone())
            } else {
                std::env::var("SERENA_MCP_INSTRUCTIONS").unwrap_or(instructions_default)
            };

            Response::ok(id, serde_json::json!({
                "protocolVersion": "2024-11-05",
                "capabilities": {
                    "experimental": {},
                    "prompts": { "listChanged": false },
                    "resources": { "subscribe": false, "listChanged": false },
                    "tools": { "listChanged": false }
                },
                "serverInfo": { "name": server_name, "version": server_version },
                "instructions": instructions
            }))
        }
        "initialized" => {
            debug!(target = "mcp", "initialized");
            Response::ok(id, serde_json::json!({}))
        }
        "tools/list" => {
            let tools = reg.list_mcp_format();
            debug!(target = "mcp", tool_count = tools.len(), "tools/list");
            Response::ok(id, serde_json::json!({ "tools": tools }))
        }
        "prompts/list" => {
            Response::ok(id, serde_json::json!({ "prompts": [] }))
        }
        "resources/list" => {
            Response::ok(id, serde_json::json!({ "resources": [] }))
        }
        "tools/call" => {
            use std::collections::HashMap;
            use tokio::sync::{OnceCell, Semaphore};

            // 全局默认并发（从环境读取，默认 8）
            fn default_global_permits() -> usize {
                std::env::var("SERENA_MCP_MAX_CONCURRENCY")
                    .ok()
                    .and_then(|v| v.parse::<usize>().ok())
                    .filter(|&v| v > 0)
                    .unwrap_or(8)
            }

            // 从环境读取特定工具的覆盖：SERENA_MCP_TOOL_CONCURRENCY__{tool_name}
            // 约定 tool_name 使用小写中划线
            fn per_tool_permits(tool: &str, global: usize) -> usize {
                let key = format!("SERENA_MCP_TOOL_CONCURRENCY__{}", tool);
                std::env::var(&key)
                    .ok()
                    .and_then(|v| v.parse::<usize>().ok())
                    .filter(|&v| v > 0)
                    .unwrap_or(global)
            }

            // 懒初始化“按工具名”的信号量映射（使用 std::sync::RwLock，避免新增依赖）
            static SEMAPHORES: OnceCell<std::sync::RwLock<HashMap<String, Arc<Semaphore>>>> =
                OnceCell::const_new();

            let start = std::time::Instant::now();

            let tool_name = params
                .get("tool")
                .or_else(|| params.get("name"))
                .and_then(|v| v.as_str())
                .unwrap_or_default()
                .to_string();

            let args = params
                .get("arguments")
                .or_else(|| params.get("args"))
                .or_else(|| params.get("input"))
                .cloned()
                .unwrap_or(Value::Null);

            let span = Span::current();
            span.record("tool_name", tracing::field::display(&tool_name));
            span.record("method", tracing::field::display("tools/call"));

            if tool_name.is_empty() {
                return Response::err(id, -32602, "invalid params: missing tool name", None);
            }

            // 读取超时设置（默认30s）
            let timeout_ms = std::env::var("SERENA_MCP_TOOL_TIMEOUT_MS")
                .ok()
                .and_then(|v| v.parse::<u64>().ok())
                .filter(|&v| v > 0)
                .unwrap_or(30_000);

            // 获取或创建该工具的信号量
            let semaphores = SEMAPHORES
                .get_or_init(|| async { std::sync::RwLock::new(HashMap::new()) })
                .await;

            // 读取或创建该工具的 Arc<Semaphore>，避免持锁 await
            let sem_arc: Arc<Semaphore> = {
                // 快速路径：读锁获取现有
                if let Some(existing) = semaphores.read().unwrap().get(&tool_name).cloned() {
                    existing
                } else {
                    // 写锁创建并插入
                    let mut w = semaphores.write().unwrap();
                    // 双检，避免竞态下重复创建
                    if let Some(existing) = w.get(&tool_name).cloned() {
                        existing
                    } else {
                        let global = default_global_permits();
                        let permits = per_tool_permits(&tool_name, global);
                        let sem = Arc::new(Semaphore::new(permits));
                        w.insert(tool_name.clone(), sem.clone());
                        sem
                    }
                }
            };

            // 在不持锁的情况下异步获取许可
            let permit = match sem_arc.acquire().await {
                Ok(p) => p,
                Err(_) => {
                    error!(target = "mcp", tool_name = %tool_name, "semaphore closed");
                    return Response::err(
                        id,
                        -32001,
                        "server unavailable: semaphore closed",
                        None,
                    );
                }
            };

            // 运行工具（ToolRegistry 内部已实现 tokio::time::timeout 包裹）
            let res = reg.run_with_timeout(&tool_name, args, ctx, timeout_ms).await;
            drop(permit); // 释放许可

            let dur = start.elapsed().as_millis() as u64;

            match res {
                Ok(v) => {
                    info!(target = "mcp", tool_name = %tool_name, duration_ms = dur, "run_tool ok");
                    // 指标事件：成功
                    serena_core::observability::metric_event("mcp.tools.call", dur, None, 0, None);
                    // 统一成功响应为 {"content":[...]} 包装（若返回非 content 结构则包装为 text）
                    let normalized = if v.get("content").is_some() {
                        v
                    } else {
                        serde_json::json!({
                            "content": [
                                { "type": "text", "text": v.to_string() }
                            ]
                        })
                    };
                    Response::ok(id, normalized)
                }
                Err(e) => {
                    use serena_core::tools::CoreErrorKind;

                    let kind = e.kind();
                    let (code, title) = match kind {
                        CoreErrorKind::Timeout => (-32002, "timeout"),
                        CoreErrorKind::NotFound => (-32601, "not_found"),
                        CoreErrorKind::Invalid => (-32602, "invalid_params"),
                        CoreErrorKind::Internal => (-32000, "internal_error"),
                    };

                    let msg = e.to_string();
                    // 规范化 error.data，便于客户端可观测
                    let data = serde_json::json!({
                        "tool": tool_name,
                        "duration_ms": dur,
                        "reason": msg,
                        "timeout_ms": timeout_ms
                    });
                    error!(target = "mcp", tool_name = %tool_name, duration_ms = dur, code = code, error_title = %title, error_message = %msg, "run_tool error");
                    // 指标事件：失败，包含错误码
                    serena_core::observability::metric_event("mcp.tools.call", dur, Some(code as i64), 0, None);
                    Response::err(id, code, title, Some(data))
                }
            }
        }
        _ => {
            error!(target = "mcp", method = %method, "method not found");
            Response::err(id, -32601, "method not found", None)
        }
    }
}
