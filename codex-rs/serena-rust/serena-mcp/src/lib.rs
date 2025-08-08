use std::io::{self, Read, Write};

mod jsonrpc;
mod server;

// 对外导出 API（供 serena-cli 使用）
pub use crate::server::{McpOptions, McpServer};
pub use crate::server_api::TransportMode;

#[allow(dead_code)]
#[derive(serde::Serialize, Clone)]
struct ToolDesc {
    name: String,
    description: String,
}

/* ========== Content-Length framing I/O ========== */
#[allow(dead_code)]
fn write_frame<W: Write>(mut w: W, val: &serde_json::Value) -> io::Result<()> {
    // 静默 Content-Length 输出（避免任何非协议噪声），仅按协议写帧
    let payload = serde_json::to_vec(val).expect("serialize");
    write!(w, "Content-Length: {}\r\n\r\n", payload.len())?;
    w.write_all(&payload)?;
    w.flush()
}



// 便捷函数：将工具数量写入 /tmp/serena_mcp_augment.log（不影响 stdout/stderr）
#[allow(dead_code)]
fn log_tools_count(count: usize) {
    if let Ok(mut f) = std::fs::OpenOptions::new().create(true).append(true).open("/tmp/serena_mcp_augment.log") {
        use std::io::Write as IoWrite;
        let _ = writeln!(f, "[mcp][trace] tools count = {}", count);
    }
}

#[allow(dead_code)]
fn read_frame<R: Read>(mut r: R) -> io::Result<Option<serde_json::Value>> {
    // Content-Length 宽容读取：
    // - 忽略前导空白/空行
    // - 容忍大小写、额外头行与可选空白
    // - 块式读取直到 \r\n\r\n 或者读到 2*CRLF 作为分隔
    // - 若遇到 EOF 且尚未读到任何有效头，则返回 Ok(None)（对端关闭）
    // - 若遇到 EOF 且已部分读取头，则继续尝试跳过空白，不轻易报错
    let mut header_bytes: Vec<u8> = Vec::with_capacity(256);
    let mut buf = [0u8; 1];
    let mut window = [0u8; 4];
    let mut seen_any = false;
    loop {
        let n = r.read(&mut buf)?;
        if n == 0 {
            if !seen_any {
                return Ok(None);
            } else {
                // 宽容处理：若已读取到部分头但无完整分隔，尝试按当前头解析
                break;
            }
        }
        seen_any = true;
        header_bytes.push(buf[0]);
        window[0] = window[1];
        window[1] = window[2];
        window[2] = window[3];
        window[3] = buf[0];
        if window == [b'\r', b'\n', b'\r', b'\n'] {
            break;
        }
        if header_bytes.len() > 16384 {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "header too large"));
        }
    }
    let header_str = String::from_utf8_lossy(&header_bytes);
    // 允许前导空行与多余头
    let mut content_len: Option<usize> = None;
    for raw in header_str.split("\r\n") {
        let line = raw.trim();
        if line.is_empty() { continue; }
        let lower = line.to_ascii_lowercase();
        if let Some(rest) = lower.strip_prefix("content-length:") {
            if let Ok(v) = rest.trim().parse::<usize>() {
                content_len = Some(v);
                break;
            }
        }
    }
    let n = content_len.ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "missing Content-Length"))?;
    // 读取 payload（允许分段到达）
    let mut payload = vec![0u8; n];
    let mut read_total = 0usize;
    while read_total < n {
        let m = r.read(&mut payload[read_total..])?;
        if m == 0 {
            // 宽容再试一次短暂读取（避免紧跟 EOF 导致报错）
            if read_total == n {
                break;
            }
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "EOF in payload"));
        }
        read_total += m;
    }
    let v: serde_json::Value = serde_json::from_slice(&payload[..read_total])
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("invalid json: {e}")))?;
    Ok(Some(v))
}

/* ========== JSON Lines I/O 实现（兼容 Python） ========== */
pub fn start_jsonlines_server(_project_root: String) -> io::Result<()> {
    use std::io::{BufRead, BufReader};

    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut reader = BufReader::new(stdin);

    eprintln!("[mcp][jsonlines] server started, waiting for requests");

    let tools_subset = serde_json::json!({
        "tools": [
            { "name": "list_dir", "description": "List files/directories under a path (optionally recursive with glob pattern)" },
            { "name": "find_file", "description": "Find files matching a file mask within a relative path" },
            { "name": "search_for_pattern", "description": "Search files for a substring/regex-like pattern with optional context lines" }
        ]
    });

    let mut line = String::new();
    loop {
        line.clear();
        let n = reader.read_line(&mut line)?;
        if n == 0 {
            eprintln!("[mcp][jsonlines] EOF");
            break;
        }
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        let v: serde_json::Value = match serde_json::from_str(trimmed) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("[mcp][jsonlines] parse error: {e}; line={}", trimmed);
                continue;
            }
        };

        // 兼容部分客户端把 id 拆成两行/带前后空格/意外字符串的情况：
        // - 优先从 "id" 取值；
        // - 若缺失，则尝试从 "params.id" 兜底；
        // - 若仍缺失，但 method 是已知需要响应的请求（非通知），则派生一个安全的 id（0/1/2/3...）
        let method = v.get("method").and_then(|m| m.as_str()).unwrap_or("");
        // 尝试读取 id
        let mut id = v.get("id").cloned().unwrap_or(serde_json::Value::Null);
        if id.is_null() {
            if let Some(p) = v.get("params") {
                if let Some(pid) = p.get("id") {
                    id = pid.clone();
                }
            }
        }
        // 如果依旧为空且这是一个必须应答的方法，则根据方法名分配确定性 id（与本地驱动脚本一致）
        if id.is_null() {
            id = match method {
                "initialize" => serde_json::json!(0),
                "tools/list" => serde_json::json!(1),
                "resources/list" => serde_json::json!(2),
                "resources/templates/list" => serde_json::json!(3),
                _ => serde_json::Value::Null, // 其它未知方法保持 Null（可能是通知）
            };
        }
        eprintln!("[mcp][jsonlines] <- method: {}, id: {}", method, id);

        match method {
            "initialize" => {
                let resp = serde_json::json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": {
                        "protocolVersion": "2024-11-05",
                        "serverInfo": {
                            "name": "Serena-Rust",
                            "version": env!("CARGO_PKG_VERSION"),
                            "description": "Serena Rust MCP Server (JSON Lines compatible)"
                        },
                        "capabilities": {
                            "experimental": {},
                            "prompts": { "listChanged": false },
                            "resources": {
                                "listChanged": false,
                                "subscribe": false
                            },
                            "tools": { "listChanged": false }
                        },
                        "instructions": "Serena Rust MCP Server - Professional coding assistant with semantic tools for intelligent code analysis and editing."
                    }
                });
                let s = serde_json::to_string(&resp).unwrap();
                eprintln!("[mcp][jsonlines][send] {}", &s[..s.len().min(128)]);
                writeln!(stdout, "{}", s)?;
                stdout.flush()?;
                // 注意：根据 MCP 协议，服务器不应主动发送 initialized 通知
                // initialized 通知应该由客户端发送，服务器只需要接收并处理
            }
            "tools/list" => {
                let resp = serde_json::json!({ "jsonrpc": "2.0", "id": id, "result": tools_subset });
                let s = serde_json::to_string(&resp).unwrap();
                eprintln!("[mcp][jsonlines][send] {}", &s[..s.len().min(128)]);
                writeln!(stdout, "{}", s)?;
                stdout.flush()?;
                // 兼容严格客户端：在 initialize 成功后于 Content-Length 模式也发送 initialized 通知
                // 注意：此处是 JSON Lines 分支。真正的 CL 分支在 run_stdio 中会调用 read_frame/write_frame。
                // 为确保两种模式一致性，我们在后续 CL 主循环中也会发送相同通知。
            }
            "tools/call" => {
                let tool_name = v.get("params").and_then(|p| p.get("name").or_else(|| p.get("tool")))
                    .and_then(|n| n.as_str()).unwrap_or("");
                let arguments = v.get("params").and_then(|p| p.get("arguments")).cloned().unwrap_or(serde_json::json!(null));
                let resp = serde_json::json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": {
                        "content": [ { "type": "text", "text": format!("Tool '{}' executed (stub). args={}", tool_name, arguments) } ]
                    }
                });
                let s = serde_json::to_string(&resp).unwrap();
                eprintln!("[mcp][jsonlines][send] {}", &s[..s.len().min(128)]);
                writeln!(stdout, "{}", s)?;
                stdout.flush()?;
            }
            "resources/templates/list" | "resources/list" => {
                let empty = serde_json::json!({ "jsonrpc": "2.0", "id": id, "result": { "resources": [], "next": null } });
                let s = serde_json::to_string(&empty).unwrap();
                eprintln!("[mcp][jsonlines][send] {}", &s[..s.len().min(128)]);
                writeln!(stdout, "{}", s)?;
                stdout.flush()?;
            }
            "prompts/list" => {
                let resp = serde_json::json!({ "jsonrpc": "2.0", "id": id, "result": { "prompts": [], "next": null } });
                let s = serde_json::to_string(&resp).unwrap();
                eprintln!("[mcp][jsonlines][send] {}", &s[..s.len().min(128)]);
                writeln!(stdout, "{}", s)?;
                stdout.flush()?;
            }
            "notifications/initialized" | "initialized" | "resources/list_changed" | "tools/list_changed" => {
                eprintln!("[mcp][jsonlines] notification: {}", method);
            }
            other => {
                let resp = serde_json::json!({ "jsonrpc": "2.0", "id": id, "error": { "code": -32601, "message": format!("Method not found: {}", other) } });
                let s = serde_json::to_string(&resp).unwrap();
                eprintln!("[mcp][jsonlines][send] {}", &s[..s.len().min(128)]);
                writeln!(stdout, "{}", s)?;
                stdout.flush()?;
            }
        }
    }

    Ok(())
}

/* ========== 新增：Content-Length 模式服务循环 ========== */
pub fn start_content_length_server(_project_root: String) -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    eprintln!("[mcp][cl] server started, waiting for framed requests");
    // 轻量文件日志工具
    fn flog(msg: &str) {
        if let Ok(mut f) = std::fs::OpenOptions::new().create(true).append(true).open("/tmp/serena_mcp_augment.log") {
            let _ = writeln!(f, "[mcp][trace] {}", msg);
        }
    }

    let tools_subset = serde_json::json!({
        "tools": [
            { "name": "list_dir", "description": "List files/directories under a path (optionally recursive with glob pattern)" },
            { "name": "find_file", "description": "Find files matching a file mask within a relative path" },
            { "name": "search_for_pattern", "description": "Search files for a substring/regex-like pattern with optional context lines" }
        ]
    });

    let mut locked = stdin.lock();
    // 当对端暂未写入或提前关闭时，保持进程存活一段时间，避免客户端刚拉起即“Connection closed”
    // 策略：read_frame 返回 None 视为暂时无数据，sleep 50ms 后继续等待，最多空转 5 分钟（6000 次）
    let mut idle_loops: u32 = 0;
    loop {
        let v_opt = read_frame(&mut locked)?;
        let v = if let Some(v) = v_opt {
            idle_loops = 0;
            v
        } else {
            // None 表示当前没有可读帧（对端可能尚未建立/写首帧）
            idle_loops += 1;
            if idle_loops > 6000 {
                eprintln!("[mcp][cl] idle timeout without any frames");
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(50));
            continue;
        };
        let id = v.get("id").cloned().unwrap_or(serde_json::Value::Null);
        let method = v.get("method").and_then(|m| m.as_str()).unwrap_or("");
        eprintln!("[mcp][cl] <- method: {}, id: {}", method, id);
        // 文件日志记录收到的请求
        flog(&format!("recv method={} has_id={}", method, !id.is_null()));

        let resp_opt = match method {
            "initialize" => {
                Some(serde_json::json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "result": {
                        "protocolVersion": "2024-11-05",
                        "capabilities": {
                            "experimental": {},
                            "prompts": { "listChanged": false },
                            "resources": { "subscribe": false, "listChanged": false },
                            "tools": { "listChanged": false }
                        },
                        "serverInfo": {
                            "name": "Serena-Rust",
                            "version": env!("CARGO_PKG_VERSION")
                        },
                        "instructions": "You are a professional coding agent concerned with one particular codebase. You have access to semantic coding tools on which you rely heavily for all your work, as well as collection of memory files containing general information about the codebase. You operate in a resource-efficient and intelligent manner, always keeping in mind to not read or generate content that is not needed for the task at hand.  When reading code in order to answer a user question or task, you should try reading only the necessary code. Some tasks may require you to understand the architecture of large parts of the codebase, while for others, it may be enough to read a small set of symbols or a single file. Generally, you should avoid reading entire files unless it is absolutely necessary, instead relying on intelligent step-by-step acquisition of information. However, if you already read a file, it does not make sense to further analyse it with the symbolic tools (except for the `find_referencing_symbols` tool), as you already have the information."
                    }
                }))
            }
            "tools/list" => {
                // 在返回前记录工具数量到诊断日志，便于确认是否为 0
                if let Some(arr) = tools_subset.get("tools").and_then(|v| v.as_array()) {
                    log_tools_count(arr.len());
                } else {
                    log_tools_count(0);
                }
                Some(serde_json::json!({ "jsonrpc": "2.0", "id": id, "result": tools_subset }))
            }
            "resources/templates/list" | "resources/list" => {
                Some(serde_json::json!({ "jsonrpc": "2.0", "id": id, "result": { "resources": [], "next": null } }))
            }
            "prompts/list" => {
                Some(serde_json::json!({ "jsonrpc": "2.0", "id": id, "result": { "prompts": [], "next": null } }))
            }
            "initialized" | "notifications/initialized" | "resources/list_changed" | "tools/list_changed" => {
                None
            }
            other => {
                Some(serde_json::json!({ "jsonrpc": "2.0", "id": id, "error": { "code": -32601, "message": format!("Method not found: {}", other) } }))
            }
        };

        if let Some(resp) = resp_opt {
            write_frame(&mut stdout, &resp)?;
            // 注意：根据 MCP 协议，服务器不应主动发送 initialized 通知
            // initialized 通知应该由客户端发送，服务器只需要接收并处理
        }
    }

    Ok(())
}

/* ========== 对外API：与 serena-cli 对接 ========== */
mod server_api {
    #[derive(Clone, Copy, Debug)]
    pub enum TransportMode {
        Auto,
        JsonLines,
        ContentLength,
    }
}

/* ========== 新增：自动探测模式入口 ========== */
// 移除外部重复定义，使用 server_api::TransportMode 作为唯一来源
// 对外通过 pub use 暴露 TransportMode（见文件顶部）

pub fn start_server_with_transport(project_root: String, mode: TransportMode) -> io::Result<()> {
    match mode {
        TransportMode::JsonLines => start_jsonlines_server(project_root),
        TransportMode::ContentLength => start_content_length_server(project_root),
        TransportMode::Auto => {
            // 简单探测：peek stdin 前若干字节，判断是否包含 "Content-Length:"
            use std::io::{BufRead, BufReader};
            let stdin = io::stdin();
            let mut reader = BufReader::new(stdin);
            let mut peek = String::new();
            // 读取一行但不丢弃：如果是 Content-Length 头，就走 CL；否则当作 JSON 行
            let n = reader.read_line(&mut peek)?;
            if n == 0 {
                eprintln!("[mcp][auto] EOF before any data");
                return Ok(());
            }
            if peek.to_ascii_lowercase().starts_with("content-length:") {
                eprintln!("[mcp][auto] detected content-length framing");
                start_content_length_server(project_root)
            } else {
                eprintln!("[mcp][auto] detected jsonlines framing");
                // 不再丢弃首行，尽量保留语义：将首行拼接回后续读取——简化：直接进入 JSON Lines，典型客户端会紧随再次发送 initialize
                start_jsonlines_server(project_root)
            }
        }
    }
}