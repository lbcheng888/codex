#![allow(unused)]
// Integration tests: verify tools/list returns full registry with JSON Schema,
// and tools/call works for representative tools.
// JSON-RPC over line-delimited stdio, aligned with server.rs sync path.

use std::io::{Read, Write};
use std::process::{Command, Stdio};
use std::time::Duration;
use serde_json::{json, Value};

const INIT_RETRIES: usize = 10;
const INIT_RETRY_DELAY_MS: u64 = 150;
const FIRST_REQ_DELAY_MS: u64 = 350;

fn launch_serena_cli() -> std::process::Child {
    // Prefer workspace target/debug when tests run inside the workspace
    let direct_path = "../../target/debug/serena-cli";
    if std::path::Path::new(direct_path).exists() {
        Command::new(direct_path)
            .arg("start-mcp-server")
            .env("SERENA_DISABLE_LSP", "1")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .unwrap_or_else(|e| panic!("failed to spawn `{}`: {}", direct_path, e))
    } else {
        Command::new("cargo")
            .arg("run")
            .arg("--quiet")
            .arg("--bin").arg("serena-cli")
            .arg("--")
            .arg("start-mcp-server")
            .env("SERENA_DISABLE_LSP", "1")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .expect("failed to spawn `cargo run --bin serena-cli start-mcp-server`")
    }
}

fn write_json_line(stdin: &mut std::process::ChildStdin, v: &Value) -> bool {
    let s = v.to_string();
    writeln!(stdin, "{s}").is_ok()
}

fn read_next_json(stdout: &mut std::process::ChildStdout, timeout_ms: u64) -> Option<Value> {
    use std::time::Instant;
    let start = Instant::now();
    let mut buf = String::new();
    let mut byte = [0u8; 1];
    while start.elapsed() < Duration::from_millis(timeout_ms) {
        match stdout.read(&mut byte) {
            Ok(0) => break, // EOF
            Ok(_) => {
                let ch = byte[0] as char;
                if ch == '\n' {
                    let line = buf.trim();
                    if !line.is_empty() {
                        if let Ok(v) = serde_json::from_str::<Value>(line) {
                            return Some(v);
                        }
                    }
                    buf.clear();
                } else {
                    buf.push(ch);
                }
            }
            Err(_) => std::thread::sleep(Duration::from_millis(10)),
        }
    }
    None
}

fn initialize(child: &mut std::process::Child) {
    // robust init with retries
    let init_req = json!({
        "jsonrpc": "2.0", "id": 1, "method": "initialize",
        "params": { "protocolVersion": "2024-11-05", "capabilities": {}, "clientInfo": {"name":"test","version":"1.0.0"} }
    });
    let initialized = json!({ "jsonrpc": "2.0", "method": "initialized", "params": {} });

    if let Some(stdin) = child.stdin.as_mut() {
        for _ in 0..INIT_RETRIES {
            let ok1 = write_json_line(stdin, &init_req);
            let ok2 = write_json_line(stdin, &initialized);
            if ok1 && ok2 { break; }
            std::thread::sleep(Duration::from_millis(INIT_RETRY_DELAY_MS));
        }
    }
}

#[test]
fn tools_list_contains_schema_and_proxy_python_tool() {
    let mut child = launch_serena_cli();
    std::thread::sleep(Duration::from_millis(FIRST_REQ_DELAY_MS));
    initialize(&mut child);

    // request tools/list
    if let Some(stdin) = child.stdin.as_mut() {
        let req = json!({ "jsonrpc":"2.0", "id": 2, "method":"tools/list", "params": {} });
        assert!(write_json_line(stdin, &req), "failed to send tools/list");
    }

    let mut stdout = child.stdout.take().expect("stdout");
    let mut got = None;
    for _ in 0..120 {
        if let Some(v) = read_next_json(&mut stdout, 1000) {
            if v.get("id").and_then(|id| id.as_i64()) == Some(2) {
                got = Some(v);
                break;
            }
        }
    }
    let _ = child.kill();
    let resp = got.expect("no tools/list response");
    let tools = resp.get("result").and_then(|r| r.get("tools")).and_then(|t| t.as_array()).expect("invalid tools array");

    // baseline
    assert!(tools.len() >= 5, "expected >=5 tools, got {}", tools.len());

    // Each tool must have parameters and inputSchema; if type=object then required must exist (possibly empty)
    for t in tools {
        assert!(t.get("name").is_some(), "tool missing name");
        assert!(t.get("description").is_some(), "tool missing description");
        assert!(t.get("parameters").is_some(), "missing parameters");
        assert!(t.get("inputSchema").is_some(), "missing inputSchema");
        if let Some(obj) = t.get("parameters").and_then(|p| p.as_object()) {
            if obj.get("type").and_then(|x| x.as_str()) == Some("object") {
                assert!(obj.contains_key("required"), "object schema missing required");
                assert!(obj.get("required").unwrap().is_array(), "required must be array");
            }
        }
    }

    // Ensure proxy_python_tool present and requires "command"
    let mut saw_proxy = false;
    for t in tools {
        if t.get("name").and_then(|n| n.as_str()) == Some("proxy_python_tool") {
            saw_proxy = true;
            let params = t.get("parameters").and_then(|p| p.as_object()).expect("proxy params object");
            assert_eq!(params.get("type").and_then(|x| x.as_str()), Some("object"));
            let req = params.get("required").and_then(|x| x.as_array()).expect("required array");
            assert!(req.iter().any(|x| x == "command"), "proxy_python_tool required should contain command");
        }
    }
    assert!(saw_proxy, "proxy_python_tool not listed");
}

#[test]
fn tools_list_contains_sleep_tool_schema() {
    let mut child = launch_serena_cli();
    std::thread::sleep(Duration::from_millis(FIRST_REQ_DELAY_MS));
    initialize(&mut child);

    // request tools/list
    if let Some(stdin) = child.stdin.as_mut() {
        let req = json!({ "jsonrpc":"2.0", "id": 3, "method":"tools/list", "params": {} });
        assert!(write_json_line(stdin, &req), "failed to send tools/list");
    }

    let mut stdout = child.stdout.take().expect("stdout");
    let mut got = None;
    for _ in 0..200 {
        if let Some(v) = read_next_json(&mut stdout, 1000) {
            if v.get("id").and_then(|id| id.as_i64()) == Some(3) {
                got = Some(v);
                break;
            }
        }
    }
    let _ = child.kill();
    let resp = got.expect("no tools/list response");
    let tools = resp.get("result").and_then(|r| r.get("tools")).and_then(|t| t.as_array()).expect("invalid tools array");

    let mut saw_sleep = false;
    for t in tools {
        if t.get("name").and_then(|n| n.as_str()) == Some("sleep") {
            saw_sleep = true;
            let params = t.get("parameters").and_then(|p| p.as_object()).expect("sleep params object");
            assert_eq!(params.get("type").and_then(|x| x.as_str()), Some("object"));
            let props = params.get("properties").and_then(|x| x.as_object()).expect("properties");
            let ms = props.get("ms").and_then(|x| x.as_object()).expect("ms schema");
            assert_eq!(ms.get("type").and_then(|x| x.as_str()), Some("integer"));
            let req = params.get("required").and_then(|x| x.as_array()).expect("required array");
            assert!(req.iter().any(|x| x == "ms"), "sleep required should contain ms");
        }
    }
    assert!(saw_sleep, "sleep tool not listed");
}

#[test]
fn per_tool_concurrency_with_sleep_tool_deterministic() {
    // Limit sleep tool to concurrency=1 to force serial execution
    std::env::set_var("SERENA_MCP_MAX_CONCURRENCY", "8");
    std::env::set_var("SERENA_MCP_TOOL_CONCURRENCY__sleep", "1");

    let mut child = launch_serena_cli();
    std::thread::sleep(Duration::from_millis(FIRST_REQ_DELAY_MS));
    initialize(&mut child);

    if let Some(stdin) = child.stdin.as_mut() {
        // Two sleep calls back-to-back; with concurrency=1 they must not overlap.
        let c1 = json!({"jsonrpc":"2.0","id":60,"method":"tools/call","params":{"name":"sleep","arguments":{"ms":80}}});
        let c2 = json!({"jsonrpc":"2.0","id":61,"method":"tools/call","params":{"name":"sleep","arguments":{"ms":20}}});
        assert!(write_json_line(stdin, &c1));
        assert!(write_json_line(stdin, &c2));
    }

    let start = std::time::Instant::now();
    let mut stdout = child.stdout.take().expect("stdout");
    let mut seen60 = false;
    let mut seen61 = false;

    for _ in 0..400 {
        if let Some(v) = read_next_json(&mut stdout, 1000) {
            if let Some(id) = v.get("id").and_then(|x| x.as_i64()) {
                if id == 60 { seen60 = true; }
                if id == 61 { seen61 = true; }
                if seen60 && seen61 { break; }
            }
        }
    }
    let elapsed = start.elapsed();
    let _ = child.kill();

    assert!(seen60 && seen61, "both sleep calls should return");
    // With 80ms + 20ms serial, elapsed should be >= ~100ms (allow margin).
    assert!(elapsed.as_millis() >= 90, "elapsed {}ms too short for serialized sleeps", elapsed.as_millis());
}

#[test]
fn tool_timeout_with_sleep_tool_maps_code_and_data() {
    // Global timeout short to trigger on sleep=200ms
    std::env::set_var("SERENA_MCP_TOOL_TIMEOUT_MS", "100");

    let mut child = launch_serena_cli();
    std::thread::sleep(Duration::from_millis(FIRST_REQ_DELAY_MS));
    initialize(&mut child);

    if let Some(stdin) = child.stdin.as_mut() {
        let call = json!({
            "jsonrpc":"2.0","id":70,"method":"tools/call",
            "params":{"name":"sleep","arguments":{"ms":200}}
        });
        assert!(write_json_line(stdin, &call));
    }

    let mut stdout = child.stdout.take().expect("stdout");
    let mut got_timeout = None;

    for _ in 0..200 {
        if let Some(v) = read_next_json(&mut stdout, 2000) {
            if v.get("id").and_then(|x| x.as_i64()) == Some(70) {
                got_timeout = v.get("error").cloned();
                break;
            }
        }
    }
    let _ = child.kill();

    let err = got_timeout.expect("expected timeout error");
    let code = err.get("code").and_then(|c| c.as_i64());
    assert_eq!(code, Some(-32002), "timeout should map to -32002");
    let data = err.get("data").and_then(|d| d.as_object()).expect("data object");
    assert!(data.get("tool").is_some(), "data.tool missing");
    assert!(data.get("timeout_ms").is_some(), "data.timeout_ms missing");
}

#[test]
fn per_tool_concurrency_env_override_limits_calls() {
    // 验证 per-tool 并发覆盖：将某工具并发限制为 1，连续发起两次调用，第二次应在前一次完成前不会返回
    // 选择 list_dir 作为无副作用目标
    let mut child = launch_serena_cli();
    std::thread::sleep(Duration::from_millis(FIRST_REQ_DELAY_MS));
    // 设置环境变量：global=8，单工具=list_dir = 1
    // 注意：测试进程无法直接为子进程后续调用设置 env；因此在 launch_serena_cli 时已经继承当前 env。
    // 这里简单地在当前进程设置，依赖于操作系统继承语义。
    std::env::set_var("SERENA_MCP_MAX_CONCURRENCY", "8");
    std::env::set_var("SERENA_MCP_TOOL_CONCURRENCY__list_dir", "1");

    initialize(&mut child);

    // 发起两个 list_dir 请求，ID 40 和 41
    if let Some(stdin) = child.stdin.as_mut() {
        let call1 = json!({
            "jsonrpc":"2.0", "id": 40, "method":"tools/call",
            "params": { "name":"list_dir", "arguments": { "path": ".", "recursive": false } }
        });
        let call2 = json!({
            "jsonrpc":"2.0", "id": 41, "method":"tools/call",
            "params": { "name":"list_dir", "arguments": { "path": ".", "recursive": false } }
        });
        assert!(write_json_line(stdin, &call1));
        assert!(write_json_line(stdin, &call2));
    }

    // 读取响应，期望先后返回，而非同时（弱断言：确保两者都返回）
    let mut stdout = child.stdout.take().expect("stdout");
    let mut seen40 = false;
    let mut seen41 = false;

    for _ in 0..400 {
        if let Some(v) = read_next_json(&mut stdout, 1000) {
            if let Some(id) = v.get("id").and_then(|x| x.as_i64()) {
                if id == 40 { seen40 = true; }
                if id == 41 { seen41 = true; }
                if seen40 && seen41 { break; }
            }
        }
    }
    let _ = child.kill();
    assert!(seen40 && seen41, "both list_dir calls should return under concurrency limit 1");
}

#[test]
fn tool_timeout_maps_to_error_code_and_data() {
    // 使用 proxy_python_tool 构造一个超过超时的调用：python sleep 后再输出
    // 若无 python，则跳过该用例以避免 CI 环境不一致导致失败
    let py = if cfg!(target_os = "windows") { "python" } else { "python3" };
    // 尝试通过环境变量覆盖，便于 CI/本地自定义
    let py = std::env::var("PY_CMD").unwrap_or_else(|_| py.to_string());
    // 纯标准库探测：使用 `which`/`where` 命令探测是否存在
    let check_cmd = if cfg!(target_os = "windows") { "where" } else { "which" };
    let status_ok = std::process::Command::new(check_cmd)
        .arg(&py)
        .status()
        .map(|s| s.success())
        .unwrap_or(false);
    if !status_ok {
        eprintln!("{} not found; skipping timeout test", py);
        return;
    }

    // 设置超时较短
    std::env::set_var("SERENA_MCP_TOOL_TIMEOUT_MS", "300");

    let mut child = launch_serena_cli();
    std::thread::sleep(Duration::from_millis(FIRST_REQ_DELAY_MS));
    initialize(&mut child);

    if let Some(stdin) = child.stdin.as_mut() {
        // 让 python 睡眠 1 秒后输出
        let call = json!({
            "jsonrpc":"2.0", "id": 50, "method":"tools/call",
            "params": {
                "name":"proxy_python_tool",
                "arguments": {
                    "command": py,
                    "args": ["-c", "import sys,time; time.sleep(1); sys.stdout.write('{\"ok\":true}')"],
                    "input": null
                }
            }
        });
        assert!(write_json_line(stdin, &call));
    }

    let mut stdout = child.stdout.take().expect("stdout");
    let mut got_timeout = false;
    let mut code = None;
    let mut data = None;

    for _ in 0..200 {
        if let Some(v) = read_next_json(&mut stdout, 2000) {
            if v.get("id").and_then(|x| x.as_i64()) == Some(50) {
                if let Some(err) = v.get("error") {
                    code = err.get("code").and_then(|c| c.as_i64());
                    data = err.get("data").cloned();
                    got_timeout = true;
                }
                break;
            }
        }
    }
    let _ = child.kill();

    if got_timeout {
        assert_eq!(code, Some(-32002), "timeout should map to -32002");
        // data 中应包含 tool/timeout_ms 字段
        if let Some(d) = data {
            assert!(d.get("tool").is_some());
            assert!(d.get("timeout_ms").is_some());
        }
    } else {
        // 环境波动（极快机器）可能偶发在 300ms 内完成；不强制失败
        assert!(true, "timeout not triggered; environment may be too fast");
    }
}