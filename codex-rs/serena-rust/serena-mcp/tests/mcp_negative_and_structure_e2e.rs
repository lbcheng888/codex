// Negative and structure E2E tests for MCP layer.
// - Validate tools/list structure has required fields
// - Validate restart_language_server_language with unsupported language returns an error

use std::io::{Read, Write};
use std::process::{Command, Stdio};
use std::time::Duration;
use serde_json::{json, Value};

const INIT_RETRIES: usize = 10;
const INIT_RETRY_DELAY_MS: u64 = 150;
const FIRST_REQ_DELAY_MS: u64 = 400;

/// Try to launch serena-cli via ../../target/debug first (when running from crates/serena-mcp),
/// fallback to `cargo run --bin serena-cli --`.
fn launch_mcp_server() -> std::process::Child {
    let direct_path = "../../target/debug/serena-cli";
    let child_res = if std::path::Path::new(direct_path).exists() {
        Command::new(direct_path)
            .env("SERENA_DISABLE_LSP", "1")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
    } else {
        Command::new("cargo")
            .arg("run")
            .arg("--quiet")
            .arg("--bin")
            .arg("serena-cli")
            .arg("--")
            .env("SERENA_DISABLE_LSP", "1")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
    };
    let mut child = child_res.expect("failed to spawn serena-cli (direct or cargo run)");

    // allow process to come up
    std::thread::sleep(Duration::from_millis(1000));

    // init handshake retries
    let init_req = json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {}
        }
    }).to_string();
    let initialized = json!({
        "jsonrpc": "2.0",
        "method": "initialized",
        "params": {}
    }).to_string();

    if let Some(stdin) = child.stdin.as_mut() {
        for _ in 0..INIT_RETRIES {
            let ok1 = writeln!(stdin, "{}", &init_req).is_ok();
            let ok2 = writeln!(stdin, "{}", &initialized).is_ok();
            if ok1 && ok2 { break; }
            std::thread::sleep(Duration::from_millis(INIT_RETRY_DELAY_MS));
        }
    }

    child
}

fn read_next_json(stdout: &mut std::process::ChildStdout, timeout_ms: u64) -> Option<Value> {
    use std::time::Instant;
    let start = Instant::now();
    let mut buf = String::new();
    let mut byte = [0u8; 1];
    while start.elapsed() < Duration::from_millis(timeout_ms) {
        match stdout.read(&mut byte) {
            Ok(0) => break,
            Ok(_) => {
                let c = byte[0] as char;
                if c == '\n' {
                    let line = buf.trim();
                    if line.is_empty() { buf.clear(); continue; }
                    if let Ok(v) = serde_json::from_str::<Value>(line) {
                        return Some(v);
                    }
                    buf.clear();
                } else {
                    buf.push(c);
                }
            }
            Err(_) => { std::thread::sleep(Duration::from_millis(10)); }
        }
    }
    None
}

fn send_request(child: &mut std::process::Child, id: i64, method: &str, params: Value) {
    let req = json!({
        "jsonrpc": "2.0",
        "id": id,
        "method": method,
        "params": params
    }).to_string();

    if let Some(stdin) = child.stdin.as_mut() {
        for _ in 0..5 {
            if writeln!(stdin, "{}", &req).is_ok() { return; }
            std::thread::sleep(Duration::from_millis(100));
        }
    }
    panic!("write request failed after retries");
}

#[test]
fn tools_list_structure_has_basic_fields() {
    let mut child = launch_mcp_server();
    std::thread::sleep(Duration::from_millis(FIRST_REQ_DELAY_MS));
    send_request(&mut child, 2, "tools/list", json!({}));

    let mut stdout = child.stdout.take().expect("stdout");
    let mut validated = false;
    for _ in 0..50 {
        if let Some(val) = read_next_json(&mut stdout, 1000) {
            if val.get("id").and_then(|v| v.as_i64()) == Some(2) {
                if let Some(result) = val.get("result") {
                    if let Some(tools) = result.get("tools").and_then(|t| t.as_array()) {
                        assert!(!tools.is_empty(), "tools/list should return non-empty tools list");
                        // check first few tools have required fields
                        for t in tools.iter().take(5) {
                            assert!(t.get("name").and_then(|v| v.as_str()).is_some(), "tool missing name");
                            // description/schema may be nested differently; check presence at least
                            assert!(t.get("description").is_some() || t.get("schema").is_some(), "tool missing description/schema");
                        }
                        validated = true;
                        break;
                    }
                }
            }
        } else {
            break;
        }
    }
    let _ = child.kill();
    assert!(validated, "Did not validate tools/list structure");
}

#[test]
fn restart_language_server_language_unsupported_returns_error() {
    let mut child = launch_mcp_server();
    std::thread::sleep(Duration::from_millis(FIRST_REQ_DELAY_MS));

    // choose a likely unsupported language name
    let unsupported = "klingon-lang";
    send_request(&mut child, 20, "tools/call", json!({
        "name": "restart_language_server_language",
        "arguments": { "language": unsupported }
    }));

    let mut stdout = child.stdout.take().expect("stdout");
    let mut got_err = false;
    for _ in 0..100 {
        if let Some(val) = read_next_json(&mut stdout, 1000) {
            if val.get("id").and_then(|v| v.as_i64()) == Some(20) {
                if val.get("error").is_some() {
                    got_err = true;
                    break;
                }
                // Some servers wrap errors into result with status:error; accept either
                if let Some(result) = val.get("result") {
                    if result.get("status").and_then(|s| s.as_str()) == Some("error") {
                        got_err = true;
                        break;
                    }
                }
            }
        } else {
            break;
        }
    }
    let _ = child.kill();
    assert!(got_err, "Unsupported language should return error-like response for restart_language_server_language");
}
