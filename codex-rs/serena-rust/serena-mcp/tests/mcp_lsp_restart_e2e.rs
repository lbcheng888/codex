// E2E tests for LSP restart tools over MCP stdio transport.
// Strategy: do not require real LSP binaries; only validate MCP tools plumbing:
// - tools/list includes restart_language_server_all and restart_language_server_language
// - tools/call returns structured responses without panicking
//
// These tests mirror the style of existing mcp_run_tool_e2e.rs tests.

use std::io::{Read, Write};
use std::process::{Command, Stdio};
use std::time::Duration;
use serde_json::{json, Value};

const INIT_RETRIES: usize = 10;
const INIT_RETRY_DELAY_MS: u64 = 150;
const FIRST_REQ_DELAY_MS: u64 = 400;

fn launch_mcp_server() -> std::process::Child {
    // Prefer the workspace target/debug at repo root when tests run from crates/serena-mcp.
    // Fallback to `cargo run --bin serena-cli --` if direct path not present.
    let direct_path = "../../target/debug/serena-cli";
    let mut child = if std::path::Path::new(direct_path).exists() {
        Command::new(direct_path)
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
            .arg("--bin")
            .arg("serena-cli")
            .arg("--")
            .env("SERENA_DISABLE_LSP", "1")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .expect("failed to spawn `cargo run --bin serena-cli`")
    };

    // Give the process time to fully start.
    std::thread::sleep(Duration::from_millis(1000));

    // Robust initialize with small retries to avoid BrokenPipe
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
            if ok1 && ok2 {
                break;
            }
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
    // Read line-wise JSON objects (each JSON-RPC message is in one line as per server writer)
    while start.elapsed() < Duration::from_millis(timeout_ms) {
        match stdout.read(&mut byte) {
            Ok(0) => break, // EOF
            Ok(_) => {
                let c = byte[0] as char;
                if c == '\n' {
                    let line = buf.trim();
                    if line.is_empty() {
                        buf.clear();
                        continue;
                    }
                    if let Ok(v) = serde_json::from_str::<Value>(line) {
                        return Some(v);
                    }
                    buf.clear();
                } else {
                    buf.push(c);
                }
            }
            Err(_) => {
                // transient read error: brief sleep and retry
                std::thread::sleep(Duration::from_millis(10));
            }
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

    // best-effort write with small retry to avoid BrokenPipe from race
    if let Some(stdin) = child.stdin.as_mut() {
        for _ in 0..5 {
            if writeln!(stdin, "{}", &req).is_ok() {
                return;
            }
            std::thread::sleep(Duration::from_millis(100));
        }
    }
    panic!("write request failed after retries");
}

#[test]
fn tools_list_contains_lsp_restart_tools() {
    let mut child = launch_mcp_server();
    // Ask for tools/list
    std::thread::sleep(Duration::from_millis(FIRST_REQ_DELAY_MS));
    send_request(&mut child, 2, "tools/list", json!({}));

    // Read responses for a short period to find our result
    let mut stdout = child.stdout.take().expect("stdout");
    let mut found = false;
    let mut attempts = 0;
    while attempts < 50 {
        attempts += 1;
        if let Some(val) = read_next_json(&mut stdout, 1000) {
            // Expect a response with "result": { "tools": [...] }
            if val.get("id").and_then(|v| v.as_i64()) == Some(2) {
                if let Some(result) = val.get("result") {
                    if let Some(tools) = result.get("tools").and_then(|t| t.as_array()) {
                        let names: Vec<String> = tools.iter()
                            .filter_map(|t| t.get("name").and_then(|n| n.as_str()).map(|s| s.to_string()))
                            .collect();
                        assert!(names.contains(&"restart_language_server_all".to_string()),
                                "tools/list should include restart_language_server_all, got: {:?}", names);
                        assert!(names.contains(&"restart_language_server_language".to_string()),
                                "tools/list should include restart_language_server_language, got: {:?}", names);
                        found = true;
                        break;
                    }
                }
            }
        } else {
            break;
        }
    }

    // Cleanup
    let _ = child.kill();
    assert!(found, "Did not receive tools/list response containing restart tools");
}

#[test]
fn tools_call_restart_all_and_language_return_ok_like() {
    let mut child = launch_mcp_server();

    // Allow server to settle before sending requests
    std::thread::sleep(Duration::from_millis(FIRST_REQ_DELAY_MS));

    // call restart_language_server_all
    send_request(&mut child, 10, "tools/call", json!({
        "name": "restart_language_server_all",
        "arguments": {}
    }));

    // call restart_language_server_language (choose a language, e.g. rust)
    send_request(&mut child, 11, "tools/call", json!({
        "name": "restart_language_server_language",
        "arguments": { "language": "rust" }
    }));

    let mut stdout = child.stdout.take().expect("stdout");
    let mut got_all = false;
    let mut got_lang = false;
    let mut attempts = 0;

    while attempts < 200 && !(got_all && got_lang) {
        attempts += 1;
        if let Some(val) = read_next_json(&mut stdout, 1000) {
            if let Some(id) = val.get("id").and_then(|v| v.as_i64()) {
                if id == 10 {
                    // Expect result to be a JSON object, at least containing "status"
                    if let Some(result) = val.get("result") {
                        if result.get("status").is_some() || result.get("value").is_some() {
                            got_all = true;
                        }
                    }
                } else if id == 11 {
                    if let Some(result) = val.get("result") {
                        if result.get("status").is_some() || result.get("value").is_some() {
                            got_lang = true;
                        }
                    }
                }
            }
        } else {
            break;
        }
    }

    let _ = child.kill();
    assert!(got_all, "Did not receive tools/call result for restart_language_server_all");
    assert!(got_lang, "Did not receive tools/call result for restart_language_server_language");
}