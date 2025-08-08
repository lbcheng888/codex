// Minimal fake LSP backend binary for resilience tests
// Modes: ok | crash | timeout | cached
// Flags: --mode=<mode> [--delay-ms=N]
use std::env;
use std::io::{self, Read, Write};
use std::thread;
use std::time::Duration;
use std::process::exit;

fn parse_arg(key: &str) -> Option<String> {
    for arg in env::args().skip(1) {
        if let Some(rest) = arg.strip_prefix(&format!("{}=", key)) {
            return Some(rest.to_string());
        }
    }
    None
}

fn main() {
    let mode = parse_arg("--mode").unwrap_or_else(|| "ok".to_string());
    let delay_ms: u64 = parse_arg("--delay-ms")
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);
    if delay_ms > 0 {
        thread::sleep(Duration::from_millis(delay_ms));
    }

    match mode.as_str() {
        // Respond to a minimal initialize / initialized / shutdown handshake and exit 0
        "ok" => {
            // Read anything from stdin (non-blocking best-effort)
            let mut buf = String::new();
            let _ = io::stdin().read_to_string(&mut buf);

            // Emit a minimal LSP-style response header + body to look alive
            let body = r#"{"jsonrpc":"2.0","id":1,"result":{"capabilities":{}}}"#;
            let header = format!("Content-Length: {}\r\n\r\n", body.len());
            let _ = io::stdout().write_all(header.as_bytes());
            let _ = io::stdout().write_all(body.as_bytes());
            let _ = io::stdout().flush();

            exit(0);
        }
        // Exit with non-zero to simulate crash
        "crash" => {
            eprintln!("fake_lsp: crashing as requested");
            exit(1);
        }
        // Hang without producing output
        "timeout" => {
            // Sleep "forever" for tests; keep process alive
            loop {
                thread::sleep(Duration::from_secs(60));
            }
        }
        // Fast success path to mimic cached startup or warm reuse
        "cached" => {
            let body = r#"{"jsonrpc":"2.0","id":1,"result":{"capabilities":{"cached":true}}}"#;
            let header = format!("Content-Length: {}\r\n\r\n", body.len());
            let _ = io::stdout().write_all(header.as_bytes());
            let _ = io::stdout().write_all(body.as_bytes());
            let _ = io::stdout().flush();
            exit(0);
        }
        other => {
            eprintln!("fake_lsp: unknown mode '{}'", other);
            exit(2);
        }
    }
}