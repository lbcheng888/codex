use std::io::{BufRead, Write};
use std::process::{Command, Stdio};

#[test]
fn ping_and_list_via_stdio() {
    // 使用 release 构建产物名在测试环境不可知，这里直接通过 cargo run 调起子进程更稳妥
    let mut child = Command::new("cargo")
        .args(["run", "-q", "-p", "serena-cli", "--bin", "serena-cli", "--", "start-mcp-server"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn server");

    // 若子进程瞬时退出，读取 stderr 以便诊断
    if let Ok(Some(status)) = child.try_wait() {
        let mut err = String::new();
        if let Some(mut es) = child.stderr.take() {
            use std::io::Read;
            let _ = es.read_to_string(&mut err);
        }
        panic!("server exited early: status={:?}, stderr={}", status.code(), err);
    }

    let mut sin = child.stdin.take().expect("stdin");
    let mut reader = std::io::BufReader::new(child.stdout.take().expect("stdout"));
    // 给子进程 Tokio runtime 一个极短时间完成读写任务启动，避免首条请求竞态
    std::thread::sleep(std::time::Duration::from_millis(50));

    // 先写一行空行让子进程 BufReader 开始阻塞读取，确保读循环就绪
    writeln!(sin).unwrap();

    // 先读取出第一条非空响应，验证是 JSON-RPC 行，帮助诊断子进程是否有输出
    let mut line = String::new();
    for _ in 0..200 {
        line.clear();
        reader.read_line(&mut line).expect("read first line");
        if !line.trim().is_empty() { break; }
    }
    // 首条非空必须像 JSON-RPC；若为空，读取 stderr 便于诊断
    if !line.contains(r#""jsonrpc":"2.0""#) {
        let mut err = String::new();
        if let Some(mut es) = child.stderr.take() {
            use std::io::Read;
            let _ = es.read_to_string(&mut err);
        }
        panic!("first resp not jsonrpc: {} ; stderr: {}", line.trim(), err);
    }

    // 发送 ping
    writeln!(sin, r#"{{"jsonrpc":"2.0","id":1,"method":"ping"}}"#).unwrap();
    // 循环读取直到匹配到 pong（跳过空行与 parse error）
    for _ in 0..400 {
        line.clear();
        reader.read_line(&mut line).expect("read ping");
        let t = line.trim();
        if t.is_empty() { continue; }
        if t.contains(r#""pong":true"#) { break; }
        // 跳过 parse error 或其他非预期行
        if t.contains(r#""error""#) { continue; }
        // 也允许先读取到其它 result 行，再继续
        if t.contains(r#""result""#) && t.contains(r#""jsonrpc":"2.0""#) { continue; }
    }
    assert!(line.contains(r#""pong":true"#), "resp: {}", line.trim());

    // list_tools
    line.clear();
    writeln!(sin, r#"{{"jsonrpc":"2.0","id":2,"method":"list_tools"}}"#).unwrap();
    for _ in 0..200 {
        line.clear();
        reader.read_line(&mut line).expect("read list");
        let t = line.trim();
        if t.is_empty() { continue; }
        if t.contains(r#""tools""#) { break; }
        if t.contains(r#""error""#) { continue; }
    }
    assert!(line.contains(r#""tools""#), "resp: {}", line.trim());

    // best-effort terminate
    // 不额外导入未使用的 traits，避免 unused_import 警告；直接尝试 kill 即可。
    let _ = child.kill();
    let _ = child.wait();
}