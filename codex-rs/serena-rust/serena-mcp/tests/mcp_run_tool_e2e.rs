use std::io::{BufRead, Write};
use std::process::{Command, Stdio};
use std::fs;

fn spawn_server_with_env(cwd: &std::path::Path) -> std::process::Child {
    Command::new("cargo")
        .args(["run", "-q", "-p", "serena-cli", "--bin", "serena-cli", "--", "start-mcp-server"])
        .env("SERENA_WORKSPACE_ROOT", cwd)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn server")
}

fn read_nonempty_line(reader: &mut std::io::BufReader<std::process::ChildStdout>, iters: usize) -> String {
    let mut line = String::new();
    for _ in 0..iters {
        line.clear();
        reader.read_line(&mut line).expect("read line");
        if !line.trim().is_empty() {
            return line;
        }
    }
    line
}

#[test]
fn run_tool_e2e_file_ops_and_errors() {
    // 将 workspace_root 设为当前仓库根目录
    let cwd = std::env::current_dir().expect("cwd");

    // 在仓库根下创建测试文件，确保 path 可为相对路径
    let file_path = cwd.join("sample.txt");
    // 准备初始文件
    fs::write(&file_path, b"hello world\nhello rust\nbye\n").expect("seed file");

    // 相对路径直接使用文件名
    let rel = "sample.txt".to_string();

    // 启动服务并设置 workspace_root 环境变量
    let mut child = spawn_server_with_env(&cwd);

    // 早退诊断
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

    // 启动等待
    std::thread::sleep(std::time::Duration::from_millis(50));
    // prime
    writeln!(sin).unwrap();

    // 等待 ready/jsonrpc 行
    let mut line = read_nonempty_line(&mut reader, 200);
    if !line.contains(r#""jsonrpc":"2.0""#) {
        let mut err = String::new();
        if let Some(mut es) = child.stderr.take() {
            use std::io::Read;
            let _ = es.read_to_string(&mut err);
        }
        panic!("first resp not jsonrpc: {} ; stderr: {}", line.trim(), err);
    }

     // 1) run_tool: file_read (success)
     // 额外 prime，确保首个 JSON 前有明确换行边界
     writeln!(sin).unwrap();
     let req = format!(
         r#"{{"jsonrpc":"2.0","id":10,"method":"tools/call","params":{{"name":"file_read","arguments":{{"path":"{}"}}}}}}"#,
         rel
     );
    // 调试打印首个请求，便于定位 JSON 结构问题
    eprintln!("DBG_REQ10: {}", req);
    writeln!(sin, "{}", req).unwrap();
    let mut got_read_ok = false;
    for _ in 0..400 {
        line.clear();
        reader.read_line(&mut line).expect("read file_read");
        let t = line.trim();
        if t.is_empty() { continue; }
        if t.contains(r#""id":10"#) && t.contains(r#""result""#) && t.contains("hello world") {
            got_read_ok = true;
            break;
        }
        if t.contains(r#""id":10"#) && t.contains(r#""error""#) { panic!("file_read returned error: {}", t); }
    }
    assert!(got_read_ok, "file_read did not return expected content");

    // 2) run_tool: search (success)
    let req = format!(
        r#"{{"jsonrpc":"2.0","id":11,"method":"tools/call","params":{{"name":"search_for_pattern","arguments":{{"path":"{}","pattern":"hello"}}}}}}"#,
        rel
    );
    writeln!(sin, "{}", req).unwrap();
    let mut got_search_ok = false;
    for _ in 0..400 {
        line.clear();
        reader.read_line(&mut line).expect("read search");
        let t = line.trim();
        if t.is_empty() { continue; }
        if t.contains(r#""id":11"#) && t.contains(r#""result""#) && t.contains("hello") {
            got_search_ok = true;
            break;
        }
        if t.contains(r#""id":11"#) && t.contains(r#""error""#) { panic!("search returned error: {}", t); }
    }
    assert!(got_search_ok, "search did not return expected hits");

    // 3) run_tool: replace (success) - replace 'hello' -> 'hi'
    let req = format!(
        r#"{{"jsonrpc":"2.0","id":12,"method":"tools/call","params":{{"name":"search_and_replace","arguments":{{"path":"{}","search":"hello","replace":"hi"}}}}}}"#,
        rel
    );
    writeln!(sin, "{}", req).unwrap();
    let mut got_replace_ok = false;
    for _ in 0..400 {
        line.clear();
        reader.read_line(&mut line).expect("read replace");
        let t = line.trim();
        if t.is_empty() { continue; }
        if t.contains(r#""id":12"#) && t.contains(r#""result""#) {
            got_replace_ok = true;
            break;
        }
        if t.contains(r#""id":12"#) && t.contains(r#""error""#) { panic!("replace returned error: {}", t); }
    }
    assert!(got_replace_ok, "replace did not return success");

    // 4) run_tool: file_read 再次读取确认替换生效
    let req = format!(
        r#"{{"jsonrpc":"2.0","id":13,"method":"tools/call","params":{{"name":"file_read","arguments":{{"path":"{}"}}}}}}"#, rel
    );
    writeln!(sin, "{}", req).unwrap();
    let mut got_read_after = false;
    for _ in 0..400 {
        line.clear();
        reader.read_line(&mut line).expect("read file_read after replace");
        let t = line.trim();
        if t.is_empty() { continue; }
        if t.contains(r#""id":13"#) && t.contains(r#""result""#) && t.contains("hi world") && !t.contains("hello world") {
            got_read_after = true;
            break;
        }
        if t.contains(r#""id":13"#) && t.contains(r#""error""#) { panic!("file_read after replace returned error: {}", t); }
    }
    assert!(got_read_after, "file_read did not reflect replaced content");

    // 5) run_tool: file_write (success) 覆盖写入
    let req = format!(
        r#"{{"jsonrpc":"2.0","id":14,"method":"tools/call","params":{{"name":"file_write","arguments":{{"path":"{}","content":"overwritten"}}}}}}"#,
        rel
    );
    writeln!(sin, "{}", req).unwrap();
    let mut got_write_ok = false;
    for _ in 0..400 {
        line.clear();
        reader.read_line(&mut line).expect("read file_write");
        let t = line.trim();
        if t.is_empty() { continue; }
        if t.contains(r#""id":14"#) && t.contains(r#""result""#) {
            got_write_ok = true;
            break;
        }
        if t.contains(r#""id":14"#) && t.contains(r#""error""#) { panic!("file_write returned error: {}", t); }
    }
    assert!(got_write_ok, "file_write did not return success");

    // 6) run_tool: file_read 确认覆盖内容
    let req = format!(
        r#"{{"jsonrpc":"2.0","id":15,"method":"tools/call","params":{{"name":"file_read","arguments":{{"path":"{}"}}}}}}"#, rel
    );
    writeln!(sin, "{}", req).unwrap();
    let mut got_read_final = false;
    for _ in 0..400 {
        line.clear();
        reader.read_line(&mut line).expect("read final file_read");
        let t = line.trim();
        if t.is_empty() { continue; }
        if t.contains(r#""id":15"#) && t.contains(r#""result""#) && t.contains("overwritten") {
            got_read_final = true;
            break;
        }
        if t.contains(r#""id":15"#) && t.contains(r#""error""#) { panic!("file_read final returned error: {}", t); }
    }
    assert!(got_read_final, "file_read final content not as expected");

    // 7) run_tool: 错误路径（不存在的工具名）
    writeln!(
        sin,
        r#"{{"jsonrpc":"2.0","id":16,"method":"tools/call","params":{{"name":"no_such_tool","arguments":{{}}}}}}"#
    ).unwrap();
    let mut got_error = false;
    for _ in 0..200 {
        line.clear();
        reader.read_line(&mut line).expect("read error");
        let t = line.trim();
        if t.is_empty() { continue; }
        if t.contains(r#""id":16"#) && t.contains(r#""error""#) {
            got_error = true;
            break;
        }
    }
    assert!(got_error, "expected error for unknown tool");

    // 8) run_tool: 错误路径（参数缺失，比如 file_read 缺 path）
    writeln!(
        sin,
        r#"{{"jsonrpc":"2.0","id":17,"method":"tools/call","params":{{"name":"file_read","arguments":{{}}}}}}"#
    ).unwrap();
    let mut got_error_args = false;
    for _ in 0..200 {
        line.clear();
        reader.read_line(&mut line).expect("read error args");
        let t = line.trim();
        if t.is_empty() { continue; }
        if t.contains(r#""id":17"#) && t.contains(r#""error""#) {
            got_error_args = true;
            break;
        }
    }
    assert!(got_error_args, "expected error for missing args");

    // 清理生成的文件
    let _ = std::fs::remove_file(&file_path);
    let _ = child.kill();
    let _ = child.wait();
}
