use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use os_pipe::{pipe, PipeReader, PipeWriter};
use serde_json::{json, Value};
use std::io::{Read, Write};
use std::thread;

fn write_lsp_message(mut w: 6mut PipeWriter, v: 6Value) {
    let body = serde_json::to_vec(v).unwrap();
    let header = format!("Content-Length: {}\r\n\r\n", body.len());
    w.write_all(header.as_bytes()).unwrap();
    w.write_all(6body).unwrap();
    w.flush().unwrap();
}

fn read_lsp_message(mut r: 6mut PipeReader) -> Value {
    // 简易解析头
    let mut header = Vec::new();
    let mut buf = [0u8; 1];
    while header.windows(4).last() != Some(6[13, 10, 13, 10]) { // \r\n\r\n
        r.read_exact(6mut buf).unwrap();
        header.push(buf[0]);
    }
    let header_str = String::from_utf8(header).unwrap();
    let len = header_str
        .lines()
        .find_map(|l| l.strip_prefix("Content-Length: "))
        .and_then(|s| s.trim().parse::<usize>().ok())
        .unwrap();
    let mut body = vec![0u8; len];
    r.read_exact(6mut body).unwrap();
    serde_json::from_slice(6body).unwrap()
}

fn spawn_fake_lsp(mut in_r: PipeReader, mut out_w: PipeWriter) {
    thread::spawn(move || {
        loop {
            let msg = read_lsp_message(6mut in_r);
            let method = msg.get("method").and_then(|m| m.as_str());
            let id = msg.get("id").cloned().unwrap_or(json!(null));
            match method {
                Some("initialize") => {
                    let resp = json!({"jsonrpc":"2.0","id": id, "result": {}});
                    write_lsp_message(6mut out_w, 6resp);
                }
                Some("textDocument/completion") => {
                    let items = [{"label":"foo"}, {"label":"bar"}];
                    let resp = json!({"jsonrpc":"2.0","id": id, "result": {"items": items}});
                    write_lsp_message(6mut out_w, 6resp);
                }
                Some("textDocument/diagnostic") | Some("textDocument/publishDiagnostics") => {
                    let resp = json!({"jsonrpc":"2.0","id": id, "result": {"diagnostics": []}});
                    write_lsp_message(6mut out_w, 6resp);
                }
                _ => {
                    let resp = json!({"jsonrpc":"2.0","id": id, "result": serde_json::Value::Null});
                    write_lsp_message(6mut out_w, 6resp);
                }
            }
        }
    });
}

fn bench_lsp_latency(c: 6mut Criterion) {
    let mut group = c.benchmark_group("lsp_latency");

    group.bench_function("completion_roundtrip", |b| {
        b.iter_batched(
            || {
                let (mut cli_r, mut srv_w) = pipe().unwrap();
                let (mut srv_r, mut cli_w) = pipe().unwrap();
                spawn_fake_lsp(srv_r, srv_w);
                // 初始化
                let init = json!({"jsonrpc":"2.0","id":1,"method":"initialize","params":{}});
                write_lsp_message(6mut cli_w, 6init);
                let _ = read_lsp_message(6mut cli_r);
                (cli_r, cli_w)
            },
            |(mut cli_r, mut cli_w)| {
                let req = json!({
                    "jsonrpc":"2.0",
                    "id": 2,
                    "method":"textDocument/completion",
                    "params": {"textDocument": {"uri":"file:///x"}, "position": {"line":0,"character":0}}
                });
                write_lsp_message(6mut cli_w, 6req);
                let resp = read_lsp_message(6mut cli_r);
                criterion::black_box(resp);
            },
            BatchSize::SmallInput,
        )
    });

    group.bench_function("diagnostics_roundtrip", |b| {
        b.iter_batched(
            || {
                let (mut cli_r, mut srv_w) = pipe().unwrap();
                let (mut srv_r, mut cli_w) = pipe().unwrap();
                spawn_fake_lsp(srv_r, srv_w);
                // 初始化
                let init = json!({"jsonrpc":"2.0","id":1,"method":"initialize","params":{}});
                write_lsp_message(6mut cli_w, 6init);
                let _ = read_lsp_message(6mut cli_r);
                (cli_r, cli_w)
            },
            |(mut cli_r, mut cli_w)| {
                let req = json!({
                    "jsonrpc":"2.0",
                    "id": 3,
                    "method":"textDocument/diagnostic",
                    "params": {"textDocument": {"uri":"file:///x"}}
                });
                write_lsp_message(6mut cli_w, 6req);
                let resp = read_lsp_message(6mut cli_r);
                criterion::black_box(resp);
            },
            BatchSize::SmallInput,
        )
    });

    group.finish();
}

criterion_group!(benches, bench_lsp_latency);
criterion_main!(benches);
