use criterion::{criterion_group, criterion_main, BatchSize, Criterion, black_box};
use serde_json::json;
use serena_mcp::jsonrpc::{Request, Response};

fn bench_tool_call_overhead(c: &mut Criterion) {
    let mut group = c.benchmark_group("tool_call_overhead");

    // 空转：构造最小 JSON-RPC 请求并往返序列化/反序列化，模拟管道开销
    group.bench_function("jsonrpc_roundtrip_small", |b| {
        b.iter_batched(
            || Request {
                jsonrpc: "2.0".to_string(),
                id: Some(json!(1)),
                method: "tool/noop".to_string(),
                params: json!({"x": 1}),
            },
            |req| {
                // encode
                let buf = serde_json::to_vec(&req).unwrap();
                // decode
                let de: Request = serde_json::from_slice(&buf).unwrap();
                // 构造响应再编码
                let resp = Response::ok(de.id.unwrap(), json!({"ok":true}));
                let _ = black_box(serde_json::to_vec(&resp).unwrap());
            },
            BatchSize::SmallInput,
        )
    });

    // 小输入：参数为小字符串与小数组
    group.bench_function("jsonrpc_small_params", |b| {
        let payload = json!({
            "s": "abcdefg",
            "arr": [1,2,3,4,5],
        });
        b.iter(|| {
            let req = Request { jsonrpc: "2.0".into(), id: Some(json!(2)), method: "tool/echo".into(), params: payload.clone() };
            let buf = serde_json::to_vec(&req).unwrap();
            let _de: Request = serde_json::from_slice(&buf).unwrap();
        })
    });

    group.finish();
}

criterion_group!(benches, bench_tool_call_overhead);
criterion_main!(benches);
