use criterion::{criterion_group, criterion_main, Criterion};
use serde::Deserialize;
use serde_json::value::RawValue;

// 定义用于零拷贝的借用结构
#[derive(Deserialize)]
struct BorrowedRequest<'a> {
    #[allow(dead_code)]
    jsonrpc: &'a str,
    #[serde(borrow)]
    id: Option<&'a RawValue>,
    #[serde(borrow)]
    method: &'a str,
    #[serde(borrow)]
    params: &'a RawValue,
}

fn bench_zero_copy(c: &mut Criterion) {
    let mut group = c.benchmark_group("serde_zero_copy");
    let input = r#"{"jsonrpc":"2.0","id":1,"method":"tool/echo","params":{"a":1,"b":[1,2,3],"s":"hello"}}"#;

    group.bench_function("borrowed_rawvalue_parse", |b| {
        b.iter(|| {
            let borrowed: BorrowedRequest = serde_json::from_str(input).unwrap();
            // 延迟访问原始 params 以避免优化
            let _slice = borrowed.params.get();
            criterion::black_box(_slice);
        })
    });

    group.bench_function("value_parse", |b| {
        use serde_json::Value;
        b.iter(|| {
            let v: Value = serde_json::from_str(input).unwrap();
            criterion::black_box(v);
        })
    });

    group.finish();
}

criterion_group!(benches, bench_zero_copy);
criterion_main!(benches);
