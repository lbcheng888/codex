use codex_core::audit::{AuditLogger, DecisionActor, DecisionType, ExternalDecisionRecord};
use codex_core::external_decider::{LocalBackendKind, LocalOssModelClient};
use codex_core::types::{CancelToken, StreamingSink, StreamEvent};
use serde_json::json;

struct CollectSink {
    pub buf: std::sync::Mutex<String>,
}
impl StreamingSink for CollectSink {
    fn on_token(&self, token: &str) { let mut b = self.buf.lock().unwrap(); b.push_str(token); }
    fn on_event(&self, _event: StreamEvent) {}
    fn on_close(&self) {}
}

#[test]
fn external_decider_mock_streams_and_audits() {
    let dir = tempfile::tempdir().unwrap();
    let audit_path = dir.path().join("audit.jsonl");
    let audit = Some(std::sync::Arc::new(AuditLogger::new(&audit_path).unwrap()));

    // 构造 Mock 后端客户端
    let client = LocalOssModelClient::new(
        "gpt-oss-20b",
        None,
        audit,
        LocalBackendKind::Mock,
        None,
        None,
    );

    let sink = CollectSink { buf: std::sync::Mutex::new(String::new()) };
    let input = json!({"prompt": "hello world from test"});
    let cancel = CancelToken::new(None);

    client.stream_chat(input, &sink, &cancel, None).unwrap();

    // 校验有流输出
    let out = sink.buf.lock().unwrap().clone();
    assert!(out.contains("external-decider") || out.contains("hello"));
    // 校验审计写入
    let content = std::fs::read_to_string(&audit_path).unwrap();
    assert_eq!(content.lines().count(), 1);
}
