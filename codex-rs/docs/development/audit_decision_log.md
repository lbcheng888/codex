# External Decision Audit Log

This module provides append-only JSON Lines (JSONL) audit logging for all external decisions (e.g., tool calls, model choices, routing decisions) with minimal overhead and optional privacy-preserving hashing for sensitive metadata fields.

## Goals
- Traceability: each decision gets a unique `decision_id` and timestamp.
- Correlation: optional `session_id`, `turn_id`, and arbitrary metadata (e.g., `trace_id`).
- Safety: append-only file writes; optional salted SHA-256 hashing for sensitive fields.
- Ergonomics: small API to log a record without impacting hot paths.

## Data Schema
A record is serialized as a single JSON object per line:
- decision_id: string (UUIDv4)
- timestamp: RFC3339 (UTC, ms precision)
- session_id, turn_id: optional strings
- decision_type: one of [tool_call, model_choice, routing, policy_check, other]
- actor: { name: string, version?: string }
- context?: JSON
- input?: JSON
- output?: JSON
- metadata?: JSON (e.g., { trace_id, latency_ms, success, error_code })
- tags: string[]

## Usage

```rust
use codex_core::audit::{AuditLogger, ExternalDecisionRecord, DecisionType, DecisionActor};

let logger = AuditLogger::new("/var/log/codex/external_decisions.jsonl")?
    .with_salt("opt-pepper");

let rec = ExternalDecisionRecord::new(
        DecisionType::ToolCall,
        DecisionActor { name: "serena-tools".into(), version: Some("1.0".into()) },
    )
    .with_session("sess-123", Some("turn-5"))
    .with_input(serde_json::json!({"tool":"file_read","args":{}}))
    .with_output(serde_json::json!({"ok":true}))
    .with_metadata(serde_json::json!({"latency_ms": 42, "user": "alice"}))
    .add_tag("external");

let rec = logger.hash_redact(&rec, &["user"]);
logger.log(&rec)?;
# Ok::<(), Box<dyn std::error::Error>>(())
```

## Notes
- The logger uses a mutex-protected append-only file to be `Send + Sync` and safe for concurrent use.
- For high-throughput services, consider batched writes or OS-level log rotation. This module does not rotate files.
- Keep secrets out of logs. Use `hash_redact` for selective hashing of metadata fields.
