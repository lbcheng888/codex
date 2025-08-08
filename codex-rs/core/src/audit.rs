use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use std::sync::Mutex;

use chrono::{DateTime, SecondsFormat, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use sha2::{Digest, Sha256};
use uuid::Uuid;

#[derive(Debug, thiserror::Error)]
pub enum AuditError {
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),
    #[error("json error: {0}")]
    Json(#[from] serde_json::Error),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DecisionType {
    ToolCall,
    ModelChoice,
    Routing,
    PolicyCheck,
    Other,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct DecisionActor {
    // e.g., "serena-tools", "openai:gpt-4o", "mcp:server_name"
    pub name: String,
    // optional version or model id
    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExternalDecisionRecord {
    pub decision_id: String,
    pub timestamp: String,
    // Session and turn identifiers from the host system, optional but recommended
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub turn_id: Option<String>,

    pub decision_type: DecisionType,
    pub actor: DecisionActor,

    // Free-form JSON payloads for input/context and output/result
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<JsonValue>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub input: Option<JsonValue>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub output: Option<JsonValue>,

    // Additional metadata like tool name, latency_ms, success, error_code, trace/span ids, etc.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<JsonValue>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub tags: Vec<String>,
}

impl ExternalDecisionRecord {
    pub fn new(decision_type: DecisionType, actor: DecisionActor) -> Self {
        let ts: DateTime<Utc> = Utc::now();
        Self {
            decision_id: Uuid::new_v4().to_string(),
            timestamp: ts.to_rfc3339_opts(SecondsFormat::Millis, true),
            session_id: None,
            turn_id: None,
            decision_type,
            actor,
            context: None,
            input: None,
            output: None,
            metadata: None,
            tags: Vec::new(),
        }
    }

    pub fn with_session(mut self, session_id: impl Into<String>, turn_id: Option<impl Into<String>>) -> Self {
        self.session_id = Some(session_id.into());
        self.turn_id = turn_id.map(|t| t.into());
        self
    }

    pub fn with_context(mut self, context: JsonValue) -> Self {
        self.context = Some(context);
        self
    }

    pub fn with_input(mut self, input: JsonValue) -> Self {
        self.input = Some(input);
        self
    }

    pub fn with_output(mut self, output: JsonValue) -> Self {
        self.output = Some(output);
        self
    }

    pub fn with_metadata(mut self, metadata: JsonValue) -> Self {
        self.metadata = Some(metadata);
        self
    }

    pub fn add_tag(mut self, tag: impl Into<String>) -> Self {
        self.tags.push(tag.into());
        self
    }
}

#[derive(Debug)]
pub struct AuditLogger {
    // append-only JSONL file
    file: Mutex<std::fs::File>,
    // Optional salt for hashing/redaction helpers
    salt: Option<String>,
}

impl AuditLogger {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self, AuditError> {
        let file = OpenOptions::new().create(true).append(true).open(path)?;
        Ok(Self { file: Mutex::new(file), salt: None })
    }

    pub fn with_salt(mut self, salt: impl Into<String>) -> Self {
        self.salt = Some(salt.into());
        self
    }

    pub fn log(&self, record: &ExternalDecisionRecord) -> Result<(), AuditError> {
        let mut file = self.file.lock().expect("audit file mutex poisoned");
        let line = serde_json::to_string(record)?;
        file.write_all(line.as_bytes())?;
        file.write_all(b"\n")?;
        // Best-effort flush to reduce data loss on crash
        file.flush()?;
        Ok(())
    }

    // Shallow hashing helper: if metadata contains listed keys with string values,
    // replace with sha256(salt + value). Returns a cloned, redacted record.
    pub fn hash_redact(&self, rec: &ExternalDecisionRecord, keys: &[&str]) -> ExternalDecisionRecord {
        let salt = self.salt.as_deref().unwrap_or("");
        let mut cloned = rec.clone();
        if let Some(mut meta) = cloned.metadata.take() {
            if let Some(obj) = meta.as_object_mut() {
                for k in keys {
                    if let Some(v) = obj.get_mut(*k) {
                        if let Some(s) = v.as_str() {
                            let mut hasher = Sha256::new();
                            hasher.update(salt.as_bytes());
                            hasher.update(s.as_bytes());
                            let digest = hasher.finalize();
                            let hex = format!("{:x}", digest);
                            *v = JsonValue::String(hex);
                        }
                    }
                }
            }
            cloned.metadata = Some(meta);
        }
        cloned
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn writes_jsonl_record() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("audit.jsonl");
        let logger = AuditLogger::new(&path).unwrap().with_salt("pepper");

        let actor = DecisionActor { name: "serena-tools".into(), version: Some("1.0".into()) };
        let rec = ExternalDecisionRecord::new(DecisionType::ToolCall, actor)
            .with_session("sess-1", Some("turn-1"))
            .with_input(serde_json::json!({"tool":"file_read","args":{"path":"README.md"}}))
            .with_output(serde_json::json!({"ok":true,"bytes":128}))
            .with_metadata(serde_json::json!({"trace_id":"abc","user":"alice"}))
            .add_tag("external")
            .add_tag("decision");

        // redact 'user' in metadata
        let redacted = logger.hash_redact(&rec, &["user"]);
        logger.log(&redacted).unwrap();

        let content = std::fs::read_to_string(&path).unwrap();
        assert!(content.lines().count() == 1);
        let parsed: ExternalDecisionRecord = serde_json::from_str(content.trim()).unwrap();
        assert_eq!(parsed.session_id.as_deref(), Some("sess-1"));
        assert!(parsed.metadata.as_ref().unwrap()["user"].as_str().unwrap().len() > 10);
    }
}
