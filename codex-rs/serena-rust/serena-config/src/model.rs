use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema, Default)]
pub struct PartialConfig {
    pub workspace_root: Option<PathBuf>,
    pub log_level: Option<String>,
    pub metrics_port: Option<u16>,
    pub concurrency_limit: Option<usize>,
    pub request_timeout_ms: Option<u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ConfigFile {
    pub version: u32,
    #[serde(default)]
    pub settings: PartialConfig,
}

impl ConfigFile {
    pub fn merge_into(self, mut base: serena_core::RuntimeConfig) -> serena_core::RuntimeConfig {
        if let Some(v) = self.settings.workspace_root {
            base.workspace_root = v;
        }
        if let Some(v) = self.settings.log_level {
            base.log_level = v;
        }
        if let Some(v) = self.settings.metrics_port {
            base.metrics_port = Some(v);
        }
        if let Some(v) = self.settings.concurrency_limit {
            base.concurrency_limit = v;
        }
        if let Some(v) = self.settings.request_timeout_ms {
            base.request_timeout_ms = v;
        }
        base
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_yaml() {
        let y = r#"
version: 1
settings:
  log_level: debug
  request_timeout_ms: 5000
"#;
        let cfg: ConfigFile = serde_yaml::from_str(y).unwrap();
        assert_eq!(cfg.version, 1);
        assert_eq!(cfg.settings.request_timeout_ms, Some(5000));
    }
}