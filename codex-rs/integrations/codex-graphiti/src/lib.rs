//! Graphiti adapter(s) for codex-core stable interfaces.
//! Phase A: add a feature-gated ContextStore backed by Graphiti/Cozo.

use async_trait::async_trait;
use codex_core::interfaces::ContextStore;
use serde_json::Value;
use anyhow::Result;
use std::sync::Arc;

#[cfg(feature = "graphiti_context")]
mod graphiti_impl {
    use super::*;
    use graphiti_core as gcore;
    use graphiti_cozo as gcozo;
    use tracing::info;

    /// Minimal scaffold for a Graphiti-backed ContextStore.
    /// For Phase A, we initialize a Cozo-backed driver and keep a simple KV namespace.
    pub struct GraphitiContextStore {
        cozo: Arc<gcozo::CozoDriver>,
        ns: String,
    }

    impl GraphitiContextStore {
        pub async fn new_sqlite_async(path: impl AsRef<std::path::Path>, namespace: impl Into<String>) -> Result<Self> {
            let db_path = path.as_ref().to_path_buf();
            let ns = namespace.into();
            // Create a Cozo DB (sqlite backend)
            let cfg = gcozo::CozoConfig { engine: "sqlite".into(), path: db_path.display().to_string(), options: serde_json::json!({}) };
            let cozo = gcozo::CozoDriver::new(cfg).await?;
            info!(ns = %ns, db = %db_path.display(), "GraphitiContextStore initialized (sqlite)");
            Ok(Self { cozo: Arc::new(cozo), ns })
        }
    }

    #[async_trait]
    impl ContextStore for GraphitiContextStore {
        async fn save(&self, key: &str, value: Value) -> Result<()> {
            self.cozo.kv_put(&self.ns, key, value).await.map_err(|e| anyhow::anyhow!(e.to_string()))
        }
        async fn load(&self, key: &str) -> Result<Option<Value>> {
            self.cozo.kv_get(&self.ns, key).await.map_err(|e| anyhow::anyhow!(e.to_string()))
        }
        async fn save_file_snapshot(&self, path: &str, content: &str) -> Result<()> {
            self.cozo.file_snapshot_put(&self.ns, path, content).await.map_err(|e| anyhow::anyhow!(e.to_string()))
        }
        async fn load_file_snapshot(&self, path: &str) -> Result<Option<String>> {
            self.cozo.file_snapshot_get(&self.ns, path).await.map_err(|e| anyhow::anyhow!(e.to_string()))
        }
    }

    pub use GraphitiContextStore as ContextStoreGraphiti;
}

#[cfg(not(feature = "graphiti_context"))]
pub mod placeholders {
    use super::*;
    /// No-op placeholder that compiles without Graphiti deps.
    pub struct ContextStoreGraphiti;
    #[async_trait]
    impl ContextStore for ContextStoreGraphiti {}
}

#[cfg(feature = "graphiti_context")]
pub use graphiti_impl::ContextStoreGraphiti;
#[cfg(not(feature = "graphiti_context"))]
pub use placeholders::ContextStoreGraphiti;
