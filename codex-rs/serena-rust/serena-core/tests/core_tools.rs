use serena_core::{tools::ToolRegistry, RuntimeConfig};
use std::sync::Arc;

#[tokio::test]
async fn registry_list_register() {
    let reg = ToolRegistry::new();
    assert!(reg.list().is_empty());
    let cfg = Arc::new(RuntimeConfig::default());
    let _ctx = serena_core::tools::ToolRegistry::default_context(cfg);
    // smoke: still empty (no panic)
    assert!(reg.list().is_empty());
}