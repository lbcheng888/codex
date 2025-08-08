use serena_core::{tools::ToolRegistry, RuntimeConfig};
use serena_tools::{UserCreate, UserGet};
use std::sync::Arc;

#[tokio::test]
async fn user_create_and_get_roundtrip() {
    let tmpdir = tempfile::tempdir().unwrap();
    let mut cfg = RuntimeConfig::default();
    cfg.workspace_root = tmpdir.path().into();
    let cfg = Arc::new(cfg);
    let ctx = serena_core::tools::ToolRegistry::default_context(cfg.clone());
    let reg = ToolRegistry::new();
    reg.register(UserCreate);
    reg.register(UserGet);

    let create_res = reg
        .run_with_timeout(
            "user_create",
            serde_json::json!({"name":"Alice","email":"alice@example.com"}),
            ctx.clone(),
            5000,
        )
        .await
        .expect("create ok");
    let id = create_res.get("id").and_then(|v| v.as_i64()).unwrap();

    let get_res = reg
        .run_with_timeout(
            "user_get",
            serde_json::json!({"id": id}),
            ctx,
            5000,
        )
        .await
        .expect("get ok");
    assert_eq!(get_res.get("name").unwrap(), "Alice");
}
