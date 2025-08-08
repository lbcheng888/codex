use serena_core::{tools::ToolRegistry, RuntimeConfig};
use serena_tools::{FileRead, FileWrite, Search, Replace, SleepTool};
use std::sync::Arc;
use tempfile::tempdir;

#[tokio::test]
async fn file_rw_search_replace() {
    let tmp = tempdir().unwrap();
    let mut cfg = RuntimeConfig::default();
    cfg.workspace_root = tmp.path().to_path_buf();
    let cfg = Arc::new(cfg);

    let reg = ToolRegistry::new();
    reg.register(FileWrite);
    reg.register(FileRead);
    reg.register(Search);
    reg.register(Replace);
    reg.register(SleepTool);

    let ctx = ToolRegistry::default_context(cfg.clone());
    let _ = reg.run_with_timeout(
        "file_write",
        serde_json::json!({"path":"a.txt","content":"hello world"}),
        ctx.clone(),
        1_000
    ).await.unwrap();

    let out = reg.run_with_timeout(
        "file_read",
        serde_json::json!({"path":"a.txt"}),
        ctx.clone(),
        1_000
    ).await.unwrap();
    assert_eq!(out.get("content").unwrap().as_str().unwrap(), "hello world");

    let _ = reg.run_with_timeout(
        "search_and_replace",
        serde_json::json!({"path":"a.txt","search":"world","replace":"rust"}),
        ctx.clone(),
        1_000
    ).await.unwrap();

    let out2 = reg.run_with_timeout(
        "file_read",
        serde_json::json!({"path":"a.txt"}),
        ctx.clone(),
        1_000
    ).await.unwrap();
    assert!(out2.get("content").unwrap().as_str().unwrap().contains("rust"));
}

#[tokio::test]
async fn sleep_tool_timeout_and_error() {
    let tmp = tempdir().unwrap();
    let mut cfg = RuntimeConfig::default();
    cfg.workspace_root = tmp.path().to_path_buf();
    let cfg = Arc::new(cfg);

    let reg = ToolRegistry::new();
    reg.register(SleepTool);
    let ctx = ToolRegistry::default_context(cfg.clone());

    // timeout case: sleep 200ms but timeout 50ms
    let res = reg.run_with_timeout(
        "sleep",
        serde_json::json!({"ms":200}),
        ctx.clone(),
        50
    ).await;
    assert!(res.is_err(), "sleep should timeout");

    // error injection: invalid argument
    let res2 = reg.run_with_timeout(
        "sleep",
        serde_json::json!({"ms":"oops"}),
        ctx,
        1000
    ).await;
    assert!(res2.is_err(), "sleep should return validation error");
}
