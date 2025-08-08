#[cfg(feature = "graphiti_context")]
mod tests {
    use codex_core::interfaces::ContextStore;
    use codex_graphiti::ContextStoreGraphiti;

    #[tokio::test]
    async fn save_and_load_roundtrip() {
        let tmp = tempfile::tempdir().unwrap();
        let db_path = tmp.path().join("graphiti.db");
        let store = ContextStoreGraphiti::new_sqlite_async(&db_path, "test_ns").await.unwrap();

        store.save("k1", serde_json::json!({"x": 1})).await.unwrap();
        let v = store.load("k1").await.unwrap().unwrap();
        assert_eq!(v["x"], 1);
    }

    #[tokio::test]
    async fn save_and_load_file_snapshot() {
        let tmp = tempfile::tempdir().unwrap();
        let db_path = tmp.path().join("graphiti.db");
        let store = ContextStoreGraphiti::new_sqlite_async(&db_path, "test_ns").await.unwrap();

        store.save_file_snapshot("/foo.rs", "fn main(){}\n").await.unwrap();
        let s = store.load_file_snapshot("/foo.rs").await.unwrap().unwrap();
        assert!(s.contains("fn main"));
    }
}
