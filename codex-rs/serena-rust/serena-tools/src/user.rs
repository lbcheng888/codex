use serena_core::tools::{Tool, ToolContext};
use serde_json::Value;
use sqlx::{sqlite::SqlitePoolOptions, Row};
use tokio::fs;
use tracing::instrument;

const DB_FILE: &str = "data.db";

use serena_core::error::{Result as CoreResult, CoreError};

async fn open_pool(ctx: &ToolContext) -> CoreResult<sqlx::SqlitePool> {
    let db_path = ctx.cfg.workspace_root.join(DB_FILE);
    // ensure parent exists
    if let Some(parent) = db_path.parent() {
        fs::create_dir_all(parent).await.ok();
    }
    let url = format!("sqlite://{}", db_path.display());
    use once_cell::sync::Lazy;
    use std::collections::HashMap;
    use std::sync::Mutex as StdMutex;

    static POOL_CACHE: Lazy<StdMutex<HashMap<String, sqlx::SqlitePool>>> = Lazy::new(|| StdMutex::new(HashMap::new()));

    // Fast path: return cached pool if exists.
    if let Some(p) = POOL_CACHE.lock().unwrap().get(&url).cloned() {
        return Ok(p);
    }

    // Slow path: create pool outside the mutex to avoid holding lock across await.
    let pool_res = SqlitePoolOptions::new()
        .max_connections(2)
        .connect(&url)
        .await;
    let pool = match pool_res {
        Ok(p) => p,
        Err(e) => {
            // fallback to in-memory if file open fails (e.g., read-only fs in tests)
            tracing::debug!(error = %e, "fallback to in-memory sqlite");
            SqlitePoolOptions::new()
                .max_connections(2)
                .connect("sqlite::memory:")
                .await
                .map_err(|e| CoreError::tool(e.to_string()))?
        }
    };
    POOL_CACHE.lock().unwrap().insert(url, pool.clone());
    // init table once
    sqlx::query(
        r#"CREATE TABLE IF NOT EXISTS users (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            name TEXT NOT NULL,
            email TEXT NOT NULL UNIQUE
        )"#,
    )
    .execute(&pool)
    .await
    .map_err(|e| CoreError::tool(e.to_string()))?;
    Ok(pool)
}

pub struct UserCreate;

#[async_trait::async_trait]
impl Tool for UserCreate {
    fn name(&self) -> &'static str {
        "user_create"
    }
    fn description(&self) -> &'static str {
        "Create a new user profile with name and email"
    }
    fn schema(&self) -> Value {
        serde_json::json!({
            "type":"object",
            "required":["name","email"],
            "properties":{
                "name":{"type":"string"},
                "email":{"type":"string"}
            }
        })
    }
    #[instrument(skip_all)]
    async fn run(&self, input: Value, ctx: ToolContext) -> serena_core::error::Result<Value> {
        let name = input
            .get("name")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing name"))?;
        let email = input
            .get("email")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing email"))?;
        let pool = open_pool(&ctx).await?;
        let row = sqlx::query("INSERT INTO users (name, email) VALUES (?1, ?2) RETURNING id")
            .bind(name)
            .bind(email)
            .fetch_one(&pool)
            .await
            .map_err(|e| serena_core::error::CoreError::tool(e.to_string()))?;
        let id: i64 = row.get(0);
        Ok(serde_json::json!({"id": id}))
    }
}

pub struct UserGet;

#[async_trait::async_trait]
impl Tool for UserGet {
    fn name(&self) -> &'static str {
        "user_get"
    }
    fn description(&self) -> &'static str {
        "Get user profile information by user ID"
    }
    fn schema(&self) -> Value {
        serde_json::json!({
            "type":"object",
            "required":["id"],
            "properties":{
                "id":{"type":"integer"}
            }
        })
    }
    #[instrument(skip_all)]
    async fn run(&self, input: Value, ctx: ToolContext) -> serena_core::error::Result<Value> {
        let id = input
            .get("id")
            .and_then(|v| v.as_i64())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing id"))?;
        let pool = open_pool(&ctx).await?;
        let maybe = sqlx::query("SELECT id, name, email FROM users WHERE id = ?1")
            .bind(id)
            .fetch_optional(&pool)
            .await
            .map_err(|e| serena_core::error::CoreError::tool(e.to_string()))?;
        if let Some(row) = maybe {
            let user_json = serde_json::json!({
                "id": row.get::<i64, _>(0),
                "name": row.get::<String, _>(1),
                "email": row.get::<String, _>(2)
            });
            Ok(user_json)
        } else {
            Err(serena_core::error::CoreError::not_found("user"))
        }
    }
}
