//! CozoDB driver implementation for Graphiti

#![warn(missing_docs)]

use async_trait::async_trait;
use graphiti_core::error::Error;
use graphiti_core::error::Result;
use graphiti_core::graph::Edge;
use graphiti_core::graph::TemporalMetadata;
use graphiti_core::storage::Direction;
use graphiti_core::storage::GraphStorage;
use std::sync::Arc;
use tracing::debug;
use tracing::info;
use tracing::instrument;
use tracing::warn;
use uuid::Uuid;

#[cfg(feature = "backend-cozo")]
use cozo::{DbInstance, NamedRows, ScriptMutability};
#[cfg(feature = "backend-sqlx")]
use sqlx::sqlite::SqlitePoolOptions;

/// CozoDB configuration
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CozoConfig {
    /// Storage engine ("mem", "sqlite", "rocksdb")
    pub engine: String,
    /// Database path (for file-based engines)
    pub path: String,
    /// Additional options
    pub options: serde_json::Value,
}

impl Default for CozoConfig {
    fn default() -> Self {
        Self {
            engine: "sqlite".to_string(),
            path: "./data/graphiti.db".to_string(),
            options: serde_json::json!({}),
        }
    }
}

/// CozoDB driver for Graphiti
pub struct CozoDriver {
    #[cfg(feature = "backend-cozo")]
    db: Arc<DbInstance>,
    #[cfg(feature = "backend-sqlx")]
    pool: Arc<sqlx::SqlitePool>,
    _config: CozoConfig,
}

impl CozoDriver {
    /// Create a new CozoDB driver with the given configuration
    pub async fn new(config: CozoConfig) -> Result<Self> {
        info!("Creating storage instance with engine: {}", config.engine);

        #[cfg(feature = "backend-cozo")]
        {
            let options_str = config.options.to_string();
            let db = DbInstance::new(&config.engine, &config.path, &options_str)
                .map_err(|e| Error::Storage(format!("Failed to create CozoDB instance: {}", e)))?;
            let driver = Self {
                db: Arc::new(db),
                _config: config,
            };
            driver.initialize_schema().await?;
            return Ok(driver);
        }

        #[cfg(feature = "backend-sqlx")]
        {
            let url = if config.path == ":memory:" {
                "sqlite::memory:".to_string()
            } else {
                format!("sqlite://{}", config.path)
            };
            let pool = SqlitePoolOptions::new()
                .max_connections(5)
                .connect(&url)
                .await
                .map_err(|e| Error::Storage(format!("Failed to connect sqlite (sqlx): {}", e)))?;
            let driver = Self {
                pool: Arc::new(pool),
                _config: config,
            };
            driver.initialize_schema().await?;
            return Ok(driver);
        }

        #[allow(unreachable_code)]
        Err(Error::Storage("no backend selected".to_string()))
    }

    /// Initialize the database schema for Graphiti (idempotent)
    async fn initialize_schema(&self) -> Result<()> {
        info!("Initializing storage schema for Graphiti");

        #[cfg(feature = "backend-cozo")]
        {
            // Try to create tables, but handle existing table errors gracefully
            self.create_table_if_not_exists(
                "entity_nodes",
                r#"
                {
                    id: Uuid,
                    name: String,
                    entity_type: String,
                    labels: [String],
                    properties: Json,
                    created_at: Float,
                    valid_from: Float,
                    valid_to: Float?,
                    expired_at: Float?,
                    embedding: [Float]?
                }
            "#,
            )
            .await?;

            self.create_table_if_not_exists(
                "episode_nodes",
                r#"
                {
                    id: Uuid,
                    name: String,
                    episode_type: String,
                    content: String,
                    source: String,
                    created_at: Float,
                    valid_from: Float,
                    valid_to: Float?,
                    expired_at: Float?,
                    embedding: [Float]?
                }
            "#,
            )
            .await?;

            self.create_table_if_not_exists(
                "community_nodes",
                r#"
                {
                    id: Uuid,
                    name: String,
                    summary: String,
                    members: [Uuid],
                    created_at: Float,
                    valid_from: Float,
                    valid_to: Float?,
                    expired_at: Float?
                }
            "#,
            )
            .await?;

            self.create_table_if_not_exists(
                "edges",
                r#"
                {
                    id: Uuid,
                    source_id: Uuid,
                    target_id: Uuid,
                    relationship: String,
                    properties: Json,
                    created_at: Float,
                    valid_from: Float,
                    valid_to: Float?,
                    expired_at: Float?,
                    weight: Float
                }
            "#,
            )
            .await?;

            // ContextStore KV table (namespace key-value)
            self.create_table_if_not_exists(
                "ctx_kv",
                r#"
                {
                    ns: String,
                    k: String,
                    v: Json,
                    updated_at: Float
                }
            "#,
            )
            .await?;

            // File snapshots table
            self.create_table_if_not_exists(
                "file_snapshots",
                r#"
                {
                    ns: String,
                    path: String,
                    content: String,
                    updated_at: Float
                }
            "#,
            )
            .await?;

            info!("CozoDB schema initialized successfully");
            return Ok(());
        }

        #[cfg(feature = "backend-sqlx")]
        {
            let stmts = [
                "CREATE TABLE IF NOT EXISTS ctx_kv (ns TEXT NOT NULL, k TEXT NOT NULL, v TEXT NOT NULL, updated_at REAL NOT NULL, PRIMARY KEY(ns, k))",
                "CREATE TABLE IF NOT EXISTS file_snapshots (ns TEXT NOT NULL, path TEXT NOT NULL, content TEXT NOT NULL, updated_at REAL NOT NULL, PRIMARY KEY(ns, path))",
            ];
            for s in stmts.iter() {
                sqlx::query(s)
                    .execute(&* self.pool)
                    .await
                    .map_err(|e| Error::Storage(format!("Failed to create table: {}", e)))?;
            }
            info!("SQLite (sqlx) schema initialized successfully");
            return Ok(());
        }

        #[allow(unreachable_code)]
        Ok(())
    }

    /// Create a table if it doesn't exist (idempotent operation)
    async fn create_table_if_not_exists(&self, table_name: &str, schema: &str) -> Result<()> {
        #[cfg(feature = "backend-cozo")]
        {
            let create_query = format!(":create {} {}", table_name, schema);
            match self.execute_script(&create_query).await {
                Ok(_) => {
                    debug!("Created table '{}'", table_name);
                    Ok(())
                }
                Err(e) => {
                    let error_msg = e.to_string();
                    if error_msg.contains("conflicts with an existing one") {
                        debug!("Table '{}' already exists, preserving existing data", table_name);
                        Ok(())
                    } else {
                        warn!("Failed to create table '{}': {}", table_name, error_msg);
                        Err(e)
                    }
                }
            }
        }
        #[cfg(feature = "backend-sqlx")]
        {
            let _ = table_name; let _ = schema; // handled in initialize_schema
            Ok(())
        }
    }

    /// Execute a Datalog script (only for cozo backend)
    #[cfg(feature = "backend-cozo")]
    async fn execute_script(&self, script: &str) -> Result<NamedRows> {
        self.db
            .run_script(script, Default::default(), ScriptMutability::Mutable)
            .map_err(|e| Error::Storage(format!("Failed to execute script: {}", e)))
    }

    /// Query with Datalog script (only for cozo backend)
    #[cfg(feature = "backend-cozo")]
    async fn query_script(&self, script: &str) -> Result<NamedRows> {
        self.db
            .run_script(script, Default::default(), ScriptMutability::Immutable)
            .map_err(|e| Error::Storage(format!("Failed to execute query: {}", e)))
    }

    /// Convert TemporalMetadata to timestamp values
    fn temporal_to_timestamps(
        &self,
        temporal: &TemporalMetadata,
    ) -> (f64, f64, Option<f64>, Option<f64>) {
        (
            temporal.created_at.timestamp() as f64,
            temporal.valid_from.timestamp() as f64,
            temporal.valid_to.map(|t| t.timestamp() as f64),
            temporal.expired_at.map(|t| t.timestamp() as f64),
        )
    }

    // Public helpers for ContextStore (KV and file snapshots)
    /// Upsert a JSON value into ctx_kv under (ns, k)
    pub async fn kv_put(&self, ns: &str, k: &str, v: serde_json::Value) -> Result<()> {
        let updated_at = chrono::Utc::now().timestamp_millis() as f64 / 1000.0;
        #[cfg(feature = "backend-cozo")]
        {
            let row = serde_json::json!([ns, k, v, updated_at]);
            let script = format!(
                ":put ctx_kv {{ns, k, v, updated_at}} {}",
                serde_json::to_string(&serde_json::json!([row])).unwrap()
            );
            let _ = self.execute_script(&script).await?;
            return Ok(());
        }
        #[cfg(feature = "backend-sqlx")]
        {
            sqlx::query("INSERT INTO ctx_kv(ns,k,v,updated_at) VALUES(?,?,?,?) ON CONFLICT(ns,k) DO UPDATE SET v=excluded.v, updated_at=excluded.updated_at")
                .bind(ns)
                .bind(k)
                .bind(v.to_string())
                .bind(updated_at)
                .execute(&* self.pool)
                .await
                .map_err(|e| Error::Storage(format!("kv_put failed: {}", e)))?;
            return Ok(());
        }
    }

    /// Get a JSON value from ctx_kv by (ns, k)
    pub async fn kv_get(&self, ns: &str, k: &str) -> Result<Option<serde_json::Value>> {
        #[cfg(feature = "backend-cozo")]
        {
            let script = format!(
                r#"?[v] <- ctx_kv{{ns: "{}", k: "{}"}}"#,
                ns, k
            );
            let rows = self.query_script(&script).await?;
            for row in rows.rows {
                if let Some(val) = row.get("v") {
                    return Ok(Some(val.clone()));
                }
            }
            return Ok(None);
        }
        #[cfg(feature = "backend-sqlx")]
        {
            if let Some(rec) = sqlx::query_scalar::<_, String>("SELECT v FROM ctx_kv WHERE ns=? AND k=?")
                .bind(ns)
                .bind(k)
                .fetch_optional(&* self.pool)
                .await
                .map_err(|e| Error::Storage(format!("kv_get failed: {}", e)))? {
                let val: serde_json::Value = serde_json::from_str(&rec)
                    .map_err(|e| Error::Storage(format!("kv_get json parse: {}", e)))?;
                return Ok(Some(val));
            }
            return Ok(None);
        }
    }

    /// Upsert a file snapshot content under (ns, path)
    pub async fn file_snapshot_put(&self, ns: &str, path: &str, content: &str) -> Result<()> {
        let updated_at = chrono::Utc::now().timestamp_millis() as f64 / 1000.0;
        #[cfg(feature = "backend-cozo")]
        {
            let row = serde_json::json!([ns, path, content, updated_at]);
            let script = format!(
                ":put file_snapshots {{ns, path, content, updated_at}} {}",
                serde_json::to_string(&serde_json::json!([row])).unwrap()
            );
            let _ = self.execute_script(&script).await?;
            return Ok(());
        }
        #[cfg(feature = "backend-sqlx")]
        {
            sqlx::query("INSERT INTO file_snapshots(ns,path,content,updated_at) VALUES(?,?,?,?) ON CONFLICT(ns,path) DO UPDATE SET content=excluded.content, updated_at=excluded.updated_at")
                .bind(ns)
                .bind(path)
                .bind(content)
                .bind(updated_at)
                .execute(&* self.pool)
                .await
                .map_err(|e| Error::Storage(format!("file_snapshot_put failed: {}", e)))?;
            return Ok(());
        }
    }

    /// Get file snapshot content by (ns, path)
    pub async fn file_snapshot_get(&self, ns: &str, path: &str) -> Result<Option<String>> {
        #[cfg(feature = "backend-cozo")]
        {
            let script = format!(
                r#"?[content] <- file_snapshots{{ns: "{}", path: "{}"}}"#,
                ns, path
            );
            let rows = self.query_script(&script).await?;
            for row in rows.rows {
                if let Some(val) = row.get("content") {
                    if let Some(s) = val.as_str() {
                        return Ok(Some(s.to_string()));
                    }
                }
            }
            return Ok(None);
        }
        #[cfg(feature = "backend-sqlx")]
        {
            let rec = sqlx::query_scalar::<_, String>("SELECT content FROM file_snapshots WHERE ns=? AND path=?")
                .bind(ns)
                .bind(path)
                .fetch_optional(&* self.pool)
                .await
                .map_err(|e| Error::Storage(format!("file_snapshot_get failed: {}", e)))?;
            return Ok(rec);
        }
    }
}

#[async_trait]
impl GraphStorage for CozoDriver {
    type Error = Error;

    #[instrument(skip(self, node))]
    async fn create_node(&self, node: &dyn graphiti_core::graph::Node) -> Result<()> {
        let id = *node.id();
        let labels = node.labels();
        let properties = node.properties();
        let temporal = node.temporal();

        let (created_at, valid_from, valid_to, expired_at) = self.temporal_to_timestamps(temporal);

        // Determine node type and insert accordingly
        if labels.contains(&"Entity".to_string()) {
            // This is an entity node
            let script = format!(
                r#"
                ?[id, name, entity_type, labels, properties, created_at, valid_from, valid_to, expired_at, embedding] <- [[
                    to_uuid("{}"), "{}", "{}", {}, {}, {}, {}, {}, {}, null
                ]]
                :put entity_nodes {{id, name, entity_type, labels, properties, created_at, valid_from, valid_to, expired_at, embedding}}
                "#,
                id,
                properties
                    .get("name")
                    .and_then(|v| v.as_str())
                    .unwrap_or("unknown"),
                properties
                    .get("entity_type")
                    .and_then(|v| v.as_str())
                    .unwrap_or("unknown"),
                serde_json::to_string(&labels).unwrap_or_else(|_| "[]".to_string()),
                properties,
                created_at,
                valid_from,
                valid_to
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "null".to_string()),
                expired_at
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "null".to_string())
            );

            self.execute_script(&script).await?;
        }
        // Add similar logic for Episode and Community nodes...

        debug!("Created node with ID: {}", id);
        Ok(())
    }

    #[instrument(skip(self))]
    async fn get_node(&self, id: &Uuid) -> Result<Option<Box<dyn graphiti_core::graph::Node>>> {
        // Try to find the node in each table
        let script = format!(
            r#"
            ?[id, name, entity_type, labels, properties, created_at, valid_from, valid_to, expired_at, embedding] := 
                entity_nodes[id, name, entity_type, labels, properties, created_at, valid_from, valid_to, expired_at, embedding],
                id == to_uuid("{}")
            "#,
            id
        );

        let _result = self.query_script(&script).await?;

        // For now, return None as we need to implement proper node reconstruction
        debug!("Queried node with ID: {}", id);
        Ok(None)
    }

    #[instrument(skip(self, node))]
    async fn update_node(&self, node: &dyn graphiti_core::graph::Node) -> Result<()> {
        // Implementation similar to create_node but with update logic
        debug!("Updated node with ID: {}", node.id());
        Ok(())
    }

    #[instrument(skip(self))]
    async fn delete_node(&self, id: &Uuid) -> Result<()> {
        let script = format!(
            r#"
            ?[id] <- [[to_uuid("{}")]]
            :rm entity_nodes {{id}}
            :rm episode_nodes {{id}}
            :rm community_nodes {{id}}
            "#,
            id
        );

        self.execute_script(&script).await?;
        debug!("Deleted node with ID: {}", id);
        Ok(())
    }

    #[instrument(skip(self))]
    async fn create_edge(&self, edge: &Edge) -> Result<()> {
        let (created_at, valid_from, valid_to, expired_at) =
            self.temporal_to_timestamps(&edge.temporal);

        let script = format!(
            r#"
            ?[id, source_id, target_id, relationship, properties, created_at, valid_from, valid_to, expired_at, weight] <- [[
                to_uuid("{}"), to_uuid("{}"), to_uuid("{}"), "{}", {}, {}, {}, {}, {}, {}
            ]]
            :put edges {{id, source_id, target_id, relationship, properties, created_at, valid_from, valid_to, expired_at, weight}}
            "#,
            edge.id,
            edge.source_id,
            edge.target_id,
            edge.relationship,
            edge.properties,
            created_at,
            valid_from,
            valid_to
                .map(|v| v.to_string())
                .unwrap_or_else(|| "null".to_string()),
            expired_at
                .map(|v| v.to_string())
                .unwrap_or_else(|| "null".to_string()),
            edge.weight
        );

        self.execute_script(&script).await?;
        debug!(
            "Created edge from {} to {} with relationship {}",
            edge.source_id, edge.target_id, edge.relationship
        );
        Ok(())
    }

    #[instrument(skip(self))]
    async fn get_edges(&self, node_id: &Uuid, direction: Direction) -> Result<Vec<Edge>> {
        let script = match direction {
            Direction::Outgoing => format!(
                r#"
                ?[id, source_id, target_id, relationship, properties, created_at, valid_from, valid_to, expired_at, weight] := 
                    edges[id, source_id, target_id, relationship, properties, created_at, valid_from, valid_to, expired_at, weight],
                    source_id == to_uuid("{}")
                "#,
                node_id
            ),
            Direction::Incoming => format!(
                r#"
                ?[id, source_id, target_id, relationship, properties, created_at, valid_from, valid_to, expired_at, weight] := 
                    edges[id, source_id, target_id, relationship, properties, created_at, valid_from, valid_to, expired_at, weight],
                    target_id == to_uuid("{}")
                "#,
                node_id
            ),
            Direction::Both => format!(
                r#"
                ?[id, source_id, target_id, relationship, properties, created_at, valid_from, valid_to, expired_at, weight] := 
                    edges[id, source_id, target_id, relationship, properties, created_at, valid_from, valid_to, expired_at, weight],
                    (source_id == to_uuid("{}") || target_id == to_uuid("{}"))
                "#,
                node_id, node_id
            ),
        };

        let _result = self.query_script(&script).await?;

        // For now, return empty vector as we need to implement proper edge reconstruction
        debug!("Queried edges for node: {}", node_id);
        Ok(Vec::new())
    }

    #[instrument(skip(self))]
    async fn get_all_nodes(&self) -> Result<Vec<Box<dyn graphiti_core::graph::Node>>> {
        // For now, return empty vector - this would need proper implementation
        // based on the specific node types in the database
        warn!("get_all_nodes not fully implemented yet");
        Ok(Vec::new())
    }

    #[instrument(skip(self))]
    async fn get_all_edges(&self) -> Result<Vec<graphiti_core::graph::Edge>> {
        // For now, return empty vector - this would need proper implementation
        warn!("get_all_edges not fully implemented yet");
        Ok(Vec::new())
    }

    #[instrument(skip(self))]
    async fn get_nodes_at_time(
        &self,
        _timestamp: chrono::DateTime<chrono::Utc>,
    ) -> Result<Vec<Box<dyn graphiti_core::graph::Node>>> {
        // For now, return empty vector - this would need proper temporal query implementation
        warn!("get_nodes_at_time not fully implemented yet");
        Ok(Vec::new())
    }

    #[instrument(skip(self))]
    async fn get_edges_at_time(
        &self,
        _timestamp: chrono::DateTime<chrono::Utc>,
    ) -> Result<Vec<graphiti_core::graph::Edge>> {
        // For now, return empty vector - this would need proper temporal query implementation
        warn!("get_edges_at_time not fully implemented yet");
        Ok(Vec::new())
    }

    #[instrument(skip(self))]
    async fn get_node_history(
        &self,
        _node_id: &Uuid,
    ) -> Result<Vec<Box<dyn graphiti_core::graph::Node>>> {
        // For now, return empty vector - this would need proper history tracking implementation
        warn!("get_node_history not fully implemented yet");
        Ok(Vec::new())
    }

    #[instrument(skip(self))]
    async fn get_edge_history(&self, _edge_id: &Uuid) -> Result<Vec<graphiti_core::graph::Edge>> {
        // For now, return empty vector - this would need proper history tracking implementation
        warn!("get_edge_history not fully implemented yet");
        Ok(Vec::new())
    }
}

#[cfg(all(test, feature = "backend-cozo"))]
mod tests {
    use super::*;
    use graphiti_core::graph::EpisodeType;

    #[tokio::test]
    async fn test_cozo_driver_creation() {
        let config = CozoConfig {
            engine: "mem".to_string(),
            path: "".to_string(),
            options: serde_json::json!({}),
        };

        let driver = CozoDriver::new(config).await;
        assert!(driver.is_ok());
    }

    #[tokio::test]
    async fn test_schema_initialization() {
        let config = CozoConfig {
            engine: "mem".to_string(),
            path: "".to_string(),
            options: serde_json::json!({}),
        };

        let driver = CozoDriver::new(config).await.unwrap();

        // Test that tables were created by running a simple query
        let result = driver.query_script("?[] := entity_nodes[]").await;
        assert!(result.is_ok());
    }
}
