use anyhow::Result;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

use crate::types::{CodeUnit, Embedding, CodeMatch, CodeContext, Language};
use crate::config::VectorStoreConfig;

/// Mock vector store for demonstration purposes
pub struct VectorStore {
    config: VectorStoreConfig,
    data: Arc<RwLock<HashMap<u64, (CodeUnit, Embedding, f32)>>>,
}

impl VectorStore {
    pub async fn new(config: &VectorStoreConfig) -> Result<Self> {
        Ok(Self {
            config: config.clone(),
            data: Arc::new(RwLock::new(HashMap::new())),
        })
    }

    /// Store a code unit with its embedding
    pub async fn store(
        &self,
        code_unit: &CodeUnit,
        embedding: &Embedding,
    ) -> Result<()> {
        let point_id = self.generate_point_id(code_unit);
        let score = 0.8; // Mock similarity score
        
        let mut data = self.data.write().await;
        data.insert(point_id, (code_unit.clone(), embedding.clone(), score));
        
        Ok(())
    }

    /// Search for similar code
    pub async fn search(
        &self,
        _embedding: &Embedding,
        limit: usize,
    ) -> Result<Vec<CodeMatch>> {
        let data = self.data.read().await;
        
        let mut matches: Vec<CodeMatch> = data
            .values()
            .take(limit)
            .map(|(code_unit, _embedding, score)| CodeMatch {
                file_path: code_unit.file_path.clone(),
                code: code_unit.content.clone(),
                score: *score,
                context: CodeContext {
                    language: code_unit.language.clone(),
                    function_name: None,
                    class_name: None,
                    imports: code_unit.imports.clone(),
                    line_range: (1, code_unit.content.lines().count()),
                },
            })
            .collect();
        
        // Sort by score (descending)
        matches.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());
        
        Ok(matches)
    }

    /// Generate unique point ID for code unit
    fn generate_point_id(
        &self,
        code_unit: &CodeUnit,
    ) -> u64 {
        use std::hash::{Hash, Hasher};
        use std::collections::hash_map::DefaultHasher;

        let mut hasher = DefaultHasher::new();
        code_unit.file_path.hash(&mut hasher);
        code_unit.content.len().hash(&mut hasher);
        hasher.finish()
    }

    /// Get collection stats
    pub async fn collection_info(
        &self,
    ) -> Result<serde_json::Value> {
        let data = self.data.read().await;
        Ok(serde_json::json!({
            "points_count": data.len(),
            "status": "active"
        }))
    }
}