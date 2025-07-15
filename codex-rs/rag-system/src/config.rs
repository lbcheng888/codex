use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RagConfig {
    pub embedding: EmbeddingConfig,
    pub vector_store: VectorStoreConfig,
    pub indexing: IndexingConfig,
    pub query: QueryConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmbeddingConfig {
    pub model_name: String,
    pub model_path: Option<PathBuf>,
    pub embedding_size: usize,
    pub max_sequence_length: usize,
    pub batch_size: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VectorStoreConfig {
    pub url: String,
    pub collection_name: String,
    pub embedding_size: usize,
    pub distance_metric: DistanceMetric,
    pub index_config: IndexConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IndexingConfig {
    pub include_patterns: Vec<String>,
    pub exclude_patterns: Vec<String>,
    pub max_file_size: usize,
    pub chunk_size: usize,
    pub chunk_overlap: usize,
    pub incremental: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueryConfig {
    pub max_results: usize,
    pub similarity_threshold: f32,
    pub context_window: usize,
    pub rerank: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DistanceMetric {
    Cosine,
    Euclidean,
    DotProduct,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IndexConfig {
    pub hnsw_config: HnswConfig,
    pub quantization_config: Option<QuantizationConfig>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HnswConfig {
    pub m: usize,
    pub ef_construct: usize,
    pub ef: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QuantizationConfig {
    pub r#type: QuantizationType,
    pub quantile: f32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum QuantizationType {
    Scalar,
    Product,
}

impl Default for RagConfig {
    fn default() -> Self {
        Self {
            embedding: EmbeddingConfig::default(),
            vector_store: VectorStoreConfig::default(),
            indexing: IndexingConfig::default(),
            query: QueryConfig::default(),
        }
    }
}

impl Default for EmbeddingConfig {
    fn default() -> Self {
        Self {
            model_name: "microsoft/codebert-base".to_string(),
            model_path: None,
            embedding_size: 768,
            max_sequence_length: 512,
            batch_size: 8,
        }
    }
}

impl Default for VectorStoreConfig {
    fn default() -> Self {
        Self {
            url: "http://localhost:6334".to_string(),
            collection_name: "uniconnect_code".to_string(),
            embedding_size: 768,
            distance_metric: DistanceMetric::Cosine,
            index_config: IndexConfig::default(),
        }
    }
}

impl Default for IndexingConfig {
    fn default() -> Self {
        Self {
            include_patterns: vec![
                "**/*.rs".to_string(),
                "**/*.kt".to_string(),
                "**/*.java".to_string(),
            ],
            exclude_patterns: vec![
                "**/target/**".to_string(),
                "**/build/**".to_string(),
                "**/.git/**".to_string(),
            ],
            max_file_size: 1024 * 1024, // 1MB
            chunk_size: 1000,
            chunk_overlap: 100,
            incremental: true,
        }
    }
}

impl Default for QueryConfig {
    fn default() -> Self {
        Self {
            max_results: 10,
            similarity_threshold: 0.7,
            context_window: 2000,
            rerank: true,
        }
    }
}

impl Default for IndexConfig {
    fn default() -> Self {
        Self {
            hnsw_config: HnswConfig::default(),
            quantization_config: None,
        }
    }
}

impl Default for HnswConfig {
    fn default() -> Self {
        Self {
            m: 16,
            ef_construct: 100,
            ef: 50,
        }
    }
}

impl RagConfig {
    pub fn from_file(path: &std::path::Path) -> anyhow::Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let config: RagConfig = serde_json::from_str(&content)?;
        Ok(config)
    }

    pub fn save_to_file(&self, path: &std::path::Path) -> anyhow::Result<()> {
        let content = serde_json::to_string_pretty(self)?;
        std::fs::write(path, content)?;
        Ok(())
    }
}