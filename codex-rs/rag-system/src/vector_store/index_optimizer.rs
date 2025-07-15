use anyhow::Result;
use qdrant_client::client::QdrantClient;
use qdrant_client::qdrant::{
    OptimizersConfigDiff, HnswConfigDiff, UpdateCollectionBuilder,
    UpdateStatus, OptimizersConfigDiffBuilder, HnswConfigDiffBuilder,
};

use crate::config::VectorStoreConfig;

/// Handles vector index optimization
pub struct IndexOptimizer {
    client: QdrantClient,
    config: VectorStoreConfig,
}

impl IndexOptimizer {
    pub fn new(client: QdrantClient, config: VectorStoreConfig) -> Self {
        Self { client, config }
    }

    /// Optimize collection for better search performance
    pub async fn optimize(&self) -> Result<()> {
        self.update_optimizers().await?;
        self.update_hnsw().await?;
        Ok(())
    }

    /// Update optimizer configuration
    pub async fn update_optimizers(&self) -> Result<()> {
        let optimizers_config = OptimizersConfigDiffBuilder::default()
            .default_segment_number(2)
            .indexing_threshold(50000)
            .memmap_threshold(100000)
            .vacuum_min_vector_number(1000)
            .max_optimization_threads(2);

        let update_request = UpdateCollectionBuilder::new(&self.config.collection_name)
            .optimizers_config(optimizers_config);

        let result = self.client.update_collection(update_request).await?;
        
        if result.status != UpdateStatus::Completed {
            anyhow::bail!("Failed to update optimizers: {:?}", result.status);
        }

        Ok(())
    }

    /// Update HNSW configuration
    pub async fn update_hnsw(&self) -> Result<()> {
        let hnsw_config = HnswConfigDiffBuilder::default()
            .m(16)
            .ef_construct(100)
            .ef(10);

        let update_request = UpdateCollectionBuilder::new(&self.config.collection_name)
            .hnsw_config(hnsw_config);

        let result = self.client.update_collection(update_request).await?;
        
        if result.status != UpdateStatus::Completed {
            anyhow::bail!("Failed to update HNSW: {:?}", result.status);
        }

        Ok(())
    }

    /// Trigger manual optimization
    pub async fn trigger_optimization(&self) -> Result<()> {
        self.client
            .update_collection(
                UpdateCollectionBuilder::new(&self.config.collection_name)
                    .optimizers_config(
                        OptimizersConfigDiffBuilder::default()
                            .indexing_threshold(1000)
                    )
            )
            .await?;

        Ok(())
    }

    /// Get optimization status
    pub async fn get_optimization_status(&self) -> Result<serde_json::Value> {
        let info = self.client
            .get_collection(&self.config.collection_name)
            .await?;

        Ok(serde_json::json!({
            "status": "active",
            "optimizer_status": "ok",
            "vectors_count": info.vectors_count,
            "indexed_vectors_count": info.indexed_vectors_count,
            "points_count": info.points_count,
            "segments_count": info.segments_count,
        }))
    }
}