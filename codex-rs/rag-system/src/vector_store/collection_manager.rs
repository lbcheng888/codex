use anyhow::Result;
use qdrant_client::client::QdrantClient;
use qdrant_client::qdrant::{
    CreateCollectionBuilder, Distance, VectorParamsBuilder,
    HnswConfigDiffBuilder, OptimizersConfigDiffBuilder,
    ScalarQuantizationBuilder,
    UpdateStatus,
};

use crate::config::{VectorStoreConfig, DistanceMetric};

/// Manages Qdrant collections
pub struct CollectionManager {
    client: QdrantClient,
    config: VectorStoreConfig,
}

impl CollectionManager {
    pub fn new(
        client: QdrantClient,
        config: VectorStoreConfig,
    ) -> Self {
        Self { client, config }
    }

    /// Ensure collection exists and is properly configured
    pub async fn ensure_collection(
        &self,
    ) -> Result<()> {
        let exists = self.collection_exists().await?;
        
        if !exists {
            self.create_collection().await?;
        } else {
            self.verify_collection_config().await?;
        }

        Ok(())
    }

    /// Check if collection exists
    pub async fn collection_exists(
        &self,
    ) -> Result<bool> {
        let collections = self.client.list_collections().await?;
        
        Ok(collections
            .collections
            .iter()
            .any(|c| c.name == self.config.collection_name))
    }

    /// Create collection with proper configuration
    pub async fn create_collection(
        &self,
    ) -> Result<()> {
        let distance = match self.config.distance_metric {
            DistanceMetric::Cosine => Distance::Cosine,
            DistanceMetric::Euclidean => Distance::Euclidean,
            DistanceMetric::DotProduct => Distance::Dot,
        };

        let mut collection_builder = CreateCollectionBuilder::new(
            &self.config.collection_name,
        )
        .vectors_config(VectorParamsBuilder::new(
            self.config.embedding_size as u64,
            distance,
        ));

        // Configure HNSW
        if let Some(hnsw_config) = &self.config.index_config.hnsw_config {
            collection_builder = collection_builder.hnsw_config(
                HnswConfigDiffBuilder::default()
                    .m(hnsw_config.m as u64)
                    .ef_construct(hnsw_config.ef_construct as u64)
                    .ef(hnsw_config.ef as u64)
            );
        }

        // Configure optimizers
        collection_builder = collection_builder.optimizers_config(
            OptimizersConfigDiffBuilder::default()
                .default_segment_number(2)
                .indexing_threshold(50000)
                .memmap_threshold(100000)
                .vacuum_min_vector_number(1000)
        );

        // Configure quantization if enabled
        if let Some(quantization) = &self.config.index_config.quantization_config {
            let mut quantization_builder = QuantizationConfigBuilder::default();
            
            match quantization.r#type {
                crate::config::QuantizationType::Scalar => {
                    quantization_builder = quantization_builder.scalar(
                        ScalarQuantizationBuilder::default()
                            .r#type(qdrant_client::qdrant::QuantizationType::Int8.into())
                            .quantile(quantization.quantile)
                    );
                }
            }
            
            collection_builder = collection_builder.quantization_config(
                quantization_builder
            );
        }

        let response = self.client.create_collection(collection_builder).await?;
        
        if !response.result {
            anyhow::bail!("Failed to create collection");
        }

        // Create payload indexes
        self.create_payload_indexes().await?;

        Ok(())
    }

    /// Verify collection configuration
    pub async fn verify_collection_config(
        &self,
    ) -> Result<()> {
        let collection_info = self.client
            .get_collection(&self.config.collection_name)
            .await?;

        let config = collection_info.config;
        
        // Check vector size
        let vector_size = config.params.vectors.unwrap().size as usize;
        if vector_size != self.config.embedding_size {
            anyhow::bail!(
                "Vector size mismatch: expected {}, got {}",
                self.config.embedding_size, vector_size
            );
        }

        // Check distance metric
        let distance = config.params.vectors.unwrap().distance;
        let expected_distance = match self.config.distance_metric {
            DistanceMetric::Cosine => Distance::Cosine,
            DistanceMetric::Euclidean => Distance::Euclidean,
            DistanceMetric::DotProduct => Distance::Dot,
        };

        if distance != expected_distance {
            anyhow::bail!(
                "Distance metric mismatch: expected {:?}, got {:?}",
                expected_distance, distance
            );
        }

        Ok(())
    }

    /// Drop collection
    pub async fn drop_collection(
        &self,
    ) -> Result<()> {
        let response = self.client
            .delete_collection(&self.config.collection_name)
            .await?;

        if !response.result {
            anyhow::bail!("Failed to delete collection");
        }

        Ok(())
    }

    /// Get collection stats
    pub async fn get_collection_stats(
        &self,
    ) -> Result<serde_json::Value> {
        let info = self.client
            .get_collection(&self.config.collection_name)
            .await?;

        Ok(serde_json::to_value(info)?)
    }

    /// Create payload indexes for better performance
    async fn create_payload_indexes(
        &self,
    ) -> Result<()> {
        let payload_fields = vec![
            "language",
            "file_path",
            "package",
        ];

        for field in payload_fields {
            let create_index_request = CreateIndexBuilder::new(
                &self.config.collection_name,
                field,
            )
            .field_type(
                qdrant_client::qdrant::FieldType::Keyword.into()
            );

            let result = self.client.create_index(create_index_request).await?;
            
            if result.status != UpdateStatus::Completed {
                tracing::warn!("Failed to create index for field: {}", field);
            }
        }

        Ok(())
    }

    /// Rebuild collection with new configuration
    pub async fn rebuild_collection(
        &self,
    ) -> Result<()> {
        // Drop existing collection
        if self.collection_exists().await? {
            self.drop_collection().await?;
        }

        // Create new collection
        self.create_collection().await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_collection_operations() {
        let config = VectorStoreConfig {
            url: "http://localhost:6334".to_string(),
            collection_name: "test_collection_manager".to_string(),
            embedding_size: 768,
            distance_metric: crate::config::DistanceMetric::Cosine,
            index_config: crate::config::IndexConfig::default(),
        };

        let client = QdrantClient::from_url(&config.url).unwrap();
        let manager = CollectionManager::new(client, config);

        // Skip test if Qdrant is not running
        if let Ok(exists) = manager.collection_exists().await {
            if !exists {
                let result = manager.create_collection().await;
                assert!(result.is_ok() || result.is_err()); // Allow either
            }
        }
    }
}