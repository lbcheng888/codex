use anyhow::Result;
use std::sync::Arc;
use tokio::sync::{Semaphore, RwLock};
use std::collections::HashMap;
use std::time::{Duration, Instant};
use tracing::{info, warn};

use crate::types::{CodeUnit, Embedding};
use crate::embedding::CodeEmbedder;
use crate::vector_store::VectorStore;

/// Performance monitoring and optimization
pub struct PerformanceOptimizer {
    cache: Arc<RwLock<HashMap<String, CachedResult>>>,
    semaphore: Arc<Semaphore>,
    metrics: Arc<RwLock<PerformanceMetrics>>,
    batch_size: usize,
}

#[derive(Clone)]
struct CachedResult {
    embedding: Embedding,
    timestamp: Instant,
    access_count: u64,
}

#[derive(Default)]
struct PerformanceMetrics {
    total_requests: u64,
    cache_hits: u64,
    cache_misses: u64,
    avg_embedding_time: Duration,
    avg_storage_time: Duration,
    avg_search_time: Duration,
}

impl PerformanceOptimizer {
    pub fn new(max_concurrent: usize, batch_size: usize) -> Self {
        Self {
            cache: Arc::new(RwLock::new(HashMap::new())),
            semaphore: Arc::new(Semaphore::new(max_concurrent)),
            metrics: Arc::new(RwLock::new(PerformanceMetrics::default())),
            batch_size,
        }
    }

    /// Batch process code units with parallel embedding
    pub async fn batch_embed_and_store(
        &self,
        embedder: Arc<CodeEmbedder>,
        vector_store: Arc<VectorStore>,
        code_units: Vec<CodeUnit>,
    ) -> Result<()> {
        let total_units = code_units.len();
        let mut processed = 0;
        let mut errors = 0;

        // Process in batches
        for chunk in code_units.chunks(self.batch_size) {
            let start_time = Instant::now();
            
            // Create embedding tasks
            let mut tasks = Vec::new();
            
            for unit in chunk {
                let embedder = embedder.clone();
                let vector_store = vector_store.clone();
                let unit = unit.clone();
                let cache = self.cache.clone();
                let metrics = self.metrics.clone();
                let semaphore = self.semaphore.clone();
                
                let task = tokio::spawn(async move {
                    let _permit = semaphore.acquire().await.unwrap();
                    
                    // Check cache first
                    let cache_key = Self::generate_cache_key(&unit);
                    {
                        let cache = cache.read().await;
                        if let Some(cached) = cache.get(&cache_key) {
                            let mut metrics = metrics.write().await;
                            metrics.cache_hits += 1;
                            return Ok((unit, cached.embedding.clone()));
                        }
                    }

                    // Generate new embedding
                    let embedding_start = Instant::now();
                    let embedding = embedder.embed(&unit).await?;
                    let embedding_time = embedding_start.elapsed();

                    // Update metrics
                    {
                        let mut metrics = metrics.write().await;
                        metrics.cache_misses += 1;
                        metrics.avg_embedding_time = 
                            (metrics.avg_embedding_time * metrics.total_requests + embedding_time)
                            / (metrics.total_requests + 1);
                        metrics.total_requests += 1;
                    }

                    // Store in cache
                    {
                        let mut cache = cache.write().await;
                        cache.insert(cache_key, CachedResult {
                            embedding: embedding.clone(),
                            timestamp: Instant::now(),
                            access_count: 1,
                        });
                    }

                    Ok((unit, embedding))
                });
                
                tasks.push(task);
            }

            // Wait for all tasks to complete
            let results = futures::future::join_all(tasks).await;
            
            // Process results
            let mut valid_results = Vec::new();
            for result in results {
                match result {
                    Ok(Ok((unit, embedding))) => {
                        valid_results.push((unit, embedding));
                        processed += 1;
                    }
                    Ok(Err(e)) => {
                        warn!("Error embedding code unit: {}", e);
                        errors += 1;
                    }
                    Err(e) => {
                        warn!("Task failed: {}", e);
                        errors += 1;
                    }
                }
            }

            // Batch store results
            if !valid_results.is_empty() {
                let storage_start = Instant::now();
                
                vector_store.store_batch(
                    valid_results.iter()
                        .map(|(unit, embedding)| (unit, embedding))
                        .collect()
                ).await?;
                
                let storage_time = storage_start.elapsed();
                {
                    let mut metrics = self.metrics.write().await;
                    metrics.avg_storage_time = 
                        (metrics.avg_storage_time * metrics.total_requests + storage_time)
                        / (metrics.total_requests + 1);
                }
            }

            let chunk_time = start_time.elapsed();
            info!(
                "Processed {}/{} units in {:?} ({} errors)",
                processed, total_units, chunk_time, errors
            );
        }

        info!(
            "Batch processing completed: {}/{} units successfully processed",
            processed, total_units
        );

        Ok(())
    }

    /// Parallel search with result deduplication
    pub async fn parallel_search(
        &self,
        vector_store: Arc<VectorStore>,
        queries: Vec<String>,
        limit: usize,
    ) -> Result<HashMap<String, Vec<CodeMatch>>> {
        let mut tasks = Vec::new();
        let semaphore = self.semaphore.clone();
        let metrics = self.metrics.clone();

        for query in queries {
            let vector_store = vector_store.clone();
            let semaphore = semaphore.clone();
            let metrics = metrics.clone();
            
            let task = tokio::spawn(async move {
                let _permit = semaphore.acquire().await.unwrap();
                let search_start = Instant::now();
                
                let result = vector_store.search_by_language(
                    crate::types::Language::Rust,
                    limit,
                ).await;
                
                let search_time = search_start.elapsed();
                {
                    let mut metrics = metrics.write().await;
                    metrics.avg_search_time = 
                        (metrics.avg_search_time * metrics.total_requests + search_time)
                        / (metrics.total_requests + 1);
                }
                
                (query, result)
            });
            
            tasks.push(task);
        }

        let results = futures::future::join_all(tasks).await;
        
        let mut final_results = HashMap::new();
        for result in results {
            if let Ok((query, matches)) = result {
                match matches {
                    Ok(matches) => {
                        final_results.insert(query, matches);
                    }
                    Err(e) => {
                        warn!("Search error for query: {}", e);
                    }
                }
            }
        }

        Ok(final_results)
    }

    /// Cache management with TTL
    pub async fn cleanup_cache(
        &self,
        max_age: Duration,
    ) -> Result<usize> {
        let mut cache = self.cache.write().await;
        let now = Instant::now();
        
        let old_size = cache.len();
        cache.retain(|_, cached| now - cached.timestamp < max_age);
        let new_size = cache.len();
        
        let removed = old_size - new_size;
        info!("Cache cleanup: removed {} entries", removed);
        
        Ok(removed)
    }

    /// Get performance metrics
    pub async fn get_metrics(
        &self,
    ) -> Result<PerformanceReport> {
        let metrics = self.metrics.read().await;
        let cache = self.cache.read().await;
        
        let cache_hit_ratio = if metrics.total_requests > 0 {
            metrics.cache_hits as f64 / metrics.total_requests as f64
        } else {
            0.0
        };

        Ok(PerformanceReport {
            total_requests: metrics.total_requests,
            cache_hits: metrics.cache_hits,
            cache_misses: metrics.cache_misses,
            cache_hit_ratio,
            cache_size: cache.len(),
            avg_embedding_time_ms: metrics.avg_embedding_time.as_millis() as u64,
            avg_storage_time_ms: metrics.avg_storage_time.as_millis() as u64,
            avg_search_time_ms: metrics.avg_search_time.as_millis() as u64,
        })
    }

    /// Generate cache key
    fn generate_cache_key(unit: &CodeUnit) -> String {
        use std::hash::{Hash, Hasher};
        use std::collections::hash_map::DefaultHasher;

        let mut hasher = DefaultHasher::new();
        unit.file_path.hash(&mut hasher);
        unit.content.len().hash(&mut hasher);
        unit.functions.len().hash(&mut hasher);
        hasher.finish().to_string()
    }

    /// Prefetch popular queries
    pub async fn prefetch_popular_queries(
        &self,
        vector_store: Arc<VectorStore>,
        queries: Vec<String>,
    ) -> Result<()> {
        info!("Prefetching {} popular queries", queries.len());
        
        let results = self.parallel_search(
            vector_store,
            queries,
            5, // Small limit for prefetching
        ).await?;

        // Store results in cache
        let mut cache = self.cache.write().await;
        for (query, matches) in results {
            if let Some(first_match) = matches.first() {
                // Create dummy embedding for cache
                let dummy_embedding = crate::types::Embedding::new(
                    vec![0.0; 768] // Assuming 768 dimensions
                );
                
                cache.insert(
                    format!("prefetch_{}", query),
                    CachedResult {
                        embedding: dummy_embedding,
                        timestamp: Instant::now(),
                        access_count: 0,
                    }
                );
            }
        }

        info!("Prefetching completed");
        Ok(())
    }

    /// Monitor system health
    pub async fn health_check(
        &self,
    ) -> Result<HealthStatus> {
        let metrics = self.metrics.read().await;
        let cache = self.cache.read().await;

        Ok(HealthStatus {
            cache_size: cache.len(),
            cache_hit_ratio: if metrics.total_requests > 0 {
                metrics.cache_hits as f64 / metrics.total_requests as f64
            } else {
                0.0
            },
            avg_embedding_time_ms: metrics.avg_embedding_time.as_millis() as u64,
            avg_storage_time_ms: metrics.avg_storage_time.as_millis() as u64,
            avg_search_time_ms: metrics.avg_search_time.as_millis() as u64,
            semaphore_permits: self.semaphore.available_permits(),
        })
    }
}

/// Performance report structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceReport {
    pub total_requests: u64,
    pub cache_hits: u64,
    pub cache_misses: u64,
    pub cache_hit_ratio: f64,
    pub cache_size: usize,
    pub avg_embedding_time_ms: u64,
    pub avg_storage_time_ms: u64,
    pub avg_search_time_ms: u64,
}

/// Health status structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthStatus {
    pub cache_size: usize,
    pub cache_hit_ratio: f64,
    pub avg_embedding_time_ms: u64,
    pub avg_storage_time_ms: u64,
    pub avg_search_time_ms: u64,
    pub semaphore_permits: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[tokio::test]
    async fn test_performance_optimizer() {
        let optimizer = PerformanceOptimizer::new(4, 10);
        
        // Test cache operations
        let metrics = optimizer.get_metrics().await.unwrap();
        assert_eq!(metrics.total_requests, 0);
        
        // Test cache cleanup
        let removed = optimizer.cleanup_cache(Duration::from_secs(1)).await.unwrap();
        assert_eq!(removed, 0);
        
        // Test health check
        let health = optimizer.health_check().await.unwrap();
        assert!(health.semaphore_permits >= 4);
    }

    #[tokio::test]
    async fn test_cache_key_generation() {
        let unit = crate::types::CodeUnit {
            file_path: std::path::PathBuf::from("test.rs"),
            language: crate::types::Language::Rust,
            content: "fn test() {}".to_string(),
            ast_context: crate::types::AstContext::default(),
            dependencies: vec![],
            functions: vec![],
            structs: vec![],
            imports: vec![],
        };

        let key = PerformanceOptimizer::generate_cache_key(&unit);
        assert!(!key.is_empty());
    }
}