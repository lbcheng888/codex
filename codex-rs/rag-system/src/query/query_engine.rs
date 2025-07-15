use anyhow::Result;
use std::sync::Arc;
use std::collections::HashMap;

use crate::embedding::CodeEmbedder;
use crate::vector_store::VectorStore;
use crate::config::QueryConfig;
use crate::types::{Language, CodeMatch};
use qdrant_client::qdrant::Filter;

use super::{CodeExample, CodePattern, CodeExplanation, ApiDocumentation};

/// Core query engine for semantic search
pub struct QueryEngine {
    embedder: Arc<CodeEmbedder>,
    vector_store: Arc<VectorStore>,
    config: QueryConfig,
    query_cache: Arc<RwLock<HashMap<String, Vec<CodeExample>>>>,
}

use tokio::sync::RwLock;

impl QueryEngine {
    pub fn new(
        embedder: Arc<CodeEmbedder>,
        vector_store: Arc<VectorStore>,
        config: QueryConfig,
    ) -> Self {
        Self {
            embedder,
            vector_store,
            config,
            query_cache: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Query for code examples
    pub async fn query_examples(
        &self,
        query: &str,
        limit: usize,
    ) -> Result<Vec<CodeExample>> {
        // Check cache first
        let cache_key = format!("{}_{}", query, limit);
        {
            let cache = self.query_cache.read().await;
            if let Some(cached) = cache.get(&cache_key) {
                return Ok(cached.clone());
            }
        }

        // Generate embedding for query
        let query_embedding = self.embedder.embed_query(query).await?;

        // Search in vector store
        let matches = self.vector_store
            .search(&query_embedding,
                limit,
            )
            .await?;

        // Filter by similarity threshold
        let filtered_matches: Vec<CodeMatch> = matches
            .into_iter()
            .filter(|m| m.score >= self.config.similarity_threshold)
            .collect();

        // Convert to code examples
        let examples = self.matches_to_examples(filtered_matches, query).await?;

        // Cache results
        {
            let mut cache = self.query_cache.write().await;
            cache.insert(cache_key, examples.clone());
        }

        Ok(examples)
    }

    /// Query with language filter
    pub async fn query_examples_by_language(
        &self,
        query: &str,
        language: Language,
        limit: usize,
    ) -> Result<Vec<CodeExample>> {
        let query_embedding = self.embedder.embed_query(query).await?;

        let filter = Filter::must([
            qdrant_client::qdrant::FieldCondition::new(
                "language",
                qdrant_client::qdrant::Match::new(format!("{:?}", language)),
            ).into(),
        ]);

        let matches = self.vector_store
            .search_with_filter(
                &query_embedding,
                limit,
                Some(filter),
            )
            .await?;

        let examples = self.matches_to_examples(matches, query).await?;
        Ok(examples)
    }

    /// Find similar code patterns
    pub async fn find_patterns(
        &self,
        code_snippet: &str,
        language: Option<String>,
        limit: usize,
    ) -> Result<Vec<CodePattern>> {
        let query_embedding = self.embedder.embed_query(code_snippet).await?;

        let mut filter = None;
        if let Some(lang) = language {
            let lang_enum = match lang.to_lowercase().as_str() {
                "rust" => Language::Rust,
                "kotlin" => Language::Kotlin,
                "java" => Language::Java,
                _ => Language::Other(lang),
            };

            filter = Some(Filter::must([
                qdrant_client::qdrant::FieldCondition::new(
                    "language",
                    qdrant_client::qdrant::Match::new(format!("{:?}", lang_enum)),
                ).into(),
            ]));
        }

        let matches = self.vector_store
            .search_with_filter(
                &query_embedding,
                limit,
                filter,
            )
            .await?;

        let patterns = self.group_matches_into_patterns(matches).await?;
        Ok(patterns)
    }

    /// Explain code functionality
    pub async fn explain_code(
        &self,
        code_snippet: &str,
        context_size: usize,
    ) -> Result<CodeExplanation> {
        let query_embedding = self.embedder.embed_query(code_snippet).await?;

        let matches = self.vector_store
            .search(&query_embedding,
                context_size,
            )
            .await?;

        let explanation = self.generate_explanation(code_snippet, &matches).await?;
        Ok(explanation)
    }

    /// Get API documentation
    pub async fn get_api_docs(
        &self,
        api_query: &str,
        language: Option<String>,
    ) -> Result<ApiDocumentation> {
        let query_embedding = self.embedder.embed_query(api_query).await?;

        let mut filter = None;
        if let Some(lang) = language {
            let lang_enum = match lang.to_lowercase().as_str() {
                "rust" => Language::Rust,
                "kotlin" => Language::Kotlin,
                "java" => Language::Java,
                _ => Language::Other(lang),
            };

            filter = Some(Filter::must([
                qdrant_client::qdrant::FieldCondition::new(
                    "language",
                    qdrant_client::qdrant::Match::new(format!("{:?}", lang_enum)),
                ).into(),
            ]));
        }

        let matches = self.vector_store
            .search_with_filter(
                &query_embedding,
                10, // Get more results for API docs
                filter,
            )
            .await?;

        let api_docs = self.generate_api_docs(api_query, &matches).await?;
        Ok(api_docs)
    }

    /// Check vector store health
    pub async fn check_vector_store(
        &self,
    ) -> Result<bool> {
        match self.vector_store.collection_info().await {
            Ok(_) => Ok(true),
            Err(_) => Ok(false),
        }
    }

    /// Check embedder health
    pub async fn check_embedder(
        &self,
    ) -> Result<bool> {
        // Simple test to check if embedder is working
        let test_embedding = self.embedder.embed_query("test").await;
        Ok(test_embedding.is_ok())
    }

    /// Convert matches to code examples
    async fn matches_to_examples(
        &self,
        matches: Vec<CodeMatch>,
        query: &str,
    ) -> Result<Vec<CodeExample>> {
        let mut examples = Vec::new();

        for m in matches {
            let language = match m.context.language {
                Language::Rust => "Rust",
                Language::Kotlin => "Kotlin",
                Language::Java => "Java",
                Language::Other(ref lang) => lang.as_str(),
            }.to_string();

            let description = self.generate_description(&m.code, query).await;

            examples.push(CodeExample {
                file_path: m.file_path.to_string_lossy().to_string(),
                language,
                code: m.code,
                description: Some(description),
                score: m.score,
                context: m.context,
            });
        }

        Ok(examples)
    }

    /// Group matches into patterns
    async fn group_matches_into_patterns(
        &self,
        matches: Vec<CodeMatch>,
    ) -> Result<Vec<CodePattern>> {
        let mut patterns = HashMap::new();

        for m in matches {
            let pattern = self.extract_pattern(&m.code).await;
            
            let entry = patterns.entry(pattern.clone()).or_insert_with(|| {
                CodePattern {
                    pattern: pattern.clone(),
                    examples: Vec::new(),
                    usage_frequency: 0.0,
                    best_practices: Vec::new(),
                }
            });

            let example = CodeExample {
                file_path: m.file_path.to_string_lossy().to_string(),
                language: match m.context.language {
                    Language::Rust => "Rust".to_string(),
                    Language::Kotlin => "Kotlin".to_string(),
                    Language::Java => "Java".to_string(),
                    Language::Other(ref lang) => lang.clone(),
                },
                code: m.code,
                description: None,
                score: m.score,
                context: m.context,
            };

            entry.examples.push(example);
            entry.usage_frequency += 1.0;
        }

        let mut result: Vec<CodePattern> = patterns.into_values().collect();
        
        // Sort by usage frequency
        result.sort_by(|a, b| b.usage_frequency.partial_cmp(&a.usage_frequency).unwrap());
        
        // Generate best practices for each pattern
        for pattern in &mut result {
            pattern.best_practices = self.generate_best_practices(&pattern.pattern).await;
        }

        Ok(result)
    }

    /// Generate explanation for code
    async fn generate_explanation(
        &self,
        code_snippet: &str,
        matches: &[CodeMatch],
    ) -> Result<CodeExplanation> {
        let mut key_concepts = Vec::new();
        let mut related_patterns = Vec::new();
        let mut usage_examples = Vec::new();

        // Extract key concepts from matches
        for m in matches {
            let concepts = self.extract_concepts(&m.code).await;
            key_concepts.extend(concepts);
        }

        // Remove duplicates
        key_concepts.sort();
        key_concepts.dedup();

        // Generate related patterns
        let patterns = self.group_matches_into_patterns(matches.to_vec()).await?;
        related_patterns.extend(patterns);

        // Create usage examples
        for m in matches.iter().take(3) {
            let example = CodeExample {
                file_path: m.file_path.to_string_lossy().to_string(),
                language: match m.context.language {
                    Language::Rust => "Rust".to_string(),
                    Language::Kotlin => "Kotlin".to_string(),
                    Language::Java => "Java".to_string(),
                    Language::Other(ref lang) => lang.clone(),
                },
                code: m.code.clone(),
                description: None,
                score: m.score,
                context: m.context.clone(),
            };
            usage_examples.push(example);
        }

        let explanation = self.create_explanation_text(code_snippet, &key_concepts).await;

        Ok(CodeExplanation {
            explanation,
            key_concepts,
            related_patterns,
            usage_examples,
        })
    }

    /// Generate API documentation
    async fn generate_api_docs(
        &self,
        api_query: &str,
        matches: &[CodeMatch],
    ) -> Result<ApiDocumentation> {
        // Group matches by API
        let mut api_groups = HashMap::new();

        for m in matches {
            let api_name = self.extract_api_name(&m.code).await;
            let entry = api_groups.entry(api_name).or_insert_with(Vec::new);
            entry.push(m);
        }

        // Find the most relevant API
        let (api_name, api_matches) = api_groups.into_iter()
            .max_by_key(|(_, matches)| matches.len())
            .unwrap_or(("Unknown API".to_string(), Vec::new()));

        // Generate documentation
        let description = self.generate_api_description(api_query, &api_matches).await;
        let usage = self.generate_api_usage(api_query, &api_matches).await;
        let parameters = self.extract_api_parameters(&api_matches).await;
        let return_type = self.extract_return_type(&api_matches).await;
        
        let examples: Vec<CodeExample> = api_matches.iter()
            .take(3)
            .map(|m| CodeExample {
                file_path: m.file_path.to_string_lossy().to_string(),
                language: match m.context.language {
                    Language::Rust => "Rust".to_string(),
                    Language::Kotlin => "Kotlin".to_string(),
                    Language::Java => "Java".to_string(),
                    Language::Other(ref lang) => lang.clone(),
                },
                code: m.code.clone(),
                description: None,
                score: m.score,
                context: m.context.clone(),
            })
            .collect();

        Ok(ApiDocumentation {
            api_name,
            description,
            usage,
            parameters,
            return_type,
            examples,
        })
    }

    /// Extract pattern from code
    async fn extract_pattern(
        &self,
        code: &str,
    ) -> String {
        // Simplified pattern extraction
        // In practice, this would use more sophisticated NLP techniques
        let lines: Vec<&str> = code.lines()
            .filter(|l| !l.trim().is_empty())
            .take(5)
            .collect();
        
        lines.join("\n")
    }

    /// Generate description for code
    async fn generate_description(
        &self,
        code: &str,
        query: &str,
    ) -> String {
        format!("Code matching query: {}\n\nSnippet: {}", 
            query, 
            code.lines().take(3).collect::<Vec<_>>().join("\n"))
    }

    /// Extract concepts from code
    async fn extract_concepts(
        &self,
        code: &str,
    ) -> Vec<String> {
        let mut concepts = Vec::new();
        
        // Simple keyword extraction
        let keywords = [
            "function", "class", "struct", "enum", "trait", "interface",
            "async", "await", "return", "if", "for", "while", "match",
            "import", "use", "pub", "private", "static", "const",
        ];

        for keyword in keywords {
            if code.contains(keyword) {
                concepts.push(keyword.to_string());
            }
        }

        concepts
    }

    /// Create explanation text
    async fn create_explanation_text(
        &self,
        code_snippet: &str,
        concepts: &[String],
    ) -> String {
        format!("This code demonstrates {} concepts including: {}", 
            concepts.len(), 
            concepts.join(", "))
    }

    /// Generate best practices
    async fn generate_best_practices(
        &self,
        pattern: &str,
    ) -> Vec<String> {
        vec![
            "Use clear variable names",
            "Add appropriate error handling",
            "Include comprehensive documentation",
            "Follow language-specific conventions",
            "Consider performance implications",
        ]
    }

    /// Extract API name
    async fn extract_api_name(
        &self,
        code: &str,
    ) -> String {
        // Simple heuristic - first function or class name
        let lines: Vec<&str> = code.lines().collect();
        
        for line in lines {
            let line = line.trim();
            if line.contains("fn ") || line.contains("fun ") || line.contains("public ") {
                return line.split_whitespace()
                    .nth(1)
                    .unwrap_or("API")
                    .to_string();
            }
        }
        
        "API".to_string()
    }

    /// Generate API description
    async fn generate_api_description(
        &self,
        query: &str,
        matches: &[CodeMatch],
    ) -> String {
        format!("API for {} based on {} examples", query, matches.len())
    }

    /// Generate API usage
    async fn generate_api_usage(
        &self,
        query: &str,
        matches: &[CodeMatch],
    ) -> String {
        format!("Usage examples for {} API", query)
    }

    /// Extract API parameters
    async fn extract_api_parameters(
        &self,
        matches: &[CodeMatch],
    ) -> Vec<super::ApiParameter> {
        // Simplified parameter extraction
        vec![
            super::ApiParameter {
                name: "param1".to_string(),
                type_name: "String".to_string(),
                description: "First parameter".to_string(),
                required: true,
                default_value: None,
            }
        ]
    }

    /// Extract return type
    async fn extract_return_type(
        &self,
        _matches: &[CodeMatch],
    ) -> String {
        "Result<T, Error>".to_string()
    }
}