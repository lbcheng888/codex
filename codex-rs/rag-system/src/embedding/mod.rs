use anyhow::Result;
use candle_core::{Device, Tensor, DType};
use candle_transformers::models::bert::{BertModel, Config, DTYPE};
use tokenizers::Tokenizer;

use crate::types::{CodeUnit, Embedding, Language};
use crate::config::EmbeddingConfig;

mod code_tokenizer;
mod semantic_analyzer;

pub use code_tokenizer::CodeTokenizer;
pub use semantic_analyzer::SemanticAnalyzer;

/// Main code embedder using CodeBERT model
pub struct CodeEmbedder {
    model: BertModel,
    tokenizer: Tokenizer,
    device: Device,
    config: EmbeddingConfig,
    semantic_analyzer: SemanticAnalyzer,
}

impl CodeEmbedder {
    pub async fn new(config: &EmbeddingConfig) -> Result<Self> {
        let device = if candle_core::utils::cuda_is_available() {
            Device::new_cuda(0)?
        } else {
            Device::Cpu
        };

        // Load model and tokenizer
        let (model, tokenizer) = Self::load_model(config, &device).await?;
        let semantic_analyzer = SemanticAnalyzer::new();

        Ok(Self {
            model,
            tokenizer,
            device,
            config: config.clone(),
            semantic_analyzer,
        })
    }

    /// Embed a code unit into a vector
    pub async fn embed(&self,
        code_unit: &CodeUnit,
    ) -> Result<Embedding> {
        // Create semantic representation
        let semantic_text = self.semantic_analyzer.analyze(code_unit);
        
        // Tokenize the code
        let tokens = self.code_tokenize(&semantic_text, code_unit.language.clone())?;
        
        // Generate embedding
        let embedding = self.generate_embedding(&tokens).await?;
        
        Ok(embedding)
    }

    /// Embed a query string
    pub async fn embed_query(&self,
        query: &str,
    ) -> Result<Embedding> {
        let tokens = self.tokenizer
            .encode(query, false)
            .map_err(|e| anyhow::anyhow!("Tokenization error: {}", e))?;

        let ids = tokens.get_ids();
        let tensor = Tensor::new(ids, &self.device)?;
        
        let token_type_ids = tensor.zeros_like()?;
        let embeddings = self.model.forward(&tensor.unsqueeze(0)?,
            &token_type_ids.unsqueeze(0)?,
        )?;

        // Use [CLS] token embedding
        let cls_embedding = embeddings.i((0, 0))?;
        let vector: Vec<f32> = cls_embedding.to_vec1()?;
        
        Ok(Embedding::new(vector))
    }

    /// Batch embed multiple code units
    pub async fn embed_batch(
        &self,
        code_units: &[CodeUnit],
    ) -> Result<Vec<Embedding>> {
        use rayon::prelude::*;
        
        let embeddings: Result<Vec<_>> = code_units
            .par_iter()
            .map(|unit| {
                let rt = tokio::runtime::Handle::current();
                rt.block_on(self.embed(unit))
            })
            .collect();

        embeddings
    }

    /// Load model and tokenizer based on config
    async fn load_model(
        config: &EmbeddingConfig,
        device: &Device,
    ) -> Result<(BertModel, Tokenizer)> {
        let model_path = if let Some(ref path) = config.model_path {
            path.clone()
        } else {
            // Use default model
            PathBuf::from("microsoft/codebert-base")
        };

        // Load tokenizer
        let tokenizer = Tokenizer::from_file(
            model_path.join("tokenizer.json")
        ).map_err(|e| anyhow::anyhow!("Failed to load tokenizer: {}", e))?;

        // Load model config
        let config_path = model_path.join("config.json");
        let config_str = tokio::fs::read_to_string(config_path).await?;
        let bert_config: Config = serde_json::from_str(&config_str)?;

        // Load model weights
        let weights_path = model_path.join("pytorch_model.bin");
        let vb = unsafe {
            candle_nn::VarBuilder::from_mmaped_safetensors(
                &[weights_path],
                DTYPE,
                device,
            )?
        };

        let model = BertModel::load(vb, &bert_config)?;
        
        Ok((model, tokenizer))
    }

    /// Tokenize code with special handling for programming languages
    fn code_tokenize(
        &self,
        text: &str,
        language: Language,
    ) -> Result<Vec<u32>> {
        // Add language-specific preprocessing
        let processed_text = match language {
            Language::Rust => self.preprocess_rust_code(text),
            Language::Kotlin => self.preprocess_kotlin_code(text),
            Language::Java => self.preprocess_java_code(text),
            Language::Other(_) => text.to_string(),
        };

        let tokens = self.tokenizer
            .encode(&processed_text,
                false,
            )
            .map_err(|e| anyhow::anyhow!("Tokenization error: {}", e))?;

        let ids = tokens.get_ids();
        
        // Truncate if necessary
        let max_len = self.config.max_sequence_length;
        let truncated_ids = if ids.len() > max_len {
            &ids[..max_len]
        } else {
            ids
        };

        Ok(truncated_ids.to_vec())
    }

    /// Generate embedding from tokenized input
    async fn generate_embedding(
        &self,
        tokens: &[u32],
    ) -> Result<Embedding> {
        let tensor = Tensor::new(tokens, &self.device)?;
        let token_type_ids = tensor.zeros_like()?;
        
        let embeddings = self.model.forward(
            &tensor.unsqueeze(0)?,
            &token_type_ids.unsqueeze(0)?,
        )?;

        // Use mean pooling over sequence length
        let mean_embedding = embeddings.mean(1)?;
        let vector: Vec<f32> = mean_embedding.to_vec1()?;
        
        Ok(Embedding::new(vector))
    }

    /// Preprocess Rust code
    fn preprocess_rust_code(&self,
        code: &str,
    ) -> String {
        // Remove comments but keep doc comments for semantic meaning
        let lines: Vec<&str> = code.lines()
            .filter(|line| {
                let trimmed = line.trim();
                !trimmed.starts_with("//") || trimmed.starts_with("///")
            })
            .collect();
        
        lines.join("\n")
    }

    /// Preprocess Kotlin code
    fn preprocess_kotlin_code(&self,
        code: &str,
    ) -> String {
        // Similar to Rust preprocessing
        let lines: Vec<&str> = code.lines()
            .filter(|line| {
                let trimmed = line.trim();
                !trimmed.starts_with("//") || trimmed.starts_with("///")
            })
            .collect();
        
        lines.join("\n")
    }

    /// Preprocess Java code
    fn preprocess_java_code(&self,
        code: &str,
    ) -> String {
        // Remove single-line comments but keep Javadoc
        let lines: Vec<&str> = code.lines()
            .filter(|line| {
                let trimmed = line.trim();
                !trimmed.starts_with("//") || trimmed.starts_with("/**")
            })
            .collect();
        
        lines.join("\n")
    }

    /// Get embedding dimensions
    pub fn dimensions(&self,
    ) -> usize {
        self.config.embedding_size
    }
}

impl Default for CodeEmbedder {
    fn default() -> Self {
        let config = EmbeddingConfig::default();
        let rt = tokio::runtime::Handle::current();
        rt.block_on(Self::new(&config)).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[tokio::test]
    async fn test_embed_code_unit() {
        let config = EmbeddingConfig {
            model_name: "microsoft/codebert-base".to_string(),
            model_path: None,
            embedding_size: 768,
            max_sequence_length: 512,
            batch_size: 8,
        };

        let embedder = CodeEmbedder::new(&config).await.unwrap();
        
        let code_unit = CodeUnit {
            file_path: std::path::PathBuf::from("test.rs"),
            language: Language::Rust,
            content: "fn add(a: i32, b: i32) -> i32 { a + b }".to_string(),
            ast_context: crate::types::AstContext::default(),
            dependencies: vec!["std::io".to_string()],
            functions: vec![],
            structs: vec![],
            imports: vec![],
        };

        let embedding = embedder.embed(&code_unit).await.unwrap();
        assert_eq!(embedding.dimensions, 768);
    }

    #[tokio::test]
    async fn test_embed_query() {
        let config = EmbeddingConfig::default();
        let embedder = CodeEmbedder::new(&config).await.unwrap();

        let embedding = embedder.embed_query("How to implement a P2P connection?").await.unwrap();
        assert_eq!(embedding.dimensions, 768);
    }
}