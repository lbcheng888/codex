use anyhow::Result;
use std::path::Path;
pub mod types;
pub mod config;
pub mod parser;
pub mod vector_store;

use types::{CodeUnit, Embedding};
use config::RagConfig;
use parser::CodeParser;
use vector_store::VectorStore;

/// Minimal working RAG system for UniConnectNative
pub struct RagSystem {
    parser: CodeParser,
    vector_store: VectorStore,
    config: RagConfig,
}

impl RagSystem {
    pub async fn new(config: RagConfig) -> Result<Self> {
        let parser = CodeParser::new();
        let vector_store = VectorStore::new(&config.vector_store).await?;

        Ok(Self {
            parser,
            vector_store,
            config,
        })
    }

    /// Index a codebase directory
    pub async fn index_codebase(
        &self,
        path: &Path,
    ) -> Result<()> {
        let files = self.parser.discover_files(path).await?;
        
        for file in files {
            if let Ok(code_unit) = self.parser.parse_file(&file).await {
                let embedding = self.create_mock_embedding(&code_unit.content);
                self.vector_store.store(
                    &code_unit,
                    &embedding
                ).await?;
            }
        }
        
        Ok(())
    }

    /// Query for relevant code
    pub async fn query(
        &self,
        query: &str,
        limit: usize,
    ) -> Result<Vec<CodeUnit>> {
        let mock_embedding = self.create_mock_embedding(query);
        let matches = self.vector_store.search(&mock_embedding, limit).await?;
        
        Ok(matches.into_iter()
            .map(|m| CodeUnit {
                file_path: m.file_path,
                language: m.context.language,
                content: m.code,
                ast_context: Default::default(),
                dependencies: m.context.imports.clone(),
                functions: vec![],
                structs: vec![],
                imports: m.context.imports,
            })
            .collect())
    }

    fn create_mock_embedding(&self, text: &str) -> Embedding {
        let hash = text.len() as f32;
        let vector = vec![hash % 1.0; self.config.vector_store.embedding_size];
        Embedding::new(vector)
    }
}