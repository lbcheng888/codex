use std::sync::Arc;
use serde_json::Value;
use serena_core::tools::{Tool, ToolContext};
use tokio::sync::Mutex;
use tracing::instrument;
use anyhow::Result;

use crate::manager::LanguageServerManager;
use crate::types::LanguageServerConfig;

/// MCP tool for getting symbols overview
pub struct GetSymbolsOverview {
    manager: Arc<Mutex<LanguageServerManager>>,
}

impl GetSymbolsOverview {
    pub async fn new(config: Arc<serena_core::RuntimeConfig>) -> Result<Self> {
        let manager = LanguageServerManager::new(config.clone()).await?;
        
        // Add default configurations for common language servers
        manager.add_config("rust".to_string(), LanguageServerConfig {
            language: "rust".to_string(),
            command: vec!["rust-analyzer".to_string()],
            working_dir: None,
            env: None,
            init_options: None,
        }).await;
        
        manager.add_config("python".to_string(), LanguageServerConfig {
            language: "python".to_string(),
            command: vec!["pyright-langserver".to_string(), "--stdio".to_string()],
            working_dir: None,
            env: None,
            init_options: None,
        }).await;
        
        Ok(Self { manager: Arc::new(Mutex::new(manager)) })
    }
}

#[async_trait::async_trait]
impl Tool for GetSymbolsOverview {
    fn name(&self) -> &'static str {
        "get_symbols_overview"
    }
    
    fn schema(&self) -> Value {
        serde_json::json!({
            "type": "object",
            "required": ["relative_path"],
            "properties": {
                "relative_path": {
                    "type": "string",
                    "description": "The relative path to the file or directory to get the overview of"
                },
                "max_answer_chars": {
                    "type": "integer",
                    "default": 200000,
                    "description": "Maximum characters in response"
                }
            }
        })
    }
    
    #[instrument(skip_all, fields(relative_path = tracing::field::Empty))]
    async fn run(&self, input: Value, _ctx: ToolContext) -> serena_core::error::Result<Value> {
        let relative_path = input
            .get("relative_path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing relative_path"))?;
        
        tracing::Span::current().record("relative_path", relative_path);
        
        match self.manager.lock().await.get_symbols_overview(relative_path).await {
            Ok(overview) => {
                let result = serde_json::to_value(overview)?;
                Ok(result)
            }
            Err(e) => {
                tracing::error!(error = %e, "Failed to get symbols overview");
                Err(serena_core::error::CoreError::validation(format!("LSP error: {}", e)))
            }
        }
    }
}

/// MCP tool for finding symbols
pub struct FindSymbol {
    manager: Arc<Mutex<LanguageServerManager>>,
}

impl FindSymbol {
    pub async fn new(config: Arc<serena_core::RuntimeConfig>) -> Result<Self> {
        let manager = LanguageServerManager::new(config.clone()).await?;
        
        // Add default configurations (same as above)
        manager.add_config("rust".to_string(), LanguageServerConfig {
            language: "rust".to_string(),
            command: vec!["rust-analyzer".to_string()],
            working_dir: None,
            env: None,
            init_options: None,
        }).await;
        
        manager.add_config("python".to_string(), LanguageServerConfig {
            language: "python".to_string(),
            command: vec!["pyright-langserver".to_string(), "--stdio".to_string()],
            working_dir: None,
            env: None,
            init_options: None,
        }).await;
        
        Ok(Self { manager: Arc::new(Mutex::new(manager)) })
    }
}

#[async_trait::async_trait]
impl Tool for FindSymbol {
    fn name(&self) -> &'static str {
        "find_symbol"
    }
    
    fn schema(&self) -> Value {
        serde_json::json!({
            "type": "object",
            "required": ["name_path"],
            "properties": {
                "name_path": {
                    "type": "string",
                    "description": "The name path pattern to search for"
                },
                "relative_path": {
                    "type": "string",
                    "description": "Optional relative path to restrict search to a specific file"
                },
                "depth": {
                    "type": "integer",
                    "default": 0,
                    "description": "Depth to retrieve descendants (e.g., 1 for class methods/attributes)"
                },
                "include_body": {
                    "type": "boolean",
                    "default": false,
                    "description": "If True, include the symbol's source code"
                }
            }
        })
    }
    
    #[instrument(skip_all, fields(name_path = tracing::field::Empty, relative_path = tracing::field::Empty))]
    async fn run(&self, input: Value, _ctx: ToolContext) -> serena_core::error::Result<Value> {
        let name_path = input
            .get("name_path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing name_path"))?;
        
        let relative_path = input.get("relative_path").and_then(|v| v.as_str());
        let depth = input.get("depth").and_then(|v| v.as_u64()).unwrap_or(0) as u32;
        let include_body = input.get("include_body").and_then(|v| v.as_bool()).unwrap_or(false);
        
        tracing::Span::current().record("name_path", name_path);
        if let Some(path) = relative_path {
            tracing::Span::current().record("relative_path", path);
        }
        
        match self.manager.lock().await.find_symbol(name_path, relative_path, depth, include_body).await {
            Ok(symbols) => {
                let result = serde_json::to_value(symbols)?;
                Ok(result)
            }
            Err(e) => {
                tracing::error!(error = %e, "Failed to find symbol");
                Err(serena_core::error::CoreError::validation(format!("LSP error: {}", e)))
            }
        }
    }
}

/// MCP tool for finding referencing symbols
pub struct FindReferencingSymbols {
    manager: Arc<Mutex<LanguageServerManager>>,
}

impl FindReferencingSymbols {
    pub async fn new(config: Arc<serena_core::RuntimeConfig>) -> Result<Self> {
        let manager = LanguageServerManager::new(config.clone()).await?;
        
        // Add default configurations (same as above)
        manager.add_config("rust".to_string(), LanguageServerConfig {
            language: "rust".to_string(),
            command: vec!["rust-analyzer".to_string()],
            working_dir: None,
            env: None,
            init_options: None,
        }).await;
        
        manager.add_config("python".to_string(), LanguageServerConfig {
            language: "python".to_string(),
            command: vec!["pyright-langserver".to_string(), "--stdio".to_string()],
            working_dir: None,
            env: None,
            init_options: None,
        }).await;
        
        Ok(Self { manager: Arc::new(Mutex::new(manager)) })
    }
}

#[async_trait::async_trait]
impl Tool for FindReferencingSymbols {
    fn name(&self) -> &'static str {
        "find_referencing_symbols"
    }
    
    fn schema(&self) -> Value {
        serde_json::json!({
            "type": "object",
            "required": ["name_path", "relative_path"],
            "properties": {
                "name_path": {
                    "type": "string",
                    "description": "The name path of the symbol to find references for"
                },
                "relative_path": {
                    "type": "string",
                    "description": "The relative path to the file containing the symbol"
                }
            }
        })
    }
    
    #[instrument(skip_all, fields(name_path = tracing::field::Empty, relative_path = tracing::field::Empty))]
    async fn run(&self, input: Value, _ctx: ToolContext) -> serena_core::error::Result<Value> {
        let name_path = input
            .get("name_path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing name_path"))?;

        let relative_path = input
            .get("relative_path")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("missing relative_path"))?;
        
        tracing::Span::current().record("name_path", name_path);
        tracing::Span::current().record("relative_path", relative_path);
        
        match self.manager.lock().await.find_referencing_symbols(name_path, relative_path).await {
            Ok(references) => {
                let result = serde_json::to_value(references)?;
                Ok(result)
            }
            Err(e) => {
                tracing::error!(error = %e, "Failed to find referencing symbols");
                Err(serena_core::error::CoreError::validation(format!("LSP error: {}", e)))
            }
        }
    }
}