use serde_json::{json, Value};
use serena_core::tools::{Tool, ToolContext};
use serena_core::error::Result;
use serena_lsp::{LanguageServerManager, LanguageServerConfig};
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::{info, instrument};
use tracing::warn;

use std::sync::OnceLock;

/// Shared LSP manager instance to avoid creating multiple managers
static LSP_MANAGER: OnceLock<Arc<Mutex<LanguageServerManager>>> = OnceLock::new();

async fn get_lsp_manager(config: Arc<serena_core::RuntimeConfig>) -> Result<Arc<Mutex<LanguageServerManager>>> {
    if let Some(manager) = LSP_MANAGER.get() {
        return Ok(manager.clone());
    }
    
    let manager = LanguageServerManager::new(config.clone()).await
        .map_err(|e| serena_core::error::CoreError::validation(format!("Failed to create LSP manager: {}", e)))?;
    
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

    // Optional common defaults for convenience (do not fail if not present at runtime)
    manager.add_config("typescript".to_string(), LanguageServerConfig {
        language: "typescript".to_string(),
        command: vec!["typescript-language-server".to_string(), "--stdio".to_string()],
        working_dir: None,
        env: None,
        init_options: None,
    }).await;
    
    let manager_arc = Arc::new(Mutex::new(manager));
    
    // Try to set it, but use whatever is there if someone else set it first
    match LSP_MANAGER.set(manager_arc.clone()) {
        Ok(()) => Ok(manager_arc),
        Err(_) => Ok(LSP_MANAGER.get().unwrap().clone()), // Someone else set it first
    }
}

//// ========================= LSP RESTART TOOLS =========================

/// Tool: restart all language servers
pub struct RestartLanguageServerAll;

#[async_trait::async_trait]
impl Tool for RestartLanguageServerAll {
    fn name(&self) -> &'static str {
        "restart_language_server_all"
    }

    fn description(&self) -> &'static str {
        "Restart all running language servers managed by Serena LSP"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "description": "Restart all running language servers managed by Serena LSP"
        })
    }

    #[instrument(skip_all)]
    async fn run(&self, _args: Value, ctx: ToolContext) -> Result<Value> {
        let manager = get_lsp_manager(ctx.cfg).await?;
        // restart_all() returns anyhow::Result<()>, map to core::Result
        let res = {
            let manager_guard = manager.lock().await;
            manager_guard.restart_all().await
        };

        match res {
            Ok(()) => Ok(json!({ "status": "ok" })),
            Err(e) => {
                warn!("Failed to restart all language servers: {}", e);
                Ok(json!({ "status": "error", "error": e.to_string() }))
            }
        }
    }
}

/// Tool: restart a specific language server by language key
pub struct RestartLanguageServerLanguage;

#[async_trait::async_trait]
impl Tool for RestartLanguageServerLanguage {
    fn name(&self) -> &'static str {
        "restart_language_server_language"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "required": ["language"],
            "properties": {
                "language": {
                    "type": "string",
                    "description": "Language identifier (e.g., rust, python, typescript)"
                }
            },
            "description": "Restart a specific language server"
        })
    }

    #[instrument(skip_all, fields(language = tracing::field::Empty))]
    async fn run(&self, args: Value, ctx: ToolContext) -> Result<Value> {
        let language = args.get("language")
            .and_then(|v| v.as_str())
            .ok_or_else(|| serena_core::error::CoreError::validation("language is required".to_string()))?;

        tracing::Span::current().record("language", language);
        info!("Restarting language server via tool for '{}'", language);

        let manager = get_lsp_manager(ctx.cfg).await?;
        let res = {
            let manager_guard = manager.lock().await;
            manager_guard.restart_language(language).await
        };

        match res {
            Ok(()) => Ok(json!({ "status": "ok", "language": language })),
            Err(e) => {
                warn!("Failed to restart language server '{}': {}", language, e);
                Ok(json!({ "status": "error", "language": language, "error": e.to_string() }))
            }
        }
    }
}

/// Tool for getting symbols overview
pub struct GetSymbolsOverview;

#[async_trait::async_trait]
impl Tool for GetSymbolsOverview {
    fn name(&self) -> &'static str {
        "get_symbols_overview"
    }

    fn description(&self) -> &'static str {
        "Get an overview of top-level symbols defined in a file or directory"
    }

    fn schema(&self) -> Value {
        json!({
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
    async fn run(&self, args: Value, ctx: ToolContext) -> Result<Value> {
        let relative_path = args["relative_path"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("relative_path is required".to_string()))?;

        tracing::Span::current().record("relative_path", relative_path);
        info!("Getting symbols overview for: {}", relative_path);

        let manager = get_lsp_manager(ctx.cfg).await?;
        
        let result = {
            let mut manager_guard = manager.lock().await;
            manager_guard.get_symbols_overview(relative_path).await
        };
        
        match result {
            Ok(overview) => {
                let result = serde_json::to_value(overview)
                    .map_err(|e| serena_core::error::CoreError::validation(format!("JSON serialization error: {}", e)))?;
                Ok(result)
            }
            Err(e) => {
                tracing::error!(error = %e, "Failed to get symbols overview");
                // Return empty overview instead of failing
                Ok(json!({}))
            }
        }
    }
}

/// Tool for finding symbols
pub struct FindSymbol;

#[async_trait::async_trait]
impl Tool for FindSymbol {
    fn name(&self) -> &'static str {
        "find_symbol"
    }

    fn description(&self) -> &'static str {
        "Find symbols (classes, functions, variables) by name pattern with optional filtering"
    }

    fn schema(&self) -> Value {
        json!({
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
    async fn run(&self, args: Value, ctx: ToolContext) -> Result<Value> {
        let name_path = args["name_path"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("name_path is required".to_string()))?;

        let relative_path = args.get("relative_path").and_then(|v| v.as_str());
        let depth = args.get("depth").and_then(|v| v.as_u64()).unwrap_or(0) as u32;
        let include_body = args.get("include_body").and_then(|v| v.as_bool()).unwrap_or(false);

        tracing::Span::current().record("name_path", name_path);
        if let Some(path) = relative_path {
            tracing::Span::current().record("relative_path", path);
        }

        info!("Finding symbol: {} in {:?}", name_path, relative_path);

        let manager = get_lsp_manager(ctx.cfg).await?;
        
        let result = {
            let mut manager_guard = manager.lock().await;
            manager_guard.find_symbol(name_path, relative_path, depth, include_body).await
        };
        
        match result {
            Ok(symbols) => {
                let result = serde_json::to_value(symbols)
                    .map_err(|e| serena_core::error::CoreError::validation(format!("JSON serialization error: {}", e)))?;
                Ok(result)
            }
            Err(e) => {
                tracing::error!(error = %e, "Failed to find symbol");
                // Return empty array instead of failing
                Ok(json!([]))
            }
        }
    }
}

/// Tool for finding referencing symbols
pub struct FindReferencingSymbols;

#[async_trait::async_trait]
impl Tool for FindReferencingSymbols {
    fn name(&self) -> &'static str {
        "find_referencing_symbols"
    }

    fn description(&self) -> &'static str {
        "Find all symbols that reference a given symbol at a specific location"
    }

    fn schema(&self) -> Value {
        json!({
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
    async fn run(&self, args: Value, ctx: ToolContext) -> Result<Value> {
        let name_path = args["name_path"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("name_path is required".to_string()))?;

        let relative_path = args["relative_path"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("relative_path is required".to_string()))?;

        tracing::Span::current().record("name_path", name_path);
        tracing::Span::current().record("relative_path", relative_path);

        info!("Finding references to symbol: {} in {}", name_path, relative_path);

        let manager = get_lsp_manager(ctx.cfg).await?;
        
        let result = {
            let mut manager_guard = manager.lock().await;
            manager_guard.find_referencing_symbols(name_path, relative_path).await
        };
        
        match result {
            Ok(references) => {
                let result = serde_json::to_value(references)
                    .map_err(|e| serena_core::error::CoreError::validation(format!("JSON serialization error: {}", e)))?;
                Ok(result)
            }
            Err(e) => {
                tracing::error!(error = %e, "Failed to find referencing symbols");
                // Return empty array instead of failing
                Ok(json!([]))
            }
        }
    }
}

/// Tool for replacing symbol body
pub struct ReplaceSymbolBody;

#[async_trait::async_trait]
impl Tool for ReplaceSymbolBody {
    fn name(&self) -> &'static str {
        "replace_symbol_body"
    }

    fn description(&self) -> &'static str {
        "Replace the complete body/definition of a symbol (function, class, method, etc.)"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "required": ["name_path", "relative_path", "body"],
            "properties": {
                "name_path": {
                    "type": "string",
                    "description": "The name path of the symbol to replace"
                },
                "relative_path": {
                    "type": "string",
                    "description": "The relative path to the file containing the symbol"
                },
                "body": {
                    "type": "string",
                    "description": "The new symbol body"
                }
            }
        })
    }

    async fn run(&self, args: Value, _ctx: ToolContext) -> Result<Value> {
        let name_path = args["name_path"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("name_path is required".to_string()))?;
        
        let relative_path = args["relative_path"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("relative_path is required".to_string()))?;
        
        let body = args["body"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("body is required".to_string()))?;

        info!("Replacing symbol '{}' in '{}'", name_path, relative_path);
        
        // For now, return a placeholder response indicating the feature is not yet implemented
        // In the full implementation, this would:
        // 1. Find the symbol using LSP
        // 2. Get its exact location and range
        // 3. Replace the text at that range with the new body
        // 4. Handle language server text synchronization
        
        Ok(json!({
            "message": format!("Symbol replacement not yet fully implemented. Would replace symbol '{}' in '{}' with {} characters of new body", 
                name_path, relative_path, body.len()),
            "implemented": false,
            "name_path": name_path,
            "relative_path": relative_path,
            "body_length": body.len()
        }))
    }
}

/// Tool for inserting content after a symbol
pub struct InsertAfterSymbol;

#[async_trait::async_trait]
impl Tool for InsertAfterSymbol {
    fn name(&self) -> &'static str {
        "insert_after_symbol"
    }

    fn description(&self) -> &'static str {
        "Insert new content after the end of a symbol definition"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "required": ["name_path", "relative_path", "body"],
            "properties": {
                "name_path": {
                    "type": "string",
                    "description": "The name path of the symbol after which to insert content"
                },
                "relative_path": {
                    "type": "string",
                    "description": "The relative path to the file containing the symbol"
                },
                "body": {
                    "type": "string",
                    "description": "The content to insert after the symbol"
                }
            }
        })
    }

    async fn run(&self, args: Value, _ctx: ToolContext) -> Result<Value> {
        let name_path = args["name_path"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("name_path is required".to_string()))?;
        
        let relative_path = args["relative_path"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("relative_path is required".to_string()))?;
        
        let body = args["body"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("body is required".to_string()))?;

        info!("Inserting after symbol '{}' in '{}'", name_path, relative_path);
        
        Ok(json!({
            "message": format!("Symbol insertion not yet fully implemented. Would insert {} characters after symbol '{}' in '{}'", 
                body.len(), name_path, relative_path),
            "implemented": false,
            "name_path": name_path,
            "relative_path": relative_path,
            "body_length": body.len()
        }))
    }
}

/// Tool for inserting content before a symbol
pub struct InsertBeforeSymbol;

#[async_trait::async_trait]
impl Tool for InsertBeforeSymbol {
    fn name(&self) -> &'static str {
        "insert_before_symbol"
    }

    fn description(&self) -> &'static str {
        "Insert new content before the beginning of a symbol definition"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "required": ["name_path", "relative_path", "body"],
            "properties": {
                "name_path": {
                    "type": "string",
                    "description": "The name path of the symbol before which to insert content"
                },
                "relative_path": {
                    "type": "string",
                    "description": "The relative path to the file containing the symbol"
                },
                "body": {
                    "type": "string",
                    "description": "The content to insert before the symbol"
                }
            }
        })
    }

    async fn run(&self, args: Value, _ctx: ToolContext) -> Result<Value> {
        let name_path = args["name_path"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("name_path is required".to_string()))?;
        
        let relative_path = args["relative_path"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("relative_path is required".to_string()))?;
        
        let body = args["body"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("body is required".to_string()))?;

        info!("Inserting before symbol '{}' in '{}'", name_path, relative_path);
        
        Ok(json!({
            "message": format!("Symbol insertion not yet fully implemented. Would insert {} characters before symbol '{}' in '{}'", 
                body.len(), name_path, relative_path),
            "implemented": false,
            "name_path": name_path,
            "relative_path": relative_path,
            "body_length": body.len()
        }))
    }
}