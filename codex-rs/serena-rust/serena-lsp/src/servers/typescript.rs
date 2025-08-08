use std::path::PathBuf;
use std::process::Stdio;
use tokio::process::Command;
use tracing::{debug, info, warn};

use crate::transport::LspTransport;
use crate::types::{LanguageServerConfig, LspError, LspResult};

/// TypeScript/JavaScript language server implementation
pub struct TypeScriptLanguageServer {
    config: LanguageServerConfig,
    transport: Option<LspTransport>,
    workspace_root: PathBuf,
}

impl TypeScriptLanguageServer {
    pub fn new(config: LanguageServerConfig, workspace_root: PathBuf) -> Self {
        Self {
            config,
            transport: None,
            workspace_root,
        }
    }
    
    /// Start the TypeScript language server
    pub async fn start(&mut self) -> LspResult<()> {
        info!("Starting TypeScript language server for {}", self.config.language);
        
        // Check if TypeScript language server is available
        if !self.is_server_available().await {
            return Err(LspError::InitializationFailed(
                "TypeScript language server not found. Please install typescript-language-server.".to_string()
            ));
        }
        
        let command = self.build_command();
        let transport = LspTransport::start(
            command,
            self.config.working_dir.clone(),
            self.config.env.clone(),
        ).await?;
        
        self.transport = Some(transport);
        
        // Initialize the server
        self.initialize().await?;
        
        info!("TypeScript language server started successfully");
        Ok(())
    }
    
    /// Check if the TypeScript language server is available
    async fn is_server_available(&self) -> bool {
        let result = Command::new(&self.config.command[0])
            .arg("--version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .await;
        
        match result {
            Ok(status) => status.success(),
            Err(_) => false,
        }
    }
    
    /// Build the command to start the language server
    fn build_command(&self) -> Vec<String> {
        let mut cmd = self.config.command.clone();
        
        // Add standard arguments for TypeScript language server
        cmd.push("--stdio".to_string());
        
        // Add TypeScript-specific options
        if self.config.language == "typescript" {
            cmd.push("--tsserver-log-verbosity".to_string());
            cmd.push("off".to_string());
        }
        
        cmd
    }
    
    /// Initialize the language server
    async fn initialize(&mut self) -> LspResult<()> {
        let init_params = self.create_initialize_params();

        let transport = self.transport.as_mut()
            .ok_or_else(|| LspError::InitializationFailed("Transport not available".to_string()))?;

        debug!("Sending initialize request to TypeScript language server");
        let response = transport.send_request("initialize".to_string(), Some(init_params)).await?;
        
        debug!("Received initialize response: {:?}", response);
        
        // Send initialized notification
        transport.send_notification("initialized".to_string(), Some(serde_json::json!({}))).await?;
        
        Ok(())
    }
    
    /// Create initialization parameters for TypeScript
    fn create_initialize_params(&self) -> serde_json::Value {
        let capabilities = serde_json::json!({
            "textDocument": {
                "synchronization": {
                    "dynamicRegistration": false,
                    "willSave": false,
                    "willSaveWaitUntil": false,
                    "didSave": false
                },
                "completion": {
                    "dynamicRegistration": false,
                    "completionItem": {
                        "snippetSupport": false,
                        "commitCharactersSupport": false,
                        "documentationFormat": ["plaintext"],
                        "deprecatedSupport": false,
                        "preselectSupport": false
                    },
                    "contextSupport": false
                },
                "hover": {
                    "dynamicRegistration": false,
                    "contentFormat": ["plaintext"]
                },
                "signatureHelp": {
                    "dynamicRegistration": false,
                    "signatureInformation": {
                        "documentationFormat": ["plaintext"]
                    }
                },
                "definition": {
                    "dynamicRegistration": false
                },
                "references": {
                    "dynamicRegistration": false
                },
                "documentHighlight": {
                    "dynamicRegistration": false
                },
                "documentSymbol": {
                    "dynamicRegistration": false,
                    "symbolKind": {
                        "valueSet": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]
                    }
                },
                "codeAction": {
                    "dynamicRegistration": false
                },
                "codeLens": {
                    "dynamicRegistration": false
                },
                "formatting": {
                    "dynamicRegistration": false
                },
                "rangeFormatting": {
                    "dynamicRegistration": false
                },
                "onTypeFormatting": {
                    "dynamicRegistration": false
                },
                "rename": {
                    "dynamicRegistration": false
                }
            },
            "workspace": {
                "applyEdit": false,
                "workspaceEdit": {
                    "documentChanges": false
                },
                "didChangeConfiguration": {
                    "dynamicRegistration": false
                },
                "didChangeWatchedFiles": {
                    "dynamicRegistration": false
                },
                "symbol": {
                    "dynamicRegistration": false,
                    "symbolKind": {
                        "valueSet": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]
                    }
                },
                "executeCommand": {
                    "dynamicRegistration": false
                }
            }
        });
        
        let mut init_options = serde_json::json!({});
        if let Some(options) = &self.config.init_options {
            init_options = options.clone();
        }
        
        serde_json::json!({
            "processId": std::process::id(),
            "rootPath": self.workspace_root.to_string_lossy(),
            "rootUri": format!("file://{}", self.workspace_root.to_string_lossy()),
            "capabilities": capabilities,
            "initializationOptions": init_options,
            "workspaceFolders": [{
                "uri": format!("file://{}", self.workspace_root.to_string_lossy()),
                "name": self.workspace_root.file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("workspace")
            }]
        })
    }
    
    /// Stop the language server
    pub async fn stop(&mut self) -> LspResult<()> {
        if let Some(mut transport) = self.transport.take() {
            info!("Stopping TypeScript language server");
            
            // Send shutdown request
            if let Err(e) = transport.send_request("shutdown".to_string(), None).await {
                warn!("Failed to send shutdown request: {}", e);
            }
            
            // Send exit notification
            if let Err(e) = transport.send_notification("exit".to_string(), None).await {
                warn!("Failed to send exit notification: {}", e);
            }
            
            transport.stop().await?;
        }
        
        Ok(())
    }
    
    /// Get document symbols
    pub async fn get_document_symbols(&mut self, file_path: &str) -> LspResult<serde_json::Value> {
        let transport = self.transport.as_mut()
            .ok_or_else(|| LspError::ServerNotRunning("TypeScript language server not started".to_string()))?;
        
        let params = serde_json::json!({
            "textDocument": {
                "uri": format!("file://{}", file_path)
            }
        });
        
        transport.send_request("textDocument/documentSymbol".to_string(), Some(params)).await
    }
    
    /// Find definition
    pub async fn find_definition(&mut self, file_path: &str, line: u32, character: u32) -> LspResult<serde_json::Value> {
        let transport = self.transport.as_mut()
            .ok_or_else(|| LspError::ServerNotRunning("TypeScript language server not started".to_string()))?;
        
        let params = serde_json::json!({
            "textDocument": {
                "uri": format!("file://{}", file_path)
            },
            "position": {
                "line": line,
                "character": character
            }
        });
        
        transport.send_request("textDocument/definition".to_string(), Some(params)).await
    }
    
    /// Find references
    pub async fn find_references(&mut self, file_path: &str, line: u32, character: u32) -> LspResult<serde_json::Value> {
        let transport = self.transport.as_mut()
            .ok_or_else(|| LspError::ServerNotRunning("TypeScript language server not started".to_string()))?;
        
        let params = serde_json::json!({
            "textDocument": {
                "uri": format!("file://{}", file_path)
            },
            "position": {
                "line": line,
                "character": character
            },
            "context": {
                "includeDeclaration": true
            }
        });
        
        transport.send_request("textDocument/references".to_string(), Some(params)).await
    }
    
    /// Check if the server is running
    pub fn is_running(&self) -> bool {
        self.transport.is_some()
    }
}

impl Drop for TypeScriptLanguageServer {
    fn drop(&mut self) {
        if self.is_running() {
            warn!("TypeScript language server dropped without proper shutdown");
        }
    }
}
