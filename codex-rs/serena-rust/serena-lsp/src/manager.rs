use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use serde_json::{json, Value};
use tokio::sync::RwLock;
use tracing::{debug, info, error, instrument};
use anyhow::Result;

use crate::transport::LspTransport;
use crate::vfs::{DocUri, DocumentChangeListener};
use crate::types::{LanguageServerConfig, LspError, LspResult, UnifiedSymbolInformation, ReferenceInSymbol, SymbolKind, Position, Range, Location};

/// Language server manager handles multiple language servers and their lifecycle
pub struct LanguageServerManager {
    symbol_index: crate::index::SymbolIndex,
    servers: RwLock<HashMap<String, LspTransport>>,
    configs: RwLock<HashMap<String, LanguageServerConfig>>,
    workspace: crate::workspace::WorkspaceContext,
    cfg: Arc<serena_core::RuntimeConfig>,
    // 并发控制：全局与按语言
    lsp_total_sem: Arc<tokio::sync::Semaphore>,
    lsp_lang_sem: RwLock<HashMap<String, Arc<tokio::sync::Semaphore>>>,
}

// Forward declarations for compatibility - will be updated once serena-agent is complete
pub struct Project {
    pub name: String,
    pub root_path: PathBuf,
    pub config: ProjectConfig,
}

pub struct ProjectConfig {
    pub language_servers: HashMap<String, ProjectLanguageServerConfig>,
}

pub struct ProjectLanguageServerConfig {
    pub enabled: bool,
    pub binary_path: Option<PathBuf>,
    pub args: Vec<String>,
    pub initialization_options: serde_json::Value,
}

impl LanguageServerManager {
    /// Resolve backend override path by environment variables.
    /// Precedence: language-specific (e.g. LSP_BACKEND_OVERRIDE_RUST for "rust") > global LSP_BACKEND_OVERRIDE.
    fn resolve_backend_override(&self, language: &str) -> Option<PathBuf> {
        let lang = language.to_ascii_lowercase();
        // language-specific first
        let per_lang = match lang.as_str() {
            "rust" => std::env::var_os("LSP_BACKEND_OVERRIDE_RUST"),
            _ => None,
        };
        if let Some(p) = per_lang {
            let pb = PathBuf::from(p);
            info!("Using language-specific LSP backend override for '{}': {}", language, pb.display());
            return Some(pb);
        }
        // then global
        if let Some(global) = std::env::var_os("LSP_BACKEND_OVERRIDE") {
            let pb = PathBuf::from(global);
            info!("Using global LSP backend override for '{}': {}", language, pb.display());
            return Some(pb);
        }
        None
    }
    /// Create a new language server manager
    pub async fn new(config: Arc<serena_core::RuntimeConfig>) -> Result<Self> {
        let total = config.lsp_total_concurrency.max(1);
        Ok(Self {
            servers: RwLock::new(HashMap::new()),
            configs: RwLock::new(HashMap::new()),
            workspace: crate::workspace::WorkspaceContext::new_single(config.workspace_root.clone()),
            symbol_index: crate::index::SymbolIndex::new("default"),
            cfg: config.clone(),
            lsp_total_sem: Arc::new(tokio::sync::Semaphore::new(total)),
            lsp_lang_sem: RwLock::new(HashMap::new()),
        })
    }
    
    /// Add a language server configuration
    pub async fn add_config(&self, language: String, config: LanguageServerConfig) {
        self.configs.write().await.insert(language, config);
    }

    /// Initialize for a project
    pub async fn initialize_for_project(&self, project: &Project) -> Result<()> {
        info!("Initializing language servers for project: {}", project.name);
        
        // Set workspace root to project root
        // Note: For simplicity, we'll keep the original workspace_root for now
        // In a full implementation, we might want to create a new manager instance
        
        // Configure language servers from project config
        for (language, lang_config) in &project.config.language_servers {
            if lang_config.enabled {
                let command_path = lang_config.binary_path.clone()
                    .unwrap_or_else(|| self.get_default_command(language));
                
                let lsp_config = LanguageServerConfig {
                    language: language.clone(),
                    command: vec![command_path.to_string_lossy().to_string()],
                    working_dir: Some(project.root_path.to_string_lossy().to_string()),
                    env: Some(HashMap::new()),
                    init_options: Some(lang_config.initialization_options.clone()),
                };
                
                self.add_config(language.clone(), lsp_config).await;
            }
        }
        
        Ok(())
    }

    /// Get default language server command for a language
    fn get_default_command(&self, language: &str) -> PathBuf {
        match language {
            "rust" => PathBuf::from("rust-analyzer"),
            "python" => PathBuf::from("pyright-langserver"),
            "typescript" => PathBuf::from("typescript-language-server"),
            "javascript" => PathBuf::from("typescript-language-server"),
            "go" => PathBuf::from("gopls"),
            "java" => PathBuf::from("jdtls"),
            "csharp" => PathBuf::from("csharp-ls"),
            "ruby" => PathBuf::from("solargraph"),
            "php" => PathBuf::from("intelephense"),
            "cpp" | "c" => PathBuf::from("clangd"),
            "dart" => PathBuf::from("dart"),
            "elixir" => PathBuf::from("elixir-ls"),
            "clojure" => PathBuf::from("clojure-lsp"),
            "terraform" => PathBuf::from("terraform-ls"),
            "kotlin" => PathBuf::from("kotlin-language-server"),
            _ => PathBuf::from(format!("{}-language-server", language)),
        }
    }

    /// 冷启动预热：为一组候选文件拉取符号并写入缓存（避免重复构建）
    pub async fn prewarm(&mut self, candidates: &HashMap<usize, Vec<String>>) -> Result<()> {
        // 逐个文件手动实现（避免复杂生命周期）
        for (root_idx, files) in candidates {
            for rel in files {
                if self.symbol_index.get(*root_idx, rel).is_some() { continue; }
                // 检测语言并确保 server 启动
                if let Ok(language) = self.detect_language(rel) {
                    let _ = self.ensure_server_started(&language).await;
                    // 构造请求
                    let uri = self.workspace.to_uri(*root_idx, rel);
                    let params = json!({ "textDocument": { "uri": uri } });
                    let mut servers = self.servers.write().await;
                    if let Some(transport) = servers.get_mut(&language) {
                        if let Ok(resp) = transport.send_request("textDocument/documentSymbol".to_string(), Some(params)).await {
                            if let Ok(symbols) = self.parse_document_symbols(resp, rel) {
                                self.symbol_index.on_open(*root_idx, rel, &symbols);
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// 启动文件监听，返回 (sender, watcher)。调用方可将 sender 克隆到消费端，自行处理 FsChange。
    pub fn start_file_watcher(&self) -> notify::Result<(std::sync::mpsc::Sender<crate::fs_watcher::FsChange>, crate::fs_watcher::FileWatcher)> {
        use std::sync::mpsc::channel;
        let (tx, _rx) = channel();
        let watcher = crate::fs_watcher::FileWatcher::start(self.workspace.roots().to_vec(), tx.clone())?;
        // 调用方负责在独立线程消费 rx，根据需要调用 self.invalidate_on_change(...)。此处仅返回 tx 与 watcher 供外部组装。
        Ok((tx, watcher))
    }

    /// 根据 URI 失效索引（供 VFS/DocStore 回调使用）
    pub fn invalidate_by_uri(&self, uri: &DocUri) {
        if let Some((root_idx, rel)) = self.workspace.from_uri(&uri.0) {
            self.symbol_index.invalidate(root_idx, &rel);
        }
    }

    /// 根据文件变更粗粒度失效索引（由监听回调驱动）
    pub fn invalidate_on_change(&self, change: &crate::fs_watcher::FsChange) {
        if let Some((root_idx, rel)) = self.workspace.to_relative_from_abs(&change.path) {
            // 简单按扩展名过滤支持的语言
            let ext_ok = std::path::Path::new(&rel).extension().and_then(|e| e.to_str()).map(|e| matches!(e, "rs"|"py"|"ts"|"tsx"|"js"|"jsx"|"go"|"java")).unwrap_or(false);
            if ext_ok { self.symbol_index.invalidate(root_idx, &rel); }
        }
    }

    /// Shutdown all language servers
    pub async fn shutdown_all(&self) -> Result<()> {
        let languages: Vec<String> = self.servers.read().await.keys().cloned().collect();
        
        for language in languages {
            if let Err(e) = self.stop_server(&language).await {
                error!("Failed to stop language server for {}: {}", language, e);
            }
        }
        
        Ok(())
    }

    /// Restart all language servers
    pub async fn restart_all(&self) -> Result<()> {
        info!("Restarting all language servers");
        
        // Get list of currently running servers
        let languages: Vec<String> = self.servers.read().await.keys().cloned().collect();
        
        // Stop all servers
        for language in &languages {
            if let Err(e) = self.stop_server(language).await {
                error!("Failed to stop language server for {}: {}", language, e);
            }
        }
        
        // Start them again
        for language in languages {
            if let Err(e) = self.start_server(&language).await {
                error!("Failed to restart language server for {}: {}", language, e);
            }
        }
        
        Ok(())
    }

    /// Restart a specific language server
    pub async fn restart_language(&self, language: &str) -> Result<()> {
        info!("Restarting language server for: {}", language);
        // Try to stop; ignore if not running
        if let Err(e) = self.stop_server(language).await {
            // Only log if it's not the typical "not found" error; otherwise continue
            tracing::warn!("Stopping server for '{}' returned error (continuing to start anyway): {}", language, e);
        }
        // Start (will error if no config exists)
        if let Err(e) = self.start_server(language).await {
            error!("Failed to start language server for {}: {}", language, e);
            return Err(anyhow::anyhow!(e));
        }
        Ok(())
    }

    /// Get status of language servers
    pub async fn get_status(&self) -> Vec<String> {
        self.servers.read().await.keys().cloned().collect()
    }
    
    /// Start a language server for the given language
    #[instrument(skip(self), fields(language = %language))]
    pub async fn start_server(&self, language: &str) -> LspResult<()> {
        let configs = self.configs.read().await;
        let config = configs.get(language)
            .ok_or_else(|| LspError::ServerNotFound(language.to_string()))?;
        
        {
            let servers = self.servers.read().await;
            if servers.contains_key(language) {
                return Ok(()); // Already started
            }
        }
        
        let mut config_clone = config.clone();
        drop(configs); // Release the read lock

        // Apply backend override if present
        if let Some(override_path) = self.resolve_backend_override(language) {
            if config_clone.command.is_empty() {
                config_clone.command = vec![override_path.to_string_lossy().to_string()];
            } else {
                // Replace the executable path (first element) with the override, preserve args
                let mut new_cmd = Vec::with_capacity(config_clone.command.len());
                new_cmd.push(override_path.to_string_lossy().to_string());
                if config_clone.command.len() > 1 {
                    new_cmd.extend_from_slice(&config_clone.command[1..]);
                }
                config_clone.command = new_cmd;
            }
        }

        let start_at = std::time::Instant::now();
        let mut transport = LspTransport::start(
            config_clone.command.clone(),
            config_clone.working_dir.clone(),
            config_clone.env.clone(),
        ).await?;
        
        // Initialize the language server
        if let Err(e) = self.initialize_server(&mut transport, &config_clone).await {
            let dur = start_at.elapsed().as_millis() as u64;
            serena_core::observability::metric_event("lsp.start_server", dur, Some(e.code() as i64), 0, None);
            return Err(e);
        }
        
        let mut servers = self.servers.write().await;
        
        servers.insert(language.to_string(), transport);
        let dur = start_at.elapsed().as_millis() as u64;
        serena_core::observability::metric_event("lsp.start_server", dur, None, 0, None);
        info!("Started language server for: {}", language);
        
        Ok(())
    }
    
    /// Stop a language server
    #[instrument(skip(self), fields(language = %language))]
    pub async fn stop_server(&self, language: &str) -> LspResult<()> {
        let start_at = std::time::Instant::now();
        let mut servers = self.servers.write().await;
        if let Some(mut transport) = servers.remove(language) {
            let res = transport.shutdown().await;
            let dur = start_at.elapsed().as_millis() as u64;
            match res {
                Ok(_) => {
                    serena_core::observability::metric_event("lsp.stop_server", dur, None, 0, None);
                    info!("Stopped language server for: {}", language);
                }
                Err(e) => {
                    // 这里没有 LspError，记录通用错误码
                    serena_core::observability::metric_event("lsp.stop_server", dur, Some(-32000), 0, None);
                    error!("Failed to stop language server for {}: {}", language, e);
return Err(LspError::IoError(std::io::Error::new(std::io::ErrorKind::Other, e.to_string())));
                }
            }
        }
        Ok(())
    }
    
    /// Get symbols overview for a file or directory
    pub async fn get_symbols_overview(&mut self, relative_path: &str) -> LspResult<HashMap<String, Vec<UnifiedSymbolInformation>>> {
        let language = self.detect_language(relative_path)?;
        self.ensure_server_started(&language).await?;
        
        let mut servers = self.servers.write().await;
        let transport = servers.get_mut(&language)
            .ok_or_else(|| LspError::ServerNotFound(language.clone()))?;
        
        // For simplicity, implement document symbols for single file
        // In a full implementation, this would handle directories by iterating files
        let uri = self.workspace.to_uri(self.workspace.sole_root_index(), relative_path);
        
        let params = json!({
            "textDocument": {
                "uri": uri
            }
        });
        
        let response = self.send_request(&language, "textDocument/documentSymbol", Some(params)).await?;
        let symbols = self.parse_document_symbols(response, relative_path)?;
        // 索引写入/更新
        self.symbol_index.on_change(self.workspace.sole_root_index(), relative_path, &symbols);
        
        let mut result = HashMap::new();
        result.insert(relative_path.to_string(), symbols);
        
        Ok(result)
    }
    
    /// Find symbols matching a name path pattern
    pub async fn find_symbol(
        &mut self,
        name_path: &str,
        relative_path: Option<&str>,
        depth: u32,
        include_body: bool,
    ) -> LspResult<Vec<UnifiedSymbolInformation>> {
        // For PoC, implement workspace symbol search
        let params = json!({
            "query": name_path
        });
        
        // Try all available languages if no specific path is given
        if let Some(path) = relative_path {
            let language = self.detect_language(path)?;
            self.ensure_server_started(&language).await?;
            
            let mut servers = self.servers.write().await;
            let transport = servers.get_mut(&language)
                .ok_or_else(|| LspError::ServerNotFound(language.clone()))?;
            
            let response = self.send_request(&language, "workspace/symbol", Some(params)).await?;
            return self.parse_workspace_symbols(response, depth, include_body).await;
        }
        
        // Try all configured languages
        let mut all_symbols = Vec::new();
        let config_languages: Vec<String> = self.configs.read().await.keys().cloned().collect();
        for language in config_languages {
            if let Ok(_) = self.ensure_server_started(&language).await {
                let mut servers = self.servers.write().await;
                if let Some(transport) = servers.get_mut(&language) {
                    if let Ok(response) = self.send_request(&language, "workspace/symbol", Some(params.clone())).await {
                        if let Ok(mut symbols) = self.parse_workspace_symbols(response, depth, include_body).await {
                            all_symbols.append(&mut symbols);
                        }
                    }
                }
            }
        }
        
        Ok(all_symbols)
    }
    
    /// Find symbols that reference the given symbol
    pub async fn find_referencing_symbols(
        &mut self,
        name_path: &str,
        relative_path: &str,
    ) -> LspResult<Vec<ReferenceInSymbol>> {
        let language = self.detect_language(relative_path)?;
        self.ensure_server_started(&language).await?;
        
        // First find the symbol to get its location
        let symbols = self.find_symbol(name_path, Some(relative_path), 0, false).await?;
        let symbol = symbols.into_iter().next()
            .ok_or_else(|| LspError::SymbolNotFound(name_path.to_string()))?;
        
        let uri = self.workspace.to_uri(self.workspace.sole_root_index(), relative_path);
        let params = json!({
            "textDocument": {
                "uri": uri
            },
            "position": {
                "line": symbol.location.range.start.line,
                "character": symbol.location.range.start.character
            },
            "context": {
                "includeDeclaration": false
            }
        });
        
        let mut servers = self.servers.write().await;
        let transport = servers.get_mut(&language)
            .ok_or_else(|| LspError::ServerNotFound(language.clone()))?;
        
        let response = self.send_request(&language, "textDocument/references", Some(params)).await?;
        self.parse_references(response).await
    }
    
    /// Initialize a language server with proper handshake
    #[instrument(skip(self, transport, config), fields(language = %config.language))]
    async fn initialize_server(&self, transport: &mut LspTransport, config: &LanguageServerConfig) -> LspResult<()> {
        let init_params = json!({
            "processId": null,
            "rootUri": format!("file://{}", self.workspace.roots()[self.workspace.sole_root_index()].display()),
            "capabilities": {
                "textDocument": {
                    "documentSymbol": {},
                    "references": {},
                    "definition": {}
                },
                "workspace": {
                    "symbol": {}
                }
            },
            "initializationOptions": config.init_options
        });
        
        let start_at = std::time::Instant::now();
        let response = transport.send_request("initialize".to_string(), Some(init_params)).await?;
        debug!("Initialize response: {:?}", response);
        
        // Send initialized notification
        transport.send_notification("initialized".to_string(), Some(json!({}))).await?;
        let dur = start_at.elapsed().as_millis() as u64;
        serena_core::observability::metric_event("lsp.initialize", dur, None, 0, None);
        
        Ok(())
    }
    
    /// Ensure a language server is started
    pub(crate) async fn ensure_server_started(&self, language: &str) -> LspResult<()> {
        {
            let servers = self.servers.read().await;
            if servers.contains_key(language) {
                return Ok(());
            }
        }
        
        self.start_server(language).await
    }
    
    /// Detect language from file extension (simple heuristic)
    pub(crate) fn detect_language(&self, relative_path: &str) -> LspResult<String> {
        let extension = std::path::Path::new(relative_path)
            .extension()
            .and_then(|ext| ext.to_str())
            .unwrap_or("");
        
        let language = match extension {
            "rs" => "rust",
            "py" => "python",
            "ts" | "tsx" => "typescript",
            "js" | "jsx" => "javascript",
            "go" => "go",
            "java" => "java",
            _ => return Err(LspError::ServerNotFound(format!("No language server for extension: {}", extension))),
        };
        
        Ok(language.to_string())
    }
    
    /// Parse document symbols response
    fn parse_document_symbols(&self, response: Value, relative_path: &str) -> LspResult<Vec<UnifiedSymbolInformation>> {
        let symbols = response.as_array()
            .ok_or_else(|| LspError::InvalidRequest("Invalid symbols response".to_string()))?;
        
        let mut result = Vec::new();
        for symbol in symbols {
            if let Some(unified) = self.convert_to_unified_symbol(symbol, relative_path) {
                result.push(unified);
            }
        }
        
        Ok(result)
    }
    
    /// Parse workspace symbols response
    async fn parse_workspace_symbols(&self, response: Value, _depth: u32, _include_body: bool) -> LspResult<Vec<UnifiedSymbolInformation>> {
        let symbols = response.as_array()
            .ok_or_else(|| LspError::InvalidRequest("Invalid symbols response".to_string()))?;
        
        let mut result = Vec::new();
        for symbol in symbols {
            if let Some(unified) = self.convert_workspace_symbol_to_unified(symbol) {
                result.push(unified);
            }
        }
        
        Ok(result)
    }
    
    /// Parse references response
    async fn parse_references(&self, response: Value) -> LspResult<Vec<ReferenceInSymbol>> {
        let locations = response.as_array()
            .ok_or_else(|| LspError::InvalidRequest("Invalid references response".to_string()))?;
        
        let mut result = Vec::new();
        for location in locations {
            if let Some(reference) = self.convert_location_to_reference(location) {
                result.push(reference);
            }
        }
        
        Ok(result)
    }
    
    /// Convert LSP symbol to unified symbol information
    fn convert_to_unified_symbol(&self, symbol: &Value, relative_path: &str) -> Option<UnifiedSymbolInformation> {
        let name = symbol.get("name")?.as_str()?.to_string();
        let kind_num = symbol.get("kind")?.as_u64()? as u8;
        let kind = match kind_num {
            1 => SymbolKind::File,
            2 => SymbolKind::Module,
            5 => SymbolKind::Class,
            6 => SymbolKind::Method,
            12 => SymbolKind::Function,
            13 => SymbolKind::Variable,
            _ => SymbolKind::Variable, // Default fallback
        };
        
        let range = symbol.get("range")?;
        let start = range.get("start")?;
        let end = range.get("end")?;
        
        let location = Location {
            relative_path: relative_path.to_string(),
            range: Range {
                start: Position {
                    line: start.get("line")?.as_u64()? as u32,
                    character: start.get("character")?.as_u64()? as u32,
                },
                end: Position {
                    line: end.get("line")?.as_u64()? as u32,
                    character: end.get("character")?.as_u64()? as u32,
                },
            },
        };
        
        Some(UnifiedSymbolInformation {
            name: name.clone(),
            name_path: name, // Simplified for PoC
            kind,
            location,
            body_location: None,
            body: None,
            children: Vec::new(),
        })
    }
    
    /// Convert workspace symbol to unified symbol
    fn convert_workspace_symbol_to_unified(&self, symbol: &Value) -> Option<UnifiedSymbolInformation> {
        let name = symbol.get("name")?.as_str()?.to_string();
        let kind_num = symbol.get("kind")?.as_u64()? as u8;
        let kind = match kind_num {
            5 => SymbolKind::Class,
            6 => SymbolKind::Method,
            12 => SymbolKind::Function,
            _ => SymbolKind::Variable,
        };
        
        let location_obj = symbol.get("location")?;
        let uri = location_obj.get("uri")?.as_str()?;
        let (_, relative_path) = self.workspace.from_uri(uri)?;
        
        let range = location_obj.get("range")?;
        let start = range.get("start")?;
        let end = range.get("end")?;
        
        let location = Location {
            relative_path: relative_path.to_string(),
            range: Range {
                start: Position {
                    line: start.get("line")?.as_u64()? as u32,
                    character: start.get("character")?.as_u64()? as u32,
                },
                end: Position {
                    line: end.get("line")?.as_u64()? as u32,
                    character: end.get("character")?.as_u64()? as u32,
                },
            },
        };
        
        Some(UnifiedSymbolInformation {
            name: name.clone(),
            name_path: name,
            kind,
            location,
            body_location: None,
            body: None,
            children: Vec::new(),
        })
    }
    
    /// Convert location to reference
    fn convert_location_to_reference(&self, location: &Value) -> Option<ReferenceInSymbol> {
        // Simplified for PoC - would need actual symbol resolution
        let uri = location.get("uri")?.as_str()?;
        let (_, relative_path) = self.workspace.from_uri(uri)?;
        
        let range = location.get("range")?;
        let start = range.get("start")?;
        
        let symbol = UnifiedSymbolInformation {
            name: "reference".to_string(),
            name_path: "reference".to_string(),
            kind: SymbolKind::Variable,
            location: Location {
                relative_path: relative_path.to_string(),
                range: Range {
                    start: Position {
                        line: start.get("line")?.as_u64()? as u32,
                        character: start.get("character")?.as_u64()? as u32,
                    },
                    end: Position {
                        line: start.get("line")?.as_u64()? as u32,
                        character: start.get("character")?.as_u64()? as u32,
                    },
                },
            },
            body_location: None,
            body: None,
            children: Vec::new(),
        };
        
        Some(ReferenceInSymbol {
            symbol,
            line: start.get("line")?.as_u64()? as u32,
            character: start.get("character")?.as_u64()? as u32,
            snippet: None,
        })
    }

    /// 通用请求发送：对指定语言的已启动服务器发送请求
    #[instrument(skip(self, params), fields(language = %language, method = %method))]
    pub async fn send_request(&self, language: &str, method: &str, params: Option<Value>) -> LspResult<Value> {
        let start_at = std::time::Instant::now();
        if let Err(e) = self.ensure_server_started(language).await {
            let dur = start_at.elapsed().as_millis() as u64;
            serena_core::observability::metric_event("lsp.request", dur, Some(e.code() as i64), 0, Some(&serde_json::json!({"method": method})));
            return Err(e);
        }

        // 并发与背压：全局 + 按语言信号量
        let total_permit = match tokio::time::timeout(
            std::time::Duration::from_millis(self.cfg.lsp_request_timeout_ms),
            self.lsp_total_sem.clone().acquire_owned()
        ).await {
            Ok(Ok(p)) => p,
            _ => {
                let dur = start_at.elapsed().as_millis() as u64;
                serena_core::observability::metric_event("lsp.request", dur, Some(-32001), 0, Some(&serde_json::json!({"method": method, "reason": "semaphore_timeout_total"})));
                return Err(LspError::Timeout("LSP total concurrency timeout".into()));
            }
        };

        // 获取/初始化语言级信号量
        let lang_sem_permit = {
            // fast path read
            if let Some(sem) = self.lsp_lang_sem.read().await.get(language) {
                let sem_arc = Arc::clone(sem);
                match tokio::time::timeout(
                    std::time::Duration::from_millis(self.cfg.lsp_request_timeout_ms),
                    sem_arc.acquire_owned()
                ).await {
                    Ok(Ok(p)) => Some(p),
                    _ => None,
                }
            } else {
                let sem_arc = {
                    let mut map = self.lsp_lang_sem.write().await;
                    let entry = map.entry(language.to_string()).or_insert_with(|| Arc::new(tokio::sync::Semaphore::new(self.cfg.lsp_per_language_concurrency.max(1))));
                    Arc::clone(entry)
                };
                match tokio::time::timeout(
                    std::time::Duration::from_millis(self.cfg.lsp_request_timeout_ms),
                    sem_arc.acquire_owned()
                ).await {
                    Ok(Ok(p)) => Some(p),
                    _ => None,
                }
            }
        };
        if lang_sem_permit.is_none() {
            let dur = start_at.elapsed().as_millis() as u64;
            serena_core::observability::metric_event("lsp.request", dur, Some(-32002), 0, Some(&serde_json::json!({"method": method, "reason": "semaphore_timeout_language"})));
            return Err(LspError::Timeout("LSP per-language concurrency timeout".into()));
        }

        let mut servers = self.servers.write().await;
        let transport = servers.get_mut(language)
            .ok_or_else(|| LspError::ServerNotFound(language.to_string()))?;

        // 执行请求加入超时，避免 LSP 卡顿
        let fut = transport.send_request(method.to_string(), params);
        let res = tokio::time::timeout(std::time::Duration::from_millis(self.cfg.lsp_request_timeout_ms), fut).await;
        let dur = start_at.elapsed().as_millis() as u64;
        match res {
            Ok(Ok(v)) => {
                serena_core::observability::metric_event("lsp.request", dur, None, 0, Some(&serde_json::json!({"method": method})));
                Ok(v)
            }
            Ok(Err(e)) => {
                serena_core::observability::metric_event("lsp.request", dur, Some(-32000), 0, Some(&serde_json::json!({"method": method})));
                Err(e)
            }
            Err(_) => {
                serena_core::observability::metric_event("lsp.request", dur, Some(-32003), 0, Some(&serde_json::json!({"method": method, "reason": "request_timeout"})));
                Err(LspError::Timeout("LSP request timed out".into()))
            }
        }
    }
}

impl DocumentChangeListener for LanguageServerManager {
    fn on_open(&self, uri: &DocUri) { self.invalidate_by_uri(uri); }
    fn on_change(&self, uri: &DocUri) { self.invalidate_by_uri(uri); }
}
