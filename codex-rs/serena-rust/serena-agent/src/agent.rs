use anyhow::{Context, Result};
use serena_core::{RuntimeConfig, ToolRegistry};
use serena_lsp::LspManager;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::info;

use crate::memory::MemoryManager;
use crate::project::{Project, ProjectManager};

/// The main Serena agent that orchestrates all functionality
pub struct SerenaAgent {
    /// Runtime configuration
    config: Arc<RuntimeConfig>,
    /// Project manager for handling multiple projects
    project_manager: ProjectManager,
    /// Currently active project
    active_project: RwLock<Option<Project>>,
    /// Memory manager for the current project
    memory_manager: RwLock<Option<MemoryManager>>,
    /// Language server manager
    lsp_manager: Arc<LspManager>,
    /// Tool registry
    tool_registry: Arc<ToolRegistry>,
    /// Agent context and modes
    context: AgentContext,
    modes: Vec<AgentMode>,
}

/// Defines the context in which the agent operates
#[derive(Debug, Clone)]
pub enum AgentContext {
    /// Desktop application context
    DesktopApp,
    /// CLI agent context
    Agent,
    /// IDE assistant context
    IdeAssistant,
    /// MCP server context
    McpServer,
}

/// Defines operational modes for the agent
#[derive(Debug, Clone)]
pub enum AgentMode {
    /// Planning mode - creates plans but doesn't execute
    Planning,
    /// Editing mode - makes direct code changes
    Editing,
    /// Interactive mode - requires user confirmation
    Interactive,
    /// One-shot mode - executes single tasks
    OneShot,
    /// JetBrains IDE integration mode
    JetBrains,
}

impl Default for AgentContext {
    fn default() -> Self {
        Self::Agent
    }
}

impl SerenaAgent {
    /// Create a new SerenaAgent
    pub async fn new(
        config: RuntimeConfig,
        context: Option<AgentContext>,
        modes: Option<Vec<AgentMode>>,
    ) -> Result<Self> {
        let config = Arc::new(config);
        let project_manager = ProjectManager::new()?;
        let lsp_manager = Arc::new(LspManager::new(config.clone()).await?);
        let tool_registry = Arc::new(ToolRegistry::new());
        
        let context = context.unwrap_or_default();
        let modes = modes.unwrap_or_else(|| vec![AgentMode::Interactive, AgentMode::Editing]);

        info!("SerenaAgent initialized with context: {:?}, modes: {:?}", context, modes);

        Ok(Self {
            config,
            project_manager,
            active_project: RwLock::new(None),
            memory_manager: RwLock::new(None),
            lsp_manager,
            tool_registry,
            context,
            modes,
        })
    }

    /// Activate a project by name or path
    pub async fn activate_project(&self, project_identifier: &str) -> Result<()> {
        info!("Activating project: {}", project_identifier);

        let project = if PathBuf::from(project_identifier).exists() {
            // It's a path - register and activate
            let path = PathBuf::from(project_identifier).canonicalize()
                .with_context(|| format!("Invalid project path: {}", project_identifier))?;
            
            let name = path.file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("unnamed_project")
                .to_string();
            
            // Register the project if not already registered
            if let Err(_) = self.project_manager.get_project(&name) {
                self.project_manager.register_project(name.clone(), path)?;
            }
            
            self.project_manager.activate_project(name)?
        } else {
            // It's a project name
            self.project_manager.activate_project(project_identifier.to_string())?
        };

        // Initialize memory manager for this project
        let memory_manager = MemoryManager::new(&project.root_path)?;
        *self.memory_manager.write().await = Some(memory_manager);

        // Initialize language servers for this project
        // Convert to the LSP manager's Project type temporarily
        let lsp_project = serena_lsp::manager::Project {
            name: project.name.clone(),
            root_path: project.root_path.clone(),
            config: serena_lsp::manager::ProjectConfig {
                language_servers: project.config.language_servers.iter()
                    .map(|(k, v)| (k.clone(), serena_lsp::manager::ProjectLanguageServerConfig {
                        enabled: v.enabled,
                        binary_path: v.binary_path.clone(),
                        args: v.args.clone(),
                        initialization_options: v.initialization_options.clone(),
                    }))
                    .collect(),
            },
        };
        self.lsp_manager.initialize_for_project(&lsp_project).await?;

        // Store the active project
        *self.active_project.write().await = Some(project.clone());

        info!("Successfully activated project: {} at {}", project.name, project.root_path.display());
        Ok(())
    }

    /// Deactivate the current project
    pub async fn deactivate_project(&self) -> Result<()> {
        if let Some(project) = self.active_project.read().await.as_ref() {
            info!("Deactivating project: {}", project.name);
            
            // Shutdown language servers
            self.lsp_manager.shutdown_all().await?;
            
            // Clear state
            *self.active_project.write().await = None;
            *self.memory_manager.write().await = None;
            
            self.project_manager.deactivate_project();
            
            info!("Project deactivated successfully");
        }
        
        Ok(())
    }

    /// Get the currently active project
    pub async fn get_active_project(&self) -> Option<Project> {
        self.active_project.read().await.clone()
    }

    /// Get the memory manager for the current project
    pub async fn get_memory_manager(&self) -> Option<MemoryManager> {
        self.memory_manager.read().await.clone()
    }

    /// Get the LSP manager
    pub fn get_lsp_manager(&self) -> Arc<LspManager> {
        self.lsp_manager.clone()
    }

    /// Get the tool registry
    pub fn get_tool_registry(&self) -> Arc<ToolRegistry> {
        self.tool_registry.clone()
    }

    /// Get the project manager
    pub fn get_project_manager(&self) -> &ProjectManager {
        &self.project_manager
    }

    /// List all available projects
    pub fn list_projects(&self) -> Result<Vec<String>> {
        self.project_manager.list_projects()
    }

    /// Register a new project
    pub fn register_project(&self, name: String, path: PathBuf) -> Result<()> {
        self.project_manager.register_project(name, path)
    }

    /// Unregister a project
    pub fn unregister_project(&self, name: &str) -> Result<()> {
        self.project_manager.unregister_project(name)
    }

    /// Auto-discover projects in a directory
    pub fn discover_projects(&self, search_dir: &std::path::Path) -> Result<Vec<String>> {
        self.project_manager.discover_projects(search_dir)
    }

    /// Get the runtime configuration
    pub fn get_config(&self) -> Arc<RuntimeConfig> {
        self.config.clone()
    }

    /// Get the agent context
    pub fn get_context(&self) -> &AgentContext {
        &self.context
    }

    /// Get the agent modes
    pub fn get_modes(&self) -> &[AgentMode] {
        &self.modes
    }

    /// Check if a specific mode is active
    pub fn has_mode(&self, mode: &AgentMode) -> bool {
        self.modes.iter().any(|m| std::mem::discriminant(m) == std::mem::discriminant(mode))
    }

    /// Switch to new modes
    pub fn switch_modes(&mut self, modes: Vec<AgentMode>) {
        info!("Switching agent modes: {:?} -> {:?}", self.modes, modes);
        self.modes = modes;
    }

    /// Get workspace root (from active project or config)
    pub async fn get_workspace_root(&self) -> PathBuf {
        if let Some(project) = self.active_project.read().await.as_ref() {
            project.root_path.clone()
        } else {
            self.config.workspace_root.clone()
        }
    }

    /// Check if onboarding has been performed for the current project
    pub async fn is_onboarded(&self) -> bool {
        if let Some(project) = self.active_project.read().await.as_ref() {
            project.is_onboarded()
        } else {
            false
        }
    }

    /// Perform onboarding for the current project
    pub async fn perform_onboarding(&self) -> Result<String> {
        let project = self.active_project.read().await
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("No active project"))?
            .clone();

        // Create memories directory
        let memories_dir = project.memories_dir();
        tokio::fs::create_dir_all(&memories_dir).await
            .with_context(|| format!("Failed to create memories directory: {}", memories_dir.display()))?;

        // Create initial onboarding memory
        if let Some(memory_manager) = self.memory_manager.read().await.as_ref() {
            let onboarding_content = format!(
                "# Project Onboarding: {}\n\n\
                Project root: {}\n\
                Onboarded at: {}\n\n\
                ## Initial Analysis\n\
                This project has been onboarded to Serena. \
                Use the various tools to explore the codebase and create additional memories as needed.\n",
                project.name,
                project.root_path.display(),
                time::OffsetDateTime::now_utc()
            );
            
            memory_manager.save_memory("onboarding", &onboarding_content)?;
        }

        info!("Onboarding completed for project: {}", project.name);
        Ok("Onboarding completed successfully. You can now use Serena tools to explore and work with this project.".to_string())
    }

    /// Save the current project configuration
    pub async fn save_project_config(&self) -> Result<()> {
        if let Some(project) = self.active_project.read().await.as_ref() {
            project.save()?;
            info!("Saved configuration for project: {}", project.name);
        }
        Ok(())
    }

    /// Restart language servers for the current project
    pub async fn restart_language_servers(&self) -> Result<()> {
        if let Some(project) = self.active_project.read().await.as_ref() {
            info!("Restarting language servers for project: {}", project.name);
            self.lsp_manager.restart_all().await?;
            info!("Language servers restarted successfully");
        }
        Ok(())
    }

    /// Get status information about the agent
    pub async fn get_status(&self) -> AgentStatus {
        let active_project = self.active_project.read().await.clone();
        let has_memory_manager = self.memory_manager.read().await.is_some();
        let lsp_status = self.lsp_manager.get_status().await;

        AgentStatus {
            active_project: active_project.map(|p| p.name),
            workspace_root: self.get_workspace_root().await,
            memory_manager_active: has_memory_manager,
            language_servers: lsp_status,
            context: self.context.clone(),
            modes: self.modes.clone(),
        }
    }
}

/// Status information about the agent
#[derive(Debug)]
pub struct AgentStatus {
    pub active_project: Option<String>,
    pub workspace_root: PathBuf,
    pub memory_manager_active: bool,
    pub language_servers: Vec<String>, // List of active language servers
    pub context: AgentContext,
    pub modes: Vec<AgentMode>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[tokio::test]
    async fn test_agent_creation() -> Result<()> {
        let config = RuntimeConfig::default();
        let agent = SerenaAgent::new(config, None, None).await?;
        
        let status = agent.get_status().await;
        assert!(matches!(status.context, AgentContext::Agent));
        assert!(!status.modes.is_empty());
        
        Ok(())
    }

    #[tokio::test]
    async fn test_project_activation() -> Result<()> {
        let temp_dir = tempdir()?;
        let project_path = temp_dir.path().to_path_buf();
        
        // Create a project indicator
        tokio::fs::write(project_path.join("Cargo.toml"), "[package]\nname = \"test\"").await?;
        
        let config = RuntimeConfig::default();
        let agent = SerenaAgent::new(config, None, None).await?;
        
        // Activate project by path
        agent.activate_project(project_path.to_str().unwrap()).await?;
        
        let active_project = agent.get_active_project().await;
        assert!(active_project.is_some());
        
        let memory_manager = agent.get_memory_manager().await;
        assert!(memory_manager.is_some());
        
        Ok(())
    }
}