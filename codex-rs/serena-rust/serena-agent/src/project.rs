use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use tracing::{info, warn};

/// Error type for project-related operations
#[derive(Debug, thiserror::Error)]
pub enum ProjectNotFoundError {
    #[error("Project '{0}' not found")]
    NotFound(String),
    #[error("Project configuration is invalid: {0}")]
    InvalidConfig(String),
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

/// Represents a Serena project with its configuration and metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Project {
    pub name: String,
    pub root_path: PathBuf,
    pub config: ProjectConfig,
}

/// Project-specific configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectConfig {
    /// Language servers to enable for this project
    pub language_servers: HashMap<String, LanguageServerConfig>,
    /// Project-specific tool configurations
    pub tools: ToolConfig,
    /// Memory settings
    pub memory: MemoryConfig,
    /// Project metadata
    pub metadata: ProjectMetadata,
}

/// Language server configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LanguageServerConfig {
    pub enabled: bool,
    pub binary_path: Option<PathBuf>,
    pub args: Vec<String>,
    pub initialization_options: serde_json::Value,
}

/// Tool configuration for the project
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolConfig {
    /// Maximum file size to process (in bytes)
    pub max_file_size: u64,
    /// File patterns to ignore
    pub ignore_patterns: Vec<String>,
    /// Include hidden files in searches
    pub include_hidden: bool,
}

/// Memory configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryConfig {
    /// Maximum number of memory files
    pub max_memories: usize,
    /// Maximum size of each memory file (in bytes)
    pub max_memory_size: u64,
}

/// Project metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectMetadata {
    pub name: String,
    pub description: Option<String>,
    pub version: Option<String>,
    pub created_at: time::OffsetDateTime,
    pub last_accessed: time::OffsetDateTime,
}

impl Default for ProjectConfig {
    fn default() -> Self {
        Self {
            language_servers: HashMap::new(),
            tools: ToolConfig::default(),
            memory: MemoryConfig::default(),
            metadata: ProjectMetadata::default(),
        }
    }
}

impl Default for ToolConfig {
    fn default() -> Self {
        Self {
            max_file_size: 10 * 1024 * 1024, // 10MB
            ignore_patterns: vec![
                ".git".to_string(),
                "node_modules".to_string(),
                "target".to_string(),
                ".venv".to_string(),
                "__pycache__".to_string(),
            ],
            include_hidden: false,
        }
    }
}

impl Default for MemoryConfig {
    fn default() -> Self {
        Self {
            max_memories: 100,
            max_memory_size: 1024 * 1024, // 1MB
        }
    }
}

impl Default for ProjectMetadata {
    fn default() -> Self {
        let now = time::OffsetDateTime::now_utc();
        Self {
            name: "unnamed_project".to_string(),
            description: None,
            version: None,
            created_at: now,
            last_accessed: now,
        }
    }
}

impl Project {
    /// Create a new project with the given name and root path
    pub fn new(name: String, root_path: PathBuf) -> Self {
        let mut config = ProjectConfig::default();
        config.metadata.name = name.clone();

        Self {
            name,
            root_path,
            config,
        }
    }

    /// Load a project from its configuration file
    pub fn load_from_path(root_path: PathBuf) -> Result<Self> {
        let serena_dir = root_path.join(".serena");
        let config_path = serena_dir.join("project.yml");
        
        if !config_path.exists() {
            // Create default project if config doesn't exist
            let name = root_path
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("unnamed_project")
                .to_string();
            
            let project = Self::new(name, root_path);
            project.save()?;
            return Ok(project);
        }

        let config_content = fs::read_to_string(&config_path)
            .with_context(|| format!("Failed to read project config: {}", config_path.display()))?;
        
        let mut config: ProjectConfig = serde_yaml::from_str(&config_content)
            .with_context(|| "Failed to parse project configuration")?;
        
        // Update last accessed time
        config.metadata.last_accessed = time::OffsetDateTime::now_utc();
        
        Ok(Self {
            name: config.metadata.name.clone(),
            root_path,
            config,
        })
    }

    /// Save the project configuration
    pub fn save(&self) -> Result<()> {
        let serena_dir = self.root_path.join(".serena");
        fs::create_dir_all(&serena_dir)
            .with_context(|| format!("Failed to create .serena directory: {}", serena_dir.display()))?;
        
        let config_path = serena_dir.join("project.yml");
        let config_content = serde_yaml::to_string(&self.config)
            .with_context(|| "Failed to serialize project configuration")?;
        
        fs::write(&config_path, config_content)
            .with_context(|| format!("Failed to write project config: {}", config_path.display()))?;
        
        info!("Saved project '{}' configuration to {}", self.name, config_path.display());
        Ok(())
    }

    /// Get the .serena directory for this project
    pub fn serena_dir(&self) -> PathBuf {
        self.root_path.join(".serena")
    }

    /// Get the memories directory for this project
    pub fn memories_dir(&self) -> PathBuf {
        self.serena_dir().join("memories")
    }

    /// Check if this project has onboarding completed
    pub fn is_onboarded(&self) -> bool {
        self.memories_dir().exists() && 
        self.memories_dir().read_dir().map_or(false, |mut entries| entries.any(|_| true))
    }

    /// Enable a language server for this project
    pub fn enable_language_server(&mut self, language: &str, config: LanguageServerConfig) {
        self.language_servers_mut().insert(language.to_string(), config);
    }

    /// Get language server configuration
    pub fn language_servers(&self) -> &HashMap<String, LanguageServerConfig> {
        &self.config.language_servers
    }

    /// Get mutable language server configuration
    pub fn language_servers_mut(&mut self) -> &mut HashMap<String, LanguageServerConfig> {
        &mut self.config.language_servers
    }
}

/// Manages multiple projects and their configurations
#[derive(Debug)]
pub struct ProjectManager {
    /// User-level Serena configuration directory
    config_dir: PathBuf,
    /// Cache of loaded projects
    projects: parking_lot::RwLock<HashMap<String, Project>>,
    /// Currently active project
    active_project: parking_lot::RwLock<Option<String>>,
}

impl ProjectManager {
    /// Create a new ProjectManager
    pub fn new() -> Result<Self> {
        let config_dir = Self::get_user_config_dir()?;
        fs::create_dir_all(&config_dir)
            .with_context(|| format!("Failed to create config directory: {}", config_dir.display()))?;
        
        Ok(Self {
            config_dir,
            projects: parking_lot::RwLock::new(HashMap::new()),
            active_project: parking_lot::RwLock::new(None),
        })
    }

    /// Get the user-level Serena configuration directory
    fn get_user_config_dir() -> Result<PathBuf> {
        let home_dir = std::env::var("HOME")
            .or_else(|_| std::env::var("USERPROFILE"))
            .context("Could not determine home directory")?;
        
        Ok(PathBuf::from(home_dir).join(".serena"))
    }

    /// Get the projects registry file path
    fn projects_registry_path(&self) -> PathBuf {
        self.config_dir.join("projects.yml")
    }

    /// Load projects registry from disk
    fn load_projects_registry(&self) -> Result<HashMap<String, PathBuf>> {
        let registry_path = self.projects_registry_path();
        
        if !registry_path.exists() {
            return Ok(HashMap::new());
        }

        let content = fs::read_to_string(&registry_path)
            .with_context(|| format!("Failed to read projects registry: {}", registry_path.display()))?;
        
        let registry: HashMap<String, PathBuf> = serde_yaml::from_str(&content)
            .with_context(|| "Failed to parse projects registry")?;
        
        Ok(registry)
    }

    /// Save projects registry to disk
    fn save_projects_registry(&self, registry: &HashMap<String, PathBuf>) -> Result<()> {
        let registry_path = self.projects_registry_path();
        let content = serde_yaml::to_string(registry)
            .with_context(|| "Failed to serialize projects registry")?;
        
        fs::write(&registry_path, content)
            .with_context(|| format!("Failed to write projects registry: {}", registry_path.display()))?;
        
        Ok(())
    }

    /// Register a new project
    pub fn register_project(&self, name: String, root_path: PathBuf) -> Result<()> {
        let mut registry = self.load_projects_registry()?;
        registry.insert(name.clone(), root_path.clone());
        self.save_projects_registry(&registry)?;
        
        // Load the project into cache
        let project = Project::load_from_path(root_path)?;
        self.projects.write().insert(name.clone(), project);
        
        info!("Registered project '{}'", name);
        Ok(())
    }

    /// Unregister a project
    pub fn unregister_project(&self, name: &str) -> Result<()> {
        let mut registry = self.load_projects_registry()?;
        
        if registry.remove(name).is_none() {
            return Err(ProjectNotFoundError::NotFound(name.to_string()).into());
        }
        
        self.save_projects_registry(&registry)?;
        self.projects.write().remove(name);
        
        info!("Unregistered project '{}'", name);
        Ok(())
    }

    /// Get a project by name
    pub fn get_project(&self, name: &str) -> Result<Project> {
        // Check cache first
        if let Some(project) = self.projects.read().get(name) {
            return Ok(project.clone());
        }

        // Load from registry
        let registry = self.load_projects_registry()?;
        let root_path = registry.get(name)
            .ok_or_else(|| ProjectNotFoundError::NotFound(name.to_string()))?;
        
        let project = Project::load_from_path(root_path.clone())?;
        self.projects.write().insert(name.to_string(), project.clone());
        
        Ok(project)
    }

    /// List all registered projects
    pub fn list_projects(&self) -> Result<Vec<String>> {
        let registry = self.load_projects_registry()?;
        let mut names: Vec<String> = registry.keys().cloned().collect();
        names.sort();
        Ok(names)
    }

    /// Activate a project
    pub fn activate_project(&self, name: String) -> Result<Project> {
        let project = self.get_project(&name)?;
        *self.active_project.write() = Some(name.clone());
        
        info!("Activated project '{}'", name);
        Ok(project)
    }

    /// Get the currently active project
    pub fn get_active_project(&self) -> Option<String> {
        self.active_project.read().clone()
    }

    /// Deactivate the current project
    pub fn deactivate_project(&self) {
        *self.active_project.write() = None;
        info!("Deactivated current project");
    }

    /// Auto-discover and register projects in a directory
    pub fn discover_projects(&self, search_dir: &Path) -> Result<Vec<String>> {
        let mut discovered = Vec::new();
        
        for entry in walkdir::WalkDir::new(search_dir)
            .min_depth(1)
            .max_depth(3)
            .into_iter()
            .filter_map(|e| e.ok())
        {
            let path = entry.path();
            
            // Look for common project indicators
            if path.is_dir() && Self::looks_like_project_root(path) {
                if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                    match self.register_project(name.to_string(), path.to_path_buf()) {
                        Ok(()) => {
                            discovered.push(name.to_string());
                            info!("Auto-discovered project: {}", name);
                        }
                        Err(e) => {
                            warn!("Failed to register discovered project '{}': {}", name, e);
                        }
                    }
                }
            }
        }
        
        Ok(discovered)
    }

    /// Check if a directory looks like a project root
    fn looks_like_project_root(path: &Path) -> bool {
        let indicators = [
            ".git",
            "Cargo.toml",
            "package.json", 
            "pyproject.toml",
            "requirements.txt",
            "go.mod",
            "pom.xml",
            "build.gradle",
            ".serena",
        ];
        
        indicators.iter().any(|&indicator| path.join(indicator).exists())
    }
}

impl Default for ProjectManager {
    fn default() -> Self {
        Self::new().expect("Failed to create ProjectManager")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_project_creation_and_saving() -> Result<()> {
        let temp_dir = tempdir()?;
        let project_path = temp_dir.path().to_path_buf();
        
        let project = Project::new("test_project".to_string(), project_path.clone());
        project.save()?;
        
        // Verify config file was created
        let config_path = project_path.join(".serena").join("project.yml");
        assert!(config_path.exists());
        
        // Load project back
        let loaded_project = Project::load_from_path(project_path)?;
        assert_eq!(loaded_project.name, "test_project");
        
        Ok(())
    }

    #[test]
    fn test_project_manager() -> Result<()> {
        let temp_dir = tempdir()?;
        let project_path = temp_dir.path().join("test_project");
        fs::create_dir_all(&project_path)?;
        
        // Create a project indicator
        fs::write(project_path.join("Cargo.toml"), "[package]\nname = \"test\"")?;
        
        let manager = ProjectManager::new()?;
        
        // Register project
        manager.register_project("test_project".to_string(), project_path.clone())?;
        
        // List projects
        let projects = manager.list_projects()?;
        assert!(projects.contains(&"test_project".to_string()));
        
        // Activate project
        let project = manager.activate_project("test_project".to_string())?;
        assert_eq!(project.name, "test_project");
        assert_eq!(manager.get_active_project(), Some("test_project".to_string()));
        
        Ok(())
    }
}