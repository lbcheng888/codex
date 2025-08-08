use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use anyhow::Result;
use tracing::info;

/// Supported programming languages
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "lowercase")]
pub enum Language {
    Python,
    Typescript,
    Javascript,
    Java,
    Csharp,
    Rust,
    Go,
    Ruby,
    Cpp,
    Php,
    Elixir,
    Clojure,
    Terraform,
}

impl Language {
    pub fn as_str(&self) -> &'static str {
        match self {
            Language::Python => "python",
            Language::Typescript => "typescript",
            Language::Javascript => "javascript",
            Language::Java => "java",
            Language::Csharp => "csharp",
            Language::Rust => "rust",
            Language::Go => "go",
            Language::Ruby => "ruby",
            Language::Cpp => "cpp",
            Language::Php => "php",
            Language::Elixir => "elixir",
            Language::Clojure => "clojure",
            Language::Terraform => "terraform",
        }
    }
}

/// Project configuration structure matching Python implementation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectConfig {
    pub project_name: String,
    pub language: Language,
    #[serde(default)]
    pub ignored_paths: Vec<String>,
    #[serde(default)]
    pub read_only: bool,
    #[serde(default = "default_true")]
    pub ignore_all_files_in_gitignore: bool,
    #[serde(default)]
    pub initial_prompt: String,
    #[serde(default = "default_encoding")]
    pub encoding: String,
    #[serde(default)]
    pub excluded_tools: Vec<String>,
    #[serde(default)]
    pub included_optional_tools: Vec<String>,
    #[serde(default)]
    pub language_servers: HashMap<String, ProjectLanguageServerConfig>,
}

fn default_true() -> bool {
    true
}

fn default_encoding() -> String {
    "utf-8".to_string()
}

/// Language server configuration for a project
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectLanguageServerConfig {
    #[serde(default = "default_true")]
    pub enabled: bool,
    pub binary_path: Option<PathBuf>,
    #[serde(default)]
    pub args: Vec<String>,
    #[serde(default)]
    pub initialization_options: serde_json::Value,
}

impl ProjectConfig {
    pub const PROJECT_FILE_NAME: &'static str = "project.yml";
    pub const SERENA_DIR_NAME: &'static str = ".serena";
    
    /// Get the relative path to the project.yml file
    pub fn rel_path_to_project_yml() -> PathBuf {
        PathBuf::from(Self::SERENA_DIR_NAME).join(Self::PROJECT_FILE_NAME)
    }
    
    /// Load project configuration from a project root directory
    pub fn load(project_root: &Path) -> Result<Self> {
        let config_path = project_root.join(Self::rel_path_to_project_yml());
        
        if !config_path.exists() {
            return Err(anyhow::anyhow!(
                "Project configuration file not found: {}",
                config_path.display()
            ));
        }
        
        let content = std::fs::read_to_string(&config_path)?;
        let config: ProjectConfig = serde_yaml::from_str(&content)?;
        
        info!("Loaded project configuration: {}", config.project_name);
        Ok(config)
    }
    
    /// Save project configuration to a project root directory
    pub fn save(&self, project_root: &Path) -> Result<()> {
        let serena_dir = project_root.join(Self::SERENA_DIR_NAME);
        std::fs::create_dir_all(&serena_dir)?;
        
        let config_path = serena_dir.join(Self::PROJECT_FILE_NAME);
        let content = serde_yaml::to_string(self)?;
        std::fs::write(&config_path, content)?;
        
        info!("Saved project configuration to: {}", config_path.display());
        Ok(())
    }
    
    /// Detect the primary language in a project directory by examining file extensions
    pub fn detect_language(project_root: &Path) -> Result<Language> {
        let mut language_counts = HashMap::new();
        Self::count_files_recursive(project_root, &mut language_counts)?;
        
        language_counts
            .into_iter()
            .max_by_key(|(_, count)| *count)
            .map(|(lang, _)| lang)
            .ok_or_else(|| anyhow::anyhow!("No source files found to determine language"))
    }
    
    fn count_files_recursive(dir: &Path, counts: &mut HashMap<Language, usize>) -> Result<()> {
        if !dir.is_dir() {
            return Ok(());
        }
        
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            
            if path.is_dir() {
                // Skip common directories that shouldn't affect language detection
                if let Some(dir_name) = path.file_name().and_then(|n| n.to_str()) {
                    if matches!(dir_name, ".git" | "node_modules" | "target" | ".venv" | "venv" | "__pycache__") {
                        continue;
                    }
                }
                Self::count_files_recursive(&path, counts)?;
            } else if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
                let language = match ext {
                    "py" => Some(Language::Python),
                    "ts" | "tsx" => Some(Language::Typescript),
                    "js" | "jsx" => Some(Language::Javascript),
                    "java" => Some(Language::Java),
                    "cs" => Some(Language::Csharp),
                    "rs" => Some(Language::Rust),
                    "go" => Some(Language::Go),
                    "rb" => Some(Language::Ruby),
                    "cpp" | "cc" | "cxx" | "hpp" | "h" => Some(Language::Cpp),
                    "php" => Some(Language::Php),
                    "ex" | "exs" => Some(Language::Elixir),
                    "clj" | "cljs" => Some(Language::Clojure),
                    "tf" => Some(Language::Terraform),
                    _ => None,
                };
                
                if let Some(lang) = language {
                    *counts.entry(lang).or_insert(0) += 1;
                }
            }
        }
        Ok(())
    }
    
    /// Create a new project configuration with auto-detected settings
    pub fn autogenerate(project_root: &Path, project_name: Option<String>) -> Result<Self> {
        let project_name = project_name.unwrap_or_else(|| {
            project_root
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("project")
                .to_string()
        });
        
        let language = Self::detect_language(project_root)?;
        
        info!("Auto-generated project config: {} ({})", project_name, language.as_str());
        
        Ok(ProjectConfig {
            project_name,
            language,
            ignored_paths: vec![],
            read_only: false,
            ignore_all_files_in_gitignore: true,
            initial_prompt: String::new(),
            encoding: "utf-8".to_string(),
            excluded_tools: vec![],
            included_optional_tools: vec![],
            language_servers: HashMap::new(),
        })
    }
}

/// A project instance combining the root path and configuration
pub struct Project {
    pub project_root: PathBuf,
    pub config: ProjectConfig,
    pub is_newly_created: bool,
}

impl Project {
    /// Load an existing project from a root directory
    pub fn load(project_root: &Path) -> Result<Self> {
        let config = ProjectConfig::load(project_root)?;
        Ok(Project {
            project_root: project_root.to_path_buf(),
            config,
            is_newly_created: false,
        })
    }
    
    /// Create a new project at the given root directory
    pub fn create(project_root: &Path, project_name: Option<String>) -> Result<Self> {
        let config = ProjectConfig::autogenerate(project_root, project_name)?;
        config.save(project_root)?;
        
        Ok(Project {
            project_root: project_root.to_path_buf(),
            config,
            is_newly_created: true,
        })
    }
    
    /// Get the project name
    pub fn project_name(&self) -> &str {
        &self.config.project_name
    }
    
    /// Get the path to the project.yml file
    pub fn path_to_project_yml(&self) -> PathBuf {
        self.project_root.join(ProjectConfig::rel_path_to_project_yml())
    }
    
    /// Get the .serena directory path
    pub fn serena_dir(&self) -> PathBuf {
        self.project_root.join(ProjectConfig::SERENA_DIR_NAME)
    }
    
    /// Get the memories directory path
    pub fn memories_dir(&self) -> PathBuf {
        self.serena_dir().join("memories")
    }
}

/// Simple project registry to manage multiple projects
pub struct ProjectRegistry {
    projects: HashMap<String, PathBuf>,
}

impl ProjectRegistry {
    pub fn new() -> Self {
        Self {
            projects: HashMap::new(),
        }
    }
    
    /// Register a project by name and path
    pub fn register(&mut self, name: String, path: PathBuf) {
        self.projects.insert(name, path);
    }
    
    /// Get project path by name
    pub fn get_project_path(&self, name: &str) -> Option<&PathBuf> {
        self.projects.get(name)
    }
    
    /// List all registered project names
    pub fn list_projects(&self) -> Vec<&String> {
        self.projects.keys().collect()
    }
    
    /// Remove a project by name
    pub fn remove(&mut self, name: &str) -> Option<PathBuf> {
        self.projects.remove(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;
    
    #[test]
    fn test_language_detection() {
        let temp_dir = tempdir().unwrap();
        let project_root = temp_dir.path();
        
        // Create some Python files
        std::fs::write(project_root.join("main.py"), "print('hello')").unwrap();
        std::fs::write(project_root.join("utils.py"), "def helper(): pass").unwrap();
        
        let language = ProjectConfig::detect_language(project_root).unwrap();
        assert_eq!(language, Language::Python);
    }
    
    #[test]
    fn test_project_config_roundtrip() {
        let temp_dir = tempdir().unwrap();
        let project_root = temp_dir.path();
        
        let config = ProjectConfig {
            project_name: "test-project".to_string(),
            language: Language::Rust,
            ignored_paths: vec!["target".to_string()],
            read_only: false,
            ignore_all_files_in_gitignore: true,
            initial_prompt: "Test project".to_string(),
            encoding: "utf-8".to_string(),
            excluded_tools: vec![],
            included_optional_tools: vec![],
            language_servers: HashMap::new(),
        };
        
        // Save and load
        config.save(project_root).unwrap();
        let loaded_config = ProjectConfig::load(project_root).unwrap();
        
        assert_eq!(config.project_name, loaded_config.project_name);
        assert_eq!(config.language, loaded_config.language);
        assert_eq!(config.ignored_paths, loaded_config.ignored_paths);
    }
}