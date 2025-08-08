use serde_json::{json, Value};
use serena_core::tools::{Tool, ToolContext};
use serena_core::error::Result;
use tracing::{info, warn};
use crate::project::Project;
use std::path::Path;

/// Tool for activating a project
pub struct ActivateProject;

#[async_trait::async_trait]
impl Tool for ActivateProject {
    fn name(&self) -> &'static str {
        "activate_project"
    }

    fn description(&self) -> &'static str {
        "Activate a project by name or path, setting up the workspace context"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "required": ["project"],
            "properties": {
                "project": {
                    "type": "string",
                    "description": "Project name or path to activate"
                }
            }
        })
    }

    async fn run(&self, args: Value, ctx: ToolContext) -> Result<Value> {
        let project_input = args["project"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("project is required".to_string()))?;

        info!("Activating project: {}", project_input);
        
        // Try to interpret the input as either a path or project name
        let project_path = if Path::new(project_input).exists() {
            // Input is a path
            Path::new(project_input).to_path_buf()
        } else {
            // For now, treat as a path relative to workspace root
            // In a full implementation, we'd check a project registry first
            ctx.cfg.workspace_root.join(project_input)
        };
        
        // Try to load existing project or create a new one
        let project = match Project::load(&project_path) {
            Ok(project) => {
                info!("Loaded existing project: {}", project.project_name());
                project
            }
            Err(_) => {
                // Try to create a new project
                match Project::create(&project_path, None) {
                    Ok(project) => {
                        info!("Created new project: {}", project.project_name());
                        project
                    }
                    Err(e) => {
                        return Err(serena_core::error::CoreError::validation(
                            format!("Failed to load or create project at {}: {}", project_path.display(), e)
                        ));
                    }
                }
            }
        };
        
        let result = if project.is_newly_created {
            json!({
                "status": "created",
                "message": format!(
                    "Created and activated new project '{}' at {} (language: {})",
                    project.project_name(),
                    project.project_root.display(),
                    project.config.language.as_str()
                ),
                "project_name": project.project_name(),
                "project_root": project.project_root,
                "language": project.config.language.as_str(),
                "config_file": project.path_to_project_yml(),
                "is_newly_created": true
            })
        } else {
            json!({
                "status": "activated",
                "message": format!(
                    "Activated existing project '{}' at {} (language: {})",
                    project.project_name(),
                    project.project_root.display(),
                    project.config.language.as_str()
                ),
                "project_name": project.project_name(),
                "project_root": project.project_root,
                "language": project.config.language.as_str(),
                "config_file": project.path_to_project_yml(),
                "is_newly_created": false
            })
        };
        
        if !project.config.initial_prompt.is_empty() {
            info!("Project has initial prompt: {}", project.config.initial_prompt);
        }
        
        Ok(result)
    }
}

/// Tool for switching agent modes
pub struct SwitchModes;

#[async_trait::async_trait]
impl Tool for SwitchModes {
    fn name(&self) -> &'static str {
        "switch_modes"
    }

    fn description(&self) -> &'static str {
        "Switch between different agent operation modes (planning, editing, interactive, etc.)"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "required": ["modes"],
            "properties": {
                "modes": {
                    "type": "array",
                    "items": {
                        "type": "string",
                        "enum": ["planning", "editing", "interactive", "one-shot", "jetbrains"]
                    },
                    "description": "List of modes to activate"
                }
            }
        })
    }

    async fn run(&self, args: Value, _ctx: ToolContext) -> Result<Value> {
        let modes = args["modes"]
            .as_array()
            .ok_or_else(|| serena_core::error::CoreError::validation("modes array is required".to_string()))?;

        let mode_names: Vec<String> = modes
            .iter()
            .filter_map(|v| v.as_str())
            .map(|s| s.to_string())
            .collect();

        info!("Switching to modes: {:?}", mode_names);
        
        // For now, return a placeholder response
        warn!("Mode switching not yet fully integrated - this is a placeholder");
        
        Ok(json!({
            "message": format!("Would switch to modes: {:?}", mode_names),
            "modes": mode_names,
            "implemented": false
        }))
    }
}

/// Tool for checking if onboarding was performed
pub struct CheckOnboardingPerformed;

#[async_trait::async_trait]
impl Tool for CheckOnboardingPerformed {
    fn name(&self) -> &'static str {
        "check_onboarding_performed"
    }

    fn description(&self) -> &'static str {
        "Check whether project onboarding has been performed and memories are available"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {}
        })
    }

    async fn run(&self, _args: Value, ctx: ToolContext) -> Result<Value> {
        info!("Checking if onboarding was performed");
        
        // Check if .serena/memories directory exists in workspace
        let memories_dir = ctx.cfg.workspace_root.join(".serena").join("memories");
        let onboarded = memories_dir.exists() && 
            memories_dir.read_dir()
                .map_or(false, |mut entries| entries.any(|_| true));
        
        let message = if onboarded {
            "The onboarding was already performed, below is the list of available memories."
        } else {
            "Onboarding has not been performed yet."
        };
        
        info!("Onboarding status: {}", if onboarded { "completed" } else { "not completed" });
        
        Ok(json!({
            "onboarded": onboarded,
            "message": message
        }))
    }
}

/// Tool for performing onboarding
pub struct Onboarding;

#[async_trait::async_trait]
impl Tool for Onboarding {
    fn name(&self) -> &'static str {
        "onboarding"
    }

    fn description(&self) -> &'static str {
        "Perform initial project onboarding to set up workspace and create initial memories"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {}
        })
    }

    async fn run(&self, _args: Value, ctx: ToolContext) -> Result<Value> {
        info!("Starting onboarding process");
        
        // Create .serena directories
        let serena_dir = ctx.cfg.workspace_root.join(".serena");
        let memories_dir = serena_dir.join("memories");
        
        tokio::fs::create_dir_all(&memories_dir).await?;
        
        // Create initial onboarding memory
        let onboarding_content = format!(
            "# Project Onboarding\n\n\
            Project root: {}\n\
            Onboarded at: {}\n\n\
            ## Instructions\n\
            This project has been onboarded to Serena. \
            Use the various tools to explore the codebase and create additional memories as needed.\n\
            \n\
            ## Available Tools\n\
            - file_read/file_write: Read and write files\n\
            - search_for_pattern: Search for text patterns\n\
            - list_dir/find_file: Navigate the file system\n\
            - memory tools: Create and manage project knowledge\n",
            ctx.cfg.workspace_root.display(),
            time::OffsetDateTime::now_utc()
        );
        
        let onboarding_file = memories_dir.join("onboarding.md");
        tokio::fs::write(&onboarding_file, onboarding_content).await?;
        
        info!("Onboarding completed successfully");
        
        Ok(json!({
            "message": "Onboarding completed successfully. You can now use Serena tools to explore and work with this project.",
            "memories_created": ["onboarding"]
        }))
    }
}

/// Tool for removing a project from configuration
pub struct RemoveProject;

#[async_trait::async_trait]
impl Tool for RemoveProject {
    fn name(&self) -> &'static str {
        "remove_project"
    }

    fn description(&self) -> &'static str {
        "Remove a project from the Serena configuration"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "required": ["project_name"],
            "properties": {
                "project_name": {
                    "type": "string",
                    "description": "Name of the project to remove"
                }
            }
        })
    }

    async fn run(&self, args: Value, _ctx: ToolContext) -> Result<Value> {
        let project_name = args["project_name"]
            .as_str()
            .ok_or_else(|| serena_core::error::CoreError::validation("project_name is required".to_string()))?;

        info!("Removing project: {}", project_name);
        
        // For now, return a placeholder response
        warn!("Project removal not yet fully integrated - this is a placeholder");
        
        Ok(json!({
            "message": format!("Would remove project: {}", project_name),
            "implemented": false
        }))
    }
}

/// Tool for getting current configuration
pub struct GetCurrentConfig;

#[async_trait::async_trait]
impl Tool for GetCurrentConfig {
    fn name(&self) -> &'static str {
        "get_current_config"
    }

    fn description(&self) -> &'static str {
        "Get the current Serena configuration settings and workspace information"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {
                "include_sensitive": {
                    "type": "boolean",
                    "description": "Whether to include sensitive configuration values (default: false)",
                    "default": false
                }
            }
        })
    }

    async fn run(&self, input: Value, ctx: ToolContext) -> Result<Value> {
        let include_sensitive = input["include_sensitive"].as_bool().unwrap_or(false);

        info!("Getting current config (include_sensitive: {})", include_sensitive);

        let mut config = std::collections::HashMap::new();

        // Workspace information
        config.insert("workspace_root", json!(ctx.cfg.workspace_root.to_string_lossy()));
        config.insert("transport", json!(format!("{:?}", ctx.cfg.transport)));
        config.insert("log_level", json!(ctx.cfg.log_level.to_string()));

        // Memory configuration
        let memory_dir = ctx.cfg.workspace_root.join(".serena").join("memories");
        config.insert("memory_store", json!({
            "path": if include_sensitive { Some(memory_dir.to_string_lossy()) } else { None },
            "exists": memory_dir.exists()
        }));

        // Runtime information
        config.insert("runtime", json!({
            "version": env!("CARGO_PKG_VERSION"),
            "rust_version": "stable"
        }));

        Ok(json!({
            "config": config,
            "timestamp": time::OffsetDateTime::now_utc().to_string(),
            "include_sensitive": include_sensitive
        }))
    }
}