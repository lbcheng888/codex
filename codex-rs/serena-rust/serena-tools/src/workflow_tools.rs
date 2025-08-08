use serde_json::{json, Value};
use serena_core::tools::{Tool, ToolContext};
use serena_core::error::{Result, CoreError};
use tracing::debug;

/// Tool for preparing for a new conversation
pub struct PrepareForNewConversation;

#[async_trait::async_trait]
impl Tool for PrepareForNewConversation {
    fn name(&self) -> &'static str {
        "prepare_for_new_conversation"
    }

    fn description(&self) -> &'static str {
        "Prepare the workspace and context for a new conversation or task session"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {
                "conversation_type": {
                    "type": "string",
                    "enum": ["debugging", "feature_development", "code_review", "refactoring", "general"],
                    "description": "Type of conversation being started"
                },
                "context_summary": {
                    "type": "string",
                    "description": "Brief summary of what this conversation will focus on"
                }
            }
        })
    }

    async fn run(&self, input: Value, ctx: ToolContext) -> Result<Value> {
        let conversation_type = input["conversation_type"].as_str().unwrap_or("general");
        let context_summary = input["context_summary"].as_str().unwrap_or("");

        debug!(target = "workflow", conversation_type = %conversation_type, "preparing for new conversation");

        // Create a preparation summary
        let preparation = json!({
            "conversation_type": conversation_type,
            "context_summary": context_summary,
            "workspace_root": ctx.cfg.workspace_root.to_string_lossy(),
            "preparation_steps": [
                "Workspace context initialized",
                "Tool registry loaded",
                "Memory system ready",
                "LSP connections established (if available)"
            ],
            "available_capabilities": [
                "File operations (read, write, search)",
                "Code analysis and symbol navigation", 
                "Memory management for context persistence",
                "Project configuration management"
            ],
            "recommendations": match conversation_type {
                "debugging" => vec![
                    "Start by reproducing the issue",
                    "Gather relevant logs and error messages",
                    "Examine recent changes that might be related",
                    "Use search tools to find similar patterns"
                ],
                "feature_development" => vec![
                    "Review existing code structure",
                    "Identify integration points",
                    "Plan the implementation approach",
                    "Consider testing requirements"
                ],
                "code_review" => vec![
                    "Understand the changes being reviewed",
                    "Check for coding standards compliance",
                    "Look for potential issues or improvements",
                    "Verify test coverage"
                ],
                "refactoring" => vec![
                    "Identify the scope of refactoring",
                    "Ensure comprehensive test coverage",
                    "Plan incremental changes",
                    "Document the refactoring rationale"
                ],
                _ => vec![
                    "Understand the current task requirements",
                    "Gather necessary context information",
                    "Plan the approach step by step",
                    "Identify potential challenges early"
                ]
            },
            "timestamp": time::OffsetDateTime::now_utc().to_string()
        });

        Ok(preparation)
    }
}

/// Tool for providing initial instructions
pub struct InitialInstructions;

#[async_trait::async_trait]
impl Tool for InitialInstructions {
    fn name(&self) -> &'static str {
        "initial_instructions"
    }

    fn description(&self) -> &'static str {
        "Provide initial instructions and context for working with the current project"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {
                "project_type": {
                    "type": "string",
                    "description": "Type of project (e.g., web, mobile, library, etc.)"
                },
                "primary_language": {
                    "type": "string", 
                    "description": "Primary programming language of the project"
                }
            }
        })
    }

    async fn run(&self, input: Value, ctx: ToolContext) -> Result<Value> {
        let project_type = input["project_type"].as_str().unwrap_or("unknown");
        let primary_language = input["primary_language"].as_str().unwrap_or("unknown");

        debug!(target = "workflow", project_type = %project_type, language = %primary_language, "providing initial instructions");

        // Check if onboarding has been performed
        let memories_dir = ctx.cfg.workspace_root.join(".serena").join("memories");
        let is_onboarded = memories_dir.exists();

        let instructions = json!({
            "project_info": {
                "workspace_root": ctx.cfg.workspace_root.to_string_lossy(),
                "project_type": project_type,
                "primary_language": primary_language,
                "is_onboarded": is_onboarded
            },
            "available_tools": {
                "file_operations": [
                    "file_read - Read file contents",
                    "file_write - Write to files", 
                    "list_dir - List directory contents",
                    "find_file - Find files by pattern"
                ],
                "search_and_edit": [
                    "search_for_pattern - Search with regex",
                    "search_and_replace - Replace text patterns",
                    "delete_lines - Remove specific lines",
                    "replace_lines - Replace line ranges"
                ],
                "code_analysis": [
                    "find_symbol - Find code symbols",
                    "find_referencing_symbols - Find symbol references",
                    "get_symbols_overview - Get file/directory symbol overview"
                ],
                "memory_management": [
                    "write_memory - Store information for later",
                    "read_memory - Retrieve stored information",
                    "list_memories - List all stored memories"
                ],
                "project_management": [
                    "activate_project - Set up project context",
                    "get_current_config - View current settings"
                ]
            },
            "getting_started": if !is_onboarded {
                vec![
                    "Run 'onboarding' tool to set up the project",
                    "Use 'list_dir' to explore the project structure",
                    "Use 'get_symbols_overview' to understand the codebase",
                    "Create memories for important findings"
                ]
            } else {
                vec![
                    "Use 'list_memories' to see existing project knowledge",
                    "Use 'get_current_config' to understand current setup",
                    "Start with your specific task or question"
                ]
            },
            "best_practices": [
                "Always read files before modifying them",
                "Use search tools to understand code patterns",
                "Create memories for important discoveries",
                "Test changes when possible",
                "Ask for clarification if requirements are unclear"
            ],
            "timestamp": time::OffsetDateTime::now_utc().to_string()
        });

        Ok(instructions)
    }
}

/// Tool for workflow state management
pub struct ManageWorkflowState;

#[async_trait::async_trait]
impl Tool for ManageWorkflowState {
    fn name(&self) -> &'static str {
        "manage_workflow_state"
    }

    fn description(&self) -> &'static str {
        "Manage the current workflow state and track progress through complex tasks"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "required": ["action"],
            "properties": {
                "action": {
                    "type": "string",
                    "enum": ["save_state", "load_state", "clear_state", "list_states"],
                    "description": "Action to perform on workflow state"
                },
                "state_name": {
                    "type": "string",
                    "description": "Name of the state (required for save/load)"
                },
                "state_data": {
                    "type": "object",
                    "description": "State data to save (required for save_state)"
                }
            }
        })
    }

    async fn run(&self, input: Value, ctx: ToolContext) -> Result<Value> {
        let action = input["action"]
            .as_str()
            .ok_or_else(|| CoreError::validation("action is required".to_string()))?;

        debug!(target = "workflow", action = %action, "managing workflow state");

        match action {
            "save_state" => {
                let state_name = input["state_name"]
                    .as_str()
                    .ok_or_else(|| CoreError::validation("state_name is required for save_state".to_string()))?;
                let state_data = input["state_data"]
                    .as_object()
                    .ok_or_else(|| CoreError::validation("state_data is required for save_state".to_string()))?;

                // Save state to memory system
                let state_content = format!(
                    "# Workflow State: {}\n\nSaved at: {}\n\n```json\n{}\n```",
                    state_name,
                    time::OffsetDateTime::now_utc(),
                    serde_json::to_string_pretty(state_data)?
                );

                let state_dir = ctx.cfg.workspace_root.join(".serena").join("workflow_states");
                tokio::fs::create_dir_all(&state_dir).await?;
                
                let state_file = state_dir.join(format!("{}.md", state_name));
                tokio::fs::write(&state_file, state_content).await?;

                Ok(json!({
                    "message": format!("Workflow state '{}' saved successfully", state_name),
                    "state_name": state_name,
                    "file_path": state_file.to_string_lossy()
                }))
            },
            "load_state" => {
                let state_name = input["state_name"]
                    .as_str()
                    .ok_or_else(|| CoreError::validation("state_name is required for load_state".to_string()))?;

                let state_file = ctx.cfg.workspace_root
                    .join(".serena")
                    .join("workflow_states")
                    .join(format!("{}.md", state_name));

                if !state_file.exists() {
                    return Err(CoreError::validation(format!("Workflow state '{}' not found", state_name)));
                }

                let content = tokio::fs::read_to_string(&state_file).await?;
                
                Ok(json!({
                    "message": format!("Workflow state '{}' loaded successfully", state_name),
                    "state_name": state_name,
                    "content": content
                }))
            },
            "list_states" => {
                let state_dir = ctx.cfg.workspace_root.join(".serena").join("workflow_states");
                
                if !state_dir.exists() {
                    return Ok(json!({
                        "states": [],
                        "message": "No workflow states found"
                    }));
                }

                let mut states = Vec::new();
                let mut entries = tokio::fs::read_dir(&state_dir).await?;
                
                while let Some(entry) = entries.next_entry().await? {
                    if let Some(name) = entry.file_name().to_str() {
                        if name.ends_with(".md") {
                            let state_name = name.trim_end_matches(".md");
                            let metadata = entry.metadata().await?;
                            let modified_secs = metadata.modified()
                                .ok()
                                .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
                                .map(|d| d.as_secs())
                                .unwrap_or(0);
                            states.push(json!({
                                "name": state_name,
                                "modified": modified_secs
                            }));
                        }
                    }
                }

                Ok(json!({
                    "states": states,
                    "count": states.len(),
                    "message": format!("Found {} workflow states", states.len())
                }))
            },
            "clear_state" => {
                let state_name = input["state_name"]
                    .as_str()
                    .ok_or_else(|| CoreError::validation("state_name is required for clear_state".to_string()))?;

                let state_file = ctx.cfg.workspace_root
                    .join(".serena")
                    .join("workflow_states")
                    .join(format!("{}.md", state_name));

                if state_file.exists() {
                    tokio::fs::remove_file(&state_file).await?;
                    Ok(json!({
                        "message": format!("Workflow state '{}' cleared successfully", state_name),
                        "state_name": state_name
                    }))
                } else {
                    Ok(json!({
                        "message": format!("Workflow state '{}' not found", state_name),
                        "state_name": state_name
                    }))
                }
            },
            _ => Err(CoreError::validation(format!("Unknown action: {}", action)))
        }
    }
}
