use serde_json::{json, Value};
use serena_core::tools::{Tool, ToolContext};
use serena_core::error::{CoreError, Result};
use std::fs;
use std::path::{Path, PathBuf};
use tracing::{info, warn};
use crate::project::Project;

/// Get the memory file path for a given name
fn get_memory_file_path(workspace_root: &Path, name: &str) -> PathBuf {
    // Strip all .md extensions from the name to avoid confusion
    let clean_name = name.replace(".md", "");
    let filename = format!("{}.md", clean_name);
    get_memory_directory_path(workspace_root).join(filename)
}

/// Get the memory directory path - try to use project-specific directory if available
fn get_memory_directory_path(workspace_root: &Path) -> PathBuf {
    // Try to load project and use its memory directory
    if let Ok(project) = Project::load(workspace_root) {
        project.memories_dir()
    } else {
        // Fall back to global .serena/memories in workspace root
        workspace_root.join(".serena").join("memories")
    }
}

/// Tool for reading memory content
pub struct ReadMemory;

#[async_trait::async_trait]
impl Tool for ReadMemory {
    fn name(&self) -> &'static str {
        "read_memory"
    }

    fn description(&self) -> &'static str {
        "Read a named memory from the project-specific memory store"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "required": ["memory_file_name"],
            "properties": {
                "memory_file_name": {
                    "type": "string",
                    "description": "The name of the memory file to read"
                },
                "max_answer_chars": {
                    "type": "integer",
                    "description": "Maximum number of characters to return",
                    "default": 200000
                }
            }
        })
    }

    async fn run(&self, args: Value, ctx: ToolContext) -> serena_core::error::Result<Value> {
        let memory_name = args["memory_file_name"]
            .as_str()
            .ok_or_else(|| CoreError::validation("memory_file_name is required".to_string()))?;

        let max_chars = args.get("max_answer_chars")
            .and_then(|v| v.as_u64())
            .unwrap_or(200000) as usize;

        let memory_file_path = get_memory_file_path(&ctx.cfg.workspace_root, memory_name);
        
        if !memory_file_path.exists() {
            return Ok(json!({
                "error": format!("Memory file '{}' not found. Consider creating it with the write_memory tool if needed.", memory_name)
            }));
        }

        match fs::read_to_string(&memory_file_path) {
            Ok(content) => {
                let content = if content.len() > max_chars {
                    warn!("Memory content truncated from {} to {} characters", content.len(), max_chars);
                    content.chars().take(max_chars).collect::<String>() + "...[truncated]"
                } else {
                    content
                };
                
                info!("Read memory '{}' ({} chars)", memory_name, content.len());
                Ok(json!({ "content": content }))
            }
            Err(e) => {
                warn!("Failed to read memory '{}': {}", memory_name, e);
                Ok(json!({ "error": e.to_string() }))
            }
        }
    }
}

/// Tool for writing memory content
pub struct WriteMemory;

#[async_trait::async_trait]
impl Tool for WriteMemory {
    fn name(&self) -> &'static str {
        "write_memory"
    }

    fn description(&self) -> &'static str {
        "Write a named memory to the project-specific memory store for future reference"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "required": ["memory_name", "content"],
            "properties": {
                "memory_name": {
                    "type": "string",
                    "description": "The name of the memory file to write"
                },
                "content": {
                    "type": "string",
                    "description": "The content to write to the memory file"
                },
                "max_answer_chars": {
                    "type": "integer",
                    "description": "Maximum number of characters for the response",
                    "default": 200000
                }
            }
        })
    }

    async fn run(&self, args: Value, ctx: ToolContext) -> serena_core::error::Result<Value> {
        let memory_name = args["memory_name"]
            .as_str()
            .ok_or_else(|| CoreError::validation("memory_name is required".to_string()))?;
        
        let content = args["content"]
            .as_str()
            .ok_or_else(|| CoreError::validation("content is required".to_string()))?;

        let memory_file_path = get_memory_file_path(&ctx.cfg.workspace_root, memory_name);
        
        // Ensure the memory directory exists
        if let Some(parent) = memory_file_path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        match tokio::fs::write(&memory_file_path, content).await {
            Ok(()) => {
                info!("Wrote memory '{}' ({} chars)", memory_name, content.len());
                Ok(json!({ "message": format!("Memory '{}' written successfully.", memory_name) }))
            }
            Err(e) => {
                warn!("Failed to write memory '{}': {}", memory_name, e);
                Ok(json!({ "error": e.to_string() }))
            }
        }
    }
}

/// Tool for listing available memories
pub struct ListMemories;

#[async_trait::async_trait]
impl Tool for ListMemories {
    fn name(&self) -> &'static str {
        "list_memories"
    }

    fn description(&self) -> &'static str {
        "List all available memories in the project-specific memory store"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "properties": {}
        })
    }

    async fn run(&self, _args: Value, ctx: ToolContext) -> Result<Value> {
        let memory_dir = get_memory_directory_path(&ctx.cfg.workspace_root);
        
        if !memory_dir.exists() {
            return Ok(json!({ "memories": [] }));
        }

        match fs::read_dir(&memory_dir) {
            Ok(entries) => {
                let mut memories = Vec::new();
                
                for entry in entries {
                    if let Ok(entry) = entry {
                        let path = entry.path();
                        
                        if path.is_file() && path.extension().map_or(false, |ext| ext == "md") {
                            if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                                memories.push(stem.to_string());
                            }
                        }
                    }
                }
                
                memories.sort();
                info!("Listed {} memories", memories.len());
                Ok(json!({ "memories": memories }))
            }
            Err(e) => {
                warn!("Failed to list memories: {}", e);
                Ok(json!({ "error": e.to_string() }))
            }
        }
    }
}

/// Tool for deleting a memory
pub struct DeleteMemory;

#[async_trait::async_trait]
impl Tool for DeleteMemory {
    fn name(&self) -> &'static str {
        "delete_memory"
    }

    fn description(&self) -> &'static str {
        "Delete a named memory from the project-specific memory store"
    }

    fn schema(&self) -> Value {
        json!({
            "type": "object",
            "required": ["memory_file_name"],
            "properties": {
                "memory_file_name": {
                    "type": "string",
                    "description": "The name of the memory file to delete"
                }
            }
        })
    }

    async fn run(&self, args: Value, ctx: ToolContext) -> serena_core::error::Result<Value> {
        let memory_name = args["memory_file_name"]
            .as_str()
            .ok_or_else(|| CoreError::validation("memory_file_name is required".to_string()))?;

        let memory_file_path = get_memory_file_path(&ctx.cfg.workspace_root, memory_name);
        
        if !memory_file_path.exists() {
            return Ok(json!({ "error": format!("Memory file '{}' not found.", memory_name) }));
        }
        
        match tokio::fs::remove_file(&memory_file_path).await {
            Ok(()) => {
                info!("Deleted memory '{}'", memory_name);
                Ok(json!({ "message": format!("Memory '{}' deleted successfully.", memory_name) }))
            }
            Err(e) => {
                warn!("Failed to delete memory '{}': {}", memory_name, e);
                Ok(json!({ "error": e.to_string() }))
            }
        }
    }
}