use anyhow::{Context, Result};
use std::fs;
use std::path::{Path, PathBuf};
use tracing::{debug, info};

/// Manages markdown-based memory files for project knowledge persistence
#[derive(Debug, Clone)]
pub struct MemoryManager {
    memory_dir: PathBuf,
}

impl MemoryManager {
    /// Create a new MemoryManager for the given project root
    pub fn new(project_root: impl AsRef<Path>) -> Result<Self> {
        let memory_dir = project_root.as_ref().join(".serena").join("memories");
        
        // Ensure the memory directory exists
        fs::create_dir_all(&memory_dir)
            .with_context(|| format!("Failed to create memory directory: {}", memory_dir.display()))?;
        
        info!("Memory manager initialized with directory: {}", memory_dir.display());
        
        Ok(Self { memory_dir })
    }

    /// Get the file path for a memory with the given name
    fn get_memory_file_path(&self, name: &str) -> PathBuf {
        // Strip all .md extensions from the name to avoid confusion
        let clean_name = name.replace(".md", "");
        let filename = format!("{}.md", clean_name);
        self.memory_dir.join(filename)
    }

    /// Load memory content by name
    pub fn load_memory(&self, name: &str) -> Result<String> {
        let memory_file_path = self.get_memory_file_path(name);
        
        if !memory_file_path.exists() {
            anyhow::bail!(
                "Memory file '{}' not found. Consider creating it with the write_memory tool if needed.",
                name
            );
        }

        let content = fs::read_to_string(&memory_file_path)
            .with_context(|| format!("Failed to read memory file: {}", memory_file_path.display()))?;
        
        debug!("Loaded memory '{}' ({} bytes)", name, content.len());
        Ok(content)
    }

    /// Save memory content with the given name
    pub fn save_memory(&self, name: &str, content: &str) -> Result<String> {
        let memory_file_path = self.get_memory_file_path(name);
        
        fs::write(&memory_file_path, content)
            .with_context(|| format!("Failed to write memory file: {}", memory_file_path.display()))?;
        
        info!("Saved memory '{}' ({} bytes)", name, content.len());
        Ok(format!("Memory '{}' written successfully.", name))
    }

    /// List all available memory names
    pub fn list_memories(&self) -> Result<Vec<String>> {
        if !self.memory_dir.exists() {
            return Ok(Vec::new());
        }

        let mut memories = Vec::new();
        
        for entry in fs::read_dir(&self.memory_dir)
            .with_context(|| format!("Failed to read memory directory: {}", self.memory_dir.display()))?
        {
            let entry = entry?;
            let path = entry.path();
            
            if path.is_file() && path.extension().map_or(false, |ext| ext == "md") {
                if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                    memories.push(stem.to_string());
                }
            }
        }
        
        memories.sort();
        debug!("Found {} memories: {:?}", memories.len(), memories);
        Ok(memories)
    }

    /// Delete a memory by name
    pub fn delete_memory(&self, name: &str) -> Result<String> {
        let memory_file_path = self.get_memory_file_path(name);
        
        if !memory_file_path.exists() {
            anyhow::bail!("Memory file '{}' not found.", name);
        }
        
        fs::remove_file(&memory_file_path)
            .with_context(|| format!("Failed to delete memory file: {}", memory_file_path.display()))?;
        
        info!("Deleted memory '{}'", name);
        Ok(format!("Memory '{}' deleted successfully.", name))
    }

    /// Check if a memory exists
    pub fn memory_exists(&self, name: &str) -> bool {
        self.get_memory_file_path(name).exists()
    }

    /// Get memory file metadata
    pub fn get_memory_info(&self, name: &str) -> Result<MemoryInfo> {
        let memory_file_path = self.get_memory_file_path(name);
        
        if !memory_file_path.exists() {
            anyhow::bail!("Memory file '{}' not found.", name);
        }
        
        let metadata = fs::metadata(&memory_file_path)
            .with_context(|| format!("Failed to get metadata for memory: {}", memory_file_path.display()))?;
        
        let size = metadata.len();
        let modified = metadata.modified()
            .with_context(|| "Failed to get modification time")?
            .into();
        
        Ok(MemoryInfo {
            name: name.to_string(),
            size_bytes: size,
            last_modified: modified,
        })
    }

    /// Get the memory directory path
    pub fn memory_dir(&self) -> &Path {
        &self.memory_dir
    }
}

/// Information about a memory file
#[derive(Debug, Clone)]
pub struct MemoryInfo {
    pub name: String,
    pub size_bytes: u64,
    pub last_modified: time::OffsetDateTime,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_memory_manager_basic_operations() -> Result<()> {
        let temp_dir = tempdir()?;
        let memory_manager = MemoryManager::new(temp_dir.path())?;

        // Test saving memory
        let result = memory_manager.save_memory("test_memory", "This is test content")?;
        assert!(result.contains("written successfully"));

        // Test loading memory
        let content = memory_manager.load_memory("test_memory")?;
        assert_eq!(content, "This is test content");

        // Test listing memories
        let memories = memory_manager.list_memories()?;
        assert_eq!(memories, vec!["test_memory"]);

        // Test memory exists
        assert!(memory_manager.memory_exists("test_memory"));
        assert!(!memory_manager.memory_exists("nonexistent"));

        // Test deleting memory
        let result = memory_manager.delete_memory("test_memory")?;
        assert!(result.contains("deleted successfully"));
        assert!(!memory_manager.memory_exists("test_memory"));

        Ok(())
    }

    #[test]
    fn test_memory_name_normalization() -> Result<()> {
        let temp_dir = tempdir()?;
        let memory_manager = MemoryManager::new(temp_dir.path())?;

        // Test saving with .md extension
        memory_manager.save_memory("test.md", "content")?;
        
        // Should be able to load without .md extension
        let content = memory_manager.load_memory("test")?;
        assert_eq!(content, "content");

        // Should be able to load with .md extension
        let content = memory_manager.load_memory("test.md")?;
        assert_eq!(content, "content");

        Ok(())
    }
}