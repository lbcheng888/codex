//! Parallel file reading tool for MCP server.
//! 
//! Provides the ability to read multiple files concurrently, with configurable
//! limits for performance and memory usage.

use futures::future::join_all;
use mcp_types::{Tool, ToolInputSchema};
use schemars::{JsonSchema, r#gen::SchemaSettings};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use tokio::fs;
use tokio::time::{timeout, Duration};
use tracing::{debug, warn};

/// Maximum number of files that can be read in parallel
const MAX_PARALLEL_FILES: usize = 50;

/// Maximum bytes per file (default: 2MB)
const DEFAULT_MAX_BYTES_PER_FILE: u64 = 2_097_152;

/// Default timeout for file operations (30 seconds)
const DEFAULT_TIMEOUT_MS: u64 = 30_000;

/// Parameters for the parallel file reading tool
#[derive(Debug, Clone, Deserialize, JsonSchema)]
#[serde(rename_all = "kebab-case")]
pub struct ParallelReadParams {
    /// Array of file paths to read
    pub file_paths: Vec<String>,
    
    /// Maximum bytes to read per file (default: 2MB)
    #[serde(default = "default_max_bytes")]
    pub max_bytes_per_file: u64,
    
    /// Timeout in milliseconds for the entire operation (default: 30s)
    #[serde(default = "default_timeout")]
    pub timeout_ms: u64,
    
    /// Base directory for resolving relative paths (optional)
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub base_dir: Option<String>,
}

fn default_max_bytes() -> u64 {
    DEFAULT_MAX_BYTES_PER_FILE
}

fn default_timeout() -> u64 {
    DEFAULT_TIMEOUT_MS
}

/// Result of reading a single file
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileReadResult {
    /// The file path that was read
    pub path: String,
    /// Content of the file (if successful)
    pub content: Option<String>,
    /// Error message (if failed)
    pub error: Option<String>,
    /// Size of the file in bytes
    pub size_bytes: Option<u64>,
    /// Whether the file was truncated due to size limits
    pub truncated: bool,
}

/// Result of the parallel read operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParallelReadResult {
    /// Results for each file, indexed by the original path
    pub files: HashMap<String, FileReadResult>,
    /// Number of files successfully read
    pub success_count: usize,
    /// Number of files that failed to read
    pub error_count: usize,
    /// Total time taken for the operation (milliseconds)
    pub duration_ms: u64,
}

/// Creates the MCP tool definition for parallel file reading
pub fn create_parallel_read_tool() -> Tool {
    let schema = SchemaSettings::draft2019_09()
        .with(|s| {
            s.inline_subschemas = true;
            s.option_add_null_type = false;
        })
        .into_generator()
        .into_root_schema_for::<ParallelReadParams>();

    #[expect(clippy::expect_used)]
    let schema_value = serde_json::to_value(&schema)
        .expect("ParallelReadParams schema should serialize to JSON");

    let tool_input_schema = serde_json::from_value::<ToolInputSchema>(schema_value)
        .unwrap_or_else(|e| {
            panic!("failed to create Tool schema for parallel_read: {e}");
        });

    Tool {
        name: "parallel_read".to_string(),
        input_schema: tool_input_schema,
        description: Some(
            "Read multiple files in parallel. Accepts an array of file paths and returns \
             the content of each file with error handling for individual failures."
                .to_string()
        ),
        annotations: None,
    }
}

/// Executes the parallel file reading operation
pub async fn execute_parallel_read(params: ParallelReadParams) -> ParallelReadResult {
    let start_time = std::time::Instant::now();
    
    debug!(
        "Starting parallel read of {} files with max_bytes={}, timeout={}ms",
        params.file_paths.len(),
        params.max_bytes_per_file,
        params.timeout_ms
    );

    // Validate input
    if params.file_paths.is_empty() {
        return ParallelReadResult {
            files: HashMap::new(),
            success_count: 0,
            error_count: 0,
            duration_ms: start_time.elapsed().as_millis() as u64,
        };
    }

    if params.file_paths.len() > MAX_PARALLEL_FILES {
        warn!(
            "Requested {} files exceeds maximum of {}, truncating",
            params.file_paths.len(),
            MAX_PARALLEL_FILES
        );
    }

    // Limit number of files to process
    let files_to_process = params.file_paths
        .iter()
        .take(MAX_PARALLEL_FILES)
        .cloned()
        .collect::<Vec<_>>();

    let timeout_duration = Duration::from_millis(params.timeout_ms);
    
    // Create futures for each file read operation
    let read_futures = files_to_process.iter().map(|path| {
        read_single_file(
            path.clone(),
            params.base_dir.as_deref(),
            params.max_bytes_per_file,
        )
    });

    // Execute all reads in parallel with timeout
    let results = match timeout(timeout_duration, join_all(read_futures)).await {
        Ok(results) => results,
        Err(_) => {
            // Timeout occurred - return partial results if possible
            warn!("Parallel read operation timed out after {}ms", params.timeout_ms);
            let mut files = HashMap::new();
            for path in &files_to_process {
                files.insert(path.clone(), FileReadResult {
                    path: path.clone(),
                    content: None,
                    error: Some("Operation timed out".to_string()),
                    size_bytes: None,
                    truncated: false,
                });
            }
            return ParallelReadResult {
                files,
                success_count: 0,
                error_count: files_to_process.len(),
                duration_ms: start_time.elapsed().as_millis() as u64,
            };
        }
    };

    // Collect results
    let mut files = HashMap::new();
    let mut success_count = 0;
    let mut error_count = 0;

    for (i, result) in results.into_iter().enumerate() {
        let original_path = &files_to_process[i];
        if result.error.is_none() {
            success_count += 1;
        } else {
            error_count += 1;
        }
        files.insert(original_path.clone(), result);
    }

    let duration_ms = start_time.elapsed().as_millis() as u64;
    
    debug!(
        "Parallel read completed: {} successful, {} failed, {}ms",
        success_count, error_count, duration_ms
    );

    ParallelReadResult {
        files,
        success_count,
        error_count,
        duration_ms,
    }
}

/// Reads a single file with error handling and size limits
async fn read_single_file(
    path: String,
    base_dir: Option<&str>,
    max_bytes: u64,
) -> FileReadResult {
    // Resolve the full path
    let full_path = if let Some(base) = base_dir {
        Path::new(base).join(&path)
    } else {
        Path::new(&path).to_path_buf()
    };

    let path_str = full_path.to_string_lossy().to_string();
    
    debug!("Reading file: {}", path_str);

    // Check if file exists and get metadata
    let metadata = match fs::metadata(&full_path).await {
        Ok(meta) => meta,
        Err(e) => {
            return FileReadResult {
                path,
                content: None,
                error: Some(format!("Failed to access file: {}", e)),
                size_bytes: None,
                truncated: false,
            };
        }
    };

    // Check if it's a file (not a directory)
    if !metadata.is_file() {
        return FileReadResult {
            path,
            content: None,
            error: Some("Path is not a file".to_string()),
            size_bytes: None,
            truncated: false,
        };
    }

    let file_size = metadata.len();
    let will_truncate = file_size > max_bytes;
    
    // Read the file content
    match fs::read(&full_path).await {
        Ok(bytes) => {
            let bytes_to_use = if will_truncate {
                &bytes[..max_bytes.min(bytes.len() as u64) as usize]
            } else {
                &bytes
            };

            match String::from_utf8(bytes_to_use.to_vec()) {
                Ok(content) => FileReadResult {
                    path,
                    content: Some(content),
                    error: None,
                    size_bytes: Some(file_size),
                    truncated: will_truncate,
                },
                Err(_) => {
                    // File contains non-UTF8 data
                    FileReadResult {
                        path,
                        content: None,
                        error: Some("File contains non-UTF8 data".to_string()),
                        size_bytes: Some(file_size),
                        truncated: false,
                    }
                }
            }
        }
        Err(e) => FileReadResult {
            path,
            content: None,
            error: Some(format!("Failed to read file: {}", e)),
            size_bytes: Some(file_size),
            truncated: false,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    use tokio::fs;

    async fn create_test_file(dir: &TempDir, name: &str, content: &str) -> String {
        let path = dir.path().join(name);
        fs::write(&path, content).await.unwrap();
        path.to_string_lossy().to_string()
    }

    #[tokio::test]
    async fn test_parallel_read_success() {
        let temp_dir = TempDir::new().unwrap();
        
        let file1 = create_test_file(&temp_dir, "file1.txt", "Content 1").await;
        let file2 = create_test_file(&temp_dir, "file2.txt", "Content 2").await;
        
        let params = ParallelReadParams {
            file_paths: vec![file1.clone(), file2.clone()],
            max_bytes_per_file: 1024,
            timeout_ms: 5000,
            base_dir: None,
        };
        
        let result = execute_parallel_read(params).await;
        
        assert_eq!(result.success_count, 2);
        assert_eq!(result.error_count, 0);
        assert_eq!(result.files.len(), 2);
        
        let file1_result = &result.files[&file1];
        assert_eq!(file1_result.content.as_ref().unwrap(), "Content 1");
        assert!(!file1_result.truncated);
        
        let file2_result = &result.files[&file2];
        assert_eq!(file2_result.content.as_ref().unwrap(), "Content 2");
        assert!(!file2_result.truncated);
    }

    #[tokio::test]
    async fn test_parallel_read_with_errors() {
        let temp_dir = TempDir::new().unwrap();
        
        let existing_file = create_test_file(&temp_dir, "exists.txt", "Exists").await;
        let nonexistent_file = temp_dir.path().join("nonexistent.txt").to_string_lossy().to_string();
        
        let params = ParallelReadParams {
            file_paths: vec![existing_file.clone(), nonexistent_file.clone()],
            max_bytes_per_file: 1024,
            timeout_ms: 5000,
            base_dir: None,
        };
        
        let result = execute_parallel_read(params).await;
        
        assert_eq!(result.success_count, 1);
        assert_eq!(result.error_count, 1);
        
        assert!(result.files[&existing_file].error.is_none());
        assert!(result.files[&nonexistent_file].error.is_some());
    }

    #[tokio::test]
    async fn test_size_limiting() {
        let temp_dir = TempDir::new().unwrap();
        
        // Create a file with 1000 bytes
        let large_content = "x".repeat(1000);
        let file_path = create_test_file(&temp_dir, "large.txt", &large_content).await;
        
        let params = ParallelReadParams {
            file_paths: vec![file_path.clone()],
            max_bytes_per_file: 100, // Limit to 100 bytes
            timeout_ms: 5000,
            base_dir: None,
        };
        
        let result = execute_parallel_read(params).await;
        
        assert_eq!(result.success_count, 1);
        let file_result = &result.files[&file_path];
        assert!(file_result.truncated);
        assert_eq!(file_result.content.as_ref().unwrap().len(), 100);
    }
}