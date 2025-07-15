//! Safe I/O utilities with async support
//! 
//! This module provides safe alternatives to unwrap() calls when working with file operations.

#[cfg(feature = "io-utils")]
use anyhow::{Context, Result};
#[cfg(feature = "io-utils")]
use std::path::Path;
#[cfg(feature = "io-utils")]
use tokio::fs;

/// Safely read file contents to string with proper error context
#[cfg(feature = "io-utils")]
pub async fn read_to_string<P: AsRef<Path>>(path: P) -> Result<String> {
    let path = path.as_ref();
    fs::read_to_string(path)
        .await
        .with_context(|| format!("Failed to read file: {}", path.display()))
}

/// Safely read file contents to bytes with proper error context
#[cfg(feature = "io-utils")]
pub async fn read_to_bytes<P: AsRef<Path>>(path: P) -> Result<Vec<u8>> {
    let path = path.as_ref();
    fs::read(path)
        .await
        .with_context(|| format!("Failed to read file: {}", path.display()))
}

/// Safely write string contents to file with proper error context
#[cfg(feature = "io-utils")]
pub async fn write_string<P: AsRef<Path>>(path: P, contents: &str) -> Result<()> {
    let path = path.as_ref();
    fs::write(path, contents)
        .await
        .with_context(|| format!("Failed to write file: {}", path.display()))
}

/// Safely write bytes to file with proper error context
#[cfg(feature = "io-utils")]
pub async fn write_bytes<P: AsRef<Path>>(path: P, contents: &[u8]) -> Result<()> {
    let path = path.as_ref();
    fs::write(path, contents)
        .await
        .with_context(|| format!("Failed to write file: {}", path.display()))
}

/// Check if a path exists
#[cfg(feature = "io-utils")]
pub async fn path_exists<P: AsRef<Path>>(path: P) -> bool {
    fs::metadata(path).await.is_ok()
}

/// Safely create directory and all parent directories
#[cfg(feature = "io-utils")]
pub async fn create_dir_all<P: AsRef<Path>>(path: P) -> Result<()> {
    let path = path.as_ref();
    fs::create_dir_all(path)
        .await
        .with_context(|| format!("Failed to create directory: {}", path.display()))
}

#[cfg(test)]
#[cfg(feature = "io-utils")]
mod tests {
    use super::*;
    use tempfile::tempdir;
    use tokio;

    #[tokio::test]
    async fn test_read_write_roundtrip() {
        let temp_dir = tempdir().expect("Failed to create temp dir");
        let file_path = temp_dir.path().join("test.txt");
        let test_content = "Hello, world!";

        // Write content
        write_string(&file_path, test_content)
            .await
            .expect("Write should succeed");

        // Read content back
        let read_content = read_to_string(&file_path)
            .await
            .expect("Read should succeed");

        assert_eq!(test_content, read_content);
    }

    #[tokio::test]
    async fn test_path_exists() {
        let temp_dir = tempdir().expect("Failed to create temp dir");
        let existing_path = temp_dir.path();
        let non_existing_path = temp_dir.path().join("non_existent");

        assert!(path_exists(existing_path).await);
        assert!(!path_exists(non_existing_path).await);
    }

    #[tokio::test]
    async fn test_create_dir_all() {
        let temp_dir = tempdir().expect("Failed to create temp dir");
        let nested_path = temp_dir.path().join("a").join("b").join("c");

        create_dir_all(&nested_path)
            .await
            .expect("Create dir all should succeed");

        assert!(path_exists(&nested_path).await);
    }
}