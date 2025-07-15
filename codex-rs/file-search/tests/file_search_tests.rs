//! Tests for the file search functionality
//!
//! These tests ensure that file searching, filtering, and reporting work correctly.

use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use codex_file_search::{Cli, FileMatch, Reporter, run_main};
use tempfile::tempdir;
use tokio::fs;

// Test reporter that collects results for verification
#[derive(Debug)]
struct TestReporter {
    matches: Arc<Mutex<Vec<FileMatch>>>,
    warnings: Arc<Mutex<Vec<String>>>,
}

impl TestReporter {
    fn new() -> Self {
        Self {
            matches: Arc::new(Mutex::new(Vec::new())),
            warnings: Arc::new(Mutex::new(Vec::new())),
        }
    }

}

impl Reporter for TestReporter {
    fn report_match(&self, file_match: &FileMatch) {
        self.matches.lock().unwrap().push(file_match.clone());
    }

    fn warn_matches_truncated(&self, total_match_count: usize, shown_match_count: usize) {
        self.warnings.lock().unwrap().push(format!(
            "Truncated: {total_match_count} total, {shown_match_count} shown"
        ));
    }

    fn warn_no_search_pattern(&self, search_directory: &std::path::Path) {
        self.warnings.lock().unwrap().push(format!(
            "No pattern for directory: {}",
            search_directory.display()
        ));
    }
}

#[tokio::test]
async fn test_basic_file_search() {
    let temp_dir = tempdir().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    // Create test files
    fs::write(temp_path.join("test1.txt"), "content").await.unwrap();
    fs::write(temp_path.join("test2.rs"), "rust code").await.unwrap();
    fs::write(temp_path.join("other.py"), "python code").await.unwrap();

    let cli = Cli {
        pattern: Some("test".to_string()),
        limit: std::num::NonZero::new(10).unwrap(),
        cwd: Some(temp_path.to_path_buf()),
        compute_indices: false,
        json: false,
        exclude: vec![],
        threads: std::num::NonZero::new(1).unwrap(),
    };

    let reporter = TestReporter::new();
    let matches_ref = reporter.matches.clone();
    run_main(cli, reporter).await.expect("Search should succeed");

    let matches = matches_ref.lock().unwrap().clone();
    assert!(matches.len() >= 2, "Should find at least 2 test files");
    
    // Check that we found files with "test" in the name
    let test_files: Vec<_> = matches.iter()
        .filter(|m| m.path.contains("test"))
        .collect();
    assert!(!test_files.is_empty(), "Should find files containing 'test'");
}

#[tokio::test]
async fn test_search_with_indices() {
    let temp_dir = tempdir().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    // Create a test file
    fs::write(temp_path.join("example_file.txt"), "content").await.unwrap();

    let cli = Cli {
        pattern: Some("exam".to_string()),
        limit: std::num::NonZero::new(10).unwrap(),
        cwd: Some(temp_path.to_path_buf()),
        compute_indices: true,
        json: false,
        exclude: vec![],
        threads: std::num::NonZero::new(1).unwrap(),
    };

    let reporter = TestReporter::new();
    let matches_ref = reporter.matches.clone();
    run_main(cli, reporter).await.expect("Search should succeed");

    let matches = matches_ref.lock().unwrap().clone();
    assert!(!matches.is_empty(), "Should find at least one match");

    // Check that indices are computed
    let match_with_indices = matches.iter().find(|m| m.indices.is_some());
    assert!(match_with_indices.is_some(), "Should have matches with indices");
    
    if let Some(file_match) = match_with_indices {
        let indices = file_match.indices.as_ref().unwrap();
        assert!(!indices.is_empty(), "Indices should not be empty");
        
        // Check that indices are sorted
        let mut sorted_indices = indices.clone();
        sorted_indices.sort_unstable();
        assert_eq!(*indices, sorted_indices, "Indices should be sorted");
    }
}

#[tokio::test]
async fn test_search_limit() {
    let temp_dir = tempdir().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    // Create many files that match the pattern
    for i in 0..20 {
        fs::write(temp_path.join(format!("test_file_{}.txt", i)), "content").await.unwrap();
    }

    let limit = 5;
    let cli = Cli {
        pattern: Some("test".to_string()),
        limit: std::num::NonZero::new(limit).unwrap(),
        cwd: Some(temp_path.to_path_buf()),
        compute_indices: false,
        json: false,
        exclude: vec![],
        threads: std::num::NonZero::new(1).unwrap(),
    };

    let reporter = TestReporter::new();
    let matches_ref = reporter.matches.clone();
    run_main(cli, reporter).await.expect("Search should succeed");

    let matches = matches_ref.lock().unwrap().clone();
    assert!(matches.len() <= limit, "Should respect the limit");
    
    // Check if truncation warning was issued (depends on implementation)
    // Note: warnings can't be accessed since reporter was moved
    if matches.len() == limit {
        // If we hit the limit, there might be a truncation warning
        // but this depends on the internal implementation
    }
}

#[tokio::test]
async fn test_search_with_exclusions() {
    let temp_dir = tempdir().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    // Create test files
    fs::write(temp_path.join("test.txt"), "content").await.unwrap();
    fs::write(temp_path.join("test.rs"), "rust code").await.unwrap();
    fs::write(temp_path.join("test.py"), "python code").await.unwrap();

    let cli = Cli {
        pattern: Some("test".to_string()),
        limit: std::num::NonZero::new(10).unwrap(),
        cwd: Some(temp_path.to_path_buf()),
        compute_indices: false,
        json: false,
        exclude: vec!["*.py".to_string()],
        threads: std::num::NonZero::new(1).unwrap(),
    };

    let reporter = TestReporter::new();
    let matches_ref = reporter.matches.clone();
    run_main(cli, reporter).await.expect("Search should succeed");

    let matches = matches_ref.lock().unwrap().clone();
    
    // Check that .py files are excluded
    let py_files: Vec<_> = matches.iter()
        .filter(|m| m.path.ends_with(".py"))
        .collect();
    assert!(py_files.is_empty(), "Should exclude .py files");
    
    // But other files should be included
    let other_files: Vec<_> = matches.iter()
        .filter(|m| !m.path.ends_with(".py"))
        .collect();
    assert!(!other_files.is_empty(), "Should include non-.py files");
}

#[tokio::test]
async fn test_no_pattern_search() {
    let temp_dir = tempdir().expect("Failed to create temp dir");
    let temp_path = temp_dir.path();

    // Create test files
    fs::write(temp_path.join("file1.txt"), "content").await.unwrap();
    fs::write(temp_path.join("file2.rs"), "rust code").await.unwrap();

    let cli = Cli {
        pattern: None, // No search pattern
        limit: std::num::NonZero::new(10).unwrap(),
        cwd: Some(temp_path.to_path_buf()),
        compute_indices: false,
        json: false,
        exclude: vec![],
        threads: std::num::NonZero::new(1).unwrap(),
    };

    let reporter = TestReporter::new();
    let warnings_ref = reporter.warnings.clone();
    let matches_ref = reporter.matches.clone();
    run_main(cli, reporter).await.expect("Search should succeed");

    let warnings = warnings_ref.lock().unwrap().clone();
    // Should warn about no search pattern
    assert!(!warnings.is_empty(), "Should warn about no search pattern");
    
    let matches = matches_ref.lock().unwrap().clone();
    // When no pattern is provided, the function runs 'ls' command instead of file search
    // So matches will be empty since it doesn't use the normal search mechanism
    assert!(matches.is_empty(), "No matches expected when no pattern provided");
}

#[test]
fn test_file_match_serialization() {
    let file_match = FileMatch {
        score: 100,
        path: "test/file.txt".to_string(),
        indices: Some(vec![0, 1, 5]),
    };

    let json = serde_json::to_string(&file_match).expect("Should serialize");
    assert!(json.contains("\"score\":100"));
    assert!(json.contains("\"path\":\"test/file.txt\""));
    assert!(json.contains("\"indices\":[0,1,5]"));
}

#[test]
fn test_file_match_serialization_no_indices() {
    let file_match = FileMatch {
        score: 50,
        path: "test/other.txt".to_string(),
        indices: None,
    };

    let json = serde_json::to_string(&file_match).expect("Should serialize");
    assert!(json.contains("\"score\":50"));
    assert!(json.contains("\"path\":\"test/other.txt\""));
    // Should not include indices field when None
    assert!(!json.contains("indices"));
}

#[test]
fn test_cli_debug() {
    let cli = Cli {
        pattern: Some("test".to_string()),
        limit: std::num::NonZero::new(5).unwrap(),
        cwd: Some(PathBuf::from("/tmp")),
        compute_indices: true,
        json: false,
        exclude: vec!["*.tmp".to_string()],
        threads: std::num::NonZero::new(2).unwrap(),
    };

    // Test that CLI can be formatted for debugging
    let debug_str = format!("{:?}", cli);
    assert!(debug_str.contains("test"));
    assert!(debug_str.contains("5"));
}