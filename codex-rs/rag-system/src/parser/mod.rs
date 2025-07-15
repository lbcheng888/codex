use anyhow::Result;
use std::path::{Path, PathBuf};
use tokio::fs;
use ignore::WalkBuilder;
use rayon::prelude::*;
use regex::Regex;

use crate::types::*;

mod simple_parser;

pub use simple_parser::SimpleParser;

/// Main code parser supporting multiple languages
pub struct CodeParser {
    parser: SimpleParser,
    include_patterns: Vec<String>,
    exclude_patterns: Vec<String>,
}

impl CodeParser {
    pub fn new() -> Self {
        Self {
            parser: SimpleParser::new(),
            include_patterns: vec![
                "**/*.rs".to_string(),
                "**/*.kt".to_string(),
                "**/*.java".to_string(),
            ],
            exclude_patterns: vec![
                "**/target/**".to_string(),
                "**/build/**".to_string(),
                "**/.git/**".to_string(),
                "**/node_modules/**".to_string(),
            ],
        }
    }

    /// Discover all code files in a directory
    pub async fn discover_files(&self,
        root_path: &Path,
    ) -> Result<Vec<PathBuf>> {
        let walker = WalkBuilder::new(root_path)
            .add_custom_ignore_filename(".ragignore")
            .git_ignore(true)
            .build();

        let mut files = Vec::new();
        
        for entry in walker {
            if let Ok(entry) = entry {
                let path = entry.path();
                // Only include regular files (skip directories, symlinks to dirs, etc.)
                if entry.file_type().map(|ft| ft.is_file()).unwrap_or(false) {
                    if self.should_include_file(path) {
                        files.push(path.to_path_buf());
                    }
                }
            }
        }

        Ok(files)
    }

    /// Parse a single file and extract code information
    pub async fn parse_file(&self,
        file_path: &Path,
    ) -> Result<CodeUnit> {
        let content = fs::read_to_string(file_path).await?;
        let language = self.detect_language(file_path);
        
        let code_unit = self.parser.parse(&content, file_path)?;

        Ok(code_unit)
    }

    /// Parse multiple files in parallel
    pub async fn parse_files_parallel(
        &self,
        files: &[PathBuf],
    ) -> Result<Vec<CodeUnit>> {
        let results: Result<Vec<_>> = files
            .par_iter()
            .map(|file| {
                let rt = tokio::runtime::Handle::current();
                rt.block_on(self.parse_file(file))
            })
            .collect();

        Ok(results?.into_iter().collect())
    }

    /// Detect language based on file extension
    fn detect_language(&self,
        path: &Path,
    ) -> Language {
        match path.extension().and_then(|s| s.to_str()) {
            Some("rs") => Language::Rust,
            Some("kt") => Language::Kotlin,
            Some("kts") => Language::Kotlin,
            Some("java") => Language::Java,
            Some(ext) => Language::Other(ext.to_string()),
            None => Language::Other("unknown".to_string()),
        }
    }

    /// Check if file should be included based on patterns
    fn should_include_file(&self,
        path: &Path,
    ) -> bool {
        let path_str = path.to_string_lossy();

        // Check exclude patterns
        for pattern in &self.exclude_patterns {
            if self.match_pattern(pattern, &path_str) {
                return false;
            }
        }

        // Check include patterns
        for pattern in &self.include_patterns {
            if self.match_pattern(pattern, &path_str) {
                return true;
            }
        }

        false
    }

    /// Simple pattern matching for file patterns
    fn match_pattern(&self, pattern: &str, path: &str) -> bool {
        let pattern = pattern.replace("**/*", ".*");
        let pattern = pattern.replace("*", ".*");
        let re = Regex::new(&pattern).unwrap();
        re.is_match(path)
    }

    /// Generic parser for unsupported languages
    fn parse_generic(
        &self,
        content: &str,
        file_path: &Path,
        language: &Language,
    ) -> Result<CodeUnit> {
        let _lines: Vec<&str> = content.lines().collect();
        let dependencies = self.extract_imports_generic(content);
        let functions = self.extract_functions_generic(content);
        let structs = self.extract_structs_generic(content);

        Ok(CodeUnit {
            file_path: file_path.to_path_buf(),
            language: language.clone(),
            content: content.to_string(),
            ast_context: AstContext {
                module_path: None,
                package_name: None,
                public_symbols: Vec::new(),
                type_definitions: Vec::new(),
            },
            dependencies,
            functions,
            structs,
            imports: Vec::new(),
        })
    }

    fn extract_imports_generic(&self,
        content: &str,
    ) -> Vec<String> {
        let re = regex::Regex::new(r"(?m)^(use|import|package)\s+([\w:]+)").unwrap();
        re.captures_iter(content)
            .filter_map(|cap| cap.get(2))
            .map(|m| m.as_str().to_string())
            .collect()
    }

    fn extract_functions_generic(
        &self,
        content: &str,
    ) -> Vec<FunctionInfo> {
        let re = regex::Regex::new(
            r"(?m)(?:pub\s+)?(?:fn|func|fun)\s+(\w+)\s*\(([^)]*)\)\s*(?:->\s*(\w+))?"
        ).unwrap();
        
        re.captures_iter(content)
            .map(|cap| FunctionInfo {
                name: cap[1].to_string(),
                signature: cap[0].to_string(),
                doc_comment: None,
                parameters: self.parse_parameters_generic(&cap[2]),
                return_type: cap.get(3).map(|m| m.as_str().to_string()),
                is_public: cap[0].starts_with("pub"),
            })
            .collect()
    }

    fn extract_structs_generic(
        &self,
        content: &str,
    ) -> Vec<StructInfo> {
        let re = regex::Regex::new(
            r"(?m)(?:pub\s+)?(?:struct|class|interface)\s+(\w+)"
        ).unwrap();
        
        re.captures_iter(content)
            .map(|cap| StructInfo {
                name: cap[1].to_string(),
                fields: Vec::new(),
                doc_comment: None,
                visibility: if cap[0].starts_with("pub") {
                    Visibility::Public
                } else {
                    Visibility::Private
                },
            })
            .collect()
    }

    fn parse_parameters_generic(&self,
        params_str: &str,
    ) -> Vec<ParameterInfo> {
        params_str.split(',')
            .map(|param| {
                let parts: Vec<&str> = param.split(':').collect();
                if parts.len() == 2 {
                    ParameterInfo {
                        name: parts[0].trim().to_string(),
                        type_name: parts[1].trim().to_string(),
                        is_optional: parts[1].contains('?'),
                    }
                } else {
                    ParameterInfo {
                        name: parts[0].trim().to_string(),
                        type_name: "unknown".to_string(),
                        is_optional: false,
                    }
                }
            })
            .collect()
    }
}

impl Default for CodeParser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
use tempfile::tempdir;

    #[tokio::test]
    async fn test_discover_files() {
        let dir = tempdir().unwrap();
        let rust_file = dir.path().join("test.rs");
        let kt_file = dir.path().join("test.kt");
        
        tokio::fs::write(&rust_file, "fn main() {}").await.unwrap();
        tokio::fs::write(&kt_file, "fun main() {}").await.unwrap();
        
        let parser = CodeParser::new();
        let files = parser.discover_files(dir.path()).await.unwrap();
        
        assert_eq!(files.len(), 2);
        assert!(files.contains(&rust_file));
        assert!(files.contains(&kt_file));
    }

    #[tokio::test]
    async fn test_detect_language() {
        let parser = CodeParser::new();
        
        assert_eq!(parser.detect_language(Path::new("test.rs")), Language::Rust);
        assert_eq!(parser.detect_language(Path::new("test.kt")), Language::Kotlin);
        assert_eq!(parser.detect_language(Path::new("test.java")), Language::Java);
        assert_eq!(parser.detect_language(Path::new("test.py")), Language::Other("py".to_string()));
    }
}
