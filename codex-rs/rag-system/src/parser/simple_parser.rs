use anyhow::Result;
use std::path::Path;
use regex::Regex;

use crate::types::*;

/// Simplified code parser for basic language support
pub struct SimpleParser;

impl SimpleParser {
    pub fn new() -> Self {
        Self
    }

    pub fn parse(
        &self,
        content: &str,
        file_path: &Path,
    ) -> Result<CodeUnit> {
        let language = self.detect_language(file_path);
        
        let dependencies = self.extract_imports_simple(content, &language);
        let functions = self.extract_functions_simple(content, &language);
        let structs = self.extract_structs_simple(content, &language);
        
        Ok(CodeUnit {
            file_path: file_path.to_path_buf(),
            language,
            content: content.to_string(),
            ast_context: AstContext::default(),
            dependencies: dependencies.clone(),
            functions,
            structs,
            imports: dependencies,
        })
    }

    fn detect_language(&self,
        path: &Path,
    ) -> Language {
        match path.extension().and_then(|s| s.to_str()) {
            Some("rs") => Language::Rust,
            Some("kt") => Language::Kotlin,
            Some("java") => Language::Java,
            Some(ext) => Language::Other(ext.to_string()),
            None => Language::Other("unknown".to_string()),
        }
    }

    fn extract_imports_simple(
        &self,
        content: &str,
        language: &Language,
    ) -> Vec<String> {
        let mut imports = Vec::new();
        
        let patterns = match language {
            Language::Rust => vec![r"use\s+([\w:]+)", r"extern\s+crate\s+(\w+)"],
            Language::Kotlin => vec![r"import\s+([\w.]+)", r"package\s+([\w.]+)"],
            Language::Java => vec![r"import\s+([\w.]+)", r"package\s+([\w.]+)"],
            Language::Other(_) => vec![],
        };

        for pattern in patterns {
            let re = Regex::new(pattern).unwrap();
            for cap in re.captures_iter(content) {
                if let Some(import) = cap.get(1) {
                    imports.push(import.as_str().to_string());
                }
            }
        }

        imports
    }

    fn extract_functions_simple(
        &self,
        content: &str,
        language: &Language,
    ) -> Vec<FunctionInfo> {
        let mut functions = Vec::new();
        
        let patterns = match language {
            Language::Rust => vec![r"fn\s+(\w+)\s*\(([^)]*)\)"],
            Language::Kotlin => vec![r"fun\s+(\w+)\s*\(([^)]*)\)"],
            Language::Java => vec![r"\w+\s+(\w+)\s*\(([^)]*)\)\s*\{"],
            Language::Other(_) => vec![],
        };

        for pattern in patterns {
            let re = Regex::new(pattern).unwrap();
            for cap in re.captures_iter(content) {
                let name = cap[1].to_string();
                let params_str = &cap[2];
                let parameters = self.parse_parameters_simple(params_str);
                
                functions.push(FunctionInfo {
                    name,
                    signature: cap[0].to_string(),
                    doc_comment: None,
                    parameters,
                    return_type: None,
                    is_public: true,
                });
            }
        }

        functions
    }

    fn extract_structs_simple(
        &self,
        content: &str,
        language: &Language,
    ) -> Vec<StructInfo> {
        let mut structs = Vec::new();
        
        let patterns = match language {
            Language::Rust => vec![r"struct\s+(\w+)", r"enum\s+(\w+)", r"trait\s+(\w+)"],
            Language::Kotlin => vec![r"class\s+(\w+)", r"interface\s+(\w+)"],
            Language::Java => vec![r"class\s+(\w+)", r"interface\s+(\w+)", r"enum\s+(\w+)"],
            Language::Other(_) => vec![],
        };

        for pattern in patterns {
            let re = Regex::new(pattern).unwrap();
            for cap in re.captures_iter(content) {
                structs.push(StructInfo {
                    name: cap[1].to_string(),
                    fields: Vec::new(),
                    doc_comment: None,
                    visibility: Visibility::Public,
                });
            }
        }

        structs
    }

    fn parse_parameters_simple(&self,
        params_str: &str,
    ) -> Vec<ParameterInfo> {
        let mut parameters = Vec::new();
        
        for param in params_str.split(',') {
            let param = param.trim();
            if param.is_empty() {
                continue;
            }
            
            let parts: Vec<&str> = param.split(':').collect();
            if parts.len() == 2 {
                parameters.push(ParameterInfo {
                    name: parts[0].trim().to_string(),
                    type_name: parts[1].trim().to_string(),
                    is_optional: false,
                });
            }
        }
        
        parameters
    }
}

impl Default for SimpleParser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_parser() {
        let parser = SimpleParser::new();
        
        let rust_code = r#"
            use std::io;
            
            struct Person {
                name: String,
                age: u32,
            }
            
            fn add(a: i32, b: i32) -> i32 {
                a + b
            }
        "#;
        
        let result = parser.parse(rust_code, Path::new("test.rs")).unwrap();
        assert_eq!(result.language, Language::Rust);
        assert_eq!(result.functions.len(), 1);
        assert_eq!(result.structs.len(), 1);
        assert!(result.dependencies.iter().any(|d| d.contains("std::io")));
    }
}