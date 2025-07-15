use anyhow::Result;
use std::collections::HashMap;

use crate::types::{CodeUnit, Language};

/// Semantic analyzer for code understanding
pub struct SemanticAnalyzer {
    rust_patterns: Vec<String>,
    kotlin_patterns: Vec<String>,
    java_patterns: Vec<String>,
    importance_weights: HashMap<String, f32>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut rust_patterns = vec![
            "unsafe", "async", "await", "dyn", "impl", "trait", "enum", "struct",
            "fn", "pub", "priv", "crate", "super", "extern", "macro",
            "Result", "Option", "Vec", "HashMap", "String", "str",
            "libp2p", "tokio", "serde", "anyhow", "async-trait",
        ];

        let mut kotlin_patterns = vec![
            "suspend", "fun", "class", "interface", "object", "data", "sealed",
            "val", "var", "const", "lateinit", "abstract", "open", "final",
            "override", "private", "protected", "public", "internal",
            "List", "MutableList", "Map", "MutableMap", "String", "Int", "Long",
            "coroutine", "flow", "liveData", "viewModel", "repository",
        ];

        let mut java_patterns = vec![
            "public", "private", "protected", "static", "final", "abstract",
            "synchronized", "volatile", "transient", "native", "strictfp",
            "class", "interface", "enum", "extends", "implements", "throws",
            "List", "Map", "Set", "String", "Integer", "Long", "Double",
            "Android", "Activity", "Fragment", "Service", "BroadcastReceiver",
        ];

        let mut importance_weights = HashMap::new();
        importance_weights.insert("libp2p".to_string(), 2.0);
        importance_weights.insert("p2p".to_string(), 2.0);
        importance_weights.insert("network".to_string(), 1.5);
        importance_weights.insert("crypto".to_string(), 1.5);
        importance_weights.insert("quic".to_string(), 1.8);
        importance_weights.insert("bootstrap".to_string(), 1.5);
        importance_weights.insert("connection".to_string(), 1.3);
        importance_weights.insert("discovery".to_string(), 1.3);
        importance_weights.insert("transport".to_string(), 1.3);

        Self {
            rust_patterns: rust_patterns.into_iter().map(String::from).collect(),
            kotlin_patterns: kotlin_patterns.into_iter().map(String::from).collect(),
            java_patterns: java_patterns.into_iter().map(String::from).collect(),
            importance_weights,
        }
    }

    /// Analyze code unit and create semantic representation
    pub fn analyze(&self,
        code_unit: &CodeUnit,
    ) -> String {
        let mut representation = String::new();

        // Add file path as context
        representation.push_str(&format!("File: {}\n", 
            code_unit.file_path.display()));

        // Add language context
        representation.push_str(&format!("Language: {:?}\n", code_unit.language));

        // Add package/module context
        if let Some(package) = &code_unit.ast_context.package_name {
            representation.push_str(&format!("Package: {}\n", package));
        }

        // Add semantic summary
        let semantic_summary = self.create_semantic_summary(code_unit);
        representation.push_str(&format!("\nSemantic Summary:\n{}", semantic_summary));

        // Add important patterns
        let patterns = self.extract_patterns(code_unit);
        if !patterns.is_empty() {
            representation.push_str(&format!("\nKey Patterns:\n{}\n", 
                patterns.join(", ")));
        }

        // Add API documentation
        let api_docs = self.generate_api_docs(code_unit);
        if !api_docs.is_empty() {
            representation.push_str(&format!("\nAPI Documentation:\n{}", api_docs));
        }

        // Add code content with context
        representation.push_str(&format!("\nCode:\n{}", code_unit.content));

        representation
    }

    /// Create semantic summary
    fn create_semantic_summary(
        &self,
        code_unit: &CodeUnit,
    ) -> String {
        let mut summary = String::new();

        // Functions summary
        if !code_unit.functions.is_empty() {
            summary.push_str("Functions:\n");
            for func in &code_unit.functions {
                let importance = self.calculate_function_importance(func);
                let stars = "*".repeat((importance * 3.0) as usize);
                summary.push_str(&format!("  {} {} {}", 
                    stars, func.name, func.signature));
                
                if let Some(doc) = &func.doc_comment {
                    summary.push_str(&format!(" - {}", doc.lines().next().unwrap_or("")));
                }
                summary.push('\n');
            }
        }

        // Types summary
        if !code_unit.structs.is_empty() {
            summary.push_str("\nTypes:\n");
            for struct_def in &code_unit.structs {
                let importance = self.calculate_type_importance(struct_def);
                let stars = "*".repeat((importance * 3.0) as usize);
                summary.push_str(&format!("  {} {} - {}", 
                    stars, struct_def.name, 
                    if struct_def.fields.len() < 5 {
                        format!("{} fields", struct_def.fields.len())
                    } else {
                        format!("{}+ fields", struct_def.fields.len())
                    }));
                
                if let Some(doc) = &struct_def.doc_comment {
                    summary.push_str(&format!(" - {}", doc.lines().next().unwrap_or("")));
                }
                summary.push('\n');
            }
        }

        // Dependencies summary
        if !code_unit.dependencies.is_empty() {
            summary.push_str("\nDependencies:\n");
            for dep in &code_unit.dependencies {
                let importance = self.calculate_dependency_importance(dep);
                let stars = "*".repeat((importance * 3.0) as usize);
                summary.push_str(&format!("  {} {}\n", stars, dep));
            }
        }

        summary
    }

    /// Extract patterns from code
    fn extract_patterns(&self,
        code_unit: &CodeUnit,
    ) -> Vec<String> {
        let mut patterns = Vec::new();
        let content = &code_unit.content.to_lowercase();

        let patterns_to_check = match code_unit.language {
            Language::Rust => &self.rust_patterns,
            Language::Kotlin => &self.kotlin_patterns,
            Language::Java => &self.java_patterns,
            Language::Other(_) => &vec![],
        };

        for pattern in patterns_to_check {
            if content.contains(&pattern.to_lowercase()) {
                patterns.push(pattern.clone());
            }
        }

        patterns
    }

    /// Generate API documentation
    fn generate_api_docs(
        &self,
        code_unit: &CodeUnit,
    ) -> String {
        let mut docs = String::new();

        // Generate function docs
        for func in &code_unit.functions {
            if func.is_public {
                docs.push_str(&format!("### {}\n", func.name));
                docs.push_str(&format!("```\n{}\n```\n", func.signature));
                
                if let Some(doc) = &func.doc_comment {
                    docs.push_str(&format!("{}", doc));
                    docs.push('\n');
                }
                
                if !func.parameters.is_empty() {
                    docs.push_str("\nParameters:\n");
                    for param in &func.parameters {
                        docs.push_str(&format!("- `{}: {}`\n", param.name, param.type_name));
                    }
                }
                
                if let Some(return_type) = &func.return_type {
                    docs.push_str(&format!("\nReturns: `{}`\n", return_type));
                }
                
                docs.push('\n');
            }
        }

        // Generate type docs
        for struct_def in &code_unit.structs {
            if let Visibility::Public = struct_def.visibility {
                docs.push_str(&format!("### {}\n", struct_def.name));
                
                if let Some(doc) = &struct_def.doc_comment {
                    docs.push_str(doc);
                    docs.push('\n');
                }
                
                if !struct_def.fields.is_empty() {
                    docs.push_str("\nFields:\n");
                    for field in &struct_def.fields {
                        let visibility = if field.is_public { "pub" } else { "private" };
                        docs.push_str(&format!("- `{} {}: {}`\n", visibility, field.name, field.type_name));
                    }
                }
                
                docs.push('\n');
            }
        }

        docs
    }

    /// Calculate function importance based on various factors
    fn calculate_function_importance(
        &self,
        func: &crate::types::FunctionInfo,
    ) -> f32 {
        let mut importance = 1.0;

        // Public functions are more important
        if func.is_public {
            importance *= 1.5;
        }

        // Functions with documentation are more important
        if func.doc_comment.is_some() {
            importance *= 1.2;
        }

        // Complex functions (more parameters) might be more important
        importance *= 1.0 + (func.parameters.len() as f32 * 0.1);

        // Check for important keywords in name and signature
        let combined_text = format!("{} {}", func.name, func.signature).to_lowercase();
        for (keyword, weight) in &self.importance_weights {
            if combined_text.contains(keyword) {
                importance *= weight;
            }
        }

        importance.min(5.0) // Cap at 5.0
    }

    /// Calculate type importance
    fn calculate_type_importance(
        &self,
        struct_def: &crate::types::StructInfo,
    ) -> f32 {
        let mut importance = 1.0;

        // Public types are more important
        if let Visibility::Public = struct_def.visibility {
            importance *= 1.5;
        }

        // Types with documentation are more important
        if struct_def.doc_comment.is_some() {
            importance *= 1.2;
        }

        // Types with more fields might be more important
        importance *= 1.0 + (struct_def.fields.len() as f32 * 0.05);

        // Check for important keywords in name
        let name_lower = struct_def.name.to_lowercase();
        for (keyword, weight) in &self.importance_weights {
            if name_lower.contains(keyword) {
                importance *= weight;
            }
        }

        importance.min(5.0)
    }

    /// Calculate dependency importance
    fn calculate_dependency_importance(
        &self,
        dependency: &str,
    ) -> f32 {
        let mut importance = 1.0;

        // Core dependencies are more important
        let dep_lower = dependency.to_lowercase();
        for (keyword, weight) in &self.importance_weights {
            if dep_lower.contains(keyword) {
                importance *= weight;
            }
        }

        // Standard library dependencies
        if dep_lower.starts_with("std::") || dep_lower.starts_with("java.") || dep_lower.starts_with("kotlin.") {
            importance *= 0.8; // Standard library is less important
        }

        importance.min(3.0)
    }

    /// Extract semantic features for embedding
    pub fn extract_features(
        &self,
        code_unit: &CodeUnit,
    ) -> Vec<f32> {
        let mut features = Vec::new();

        // Language features
        let lang_feature = match code_unit.language {
            Language::Rust => 1.0,
            Language::Kotlin => 2.0,
            Language::Java => 3.0,
            Language::Other(_) => 0.0,
        };
        features.push(lang_feature);

        // Code complexity features
        features.push(code_unit.functions.len() as f32);
        features.push(code_unit.structs.len() as f32);
        features.push(code_unit.dependencies.len() as f32);

        // Documentation features
        let doc_ratio = code_unit.functions.iter()
            .filter(|f| f.doc_comment.is_some())
            .count() as f32 / code_unit.functions.len().max(1) as f32;
        features.push(doc_ratio);

        // Public API ratio
        let public_ratio = code_unit.functions.iter()
            .filter(|f| f.is_public)
            .count() as f32 / code_unit.functions.len().max(1) as f32;
        features.push(public_ratio);

        // Average complexity
        let avg_params = code_unit.functions.iter()
            .map(|f| f.parameters.len() as f32)
            .sum::<f32>() / code_unit.functions.len().max(1) as f32;
        features.push(avg_params);

        // Import patterns
        let std_imports = code_unit.dependencies.iter()
            .filter(|d| d.starts_with("std::") || d.starts_with("java.") || d.starts_with("kotlin."))
            .count() as f32;
        features.push(std_imports);

        features
    }
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_semantic_summary() {
        let analyzer = SemanticAnalyzer::new();
        
        let code_unit = crate::types::CodeUnit {
            file_path: std::path::PathBuf::from("test.rs"),
            language: Language::Rust,
            content: "fn add(a: i32, b: i32) -> i32 { a + b }".to_string(),
            ast_context: crate::types::AstContext::default(),
            dependencies: vec!["std::io".to_string()],
            functions: vec![crate::types::FunctionInfo {
                name: "add".to_string(),
                signature: "fn add(a: i32, b: i32) -> i32".to_string(),
                doc_comment: Some("Adds two numbers".to_string()),
                parameters: vec![
                    crate::types::ParameterInfo {
                        name: "a".to_string(),
                        type_name: "i32".to_string(),
                        is_optional: false,
                    },
                    crate::types::ParameterInfo {
                        name: "b".to_string(),
                        type_name: "i32".to_string(),
                        is_optional: false,
                    },
                ],
                return_type: Some("i32".to_string()),
                is_public: true,
            }],
            structs: vec![],
            imports: vec![],
        };

        let summary = analyzer.create_semantic_summary(&code_unit);
        assert!(summary.contains("Functions:"));
        assert!(summary.contains("add"));
    }

    #[test]
    fn test_extract_patterns() {
        let analyzer = SemanticAnalyzer::new();
        
        let code_unit = crate::types::CodeUnit {
            file_path: std::path::PathBuf::from("test.rs"),
            language: Language::Rust,
            content: "use libp2p::PeerId;".to_string(),
            ast_context: crate::types::AstContext::default(),
            dependencies: vec![],
            functions: vec![],
            structs: vec![],
            imports: vec![],
        };

        let patterns = analyzer.extract_patterns(&code_unit);
        assert!(patterns.contains(&"libp2p".to_string()));
    }
}