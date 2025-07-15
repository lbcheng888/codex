use anyhow::Result;
use tokenizers::Tokenizer;
use std::collections::HashMap;

use crate::types::Language;

/// Specialized tokenizer for code
pub struct CodeTokenizer {
    base_tokenizer: Tokenizer,
    language_keywords: HashMap<Language, Vec<String>>,
    special_tokens: HashMap<String, u32>,
}

impl CodeTokenizer {
    pub fn new(base_tokenizer: Tokenizer) -> Self {
        let mut language_keywords = HashMap::new();
        
        // Rust keywords
        language_keywords.insert(Language::Rust, vec![
            "fn", "struct", "enum", "impl", "trait", "use", "mod", "pub", "priv",
            "const", "static", "let", "mut", "ref", "match", "if", "else", "loop",
            "while", "for", "in", "break", "continue", "return", "unsafe", "async",
            "await", "dyn", "dyn", "Self", "self", "super", "crate", "extern",
        ].into_iter().map(String::from).collect());

        // Kotlin keywords
        language_keywords.insert(Language::Kotlin, vec![
            "fun", "class", "interface", "object", "val", "var", "const", "lateinit",
            "abstract", "open", "final", "sealed", "override", "private", "protected",
            "public", "internal", "import", "package", "return", "break", "continue",
            "throw", "try", "catch", "finally", "if", "else", "when", "for", "while",
            "do", "as", "is", "in", "!in", "this", "super", "null", "true", "false",
        ].into_iter().map(String::from).collect());

        // Java keywords
        language_keywords.insert(Language::Java, vec![
            "class", "interface", "extends", "implements", "public", "private",
            "protected", "static", "final", "abstract", "synchronized", "volatile",
            "transient", "native", "strictfp", "package", "import", "return",
            "if", "else", "switch", "case", "default", "while", "do", "for",
            "break", "continue", "try", "catch", "finally", "throw", "throws",
            "new", "this", "super", "null", "true", "false", "instanceof",
        ].into_iter().map(String::from).collect());

        let mut special_tokens = HashMap::new();
        special_tokens.insert("[CLS]".to_string(), 101);
        special_tokens.insert("[SEP]".to_string(), 102);
        special_tokens.insert("[PAD]".to_string(), 0);
        special_tokens.insert("[UNK]".to_string(), 100);
        special_tokens.insert("[MASK]".to_string(), 103);

        Self {
            base_tokenizer,
            language_keywords,
            special_tokens,
        }
    }

    /// Tokenize code with language-specific handling
    pub fn tokenize_code(
        &self,
        code: &str,
        language: Language,
    ) -> Result<Vec<u32>> {
        // Preprocess code based on language
        let preprocessed = self.preprocess_code(code, language);
        
        // Tokenize with special handling for code
        let encoding = self.base_tokenizer
            .encode(preprocessed, false)
            .map_err(|e| anyhow::anyhow!("Tokenization error: {}", e))?;

        let mut tokens = encoding.get_ids().to_vec();
        
        // Add language-specific tokens
        tokens = self.add_language_context(tokens, language);
        
        Ok(tokens)
    }

    /// Create semantic representation for embedding
    pub fn create_semantic_representation(
        &self,
        code_unit: &crate::types::CodeUnit,
    ) -> String {
        let mut representation = String::new();
        
        // Add file metadata
        representation.push_str(&format!("Language: {:?}\n", code_unit.language));
        if let Some(package) = &code_unit.ast_context.package_name {
            representation.push_str(&format!("Package: {}\n", package));
        }
        
        // Add imports
        if !code_unit.imports.is_empty() {
            representation.push_str("Imports:\n");
            for import in &code_unit.imports {
                representation.push_str(&format!("  {}\n", import));
            }
        }
        
        // Add functions
        if !code_unit.functions.is_empty() {
            representation.push_str("Functions:\n");
            for func in &code_unit.functions {
                representation.push_str(&format!("  {} - {}\n", func.name, func.signature));
                if let Some(doc) = &func.doc_comment {
                    representation.push_str(&format!("    Documentation: {}\n", doc));
                }
            }
        }
        
        // Add classes/structs
        if !code_unit.structs.is_empty() {
            representation.push_str("Types:\n");
            for struct_def in &code_unit.structs {
                representation.push_str(&format!("  {}\n", struct_def.name));
                if let Some(doc) = &struct_def.doc_comment {
                    representation.push_str(&format!("    Documentation: {}\n", doc));
                }
            }
        }
        
        // Add code content
        representation.push_str("\nCode:\n");
        representation.push_str(&code_unit.content);
        
        representation
    }

    /// Preprocess code for better tokenization
    fn preprocess_code(
        &self,
        code: &str,
        language: Language,
    ) -> String {
        let mut processed = String::new();
        
        for line in code.lines() {
            let trimmed = line.trim();
            
            // Skip empty lines and comments (but keep doc comments)
            if trimmed.is_empty() {
                continue;
            }
            
            if self.should_skip_line(trimmed, language) {
                continue;
            }
            
            processed.push_str(line);
            processed.push('\n');
        }
        
        processed
    }

    /// Check if a line should be skipped
    fn should_skip_line(
        &self,
        line: &str,
        language: Language,
    ) -> bool {
        let trimmed = line.trim();
        
        match language {
            Language::Rust => {
                trimmed.starts_with("//") && !trimmed.starts_with("///")
            }
            Language::Kotlin => {
                trimmed.starts_with("//") && !trimmed.starts_with("/**")
            }
            Language::Java => {
                trimmed.starts_with("//") && !trimmed.starts_with("/**")
            }
            Language::Other(_) => false,
        }
    }

    /// Add language context tokens
    fn add_language_context(
        &self,
        mut tokens: Vec<u32>,
        language: Language,
    ) -> Vec<u32> {
        // Add language identifier token at the beginning
        let language_token = match language {
            Language::Rust => 50000, // Special token ID for Rust
            Language::Kotlin => 50001, // Special token ID for Kotlin
            Language::Java => 50002, // Special token ID for Java
            Language::Other(_) => 50003, // Special token ID for other languages
        };
        
        let mut result = vec![language_token];
        result.append(&mut tokens);
        result
    }

    /// Handle special code tokens
    pub fn handle_special_tokens(&self,
        text: &str,
    ) -> String {
        let mut processed = text.to_string();
        
        // Replace common code patterns
        processed = processed.replace("{", " { ");
        processed = processed.replace("}", " } ");
        processed = processed.replace("(", " ( ");
        processed = processed.replace(")", " ) ");
        processed = processed.replace("[", " [ ");
        processed = processed.replace("]", " ] ");
        processed = processed.replace(";", " ; ");
        processed = processed.replace(",", " , ");
        
        // Normalize whitespace
        let re = regex::Regex::new(r"\s+").unwrap();
        processed = re.replace_all(&processed, " ").to_string();
        
        processed.trim().to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_tokenize_code() {
        let tokenizer_path = tempdir().unwrap();
        let tokenizer = Tokenizer::from_pretrained("microsoft/codebert-base", None)
            .expect("Failed to load tokenizer");
        
        let code_tokenizer = CodeTokenizer::new(tokenizer);
        
        let code = r#"
        fn add(a: i32, b: i32) -> i32 {
            a + b
        }
        "#;
        
        let tokens = code_tokenizer.tokenize_code(code, Language::Rust)
            .expect("Tokenization failed");
        
        assert!(!tokens.is_empty());
        assert!(tokens.len() <= 512);
    }

    #[test]
    fn test_create_semantic_representation() {
        let tokenizer_path = tempdir().unwrap();
        let tokenizer = Tokenizer::from_pretrained("microsoft/codebert-base", None)
            .expect("Failed to load tokenizer");
        
        let code_tokenizer = CodeTokenizer::new(tokenizer);
        
        let code_unit = crate::types::CodeUnit {
            file_path: std::path::PathBuf::from("test.rs"),
            language: Language::Rust,
            content: "fn test() {}".to_string(),
            ast_context: crate::types::AstContext::default(),
            dependencies: vec!["std::io".to_string()],
            functions: vec![crate::types::FunctionInfo {
                name: "test".to_string(),
                signature: "fn test()".to_string(),
                doc_comment: Some("Test function".to_string()),
                parameters: vec![],
                return_type: None,
                is_public: true,
            }],
            structs: vec![],
            imports: vec![],
        };
        
        let representation = code_tokenizer.create_semantic_representation(&code_unit);
        
        assert!(representation.contains("Language: Rust"));
        assert!(representation.contains("Functions:"));
        assert!(representation.contains("test"));
    }
}