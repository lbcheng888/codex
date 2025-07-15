use anyhow::Result;
use std::path::Path;
use regex::Regex;

use crate::types::*;

pub struct JavaParser;

impl JavaParser {
    pub fn new() -> Self {
        Self
    }

    pub fn parse(
        &self,
        content: &str,
        file_path: &Path,
    ) -> Result<CodeUnit> {
        let ast_context = self.extract_ast_context(content)?;
        let dependencies = self.extract_dependencies(content)?;
        let functions = self.extract_methods(content)?;
        let structs = self.extract_classes(content)?;
        let imports = self.extract_imports(content)?;

        Ok(CodeUnit {
            file_path: file_path.to_path_buf(),
            language: Language::Java,
            content: content.to_string(),
            ast_context,
            dependencies,
            functions,
            structs,
            imports,
        })
    }

    fn extract_ast_context(
        &self,
        content: &str,
    ) -> Result<AstContext> {
        let package_name = self.extract_package_name(content)?;
        let public_symbols = self.extract_public_symbols(content)?;
        let type_definitions = self.extract_type_definitions(content)?;

        Ok(AstContext {
            module_path: None,
            package_name,
            public_symbols,
            type_definitions,
        })
    }

    fn extract_package_name(
        &self,
        content: &str,
    ) -> Result<Option<String>> {
        let re = Regex::new(r"package\s+([\w.]+);")
            .expect("Invalid regex pattern");
        
        if let Some(caps) = re.captures(content) {
            Ok(Some(caps[1].to_string()))
        } else {
            Ok(None)
        }
    }

    fn extract_dependencies(
        &self,
        content: &str,
    ) -> Result<Vec<String>> {
        let mut dependencies = Vec::new();

        // Extract imports
        let import_re = Regex::new(r"import\s+([\w.*]+);")
            .expect("Invalid regex pattern");
        
        for cap in import_re.captures_iter(content) {
            dependencies.push(cap[1].to_string());
        }

        Ok(dependencies)
    }

    fn extract_methods(
        &self,
        content: &str,
    ) -> Result<Vec<FunctionInfo>> {
        let mut methods = Vec::new();

        // Method pattern with optional modifiers and return type
        let method_re = Regex::new(r"(?s)(?m)((?:/\*\*[\s\S]*?\*/\s*)?)(?:(public|private|protected|static|final|synchronized)\s+)*(\w+)\s+(\w+)\s*\(([^)]*)\)\s*(?:throws\s+[^\s{]+\s*)?\s*\{")
            .expect("Invalid regex pattern");

        for cap in method_re.captures_iter(content) {
            let javadoc = cap.get(1).map(|m| {
                m.as_str()
                    .trim_start_matches("/**")
                    .trim_end_matches("*/")
                    .trim()
                    .to_string()
            });

            let modifiers = cap.get(2).map(|m| m.as_str());
            let return_type = cap[3].to_string();
            let name = cap[4].to_string();
            let params_str = &cap[5];

            let parameters = self.parse_parameters(params_str);
            let is_public = modifiers.map(|m| m.contains("public")).unwrap_or(false);

            methods.push(FunctionInfo {
                name,
                signature: cap[0].to_string(),
                doc_comment: javadoc,
                parameters,
                return_type: Some(return_type),
                is_public,
            });
        }

        Ok(methods)
    }

    fn extract_classes(
        &self,
        content: &str,
    ) -> Result<Vec<StructInfo>> {
        let mut classes = Vec::new();

        // Class pattern with optional modifiers
        let class_re = Regex::new(r"(?s)(?m)((?:/\*\*[\s\S]*?\*/\s*)?)(?:(public|private|protected|static|final|abstract)\s+)*(?:class|interface|enum)\s+(\w+)")
            .expect("Invalid regex pattern");

        for cap in class_re.captures_iter(content) {
            let javadoc = cap.get(1).map(|m| {
                m.as_str()
                    .trim_start_matches("/**")
                    .trim_end_matches("*/")
                    .trim()
                    .to_string()
            });

            let modifiers = cap.get(2).map(|m| m.as_str());
            let name = cap[3].to_string();

            let fields = self.extract_fields(content);
            let is_public = modifiers.map(|m| m.contains("public")).unwrap_or(false);

            classes.push(StructInfo {
                name,
                fields,
                doc_comment: javadoc,
                visibility: if is_public {
                    Visibility::Public
                } else {
                    Visibility::Private
                },
            });
        }

        Ok(classes)
    }

    fn extract_imports(
        &self,
        content: &str,
    ) -> Result<Vec<String>> {
        let mut imports = Vec::new();

        // Import statements
        let import_re = Regex::new(r"import\s+([\w.*]+);")
            .expect("Invalid regex pattern");

        for cap in import_re.captures_iter(content) {
            imports.push(cap[1].to_string());
        }

        Ok(imports)
    }

    fn parse_parameters(
        &self,
        params_str: &str,
    ) -> Vec<ParameterInfo> {
        let mut parameters = Vec::new();

        // Handle empty parameters
        if params_str.trim().is_empty() {
            return parameters;
        }

        for param in params_str.split(',') {
            let param = param.trim();
            if param.is_empty() {
                continue;
            }

            // Handle final modifier
            let parts: Vec<&str> = param.split_whitespace().collect();
            let type_start = if parts.get(0).map(|s| *s == "final").unwrap_or(false) {
                1
            } else {
                0
            };

            if parts.len() > type_start + 1 {
                let type_name = parts[type_start].to_string();
                let name = parts[type_start + 1].to_string();
                let is_optional = type_name.ends_with("[]");

                parameters.push(ParameterInfo {
                    name,
                    type_name,
                    is_optional,
                });
            }
        }

        parameters
    }

    fn extract_fields(
        &self,
        content: &str,
    ) -> Vec<FieldInfo> {
        let mut fields = Vec::new();

        // Field declarations with optional modifiers
        let field_re = Regex::new(r"(?m)(?:(public|private|protected)\s+)?(?:(static|final)\s+)*(\w+)\s+(\w+)\s*;")
            .expect("Invalid regex pattern");

        for cap in field_re.captures_iter(content) {
            let visibility = cap.get(1).map(|m| m.as_str());
            let type_name = cap[3].to_string();
            let name = cap[4].to_string();
            let is_public = visibility.map(|v| v == "public").unwrap_or(false);

            fields.push(FieldInfo {
                name,
                type_name,
                is_public,
            });
        }

        fields
    }

    fn extract_public_symbols(
        &self,
        content: &str,
    ) -> Result<Vec<String>> {
        let mut symbols = Vec::new();

        // Public classes, methods, fields
        let public_re = Regex::new(r"(?m)public\s+(?:class|interface|enum|method|field)\s+(\w+)")
            .expect("Invalid regex pattern");

        for cap in public_re.captures_iter(content) {
            symbols.push(cap[1].to_string());
        }

        Ok(symbols)
    }

    fn extract_type_definitions(
        &self,
        content: &str,
    ) -> Result<Vec<TypeDefinition>> {
        let mut definitions = Vec::new();

        // Inner classes and interfaces
        let inner_re = Regex::new(r"(?m)(?:class|interface)\s+(\w+)")
            .expect("Invalid regex pattern");

        for cap in inner_re.captures_iter(content) {
            definitions.push(TypeDefinition {
                name: cap[1].to_string(),
                kind: TypeKind::Class,
                definition: cap[0].to_string(),
            });
        }

        Ok(definitions)
    }
}

impl Default for JavaParser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_method() {
        let parser = JavaParser::new();
        let content = r#"
package com.example;

/**
 * Adds two numbers
 */
public class Calculator {
    public int add(int a, int b) {
        return a + b;
    }
}
        "#;

        let result = parser.parse(content, Path::new("Test.java")).unwrap();
        assert_eq!(result.language, Language::Java);
        assert_eq!(result.functions.len(), 1);
        
        let method = &result.functions[0];
        assert_eq!(method.name, "add");
        assert_eq!(method.parameters.len(), 2);
        assert_eq!(method.parameters[0].name, "a");
        assert_eq!(method.parameters[0].type_name, "int");
        assert_eq!(method.return_type, Some("int".to_string()));
        assert!(method.is_public);
    }

    #[test]
    fn test_parse_class() {
        let parser = JavaParser::new();
        let content = r#"
package com.example;

/**
 * A person class
 */
public class Person {
    private String name;
    public int age;
}
        "#;

        let result = parser.parse(content, Path::new("Person.java")).unwrap();
        assert_eq!(result.structs.len(), 1);
        
        let class = &result.structs[0];
        assert_eq!(class.name, "Person");
        assert_eq!(class.fields.len(), 2);
    }
}