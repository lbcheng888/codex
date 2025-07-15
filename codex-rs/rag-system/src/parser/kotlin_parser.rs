use anyhow::Result;
use std::path::Path;
use regex::Regex;

use crate::types::*;

pub struct KotlinParser;

impl KotlinParser {
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
        let functions = self.extract_functions(content)?;
        let structs = self.extract_classes(content)?;
        let imports = self.extract_imports(content)?;

        Ok(CodeUnit {
            file_path: file_path.to_path_buf(),
            language: Language::Kotlin,
            content: content.to_string(),
            ast_context,
            dependencies,
            functions,
            structs,
            imports,
        })
    }

    fn extract_ast_context(&self,
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

    fn extract_package_name(&self,
        content: &str,
    ) -> Result<Option<String>> {
        let re = Regex::new(r"package\s+([\w.]+)")
            .expect("Invalid regex pattern");
        
        if let Some(caps) = re.captures(content) {
            Ok(Some(caps[1].to_string()))
        } else {
            Ok(None)
        }
    }

    fn extract_dependencies(&self,
        content: &str,
    ) -> Result<Vec<String>> {
        let mut dependencies = Vec::new();

        // Extract imports
        let import_re = Regex::new(r"import\s+([\w.]+(?:\*)?)")
            .expect("Invalid regex pattern");
        
        for cap in import_re.captures_iter(content) {
            dependencies.push(cap[1].to_string());
        }

        // Extract dependencies from build.gradle files
        if content.contains("dependencies {") {
            let dep_re = Regex::new(r"(\w+)\s*['\"]([^'\"]+)['\"]")
                .expect("Invalid regex pattern");
            
            for cap in dep_re.captures_iter(content) {
                dependencies.push(format!("{}:{}", &cap[1], &cap[2]));
            }
        }

        Ok(dependencies)
    }

    fn extract_functions(&self,
        content: &str,
    ) -> Result<Vec<FunctionInfo>> {
        let mut functions = Vec::new();

        // Function pattern: fun name(parameters): ReturnType
        let func_re = Regex::new(r"(?s)(?m)((?:/\*\*[\s\S]*?\*/\s*)?)(?:(public|private|internal|protected)\s+)?fun\s+(\w+)\s*\(([^)]*)\)\s*(?::\s*([^\s{]+))?")
            .expect("Invalid regex pattern");

        for cap in func_re.captures_iter(content) {
            let doc_comment = cap.get(1).map(|m| {
                m.as_str()
                    .trim_start_matches("/**")
                    .trim_end_matches("*/")
                    .trim()
                    .to_string()
            });

            let visibility = cap.get(2).map(|m| m.as_str());
            let name = cap[3].to_string();
            let params_str = &cap[4];
            let return_type = cap.get(5).map(|m| m.as_str().to_string());

            let parameters = self.parse_parameters(params_str);
            let is_public = visibility.map(|v| v == "public").unwrap_or(false);

            functions.push(FunctionInfo {
                name,
                signature: cap[0].to_string(),
                doc_comment,
                parameters,
                return_type,
                is_public,
            });
        }

        Ok(functions)
    }

    fn extract_classes(&self,
        content: &str,
    ) -> Result<Vec<StructInfo>> {
        let mut classes = Vec::new();

        // Class pattern: (visibility)? class/interface name
        let class_re = Regex::new(r"(?s)(?m)((?:/\*\*[\s\S]*?\*/\s*)?)(?:(public|private|internal|protected)\s+)?(?:data\s+)?(?:class|interface)\s+(\w+)")
            .expect("Invalid regex pattern");

        for cap in class_re.captures_iter(content) {
            let doc_comment = cap.get(1).map(|m| {
                m.as_str()
                    .trim_start_matches("/**")
                    .trim_end_matches("*/")
                    .trim()
                    .to_string()
            });

            let visibility = cap.get(2).map(|m| m.as_str());
            let name = cap[3].to_string();

            let fields = self.extract_fields(content);
            let is_public = visibility.map(|v| v == "public").unwrap_or(false);

            classes.push(StructInfo {
                name,
                fields,
                doc_comment,
                visibility: if is_public {
                    Visibility::Public
                } else {
                    Visibility::Private
                },
            });
        }

        Ok(classes)
    }

    fn extract_imports(&self,
        content: &str,
    ) -> Result<Vec<String>> {
        let mut imports = Vec::new();

        // Import statements
        let import_re = Regex::new(r"import\s+([\w.]+(?:\.\*)?)")
            .expect("Invalid regex pattern");

        for cap in import_re.captures_iter(content) {
            imports.push(cap[1].to_string());
        }

        Ok(imports)
    }

    fn parse_parameters(&self,
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
                let name = parts[0].trim().to_string();
                let type_name = parts[1].trim().to_string();
                let is_optional = type_name.ends_with('?');

                parameters.push(ParameterInfo {
                    name,
                    type_name,
                    is_optional,
                });
            }
        }

        parameters
    }

    fn extract_fields(&self,
        content: &str,
    ) -> Vec<FieldInfo> {
        let mut fields = Vec::new();

        // Property pattern inside classes
        let prop_re = Regex::new(r"(?m)(?:(val|var)\s+)?(\w+)\s*:\s*([^\s=]+)")
            .expect("Invalid regex pattern");

        for cap in prop_re.captures_iter(content) {
            let name = cap[2].to_string();
            let type_name = cap[3].to_string();
            let is_public = !content.contains(&format!("private {}: {}", name, type_name));

            fields.push(FieldInfo {
                name,
                type_name,
                is_public,
            });
        }

        fields
    }

    fn extract_public_symbols(&self,
        content: &str,
    ) -> Result<Vec<String>> {
        let mut symbols = Vec::new();

        // Public classes, functions, properties
        let public_re = Regex::new(r"(?m)public\s+(?:fun|class|val|var)\s+(\w+)")
            .expect("Invalid regex pattern");

        for cap in public_re.captures_iter(content) {
            symbols.push(cap[1].to_string());
        }

        Ok(symbols)
    }

    fn extract_type_definitions(&self,
        content: &str,
    ) -> Result<Vec<TypeDefinition>> {
        let mut definitions = Vec::new();

        // Type aliases
        let type_alias_re = Regex::new(r"typealias\s+(\w+)\s*=\s*([^\n]+)")
            .expect("Invalid regex pattern");

        for cap in type_alias_re.captures_iter(content) {
            definitions.push(TypeDefinition {
                name: cap[1].to_string(),
                kind: TypeKind::Interface,
                definition: cap[2].trim().to_string(),
            });
        }

        Ok(definitions)
    }
}

impl Default for KotlinParser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_function() {
        let parser = KotlinParser::new();
        let content = r#"
package com.example

/**
 * Adds two numbers
 */
fun add(a: Int, b: Int): Int {
    return a + b
}
        "#;

        let result = parser.parse(content, Path::new("test.kt")).unwrap();
        assert_eq!(result.language, Language::Kotlin);
        assert_eq!(result.functions.len(), 1);
        
        let func = &result.functions[0];
        assert_eq!(func.name, "add");
        assert_eq!(func.parameters.len(), 2);
        assert_eq!(func.parameters[0].name, "a");
        assert_eq!(func.parameters[0].type_name, "Int");
        assert_eq!(func.return_type, Some("Int".to_string()));
        assert!(func.is_public);
    }

    #[test]
    fn test_parse_class() {
        let parser = KotlinParser::new();
        let content = r#"
package com.example

/**
 * A person class
 */
class Person(val name: String, var age: Int)
        "#;

        let result = parser.parse(content, Path::new("test.kt")).unwrap();
        assert_eq!(result.structs.len(), 1);
        
        let class = &result.structs[0];
        assert_eq!(class.name, "Person");
        assert_eq!(class.fields.len(), 2);
    }
}