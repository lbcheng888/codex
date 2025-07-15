use anyhow::Result;
use std::path::Path;
use tree_sitter::{Parser, Node, Tree};
use tree_sitter_rust::language;

use crate::types::*;

pub struct RustParser {
    parser: Parser,
}

impl RustParser {
    pub fn new() -> Self {
        let mut parser = Parser::new();
        parser.set_language(language()).expect("Error loading Rust grammar");
        Self { parser }
    }

    pub fn parse(
        &self,
        content: &str,
        file_path: &Path,
    ) -> Result<CodeUnit> {
        let tree = self.parser.parse(content, None)
            .ok_or_else(|| anyhow::anyhow!("Failed to parse Rust code"))?;

        let root_node = tree.root_node();
        let ast_context = self.extract_ast_context(&root_node, content)?;
        let dependencies = self.extract_dependencies(&root_node, content)?;
        let functions = self.extract_functions(&root_node, content)?;
        let structs = self.extract_structs(&root_node, content)?;
        let imports = self.extract_imports(&root_node, content)?;

        Ok(CodeUnit {
            file_path: file_path.to_path_buf(),
            language: Language::Rust,
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
        root_node: &Node,
        content: &str,
    ) -> Result<AstContext> {
        let mut cursor = root_node.walk();
        let mut module_path = None;
        let mut package_name = None;
        let mut public_symbols = Vec::new();
        let mut type_definitions = Vec::new();

        // Extract module path from mod declarations
        for node in root_node.children(&mut cursor) {
            if node.kind() == "mod_item" {
                if let Some(name) = node.child(0)
                    .and_then(|n| self.node_text(&n, content)) {
                    module_path = Some(name);
                }
            }
        }

        // Extract public symbols
        self.extract_public_symbols(root_node, content, &mut public_symbols)?;

        // Extract type definitions
        self.extract_type_definitions(root_node, content, &mut type_definitions)?;

        Ok(AstContext {
            module_path,
            package_name,
            public_symbols,
            type_definitions,
        })
    }

    fn extract_dependencies(
        &self,
        root_node: &Node,
        content: &str,
    ) -> Result<Vec<String>> {
        let mut dependencies = Vec::new();
        let mut cursor = root_node.walk();

        for node in root_node.children(&mut cursor) {
            if node.kind() == "use_declaration" {
                if let Some(path) = self.extract_use_path(&node, content) {
                    dependencies.push(path);
                }
            }
        }

        Ok(dependencies)
    }

    fn extract_functions(
        &self,
        root_node: &Node,
        content: &str,
    ) -> Result<Vec<FunctionInfo>> {
        let mut functions = Vec::new();
        let mut cursor = root_node.walk();

        self.walk_functions(root_node, content, &mut functions, &mut cursor)?;
        Ok(functions)
    }

    fn extract_structs(
        &self,
        root_node: &Node,
        content: &str,
    ) -> Result<Vec<StructInfo>> {
        let mut structs = Vec::new();
        let mut cursor = root_node.walk();

        for node in root_node.children(&mut cursor) {
            match node.kind() {
                "struct_item" | "enum_item" | "type_alias" => {
                    if let Some(struct_info) = self.parse_struct(&node, content)? {
                        structs.push(struct_info);
                    }
                }
                _ => {}
            }
        }

        Ok(structs)
    }

    fn extract_imports(
        &self,
        root_node: &Node,
        content: &str,
    ) -> Result<Vec<String>> {
        let mut imports = Vec::new();
        let mut cursor = root_node.walk();

        for node in root_node.children(&mut cursor) {
            if node.kind() == "use_declaration" {
                if let Some(import) = self.node_text(&node, content) {
                    imports.push(import);
                }
            }
        }

        Ok(imports)
    }

    fn walk_functions(
        &self,
        node: &Node,
        content: &str,
        functions: &mut Vec<FunctionInfo>,
        cursor: &mut tree_sitter::TreeCursor,
    ) -> Result<()> {
        if node.kind() == "function_item" {
            if let Some(func) = self.parse_function(node, content)? {
                functions.push(func);
            }
        }

        for child in node.children(cursor) {
            self.walk_functions(&child, content, functions, cursor)?;
        }

        Ok(())
    }

    fn parse_function(
        &self,
        node: &Node,
        content: &str,
    ) -> Result<Option<FunctionInfo>> {
        let mut cursor = node.walk();
        
        // Find function name
        let name = node.child_by_field_name("name")
            .and_then(|n| self.node_text(&n, content));

        let Some(name) = name else {
            return Ok(None);
        };

        // Find parameters
        let parameters = node.child_by_field_name("parameters")
            .map(|params| self.parse_parameters(&params, content))
            .unwrap_or_default();

        // Find return type
        let return_type = node.child_by_field_name("return_type")
            .and_then(|ret| self.node_text(&ret, content));

        // Find doc comment
        let doc_comment = self.extract_doc_comment(node, content);

        // Check visibility
        let is_public = self.is_public(node, content);

        // Build signature
        let signature = self.build_function_signature(node, content)?;

        Ok(Some(FunctionInfo {
            name,
            signature,
            doc_comment,
            parameters,
            return_type,
            is_public,
        }))
    }

    fn parse_struct(
        &self,
        node: &Node,
        content: &str,
    ) -> Result<Option<StructInfo>> {
        let kind = node.kind();
        let name = node.child_by_field_name("name")
            .and_then(|n| self.node_text(&n, content));

        let Some(name) = name else {
            return Ok(None);
        };

        let fields = match kind {
            "struct_item" => self.parse_struct_fields(node, content)?,
            "enum_item" => self.parse_enum_variants(node, content)?,
            _ => Vec::new(),
        };

        let doc_comment = self.extract_doc_comment(node, content);
        let visibility = if self.is_public(node, content) {
            Visibility::Public
        } else {
            Visibility::Private
        };

        Ok(Some(StructInfo {
            name,
            fields,
            doc_comment,
            visibility,
        }))
    }

    fn parse_parameters(
        &self,
        params_node: &Node,
        content: &str,
    ) -> Vec<ParameterInfo> {
        let mut parameters = Vec::new();
        let mut cursor = params_node.walk();

        for param in params_node.children(&mut cursor) {
            if param.kind() == "parameter" {
                if let Some(param_info) = self.parse_parameter(&param, content) {
                    parameters.push(param_info);
                }
            }
        }

        parameters
    }

    fn parse_parameter(
        &self,
        param_node: &Node,
        content: &str,
    ) -> Option<ParameterInfo> {
        let name = param_node.child_by_field_name("name")
            .and_then(|n| self.node_text(&n, content));
        let type_name = param_node.child_by_field_name("type")
            .and_then(|n| self.node_text(&n, content));

        match (name, type_name) {
            (Some(name), Some(type_name)) => Some(ParameterInfo {
                name,
                type_name,
                is_optional: false, // TODO: Handle Option types
            }),
            _ => None,
        }
    }

    fn parse_struct_fields(
        &self,
        struct_node: &Node,
        content: &str,
    ) -> Result<Vec<FieldInfo>> {
        let mut fields = Vec::new();
        let mut cursor = struct_node.walk();

        if let Some(body) = struct_node.child_by_field_name("body") {
            for field in body.children(&mut cursor) {
                if field.kind() == "field_declaration" {
                    if let Some(field_info) = self.parse_field(&field, content) {
                        fields.push(field_info);
                    }
                }
            }
        }

        Ok(fields)
    }

    fn parse_enum_variants(
        <!--  rest of the file omitted for brevity  -->
        &self,
        _enum_node: &Node,
        _content: &str,
    ) -> Result<Vec<FieldInfo>> {
        // Simplified enum parsing - return empty for now
        Ok(Vec::new())
    }

    fn parse_field(
        &self,
        field_node: &Node,
        content: &str,
    ) -> Option<FieldInfo> {
        let name = field_node.child_by_field_name("name")
            .and_then(|n| self.node_text(&n, content));
        let type_name = field_node.child_by_field_name("type")
            .and_then(|n| self.node_text(&n, content));

        match (name, type_name) {
            (Some(name), Some(type_name)) => {
                let is_public = self.is_public(field_node, content);
                Some(FieldInfo {
                    name,
                    type_name,
                    is_public,
                })
            }
            _ => None,
        }
    }

    fn extract_use_path(
        &self,
        use_node: &Node,
        content: &str,
    ) -> Option<String> {
        let path_node = use_node.child_by_field_name("argument")
            .or_else(|| use_node.child_by_field_name("path"));
        
        path_node.and_then(|n| self.node_text(&n, content))
    }

    fn extract_doc_comment(
        &self,
        node: &Node,
        content: &str,
    ) -> Option<String> {
        let mut cursor = node.walk();
        
        // Look for line comments before the node
        let start_line = node.start_position().row;
        let mut comment_lines = Vec::new();

        // Simple heuristic: look for /// comments
        for _ in 0..5 { // Check up to 5 lines back
            if let Some(line) = content.lines().nth(start_line.saturating_sub(1)) {
                if line.trim_start().starts_with("///") {
                    comment_lines.push(line.trim_start()[3..].trim());
                }
            }
        }

        if !comment_lines.is_empty() {
            comment_lines.reverse();
            Some(comment_lines.join("\n"))
        } else {
            None
        }
    }

    fn is_public(
        &self,
        node: &Node,
        content: &str,
    ) -> bool {
        let mut current = *node;
        while let Some(prev) = current.prev_sibling() {
            if prev.kind() == "visibility_modifier" {
                if let Some(text) = self.node_text(&prev, content) {
                    return text.contains("pub");
                }
            }
            current = prev;
        }
        false
    }

    fn extract_public_symbols(
        &self,
        _root_node: &Node,
        _content: &str,
        _symbols: &mut Vec<String>,
    ) -> Result<()> {
        // TODO: Implement public symbol extraction
        Ok(())
    }

    fn extract_type_definitions(
        &self,
        _root_node: &Node,
        _content: &str,
        _definitions: &mut Vec<TypeDefinition>,
    ) -> Result<()> {
        // TODO: Implement type definition extraction
        Ok(())
    }

    fn build_function_signature(
        &self,
        node: &Node,
        content: &str,
    ) -> Result<String> {
        let start = node.start_byte();
        let end = node.end_byte();
        
        if let Some(signature) = content.get(start..end) {
            Ok(signature.to_string())
        } else {
            Ok("".to_string())
        }
    }

    fn node_text(
        &self,
        node: &Node,
        content: &str,
    ) -> Option<String> {
        let start = node.start_byte();
        let end = node.end_byte();
        content.get(start..end).map(|s| s.to_string())
    }
}

impl Default for RustParser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_function() {
        let parser = RustParser::new();
        let content = r#"
/// This is a test function
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
        "#;

        let result = parser.parse(content, Path::new("test.rs")).unwrap();
        assert_eq!(result.language, Language::Rust);
        assert_eq!(result.functions.len(), 1);
        
        let func = &result.functions[0];
        assert_eq!(func.name, "add");
        assert_eq!(func.parameters.len(), 2);
        assert_eq!(func.parameters[0].name, "a");
        assert_eq!(func.parameters[0].type_name, "i32");
        assert_eq!(func.return_type, Some("i32".to_string()));
        assert!(func.is_public);
        assert!(func.doc_comment.is_some());
    }

    #[test]
    fn test_parse_struct() {
        let parser = RustParser::new();
        let content = r#"
/// A test struct
pub struct Person {
    pub name: String,
    age: u32,
}
        "#;

        let result = parser.parse(content, Path::new("test.rs")).unwrap();
        assert_eq!(result.structs.len(), 1);
        
        let struct_info = &result.structs[0];
        assert_eq!(struct_info.name, "Person");
        assert_eq!(struct_info.fields.len(), 2);
        assert!(struct_info.is_public);
        assert!(struct_info.doc_comment.is_some());
    }
}