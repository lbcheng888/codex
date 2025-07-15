use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeUnit {
    pub file_path: PathBuf,
    pub language: Language,
    pub content: String,
    pub ast_context: AstContext,
    pub dependencies: Vec<String>,
    pub functions: Vec<FunctionInfo>,
    pub structs: Vec<StructInfo>,
    pub imports: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Language {
    Rust,
    Kotlin,
    Java,
    Other(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AstContext {
    pub module_path: Option<String>,
    pub package_name: Option<String>,
    pub public_symbols: Vec<String>,
    pub type_definitions: Vec<TypeDefinition>,
}

impl Default for AstContext {
    fn default() -> Self {
        Self {
            module_path: None,
            package_name: None,
            public_symbols: Vec::new(),
            type_definitions: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionInfo {
    pub name: String,
    pub signature: String,
    pub doc_comment: Option<String>,
    pub parameters: Vec<ParameterInfo>,
    pub return_type: Option<String>,
    pub is_public: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParameterInfo {
    pub name: String,
    pub type_name: String,
    pub is_optional: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructInfo {
    pub name: String,
    pub fields: Vec<FieldInfo>,
    pub doc_comment: Option<String>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldInfo {
    pub name: String,
    pub type_name: String,
    pub is_public: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Visibility {
    Public,
    Private,
    Crate,
    Protected,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeDefinition {
    pub name: String,
    pub kind: TypeKind,
    pub definition: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeKind {
    Struct,
    Enum,
    Trait,
    Interface,
    Class,
    Function,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeMatch {
    pub file_path: PathBuf,
    pub code: String,
    pub score: f32,
    pub context: CodeContext,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeContext {
    pub language: Language,
    pub function_name: Option<String>,
    pub class_name: Option<String>,
    pub imports: Vec<String>,
    pub line_range: (usize, usize),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Embedding {
    pub vector: Vec<f32>,
    pub dimensions: usize,
}

impl Embedding {
    pub fn new(vector: Vec<f32>) -> Self {
        let dimensions = vector.len();
        Self { vector, dimensions }
    }

    pub fn cosine_similarity(&self, other: &Self) -> f32 {
        let dot_product = self.vector.iter()
            .zip(other.vector.iter())
            .map(|(a, b)| a * b)
            .sum::<f32>();
        
        let norm_a = self.vector.iter().map(|x| x * x).sum::<f32>().sqrt();
        let norm_b = other.vector.iter().map(|x| x * x).sum::<f32>().sqrt();
        
        if norm_a > 0.0 && norm_b > 0.0 {
            dot_product / (norm_a * norm_b)
        } else {
            0.0
        }
    }
}