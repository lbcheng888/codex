use serde::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};

/// LSP Symbol Kinds based on the Language Server Protocol specification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize_repr, Deserialize_repr)]
#[repr(u8)]
pub enum SymbolKind {
    File = 1,
    Module = 2,
    Namespace = 3,
    Package = 4,
    Class = 5,
    Method = 6,
    Property = 7,
    Field = 8,
    Constructor = 9,
    Enum = 10,
    Interface = 11,
    Function = 12,
    Variable = 13,
    Constant = 14,
    String = 15,
    Number = 16,
    Boolean = 17,
    Array = 18,
    Object = 19,
    Key = 20,
    Null = 21,
    EnumMember = 22,
    Struct = 23,
    Event = 24,
    Operator = 25,
    TypeParameter = 26,
}

impl SymbolKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            SymbolKind::File => "file",
            SymbolKind::Module => "module",
            SymbolKind::Namespace => "namespace",
            SymbolKind::Package => "package",
            SymbolKind::Class => "class",
            SymbolKind::Method => "method",
            SymbolKind::Property => "property",
            SymbolKind::Field => "field",
            SymbolKind::Constructor => "constructor",
            SymbolKind::Enum => "enum",
            SymbolKind::Interface => "interface",
            SymbolKind::Function => "function",
            SymbolKind::Variable => "variable",
            SymbolKind::Constant => "constant",
            SymbolKind::String => "string",
            SymbolKind::Number => "number",
            SymbolKind::Boolean => "boolean",
            SymbolKind::Array => "array",
            SymbolKind::Object => "object",
            SymbolKind::Key => "key",
            SymbolKind::Null => "null",
            SymbolKind::EnumMember => "enum_member",
            SymbolKind::Struct => "struct",
            SymbolKind::Event => "event",
            SymbolKind::Operator => "operator",
            SymbolKind::TypeParameter => "type_parameter",
        }
    }
}

/// Position in a document
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position {
    /// Line position in a document (zero-based)
    pub line: u32,
    /// Character offset on a line in a document (zero-based)
    pub character: u32,
}

/// Range in a document
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Range {
    /// The range's start position
    pub start: Position,
    /// The range's end position
    pub end: Position,
}

/// Location in a document
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Location {
    /// The file path (relative to workspace root)
    pub relative_path: String,
    /// The range within the file
    pub range: Range,
}

/// Unified symbol information combining various LSP symbol types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnifiedSymbolInformation {
    /// The name of the symbol
    pub name: String,
    /// The symbol's full name path (e.g., "class/method")
    pub name_path: String,
    /// The kind of symbol
    pub kind: SymbolKind,
    /// The location of the symbol
    pub location: Location,
    /// Optional body location (for symbols with bodies)
    pub body_location: Option<Range>,
    /// Symbol source code (if requested)
    pub body: Option<String>,
    /// Children symbols (if depth > 0)
    pub children: Vec<UnifiedSymbolInformation>,
}

/// Reference to a symbol with its location
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReferenceInSymbol {
    /// The symbol being referenced
    pub symbol: UnifiedSymbolInformation,
    /// Line where the reference occurs
    pub line: u32,
    /// Character position where the reference occurs
    pub character: u32,
    /// Code snippet around the reference
    pub snippet: Option<String>,
}

/// Language server configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LanguageServerConfig {
    /// Language identifier
    pub language: String,
    /// Command to start the language server
    pub command: Vec<String>,
    /// Working directory for the server
    pub working_dir: Option<String>,
    /// Additional environment variables
    pub env: Option<std::collections::HashMap<String, String>>,
    /// Initialization options
    pub init_options: Option<serde_json::Value>,
}

/// Error types for LSP operations
#[derive(Debug, thiserror::Error)]
pub enum LspError {
    #[error("Language server not found: {0}")]
    ServerNotFound(String),
    
    #[error("Language server communication error: {0}")]
    CommunicationError(String),
    
    #[error("Language server initialization failed: {0}")]
    InitializationFailed(String),
    
    #[error("Symbol not found: {0}")]
    SymbolNotFound(String),

    #[error("Server not running: {0}")]
    ServerNotRunning(String),
    
    #[error("Invalid request: {0}")]
    InvalidRequest(String),
    
    #[error("Server crashed: {0}")]
    ServerCrashed(String),

    #[error("Operation timed out: {0}")]
    Timeout(String),
    
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
    
    #[error("JSON error: {0}")]
    JsonError(#[from] serde_json::Error),
}

impl LspError {
    /// 标准化错误码，便于跨组件观测（类比 JSON-RPC 常见码域）
    pub fn code(&self) -> i64 {
        match self {
            LspError::ServerNotFound(_) => -32601,      // Method not found 类似
            LspError::InvalidRequest(_) => -32602,      // Invalid params 类似
            LspError::CommunicationError(_) => -32001,  // Server unavailable
            LspError::InitializationFailed(_) => -32003,
            LspError::ServerCrashed(_) => -32004,
            LspError::ServerNotRunning(_) => -32006,
            LspError::SymbolNotFound(_) => -32007,
            LspError::IoError(_) => -32005,
            LspError::JsonError(_) => -32700,           // Parse error
            LspError::Timeout(_) => -32008,
        }
    }
}

pub type LspResult<T> = Result<T, LspError>;
