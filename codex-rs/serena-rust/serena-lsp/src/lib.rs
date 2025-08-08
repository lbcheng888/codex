pub mod types;
pub mod manager;
pub mod transport;

// In-process 集成入口：无 JSON-RPC，直接内存调用
pub mod vfs;
pub mod workspace_edit;
pub mod inproc;
pub mod workspace;
pub mod index;
pub mod fs_watcher;
pub mod mcp_interface;
pub mod servers;

pub use types::*;
pub use manager::LanguageServerManager;
pub use mcp_interface::{GetSymbolsOverview, FindSymbol, FindReferencingSymbols};

// Alias for backward compatibility and easier imports
pub use manager::LanguageServerManager as LspManager;