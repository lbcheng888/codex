use serde::{Deserialize, Serialize};
use std::sync::Arc;

use crate::vfs::DocUri;

/// LSP 风格的文档范围
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position { pub line: u32, pub character: u32 }
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Range { pub start: Position, pub end: Position }

/// 编辑操作：在某文档的某范围替换成新文本
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TextEdit { pub range: Range, pub new_text: String }

/// 单文件的编辑
#[derive(Debug, Clone)]
pub struct TextDocumentEdit { pub uri: DocUri, pub edits: Vec<TextEdit> }

/// 跨工作区的批量编辑（可包含多文件）
#[derive(Debug, Clone)]
pub struct WorkspaceEdit { pub document_changes: Vec<TextDocumentEdit> }

/// 将 WorkspaceEdit 原子应用到 codex 的编辑管线的接口
/// 实现方需提供冲突检测（基于当前缓冲区版本/光标/选择或更高级策略）
pub trait WorkspaceEditApplier: Send + Sync {
    fn apply_atomically(&self, edit: WorkspaceEdit) -> anyhow::Result<()>;
}

/// 默认占位实现：直接返回未实现
pub struct NoopWorkspaceEditApplier;
impl WorkspaceEditApplier for NoopWorkspaceEditApplier {
    fn apply_atomically(&self, _edit: WorkspaceEdit) -> anyhow::Result<()> {
        anyhow::bail!("WorkspaceEditApplier not implemented")
    }
}
