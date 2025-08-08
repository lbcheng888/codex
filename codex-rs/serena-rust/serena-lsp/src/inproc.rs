use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Result;
use serde_json::json;
use tokio::runtime::Handle;
use tracing::{debug, instrument};

use crate::manager::LanguageServerManager;
use crate::types::LspError;
use crate::vfs::{DocUri, DocumentSnapshot, DocumentStore, TextChange, Version};
use crate::workspace_edit::{WorkspaceEdit, WorkspaceEditApplier};

/// 统一线程池与队列：外部传入 tokio Handle，避免跨运行时切换
#[derive(Clone)]
pub struct InprocRuntime {
    pub handle: Handle,
}

/// In-process LSP 门面，提供无 JSON-RPC 的直接调用接口
pub struct InProcessLspService {
    lsp: LanguageServerManager,
    vfs: Arc<dyn DocumentStore>,
    applier: Arc<dyn WorkspaceEditApplier>,
    rt: InprocRuntime,
}

impl InProcessLspService {
    pub async fn new(
        lsp: LanguageServerManager,
        vfs: Arc<dyn DocumentStore>,
        applier: Arc<dyn WorkspaceEditApplier>,
        rt: InprocRuntime,
    ) -> Self {
        Self { lsp, vfs, applier, rt }
    }

    /// 文档打开（共享 codex 的 VFS）：仅更新本地镜像，不经由 JSON-RPC 通知
    pub fn did_open(&self, uri: DocUri, language: String, text: String, version: Version) {
        self.vfs.open(uri, language, text, version);
    }

    /// 增量变更：更新 VFS，并可选择向后端 LSP 同步（后续可加 debounce/批处理）
    pub fn did_change(&self, uri: &DocUri, version: Version, changes: &[TextChange]) -> Result<DocumentSnapshot> {
        let snap = self.vfs.apply_changes(uri, version, changes)?;
        Ok(snap)
    }

    /// 请求补全：直接调用 LanguageServerManager，通过内存数据构造 LSP 请求
    #[instrument(skip_all, fields(uri = %uri.0, line = line, character = character))]
    pub async fn completion(&mut self, uri: &DocUri, line: u32, character: u32) -> Result<serde_json::Value> {
        let language = self.detect_language_from_uri(uri)?;
        self.lsp.ensure_server_started(&language).await.map_err(|e| anyhow::anyhow!(e))?;

        let file_path = self.uri_to_file_path(uri);
        let params = json!({
            "textDocument": { "uri": format!("file://{}", file_path.display()) },
            "position": { "line": line, "character": character },
            "context": { "triggerKind": 1 }
        });

        let resp = self.lsp.send_request(&language, "textDocument/completion", Some(params)).await
            .map_err(|e| anyhow::anyhow!(e))?;
        Ok(resp)
    }

    /// 请求诊断：通常由服务端推送，这里提供主动请求的兜底方式（部分 LSP 不支持）
    pub async fn diagnostics_pull(&mut self, uri: &DocUri) -> Result<Option<serde_json::Value>> {
        let language = self.detect_language_from_uri(uri)?;
        if self.lsp.ensure_server_started(&language).await.is_err() { return Ok(None); }
        // 许多服务器不支持 pull 诊断，这里先返回 None，留待具体服务器适配
        Ok(None)
    }

    /// 代码操作
    pub async fn code_actions(&mut self, uri: &DocUri, range: (u32, u32, u32, u32)) -> Result<serde_json::Value> {
        let language = self.detect_language_from_uri(uri)?;
        self.lsp.ensure_server_started(&language).await.map_err(|e| anyhow::anyhow!(e))?;
        let file_path = self.uri_to_file_path(uri);
        let params = json!({
            "textDocument": { "uri": format!("file://{}", file_path.display()) },
            "range": { "start": {"line": range.0, "character": range.1}, "end": {"line": range.2, "character": range.3} },
            "context": { "diagnostics": [] }
        });
        let resp = self.lsp.send_request(&language, "textDocument/codeAction", Some(params)).await
            .map_err(|e| anyhow::anyhow!(e))?;
        Ok(resp)
    }

    /// 重命名
    pub async fn rename(&mut self, uri: &DocUri, pos: (u32, u32), new_name: &str) -> Result<WorkspaceEdit> {
        let language = self.detect_language_from_uri(uri)?;
        self.lsp.ensure_server_started(&language).await.map_err(|e| anyhow::anyhow!(e))?;
        let file_path = self.uri_to_file_path(uri);
        let params = json!({
            "textDocument": { "uri": format!("file://{}", file_path.display()) },
            "position": { "line": pos.0, "character": pos.1 },
            "newName": new_name
        });
        let resp = self.lsp.send_request(&language, "textDocument/rename", Some(params)).await
            .map_err(|e| anyhow::anyhow!(e))?;
        let edit = self.convert_workspace_edit(resp)?;
        Ok(edit)
    }

    /// 将 WorkspaceEdit 交由 codex 的编辑管线原子应用
    pub fn apply_workspace_edit(&self, edit: WorkspaceEdit) -> Result<()> {
        self.applier.apply_atomically(edit).map_err(|e| anyhow::anyhow!(e))
    }

    fn detect_language_from_uri(&self, uri: &DocUri) -> Result<String> {
        // 复用 manager 的简单检测逻辑
        let rel = self.uri_to_relative(uri);
        self.lsp.detect_language(&rel).map_err(|e| anyhow::anyhow!(e))
    }

    fn uri_to_file_path(&self, uri: &DocUri) -> std::path::PathBuf {
        if let Some(rest) = uri.0.strip_prefix("file://") {
            return std::path::PathBuf::from(rest);
        }
        self.vfs.workspace_root().join(&uri.0)
    }

    fn uri_to_relative(&self, uri: &DocUri) -> String {
        let p = self.uri_to_file_path(uri);
        let root = self.vfs.workspace_root();
        let p_str = p.to_string_lossy();
        let root_str = root.to_string_lossy();
        if p_str.starts_with(&*root_str) {
            let rel = &p_str[root_str.len()..];
            rel.trim_start_matches('/')
                .trim_start_matches('\\')
                .to_string()
        } else {
            p_str.to_string()
        }
    }

    fn convert_workspace_edit(&self, v: serde_json::Value) -> Result<WorkspaceEdit> {
        // 支持两种格式：{documentChanges:[{textDocument:{uri:..},edits:[{range:{..}, newText}] }]} 或 {changes:{uri:[TextEdit]}}
        let mut map: Vec<crate::workspace_edit::TextDocumentEdit> = vec![];
        if let Some(arr) = v.get("documentChanges").and_then(|dc| dc.as_array()) {
            for item in arr {
                let uri = item
                    .get("textDocument")
                    .and_then(|td| td.get("uri"))
                    .and_then(|u| u.as_str())
                    .ok_or_else(|| anyhow::anyhow!("invalid documentChanges item"))?;
                let edits = item
                    .get("edits")
                    .and_then(|e| e.as_array())
                    .ok_or_else(|| anyhow::anyhow!("invalid edits"))?;
                map.push(crate::workspace_edit::TextDocumentEdit {
                    uri: DocUri(uri.to_string()),
                    edits: edits
                        .iter()
                        .map(|e| crate::workspace_edit::TextEdit {
                            range: crate::workspace_edit::Range {
                                start: crate::workspace_edit::Position {
                                    line: e["range"]["start"]["line"].as_u64().unwrap_or(0) as u32,
                                    character: e["range"]["start"]["character"].as_u64().unwrap_or(0) as u32,
                                },
                                end: crate::workspace_edit::Position {
                                    line: e["range"]["end"]["line"].as_u64().unwrap_or(0) as u32,
                                    character: e["range"]["end"]["character"].as_u64().unwrap_or(0) as u32,
                                },
                            },
                            new_text: e["newText"].as_str().unwrap_or("").to_string(),
                        })
                        .collect(),
                });
            }
        } else if let Some(obj) = v.get("changes").and_then(|c| c.as_object()) {
            for (uri, edits) in obj.iter() {
                let edits_arr = edits.as_array().ok_or_else(|| anyhow::anyhow!("invalid changes entry"))?;
                map.push(crate::workspace_edit::TextDocumentEdit {
                    uri: DocUri(uri.clone()),
                    edits: edits_arr
                        .iter()
                        .map(|e| crate::workspace_edit::TextEdit {
                            range: crate::workspace_edit::Range {
                                start: crate::workspace_edit::Position {
                                    line: e["range"]["start"]["line"].as_u64().unwrap_or(0) as u32,
                                    character: e["range"]["start"]["character"].as_u64().unwrap_or(0) as u32,
                                },
                                end: crate::workspace_edit::Position {
                                    line: e["range"]["end"]["line"].as_u64().unwrap_or(0) as u32,
                                    character: e["range"]["end"]["character"].as_u64().unwrap_or(0) as u32,
                                },
                            },
                            new_text: e["newText"].as_str().unwrap_or("").to_string(),
                        })
                        .collect(),
                });
            }
        } else {
            anyhow::bail!("Unsupported workspace edit shape: {}", v);
        }
        Ok(WorkspaceEdit { document_changes: map })
    }
}
