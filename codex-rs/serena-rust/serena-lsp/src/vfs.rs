use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use parking_lot::RwLock;

/// 文档版本号（单调递增）
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Version(pub i64);

/// 文档标识（统一使用 file:// URI 或工作区相对路径）
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DocUri(pub String);

/// 文本变更（增量）
#[derive(Debug, Clone)]
pub struct TextChange {
    pub range_start: usize, // 以 UTF-8 字节位移或 UTF-16? 这里简化为字节位移
    pub range_len: usize,
    pub text: String,
}

/// 文档镜像内容与元信息
#[derive(Debug, Clone)]
pub struct DocumentSnapshot {
    pub uri: DocUri,
    pub language: String,
    pub version: Version,
    pub text: Arc<String>,
}

/// 文档变更监听器：用于广播更新/失效索引
pub trait DocumentChangeListener: Send + Sync {
    fn on_open(&self, uri: &DocUri);
    fn on_change(&self, uri: &DocUri);
}

/// 文档存储（与 codex 的 VFS 对接时，可由 codex 实现并注入）
pub trait DocumentStore: Send + Sync {
    fn open(&self, uri: DocUri, language: String, text: String, version: Version);
    fn apply_changes(&self, uri: &DocUri, version: Version, changes: &[TextChange]) -> anyhow::Result<DocumentSnapshot>;
    fn get(&self, uri: &DocUri) -> Option<DocumentSnapshot>;
    fn workspace_root(&self) -> PathBuf;
}

/// 简单内存实现（占位，默认仅用于测试或原型）
pub struct InMemoryDocumentStore {
    root: PathBuf,
    inner: RwLock<HashMap<DocUri, (String, String, Version)>>, // (language, text, version)
    listener: RwLock<Option<Arc<dyn DocumentChangeListener>>>,
}

impl InMemoryDocumentStore {
    pub fn new(root: PathBuf) -> Self {
        Self { root, inner: RwLock::new(HashMap::new()), listener: RwLock::new(None) }
    }
    pub fn set_listener(&self, listener: Arc<dyn DocumentChangeListener>) { *self.listener.write() = Some(listener); }
}

impl DocumentStore for InMemoryDocumentStore {
fn open(&self, uri: DocUri, language: String, text: String, version: Version) {
        self.inner.write().insert(uri.clone(), (language, text, version));
        if let Some(l) = &*self.listener.read() { l.on_open(&uri); }
    }

    fn apply_changes(&self, uri: &DocUri, version: Version, changes: &[TextChange]) -> anyhow::Result<DocumentSnapshot> {
        let mut guard = self.inner.write();
        let entry = guard.get_mut(uri).ok_or_else(|| anyhow::anyhow!("document not opened: {}", uri.0))?;
        let (language, text_ref, ver_ref) = entry;
        // 版本递增与冲突检测
        if version.0 <= ver_ref.0 {
            anyhow::bail!("stale version: incoming={}, current={}", version.0, ver_ref.0);
        }
        let mut buf = text_ref.clone();
        // 按从后到前应用，避免位移冲突
        let mut changes_sorted = changes.to_vec();
        changes_sorted.sort_by_key(|c| c.range_start);
        for change in changes_sorted.into_iter().rev() {
            let start = change.range_start.min(buf.len());
            let end = start.saturating_add(change.range_len).min(buf.len());
            buf.replace_range(start..end, &change.text);
        }
        *text_ref = buf.clone();
        *ver_ref = version;
        if let Some(l) = &*self.listener.read() { l.on_change(uri); }
        Ok(DocumentSnapshot { uri: uri.clone(), language: language.clone(), version: *ver_ref, text: Arc::new(buf) })
    }

    fn get(&self, uri: &DocUri) -> Option<DocumentSnapshot> {
        self.inner.read().get(uri).map(|(lang, text, ver)| DocumentSnapshot {
            uri: uri.clone(),
            language: lang.clone(),
            version: *ver,
            text: Arc::new(text.clone()),
        })
    }

    fn workspace_root(&self) -> PathBuf { self.root.clone() }
}
