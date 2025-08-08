use std::path::{Component, Path, PathBuf};

/// 多根工作区上下文与路径规范化
/// - 统一 file:// URI 与相对路径之间的转换
/// - 支持多根：以根序号作为前缀命名空间（rootN:/rel/path），避免歧义
#[derive(Debug, Clone)]
pub struct WorkspaceContext {
    roots: Vec<PathBuf>,
}

impl WorkspaceContext {
    /// 使用单一根初始化
    pub fn new_single(root: PathBuf) -> Self {
        Self { roots: vec![root] }
    }

    /// 添加一个新的根目录
    pub fn add_root(&mut self, root: PathBuf) {
        self.roots.push(root);
    }

    /// 返回所有根
    pub fn roots(&self) -> &[PathBuf] { &self.roots }

    /// 规范化相对路径（去除 .. 与 .，统一分隔符）
    pub fn normalize_relative(rel: &str) -> String {
        let mut out = PathBuf::new();
        for comp in Path::new(rel).components() {
            match comp {
                Component::CurDir => {},
                Component::ParentDir => { out.pop(); },
                Component::Normal(s) => out.push(s),
                _ => {}
            }
        }
        out.to_string_lossy().replace('\u{5c}', "/")
    }

    /// 将一个根索引与相对路径组合为 file:// URI
    pub fn to_uri(&self, root_index: usize, relative_path: &str) -> String {
        let norm = Self::normalize_relative(relative_path);
        let root = &self.roots[root_index];
        format!("file://{}/{}", root.display(), norm)
    }

    /// 尝试从绝对路径推导 (root_index, relative_path)
    pub fn to_relative_from_abs(&self, abs: &Path) -> Option<(usize, String)> {
        for (i, root) in self.roots.iter().enumerate() {
            if let Ok(rel) = abs.strip_prefix(root) {
                return Some((i, Self::normalize_relative(&rel.to_string_lossy())));
            }
        }
        None
    }

    /// 从 file:// URI 解析出 (root_index, relative_path)
    pub fn from_uri(&self, uri: &str) -> Option<(usize, String)> {
        if let Some(path) = uri.strip_prefix("file://") {
            let path = PathBuf::from(path);
            self.to_relative_from_abs(&path)
        } else {
            None
        }
    }

    /// 辅助：如果只有单根，返回其索引 0
    pub fn sole_root_index(&self) -> usize { 0 }
}
