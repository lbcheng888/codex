use std::collections::HashMap;
use std::time::Duration;

use serde_json::Value;
use serena_core::storage::{InMemoryStorage, Storage};

use crate::types::UnifiedSymbolInformation;
use crate::workspace::WorkspaceContext;

/// 符号索引与缓存：统一增量生命周期与淘汰策略
pub struct SymbolIndex {
    storage: InMemoryStorage,
    /// 命名空间前缀，用于区分不同项目/根
    ns: String,
}

impl SymbolIndex {
    pub fn new(ns: impl Into<String>) -> Self {
        Self { storage: InMemoryStorage::default(), ns: ns.into() }
    }

    fn key_for(&self, root_idx: usize, rel: &str) -> String {
        format!("{}:root{}:{}", self.ns, root_idx, rel)
    }

    /// 文档初次打开：全量缓存其符号/摘要等
    pub fn on_open(&self, root_idx: usize, rel: &str, symbols: &[UnifiedSymbolInformation]) {
        let key = self.key_for(root_idx, rel);
        let val = serde_json::to_value(symbols).unwrap_or(Value::Null);
        // 默认 30 分钟过期，避免无限增长
        self.storage.put_with_ttl(&key, val, Duration::from_secs(30 * 60));
    }

    /// 增量更新：根据编辑器变更刷新对应文件的符号索引
    pub fn on_change(&self, root_idx: usize, rel: &str, symbols: &[UnifiedSymbolInformation]) {
        self.on_open(root_idx, rel, symbols)
    }

    /// 读取：命中则返回，未命中由上层触发构建
    pub fn get(&self, root_idx: usize, rel: &str) -> Option<Vec<UnifiedSymbolInformation>> {
        let key = self.key_for(root_idx, rel);
        self.storage.get(&key).and_then(|v| serde_json::from_value(v).ok())
    }

    /// 淘汰清理
    pub fn cleanup(&self) { self.storage.cleanup_expired(); }

    /// 失效指定键
    pub fn invalidate(&self, root_idx: usize, rel: &str) {
        let key = self.key_for(root_idx, rel);
        self.storage.delete(&key);
    }

    /// 冷启动预热：在打开仓库时预载关键模块与符号
    /// 策略：
    /// - 首次尝试从 language servers 的 workspace/symbol 或 documentSymbol 拉取顶层符号
    /// - 写入有界 TTL 缓存，避免重复构建
    pub async fn prewarm<F, Fut>(
        &self,
        workspace: &WorkspaceContext,
        candidates: &HashMap<usize, Vec<String>>, // root_idx -> relative paths to warm
        fetch_symbols: F,
    ) where
        F: Fn(usize, &str) -> Fut + Send + Sync,
        Fut: std::future::Future<Output = Vec<UnifiedSymbolInformation>>,
    {
        for (root_idx, files) in candidates {
            for rel in files {
                if self.get(*root_idx, rel).is_none() {
                    let syms = fetch_symbols(*root_idx, rel).await;
                    self.on_open(*root_idx, rel, &syms);
                }
            }
        }
        // 可根据需要，扩展为按语言做 workspace 广域查询，并分桶入库
    }
}
