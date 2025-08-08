use std::collections::HashMap;
use std::sync::Arc;
use parking_lot::RwLock;
use serde_json::Value;
use std::time::{Duration, Instant};

pub trait Storage: Send + Sync {
    fn get(&self, key: &str) -> Option<Value>;
    fn put(&self, key: &str, val: Value);
    fn put_with_ttl(&self, key: &str, val: Value, ttl: Duration);
    fn delete(&self, key: &str);
    fn list(&self) -> Vec<String>;
    fn cleanup_expired(&self);
}

#[derive(Clone)]
pub struct CacheEntry {
    value: Value,
    expires_at: Option<Instant>,
}

#[derive(Default, Clone)]
pub struct InMemoryStorage {
    inner: Arc<RwLock<HashMap<String, CacheEntry>>>,
}

impl Storage for InMemoryStorage {
    fn get(&self, key: &str) -> Option<Value> {
        let storage = self.inner.read();
        if let Some(entry) = storage.get(key) {
            if let Some(expires_at) = entry.expires_at {
                if Instant::now() > expires_at {
                    return None; // Expired
                }
            }
            Some(entry.value.clone())
        } else {
            None
        }
    }
    
    fn put(&self, key: &str, val: Value) {
        let entry = CacheEntry {
            value: val,
            expires_at: None,
        };
        self.inner.write().insert(key.to_string(), entry);
    }
    
    fn put_with_ttl(&self, key: &str, val: Value, ttl: Duration) {
        let entry = CacheEntry {
            value: val,
            expires_at: Some(Instant::now() + ttl),
        };
        self.inner.write().insert(key.to_string(), entry);
    }

    fn delete(&self, key: &str) {
        self.inner.write().remove(key);
    }
    
    fn list(&self) -> Vec<String> {
        let now = Instant::now();
        self.inner.read()
            .iter()
            .filter(|(_, entry)| {
                entry.expires_at.map_or(true, |expires_at| now <= expires_at)
            })
            .map(|(k, _)| k.clone())
            .collect()
    }
    
    fn cleanup_expired(&self) {
        let now = Instant::now();
        let mut storage = self.inner.write();
        storage.retain(|_, entry| {
            entry.expires_at.map_or(true, |expires_at| now <= expires_at)
        });
    }
}