use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use rand::{rngs::StdRng, Rng, SeedableRng};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use tokio::time::timeout;

use crate::observability::metric_event;
use crate::compare::{compare, ComparePolicy, CompareVerdict};

pub type SiteId = String;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ShimGlobalToggle {
    #[serde(default)]
    pub enable_shadow: bool,
    #[serde(default)]
    pub enable_dual_write: bool,
    #[serde(default)]
    pub enable_single_write_new: bool,
    #[serde(default = "default_sampling")] 
    pub sampling_rate: f64, // 0.0..=1.0
}

fn default_sampling() -> f64 { 0.05 }

impl Default for ShimGlobalToggle {
    fn default() -> Self {
        Self { enable_shadow: true, enable_dual_write: false, enable_single_write_new: false, sampling_rate: default_sampling() }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ShimSiteOverride {
    pub enable_shadow: Option<bool>,
    pub enable_dual_write: Option<bool>,
    pub enable_single_write_new: Option<bool>,
    pub sampling_rate: Option<f64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ShimThresholds {
    #[serde(default = "default_consistency")] 
    pub consistency_ok_ratio: f64, // e.g., 0.9995
    #[serde(default = "default_fail_ratio")] 
    pub max_error_ratio: f64, // e.g., 0.02
}
fn default_consistency() -> f64 { 0.9995 }
fn default_fail_ratio() -> f64 { 0.02 }

impl Default for ShimThresholds {
    fn default() -> Self { Self { consistency_ok_ratio: default_consistency(), max_error_ratio: default_fail_ratio() } }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ShimConfig {
    #[serde(default)]
    pub global: ShimGlobalToggle,
    #[serde(default)]
    pub sites: HashMap<SiteId, ShimSiteOverride>,
    #[serde(default)]
    pub compare_policy: ComparePolicy,
    #[serde(default)]
    pub thresholds: ShimThresholds,
}

impl Default for ShimConfig {
    fn default() -> Self {
        Self { global: ShimGlobalToggle::default(), sites: HashMap::new(), compare_policy: ComparePolicy::default(), thresholds: ShimThresholds::default() }
    }
}

impl ShimConfig {
    pub fn effective_for(&self, site: &SiteId) -> ShimGlobalToggle {
        if let Some(ov) = self.sites.get(site) {
            let mut g = self.global.clone();
            if let Some(v) = ov.enable_shadow { g.enable_shadow = v; }
            if let Some(v) = ov.enable_dual_write { g.enable_dual_write = v; }
            if let Some(v) = ov.enable_single_write_new { g.enable_single_write_new = v; }
            if let Some(v) = ov.sampling_rate { g.sampling_rate = v; }
            return g;
        }
        self.global.clone()
    }
}

#[derive(Clone)]
pub struct ShimRuntime {
    pub cfg: Arc<ShimConfig>,
    rng: Arc<tokio::sync::Mutex<StdRng>>, // for sampling
}

impl ShimRuntime {
    pub fn new(cfg: ShimConfig) -> Self {
        Self { cfg: Arc::new(cfg), rng: Arc::new(tokio::sync::Mutex::new(StdRng::from_entropy())) }
    }

    fn hit(&self, rate: f64) -> bool {
        let r = rate.clamp(0.0, 1.0);
        // fast path: 0 or 1
        if r <= 0.0 { return false; }
        if r >= 1.0 { return true; }
        // non-blocking best-effort: try_lock first
        if let Ok(mut guard) = self.rng.try_lock() {
            let v: f64 = guard.gen();
            return v < r;
        }
        // fallback: deterministic threshold on time
        let nanos = std::time::Instant::now().elapsed().as_nanos();
        (nanos % 1_000_000) as f64 / 1_000_000.0 < r
    }

    pub async fn shadow_call<F, Fut, T>(&self, site: SiteId, new_call: F, old_output: &Value, op_name: &str) where
        F: FnOnce() -> Fut + Send + 'static,
        Fut: std::future::Future<Output = anyhow::Result<Value>> + Send + 'static,
        T: Send + 'static,
    {
        let eff = self.cfg.effective_for(&site);
        if !eff.enable_shadow { return; }
        if !self.hit(eff.sampling_rate) { return; }

        let policy = self.cfg.compare_policy.clone();
        let site_clone = site.clone();
        let op_name_owned = op_name.to_string();
        let old = old_output.clone();
        tokio::spawn(async move {
            let start = std::time::Instant::now();
            let res = new_call().await;
            let dur = start.elapsed();
            match res {
                Ok(new_out) => {
                    let verdict = compare(&old, &new_out, &policy);
                    report_compare(&site_clone, &op_name_owned, &verdict, dur);
                }
                Err(e) => {
                    metric_event("shadow_error", dur.as_millis() as u64, Some(-1), 0, Some(&json!({
                        "site": site_clone,
                        "op": op_name_owned,
                        "error": e.to_string(),
                    })));
                }
            }
        });
    }

    pub async fn dual_write<F1, Fut1, F2, Fut2>(&self, site: SiteId, old_write: F1, new_write: F2, op_name: &str, timeout_ms: Option<u64>) -> anyhow::Result<Value>
    where
        F1: FnOnce() -> Fut1,
        Fut1: std::future::Future<Output = anyhow::Result<Value>>,
        F2: FnOnce() -> Fut2 + Send + 'static,
        Fut2: std::future::Future<Output = anyhow::Result<Value>> + Send + 'static,
    {
        let eff = self.cfg.effective_for(&site);
        if eff.enable_single_write_new {
            // 单写新路径
            return new_write().await;
        }
        if eff.enable_dual_write {
            // 双写：以旧路径为承诺，新的仅记录
            let start = std::time::Instant::now();
            let old_fut = old_write();
            let new_fut = new_write();
            let op_name_owned2 = op_name.to_string();
            let new_fut = async move {
                let r = new_fut.await;
                let dur = start.elapsed();
                match &r {
                    Ok(_) => metric_event("dual_write_new_ok", dur.as_millis() as u64, None, 0, None),
                    Err(e) => metric_event("dual_write_new_err", dur.as_millis() as u64, Some(-1), 0, Some(&json!({"op": op_name_owned2, "error": e.to_string()}))),
                }
                r
            };
            let old_res = if let Some(ms) = timeout_ms { timeout(Duration::from_millis(ms), old_fut).await.map_err(|e| anyhow::anyhow!(e))?? } else { old_fut.await? };
            // fire and forget new write
            tokio::spawn(async move { let _ = new_fut.await; });
            Ok(old_res)
        } else {
            // 默认旧路径
            old_write().await
        }
    }
}

fn report_compare(site: &str, op: &str, verdict: &CompareVerdict, dur: std::time::Duration) {
    let extra = json!({
        "site": site,
        "op": op,
        "equal": verdict.equal,
        "diff_count": verdict.diffs.len(),
        "diffs_sample": verdict.diffs.get(0).cloned().unwrap_or_default(),
    });
    metric_event("shadow_compare", dur.as_millis() as u64, None, 0, Some(&extra));
}

