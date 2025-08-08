//! 通用恢复与重试工具：指数退避+抖动、可取消重试、任务级幂等与部分回滚接口
//! 该模块不直接依赖具体后端；调用方可在边界做错误类型转换并实现 `Idempotent`/`Transactional`。

use std::future::Future;
use std::time::Duration;

use crate::types::CancelToken;

/// 重试策略
#[derive(Clone, Debug)]
pub struct RetryPolicy {
    pub max_retries: u32,
    pub initial_delay_ms: u64,
    pub max_delay_ms: Option<u64>,
}

impl Default for RetryPolicy {
    fn default() -> Self {
        Self { max_retries: 5, initial_delay_ms: 200, max_delay_ms: Some(15_000) }
    }
}

/// 指数退避+抖动（与 util::backoff 一致，附带上限）
fn backoff_with_cap(attempt: u64, cap_ms: Option<u64>) -> Duration {
    let d = crate::util::backoff(attempt);
    if let Some(cap) = cap_ms { d.min(Duration::from_millis(cap)) } else { d }
}

/// 通用异步重试：
/// - is_retryable(err) 决定是否继续
/// - honor_cancel 若取消或超时立刻返回 Err(err)
pub async fn retry_async<T, E, Fut, F, RFn, CFn>(
    mut op: F,
    policy: RetryPolicy,
    mut is_retryable: RFn,
    cancel: &CancelToken,
    mut on_retry: CFn,
) -> Result<T, E>
where
    F: FnMut(u32) -> Fut,
    Fut: Future<Output = Result<T, E>>,
    RFn: FnMut(&E) -> bool,
    CFn: FnMut(u32, &E, Duration),
{
    let mut attempt: u32 = 0;
    loop {
        attempt += 1;
        let res = op(attempt).await;
        match res {
            Ok(v) => return Ok(v),
            Err(e) => {
                if attempt > policy.max_retries || !is_retryable(&e) || cancel.is_cancelled() || cancel.is_expired() {
                    return Err(e);
                }
                let delay = backoff_with_cap(attempt as u64, policy.max_delay_ms);
                on_retry(attempt, &e, delay);
                // 尊重取消/截止
                let sleep = tokio::time::sleep(delay);
                tokio::pin!(sleep);
                tokio::select! {
                    _ = &mut sleep => {}
                    else => {}
                }
            }
        }
    }
}

/// 任务幂等性：实现该 trait 的任务需提供稳定 key（可用于去重/幂等存档）。
pub trait Idempotent {
    /// 幂等键：对同一语义任务保持稳定，以支持重试/去重
    fn idempotency_key(&self) -> String;
}

/// 具备部分完成回滚的事务式任务
#[async_trait::async_trait]
pub trait Transactional: Send {
    type Output;
    type Error;

    /// 执行任务（可多次调用，要求幂等）
    async fn apply(&mut self) -> Result<Self::Output, Self::Error>;

    /// 对已完成的部分进行回滚（尽力而为）
    async fn rollback(&mut self);
}

/// 在失败后进行一次回滚的包装执行
pub async fn apply_with_rollback<T>(task: &mut T) -> Result<T::Output, T::Error>
where
    T: Transactional,
{
    match task.apply().await {
        Ok(v) => Ok(v),
        Err(e) => {
            task.rollback().await;
            Err(e)
        }
    }
}

/// 支持取消与中断恢复的轮廓：调用方可将快照存入外部存储以实现 resume。
#[async_trait::async_trait]
pub trait Resumable {
    type Snapshot: Clone + Send + Sync + 'static;

    /// 产生可持久化的快照（最小必要信息）
    async fn snapshot(&self) -> Self::Snapshot;
    /// 从快照恢复内存态
    async fn restore(&mut self, snapshot: Self::Snapshot);
}
