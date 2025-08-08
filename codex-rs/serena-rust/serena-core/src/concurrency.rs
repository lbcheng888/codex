use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{Semaphore, RwLock};
use tokio::time::timeout;
use crate::error::{CoreError, Result};
use crate::observability::get_metrics;

/// Resource pool for managing concurrent operations
#[derive(Debug)]
pub struct ResourcePool<T> {
    resources: Arc<RwLock<Vec<T>>>,
    semaphore: Arc<Semaphore>,
    max_size: usize,
}

impl<T: Send + Sync + 'static> ResourcePool<T> {
    pub fn new(max_size: usize) -> Self {
        Self {
            resources: Arc::new(RwLock::new(Vec::with_capacity(max_size))),
            semaphore: Arc::new(Semaphore::new(max_size)),
            max_size,
        }
    }
    
    /// Add a resource to the pool
    pub async fn add_resource(&self, resource: T) -> Result<()> {
        let mut resources = self.resources.write().await;
        if resources.len() >= self.max_size {
            return Err(CoreError::validation("Resource pool is full"));
        }
        resources.push(resource);
        Ok(())
    }
    
    /// Acquire a resource from the pool with timeout
    pub async fn acquire(&self, timeout_duration: Duration) -> Result<PooledResource<T>> {
        let permit = timeout(timeout_duration, self.semaphore.clone().acquire_owned())
            .await
            .map_err(|_| CoreError::timeout("Resource acquisition timeout", timeout_duration.as_millis() as u64))?
            .map_err(|_| CoreError::internal("Semaphore closed"))?;
        
        let mut resources = self.resources.write().await;
        if let Some(resource) = resources.pop() {
            Ok(PooledResource {
                resource: Some(resource),
                pool: self.resources.clone(),
                _permit: permit,
            })
        } else {
            Err(CoreError::not_found("No resources available in pool"))
        }
    }
    
    /// Get current pool size
    pub async fn size(&self) -> usize {
        self.resources.read().await.len()
    }
    
    /// Get available permits
    pub fn available_permits(&self) -> usize {
        self.semaphore.available_permits()
    }
}

/// RAII wrapper for pooled resources
pub struct PooledResource<T: Send + Sync + 'static> {
    resource: Option<T>,
    pool: Arc<RwLock<Vec<T>>>,
    _permit: tokio::sync::OwnedSemaphorePermit,
}

impl<T: Send + Sync + 'static> PooledResource<T> {
    /// Get a reference to the resource
    pub fn get(&self) -> Option<&T> {
        self.resource.as_ref()
    }
    
    /// Get a mutable reference to the resource
    pub fn get_mut(&mut self) -> Option<&mut T> {
        self.resource.as_mut()
    }
}

impl<T: Send + Sync + 'static> Drop for PooledResource<T> {
    fn drop(&mut self) {
        if let Some(resource) = self.resource.take() {
            let pool = self.pool.clone();
            tokio::spawn(async move {
                let mut resources = pool.write().await;
                resources.push(resource);
            });
        }
    }
}

/// Concurrent task executor with rate limiting
#[derive(Debug)]
pub struct TaskExecutor {
    semaphore: Arc<Semaphore>,
    timeout_duration: Duration,
}

impl TaskExecutor {
    pub fn new(max_concurrent: usize, timeout_duration: Duration) -> Self {
        Self {
            semaphore: Arc::new(Semaphore::new(max_concurrent)),
            timeout_duration,
        }
    }
    
    /// Execute a task with concurrency and timeout control
    pub async fn execute<F, Fut, T>(&self, task: F) -> Result<T>
    where
        F: FnOnce() -> Fut,
        Fut: std::future::Future<Output = Result<T>>,
    {
        let _permit = timeout(self.timeout_duration, self.semaphore.acquire())
            .await
            .map_err(|_| CoreError::timeout("Task execution timeout", self.timeout_duration.as_millis() as u64))?
            .map_err(|_| CoreError::internal("Semaphore closed"))?;
        
        let start = std::time::Instant::now();
        let result = timeout(self.timeout_duration, task()).await;
        let duration = start.elapsed();
        
        match result {
            Ok(Ok(value)) => {
                get_metrics().add_request_time(duration);
                Ok(value)
            }
            Ok(Err(e)) => {
                get_metrics().add_request_time(duration);
                Err(e)
            }
            Err(_) => {
                get_metrics().add_request_time(duration);
                Err(CoreError::timeout("Task execution timeout", self.timeout_duration.as_millis() as u64))
            }
        }
    }
    
    /// Get available permits
    pub fn available_permits(&self) -> usize {
        self.semaphore.available_permits()
    }
}

/// Circuit breaker for handling failures
#[derive(Debug)]
pub struct CircuitBreaker {
    failure_threshold: usize,
    recovery_timeout: Duration,
    state: Arc<RwLock<CircuitState>>,
}

#[derive(Debug, Clone)]
enum CircuitState {
    Closed { failures: usize },
    Open { opened_at: std::time::Instant },
    HalfOpen,
}

impl CircuitBreaker {
    pub fn new(failure_threshold: usize, recovery_timeout: Duration) -> Self {
        Self {
            failure_threshold,
            recovery_timeout,
            state: Arc::new(RwLock::new(CircuitState::Closed { failures: 0 })),
        }
    }
    
    /// Execute a function with circuit breaker protection
    pub async fn call<F, Fut, T>(&self, f: F) -> Result<T>
    where
        F: FnOnce() -> Fut,
        Fut: std::future::Future<Output = Result<T>>,
    {
        // Check if circuit is open
        {
            let state = self.state.read().await;
            match &*state {
                CircuitState::Open { opened_at } => {
                    if opened_at.elapsed() < self.recovery_timeout {
                        return Err(CoreError::validation("Circuit breaker is open"));
                    }
                    // Time to try half-open
                }
                _ => {}
            }
        }
        
        // Transition to half-open if needed
        {
            let mut state = self.state.write().await;
            if let CircuitState::Open { opened_at } = &*state {
                if opened_at.elapsed() >= self.recovery_timeout {
                    *state = CircuitState::HalfOpen;
                }
            }
        }
        
        // Execute the function
        let result = f().await;
        
        // Update state based on result
        let mut state = self.state.write().await;
        match result {
            Ok(value) => {
                // Success - reset or close circuit
                *state = CircuitState::Closed { failures: 0 };
                Ok(value)
            }
            Err(e) => {
                // Failure - increment counter or open circuit
                match &*state {
                    CircuitState::Closed { failures } => {
                        let new_failures = failures + 1;
                        if new_failures >= self.failure_threshold {
                            *state = CircuitState::Open { opened_at: std::time::Instant::now() };
                        } else {
                            *state = CircuitState::Closed { failures: new_failures };
                        }
                    }
                    CircuitState::HalfOpen => {
                        *state = CircuitState::Open { opened_at: std::time::Instant::now() };
                    }
                    CircuitState::Open { .. } => {
                        // Already open, no change
                    }
                }
                Err(e)
            }
        }
    }
    
    /// Get current circuit state
    pub async fn is_open(&self) -> bool {
        let state = self.state.read().await;
        matches!(*state, CircuitState::Open { .. })
    }
    
    /// Get failure count (only valid when closed)
    pub async fn failure_count(&self) -> usize {
        let state = self.state.read().await;
        match &*state {
            CircuitState::Closed { failures } => *failures,
            _ => 0,
        }
    }
}

/// Rate limiter using token bucket algorithm
#[derive(Debug)]
pub struct RateLimiter {
    tokens: Arc<RwLock<f64>>,
    max_tokens: f64,
    refill_rate: f64, // tokens per second
    last_refill: Arc<RwLock<std::time::Instant>>,
}

impl RateLimiter {
    pub fn new(max_tokens: f64, refill_rate: f64) -> Self {
        Self {
            tokens: Arc::new(RwLock::new(max_tokens)),
            max_tokens,
            refill_rate,
            last_refill: Arc::new(RwLock::new(std::time::Instant::now())),
        }
    }
    
    /// Try to acquire tokens
    pub async fn acquire(&self, tokens_needed: f64) -> Result<()> {
        self.refill_tokens().await;
        
        let mut tokens = self.tokens.write().await;
        if *tokens >= tokens_needed {
            *tokens -= tokens_needed;
            Ok(())
        } else {
            Err(CoreError::validation("Rate limit exceeded"))
        }
    }
    
    /// Refill tokens based on elapsed time
    async fn refill_tokens(&self) {
        let now = std::time::Instant::now();
        let mut last_refill = self.last_refill.write().await;
        let elapsed = now.duration_since(*last_refill).as_secs_f64();
        
        if elapsed > 0.0 {
            let mut tokens = self.tokens.write().await;
            let new_tokens = (*tokens + elapsed * self.refill_rate).min(self.max_tokens);
            *tokens = new_tokens;
            *last_refill = now;
        }
    }
    
    /// Get current token count
    pub async fn available_tokens(&self) -> f64 {
        self.refill_tokens().await;
        *self.tokens.read().await
    }
}
