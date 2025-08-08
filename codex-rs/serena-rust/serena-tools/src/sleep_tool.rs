use async_trait::async_trait;
use serde_json::{json, Value};
use tokio::time::{sleep, Duration};

use serena_core::error::{CoreError, Result};
use serena_core::tools::{Tool, ToolContext};

pub struct SleepTool;

#[async_trait]
impl Tool for SleepTool {
    fn name(&self) -> &'static str {
        "sleep"
    }

    fn description(&self) -> &'static str {
        "Sleep for the specified milliseconds, then return { \"slept_ms\": ms }"
    }

    fn schema(&self) -> Value {
        // MCP-compatible: both parameters and inputSchema will mirror this object schema via ToolRegistry
        json!({
            "type": "object",
            "properties": {
                "ms": {
                    "type": "integer",
                    "minimum": 0,
                    "description": "Milliseconds to sleep before returning"
                }
            },
            "required": ["ms"]
        })
    }

    async fn run(&self, input: Value, _ctx: ToolContext) -> Result<Value> {
        // Validate input
        let ms = input.get("ms").and_then(|v| v.as_i64())
            .ok_or_else(|| CoreError::validation("missing or invalid 'ms' (integer)"))?;

        if ms < 0 {
            return Err(CoreError::validation("'ms' must be >= 0"));
        }

        // Sleep deterministically
        let ms_u64 = ms as u64;
        sleep(Duration::from_millis(ms_u64)).await;

        Ok(json!({ "slept_ms": ms_u64 }))
    }
}