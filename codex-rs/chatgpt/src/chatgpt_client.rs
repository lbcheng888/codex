use codex_core::config::Config;

use crate::chatgpt_token::get_chatgpt_token_data;
use crate::chatgpt_token::init_chatgpt_token_from_auth;

use std::time::Duration;

/// Maximum amount of time we allow a single request to spend on the wire. If the
/// timeout triggers we will automatically retry the request (see
/// [`MAX_RETRIES`]).
const REQUEST_TIMEOUT: Duration = Duration::from_secs(30);

/// Number of times we retry a ChatGPT request when it fails due to a transient
/// network error such as timeouts or connection resets.
///
/// We deliberately keep this number low because the caller (usually the agent
/// loop) already has higher-level retry/backoff logic. The goal here is merely
/// to paper over the occasional flaky connection instead of surfacing an error
/// to the rest of the system.
const MAX_RETRIES: usize = 3;

use anyhow::Context;
use serde::de::DeserializeOwned;

/// Make a GET request to the ChatGPT backend API.
pub(crate) async fn chatgpt_get_request<T: DeserializeOwned>(
    config: &Config,
    path: String,
) -> anyhow::Result<T> {
    let chatgpt_base_url = &config.chatgpt_base_url;
    init_chatgpt_token_from_auth(&config.codex_home).await?;

    // Build once so we can reuse the connection pool across retries.
    let client = reqwest::Client::builder()
        .timeout(REQUEST_TIMEOUT)
        .build()
        .context("Failed to build HTTP client")?;

    let url = format!("{chatgpt_base_url}{path}");

    let token = get_chatgpt_token_data()
        .ok_or_else(|| anyhow::anyhow!("ChatGPT token not available"))?;

    // Simple exponential back-off: 0.5s, 1s, 2s … (capped by REQUEST_TIMEOUT)
    let mut attempt = 0;
    loop {
        attempt += 1;

        let response_res = client
            .get(&url)
            .bearer_auth(&token.access_token)
            .header("chatgpt-account-id", &token.account_id)
            .header("Content-Type", "application/json")
            .header("User-Agent", "codex-cli")
            .send()
            .await;

        match response_res {
            Ok(response) => {
                if response.status().is_success() {
                    let result: T = response
                        .json()
                        .await
                        .context("Failed to parse JSON response")?;
                    return Ok(result);
                }

                // For HTTP 5xx we retry; for others bail immediately.
                if response.status().is_server_error() && attempt <= MAX_RETRIES {
                    tracing::warn!(
                        "ChatGPT request to {} returned {} – retry {}/{}", 
                        url,
                        response.status(),
                        attempt,
                        MAX_RETRIES
                    );
                } else {
                    let status = response.status();
                    let body = response.text().await.unwrap_or_default();
                    anyhow::bail!("Request failed with status {}: {}", status, body);
                }
            }
            Err(err) => {
                if attempt > MAX_RETRIES {
                    return Err(err).context("Failed to send request");
                }
                tracing::warn!(
                    "ChatGPT request to {} error: {} – retry {}/{}", 
                    url, err, attempt, MAX_RETRIES
                );
            }
        }

        // Back-off before next attempt.
        let backoff = Duration::from_millis(500u64.saturating_mul(1 << (attempt - 1).min(4)));
        tokio::time::sleep(backoff).await;
    }
}
