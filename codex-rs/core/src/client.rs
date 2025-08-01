use std::time::Duration;
use std::path::Path;
use std::io::BufRead;

use bytes::Bytes;
use codex_login::AuthMode;
use codex_login::CodexAuth;
use eventsource_stream::Eventsource;
use futures::prelude::*;
use reqwest::StatusCode;
use serde::Deserialize;
use serde::Serialize;
use tokio::sync::mpsc;
use tokio::time::timeout;
use tokio_util::io::ReaderStream;
use tracing::trace;
use tracing::warn;
use tracing::debug;
use uuid::Uuid;

use crate::chat_completions::AggregateStreamExt;
use crate::chat_completions::stream_chat_completions;
use crate::client_common::Prompt;
use crate::client_common::ResponseEvent;
use crate::client_common::ResponseStream;
use crate::client_common::ResponsesApiRequest;
use crate::client_common::create_reasoning_param_for_request;
use crate::config::Config;
use crate::config_types::ReasoningEffort as ReasoningEffortConfig;
use crate::config_types::ReasoningSummary as ReasoningSummaryConfig;
use crate::error::CodexErr;
use crate::error::EnvVarError;
use crate::error::Result;
use crate::flags::CODEX_RS_SSE_FIXTURE;
use crate::model_provider_info::ModelProviderInfo;
use crate::model_provider_info::WireApi;
use crate::models::ContentItem;
use crate::models::ResponseItem;
use crate::openai_tools::create_tools_json_for_responses_api;
use crate::protocol::TokenUsage;
use crate::util::backoff;
use std::sync::Arc;

#[derive(Clone)]
pub struct ModelClient {
    config: Arc<Config>,
    auth: Option<CodexAuth>,
    client: reqwest::Client,
    provider: ModelProviderInfo,
    session_id: Uuid,
    effort: ReasoningEffortConfig,
    summary: ReasoningSummaryConfig,
}

impl ModelClient {
    pub fn new(
        config: Arc<Config>,
        auth: Option<CodexAuth>,
        provider: ModelProviderInfo,
        effort: ReasoningEffortConfig,
        summary: ReasoningSummaryConfig,
        session_id: Uuid,
    ) -> Self {
        Self {
            config,
            auth,
            client: reqwest::Client::new(),
            provider,
            session_id,
            effort,
            summary,
        }
    }

    /// Dispatches to either the Responses or Chat implementation depending on
    /// the provider config.  Public callers always invoke `stream()` – the
    /// specialised helpers are private to avoid accidental misuse.
    pub async fn stream(&self, prompt: &Prompt) -> Result<ResponseStream> {
        crate::debug_log!(
            "[CLIENT] stream() called with {} input items",
            prompt.input.len()
        );
        crate::debug_log!(
            "[CLIENT] Using provider: {:?}, model: {}",
            self.provider.wire_api,
            self.config.model
        );
        crate::debug_log!("[CLIENT] Provider base_url: {:?}", self.provider.base_url);
        crate::debug_log!("[CLIENT] Provider env_key: {:?}", self.provider.env_key);

        match self.provider.wire_api {
            WireApi::Responses => {
                crate::debug_log!("[CLIENT] Using Responses API");
                self.stream_responses(prompt).await
            }
            WireApi::Chat => {
                crate::debug_log!("[CLIENT] Using Chat Completions API");
                // Create the raw streaming connection first.
                crate::debug_log!("[CLIENT] Calling stream_chat_completions...");
                let response_stream = stream_chat_completions(
                    prompt,
                    &self.config.model,
                    self.config.include_plan_tool,
                    &self.client,
                    &self.provider,
                )
                .await?;
                crate::debug_log!("[CLIENT] stream_chat_completions completed successfully");

                // Wrap it with the aggregation adapter so callers see *only*
                // the final assistant message per turn (matching the
                // behaviour of the Responses API).
                let mut aggregated = response_stream.aggregate();

                // Bridge the aggregated stream back into a standard
                // `ResponseStream` by forwarding events through a channel.
                let (tx, rx) = mpsc::channel::<Result<ResponseEvent>>(16);

                tokio::spawn(async move {
                    use futures::StreamExt;
                    while let Some(ev) = aggregated.next().await {
                        // Exit early if receiver hung up.
                        if tx.send(ev).await.is_err() {
                            break;
                        }
                    }
                });

                Ok(ResponseStream { rx_event: rx })
            }
        }
    }

    /// Implementation for the OpenAI *Responses* experimental API.
    async fn stream_responses(&self, prompt: &Prompt) -> Result<ResponseStream> {
        if let Some(path) = &*CODEX_RS_SSE_FIXTURE {
            // short circuit for tests
            warn!(path, "Streaming from fixture");
            return self.stream_from_fixture_local(path, self.provider.clone()).await;
        }

        let auth = self.auth.as_ref().ok_or_else(|| {
            CodexErr::EnvVar(EnvVarError {
                var: "OPENAI_API_KEY".to_string(),
                instructions: Some("Create an API key (https://platform.openai.com) and export it as an environment variable.".to_string()),
            })
        })?;

        let store = prompt.store && auth.mode != AuthMode::ChatGPT;

        // For Azure providers, try to get token from the configured env_key first
        let token = if let Some(env_key) = &self.provider.env_key {
            if let Ok(azure_token) = std::env::var(env_key) {
                azure_token
            } else {
                auth.get_token().await?
            }
        } else {
            auth.get_token().await?
        };

        let full_instructions = prompt.get_full_instructions(&self.config.model);
        let tools_json = create_tools_json_for_responses_api(
            prompt,
            &self.config.model,
            self.config.include_plan_tool,
        )?;
        let reasoning = create_reasoning_param_for_request(&self.config, self.effort, self.summary);

        // Request encrypted COT if we are not storing responses,
        // otherwise reasoning items will be referenced by ID
        let include: Vec<String> = if !store && reasoning.is_some() {
            vec!["reasoning.encrypted_content".to_string()]
        } else {
            vec![]
        };

        let mut input_with_instructions = Vec::with_capacity(prompt.input.len() + 1);
        if let Some(ui) = &prompt.user_instructions {
            input_with_instructions.push(ResponseItem::Message {
                id: None,
                role: "user".to_string(),
                content: vec![ContentItem::InputText { text: ui.clone() }],
            });
        }
        input_with_instructions.extend(prompt.input.clone());

        // Build the canonical Responses API payload (no simplified payload).
        let payload = ResponsesApiRequest {
            model: &self.config.model,
            instructions: &full_instructions,
            input: &input_with_instructions,
            tools: &tools_json,
            tool_choice: "auto",
            parallel_tool_calls: false,
            reasoning,
            store,
            stream: true,
            include,
        };

        trace!(
            "POST to {}: {}",
            self.provider.get_full_url(),
            serde_json::to_string(&payload)?
        );

        // Prefer provider's full URL, which should point to ".../responses"
        // when wire_api == Responses.
        let full_url = self.provider.get_full_url();

        let mut attempt = 0;
        let max_retries = self.provider.request_max_retries();

        loop {
            attempt += 1;

            // Use Bearer by default; for Azure with env_key configured, use that token as api-key header if required.
            // We apply provider headers afterwards so provider can override or add specific headers.
            let mut req_builder = self
                .client
                .post(&full_url)
                .header(reqwest::header::CONTENT_TYPE, "application/json")
                .header(reqwest::header::ACCEPT, "text/event-stream")
                // Explicit session and originator headers for observability and tests
                .header("session_id", self.session_id.to_string())
                .header("originator", "codex_cli_rs");

            // For Azure scenarios that require "api-key" header instead of Bearer, provider.apply_http_headers
            // should inject it. As a safe default, attach Bearer when we have a token.
            req_builder = req_builder.bearer_auth(&token).json(&payload);

            let req_builder = self.provider.apply_http_headers(req_builder);

            let res = req_builder.send().await;
            if let Ok(resp) = &res {
                trace!(
                    "Response status: {}, request-id: {}",
                    resp.status(),
                    resp.headers()
                        .get("x-request-id")
                        .map(|v| v.to_str().unwrap_or_default())
                        .unwrap_or_default()
                );
            }

            match res {
                Ok(resp) if resp.status().is_success() => {
                    let content_type = resp
                        .headers()
                        .get("content-type")
                        .and_then(|v| v.to_str().ok())
                        .unwrap_or("");

                    trace!("Content-Type = '{}'", content_type);

                    // If provider returns JSON (e.g., some Azure configurations), process as complete JSON.
                    if content_type.contains("application/json") && !content_type.contains("event-stream") {
                        let response_text = resp.text().await.map_err(CodexErr::Reqwest)?;
                        let (tx_event, rx_event) = mpsc::channel::<Result<ResponseEvent>>(1600);
                        tokio::spawn(async move {
                            if let Err(e) = process_complete_json_response(response_text, tx_event.clone()).await {
                                let _ = tx_event.send(Err(e)).await;
                            }
                        });
                        return Ok(ResponseStream { rx_event });
                    }

                    // Default: SSE stream
                    let (tx_event, rx_event) = mpsc::channel::<Result<ResponseEvent>>(1600);
                    let stream = resp.bytes_stream().map_err(CodexErr::Reqwest);
                    tokio::spawn(sse_shared::process_sse(
                        stream,
                        tx_event,
                        self.provider.stream_idle_timeout(),
                    ));
                    return Ok(ResponseStream { rx_event });
                }
                Ok(res) => {
                    let status = res.status();
                    if !(status == StatusCode::TOO_MANY_REQUESTS || status.is_server_error()) {
                        let body = res.text().await.unwrap_or_default();
                        eprintln!("=== Codex-rs Responses Error ===");
                        eprintln!("Status: {}", status);
                        eprintln!("Body: {}", body);
                        eprintln!("===============================");
                        return Err(CodexErr::UnexpectedStatus(status, body));
                    }

                    if attempt > max_retries {
                        return Err(CodexErr::RetryLimit(status));
                    }

                    let retry_after_secs = res
                        .headers()
                        .get(reqwest::header::RETRY_AFTER)
                        .and_then(|v| v.to_str().ok())
                        .and_then(|s| s.parse::<u64>().ok());

                    // Unify delay units in seconds.
                    let delay = if status == StatusCode::TOO_MANY_REQUESTS {
                        let api_suggested = retry_after_secs.unwrap_or(60);
                        let minimum = std::cmp::max(api_suggested, 60);
                        crate::debug_log!(
                            "[CLIENT] 429 rate limited. API suggested: {}s, Using: {}s",
                            retry_after_secs.unwrap_or(0),
                            minimum
                        );
                        Duration::from_secs(minimum)
                    } else {
                        retry_after_secs
                            .map(Duration::from_secs)
                            .unwrap_or_else(|| backoff(attempt))
                    };

                    crate::debug_log!(
                        "[CLIENT] Retrying in {:?} (attempt {}/{})",
                        delay,
                        attempt,
                        max_retries
                    );
                    tokio::time::sleep(delay).await;
                }
                Err(e) => {
                    if attempt > max_retries {
                        return Err(e.into());
                    }
                    let delay = backoff(attempt);
                    tokio::time::sleep(delay).await;
                }
            }
        }
    }

    pub fn get_provider(&self) -> ModelProviderInfo {
        self.provider.clone()
    }
}

mod sse_shared {
    use super::*;
    use serde::Deserialize;
    use serde::Serialize;

    #[derive(Debug, Deserialize, Serialize)]
    pub struct SseEvent {
        #[serde(rename = "type")]
        pub kind: String,
        pub response: Option<serde_json::Value>,
        pub item: Option<serde_json::Value>,
        pub delta: Option<String>,
    }

    #[derive(Debug, Deserialize)]
    pub struct ResponseCompleted {
        pub id: String,
        pub usage: Option<ResponseCompletedUsage>,
    }

    #[derive(Debug, Deserialize)]
    pub struct ResponseCompletedUsage {
        pub input_tokens: u64,
        pub input_tokens_details: Option<ResponseCompletedInputTokensDetails>,
        pub output_tokens: u64,
        pub output_tokens_details: Option<ResponseCompletedOutputTokensDetails>,
        pub total_tokens: u64,
    }

    impl From<ResponseCompletedUsage> for TokenUsage {
        fn from(val: ResponseCompletedUsage) -> Self {
            TokenUsage {
                input_tokens: val.input_tokens,
                cached_input_tokens: val.input_tokens_details.map(|d| d.cached_tokens),
                output_tokens: val.output_tokens,
                reasoning_output_tokens: val.output_tokens_details.map(|d| d.reasoning_tokens),
                total_tokens: val.total_tokens,
            }
        }
    }

    #[derive(Debug, Deserialize)]
    pub struct ResponseCompletedInputTokensDetails {
        pub cached_tokens: u64,
    }

    #[derive(Debug, Deserialize)]
    pub struct ResponseCompletedOutputTokensDetails {
        pub reasoning_tokens: u64,
    }

    pub async fn process_sse<S>(
        stream: S,
        tx_event: mpsc::Sender<Result<ResponseEvent>>,
        idle_timeout: Duration,
    ) where
        S: futures::Stream<Item = Result<Bytes>> + Unpin,
    {
        let mut stream = stream.eventsource();

        let mut response_completed: Option<ResponseCompleted> = None;

        loop {
            let sse = match tokio::time::timeout(idle_timeout, stream.next()).await {
                Ok(Some(Ok(sse))) => sse,
                Ok(Some(Err(e))) => {
                    tracing::debug!("SSE Error: {e:#}");
                    let event = CodexErr::Stream(e.to_string());
                    let _ = tx_event.send(Err(event)).await;
                    return;
                }
                Ok(None) => {
                    match response_completed {
                        Some(ResponseCompleted { id: response_id, usage }) => {
                            let event = ResponseEvent::Completed {
                                response_id,
                                token_usage: usage.map(Into::into),
                            };
                            let _ = tx_event.send(Ok(event)).await;
                        }
                        None => {
                            let _ = tx_event
                                .send(Err(CodexErr::Stream(
                                    "stream closed before response.completed".into(),
                                )))
                                .await;
                        }
                    }
                    return;
                }
                Err(_) => {
                    let _ = tx_event
                        .send(Err(CodexErr::Stream("idle timeout waiting for SSE".into())))
                        .await;
                    return;
                }
            };

            let event: SseEvent = match serde_json::from_str(&sse.data) {
                Ok(event) => event,
                Err(e) => {
                    tracing::debug!("Failed to parse SSE event: {e}, data: {}", &sse.data);
                    continue;
                }
            };

            tracing::trace!(?event, "SSE event");
            match event.kind.as_str() {
                "response.output_item.done" => {
                    let Some(item_val) = event.item else { continue };
                    let Ok(item) = serde_json::from_value::<ResponseItem>(item_val) else {
                        tracing::debug!("failed to parse ResponseItem from output_item.done");
                        continue;
                    };

                    let event = ResponseEvent::OutputItemDone(item);
                    if tx_event.send(Ok(event)).await.is_err() {
                        return;
                    }
                }
                "response.output_text.delta" => {
                    if let Some(delta) = event.delta {
                        let event = ResponseEvent::OutputTextDelta(delta);
                        if tx_event.send(Ok(event)).await.is_err() {
                            return;
                        }
                    }
                }
                "response.reasoning_summary_text.delta" => {
                    if let Some(delta) = event.delta {
                        let event = ResponseEvent::ReasoningSummaryDelta(delta);
                        if tx_event.send(Ok(event)).await.is_err() {
                            return;
                        }
                    }
                }
                "response.created" => {
                    if event.response.is_some() {
                        let _ = tx_event.send(Ok(ResponseEvent::Created {})).await;
                    }
                }
                "response.failed" => {
                    if let Some(resp_val) = event.response {
                        let error = resp_val
                            .get("error")
                            .and_then(|v| v.get("message"))
                            .and_then(|v| v.as_str())
                            .unwrap_or("response.failed event received");

                        let _ = tx_event
                            .send(Err(CodexErr::Stream(error.to_string())))
                            .await;
                    }
                }
                "response.completed" => {
                    if let Some(resp_val) = event.response {
                        match serde_json::from_value::<ResponseCompleted>(resp_val) {
                            Ok(r) => {
                                response_completed = Some(r);
                            }
                            Err(e) => {
                                tracing::debug!("failed to parse ResponseCompleted: {e}");
                                continue;
                            }
                        };
                    };
                }
                "response.content_part.done"
                | "response.function_call_arguments.delta"
                | "response.in_progress"
                | "response.output_item.added"
                | "response.output_text.done"
                | "response.reasoning_summary_part.added"
                | "response.reasoning_summary_text.done" => {}
                other => {
                    tracing::debug!("sse event: {}", other);
                }
            }
        }
    }
}


impl ModelClient {
    /// used in tests to stream from a text SSE file
    async fn stream_from_fixture_local(
        &self,
        path: impl AsRef<Path>,
        provider: ModelProviderInfo,
    ) -> Result<ResponseStream> {
        let (tx_event, rx_event) = mpsc::channel::<Result<ResponseEvent>>(1600);
        let f = std::fs::File::open(path.as_ref())?;
        let reader = std::io::BufReader::new(f);
        let mut content = String::new();
        for line in reader.lines() {
            let line = line?;
            content.push_str(&line);
            content.push_str("\n\n");
        }

        let rdr = std::io::Cursor::new(content);
        let stream = ReaderStream::new(rdr).map_err(CodexErr::Io);
        tokio::spawn(sse_shared::process_sse(
            stream,
            tx_event,
            provider.stream_idle_timeout(),
        ));
        Ok(ResponseStream { rx_event })
    }
}

#[cfg(test)]
mod tests {
    #![allow(clippy::expect_used, clippy::unwrap_used)]

    use super::*;
    use serde_json::json;
    use tokio::sync::mpsc;
    use tokio_test::io::Builder as IoBuilder;
    use tokio_util::io::ReaderStream;

    // ────────────────────────────
    // Helpers
    // ────────────────────────────

    /// Runs the SSE parser on pre-chunked byte slices and returns every event
    /// (including any final `Err` from a stream-closure check).
    async fn collect_events(
        chunks: &[&[u8]],
        provider: ModelProviderInfo,
    ) -> Vec<Result<ResponseEvent>> {
        let mut builder = IoBuilder::new();
        for chunk in chunks {
            builder.read(chunk);
        }

        let reader = builder.build();
        let stream = ReaderStream::new(reader).map_err(CodexErr::Io);
        let (tx, mut rx) = mpsc::channel::<Result<ResponseEvent>>(16);
        tokio::spawn(Self::process_sse_local(stream, tx, provider.stream_idle_timeout()));

        let mut events = Vec::new();
        while let Some(ev) = rx.recv().await {
            events.push(ev);
        }
        events
    }

    /// Builds an in-memory SSE stream from JSON fixtures and returns only the
    /// successfully parsed events (panics on internal channel errors).
    async fn run_sse(
        events: Vec<serde_json::Value>,
        provider: ModelProviderInfo,
    ) -> Vec<ResponseEvent> {
        let mut body = String::new();
        for e in events {
            let kind = e
                .get("type")
                .and_then(|v| v.as_str())
                .expect("fixture event missing type");
            if e.as_object().map(|o| o.len() == 1).unwrap_or(false) {
                body.push_str(&format!("event: {kind}\n\n"));
            } else {
                body.push_str(&format!("event: {kind}\ndata: {e}\n\n"));
            }
        }

        let (tx, mut rx) = mpsc::channel::<Result<ResponseEvent>>(8);
        let stream = ReaderStream::new(std::io::Cursor::new(body)).map_err(CodexErr::Io);
        tokio::spawn(Self::process_sse_local(stream, tx, provider.stream_idle_timeout()));

        let mut out = Vec::new();
        while let Some(ev) = rx.recv().await {
            out.push(ev.expect("channel closed"));
        }
        out
    }

    // ────────────────────────────
    // Tests from `implement-test-for-responses-api-sse-parser`
    // ────────────────────────────

    #[tokio::test]
    async fn parses_items_and_completed() {
        let item1 = json!({
            "type": "response.output_item.done",
            "item": {
                "type": "message",
                "role": "assistant",
                "content": [{"type": "output_text", "text": "Hello"}]
            }
        })
        .to_string();

        let item2 = json!({
            "type": "response.output_item.done",
            "item": {
                "type": "message",
                "role": "assistant",
                "content": [{"type": "output_text", "text": "World"}]
            }
        })
        .to_string();

        let completed = json!({
            "type": "response.completed",
            "response": { "id": "resp1" }
        })
        .to_string();

        let sse1 = format!("event: response.output_item.done\ndata: {item1}\n\n");
        let sse2 = format!("event: response.output_item.done\ndata: {item2}\n\n");
        let sse3 = format!("event: response.completed\ndata: {completed}\n\n");

        let provider = ModelProviderInfo {
            name: "test".to_string(),
            base_url: Some("https://test.com".to_string()),
            env_key: Some("TEST_API_KEY".to_string()),
            env_key_instructions: None,
            wire_api: WireApi::Responses,
            query_params: None,
            http_headers: None,
            env_http_headers: None,
            request_max_retries: Some(0),
            stream_max_retries: Some(0),
            stream_idle_timeout_ms: Some(1000),
            requires_auth: false,
        };

        let events = collect_events(
            &[sse1.as_bytes(), sse2.as_bytes(), sse3.as_bytes()],
            provider,
        )
        .await;

        assert_eq!(events.len(), 3);

        matches!(
            &events[0],
            Ok(ResponseEvent::OutputItemDone(ResponseItem::Message { role, .. }))
                if role == "assistant"
        );

        matches!(
            &events[1],
            Ok(ResponseEvent::OutputItemDone(ResponseItem::Message { role, .. }))
                if role == "assistant"
        );

        match &events[2] {
            Ok(ResponseEvent::Completed {
                response_id,
                token_usage,
            }) => {
                assert_eq!(response_id, "resp1");
                assert!(token_usage.is_none());
            }
            other => panic!("unexpected third event: {other:?}"),
        }
    }

    #[tokio::test]
    async fn error_when_missing_completed() {
        let item1 = json!({
            "type": "response.output_item.done",
            "item": {
                "type": "message",
                "role": "assistant",
                "content": [{"type": "output_text", "text": "Hello"}]
            }
        })
        .to_string();

        let sse1 = format!("event: response.output_item.done\ndata: {item1}\n\n");
        let provider = ModelProviderInfo {
            name: "test".to_string(),
            base_url: Some("https://test.com".to_string()),
            env_key: Some("TEST_API_KEY".to_string()),
            env_key_instructions: None,
            wire_api: WireApi::Responses,
            query_params: None,
            http_headers: None,
            env_http_headers: None,
            request_max_retries: Some(0),
            stream_max_retries: Some(0),
            stream_idle_timeout_ms: Some(1000),
            requires_auth: false,
        };

        let events = collect_events(&[sse1.as_bytes()], provider).await;

        assert_eq!(events.len(), 2);

        matches!(events[0], Ok(ResponseEvent::OutputItemDone(_)));

        match &events[1] {
            Err(CodexErr::Stream(msg)) => {
                assert_eq!(msg, "stream closed before response.completed")
            }
            other => panic!("unexpected second event: {other:?}"),
        }
    }

    // ────────────────────────────
    // Table-driven test from `main`
    // ────────────────────────────

    /// Verifies that the adapter produces the right `ResponseEvent` for a
    /// variety of incoming `type` values.
    #[tokio::test]
    async fn table_driven_event_kinds() {
        struct TestCase {
            name: &'static str,
            event: serde_json::Value,
            expect_first: fn(&ResponseEvent) -> bool,
            expected_len: usize,
        }

        fn is_created(ev: &ResponseEvent) -> bool {
            matches!(ev, ResponseEvent::Created)
        }
        fn is_output(ev: &ResponseEvent) -> bool {
            matches!(ev, ResponseEvent::OutputItemDone(_))
        }
        fn is_completed(ev: &ResponseEvent) -> bool {
            matches!(ev, ResponseEvent::Completed { .. })
        }

        let completed = json!({
            "type": "response.completed",
            "response": {
                "id": "c",
                "usage": {
                    "input_tokens": 0,
                    "input_tokens_details": null,
                    "output_tokens": 0,
                    "output_tokens_details": null,
                    "total_tokens": 0
                },
                "output": []
            }
        });

        let cases = vec![
            TestCase {
                name: "created",
                event: json!({"type": "response.created", "response": {}}),
                expect_first: is_created,
                expected_len: 2,
            },
            TestCase {
                name: "output_item.done",
                event: json!({
                    "type": "response.output_item.done",
                    "item": {
                        "type": "message",
                        "role": "assistant",
                        "content": [
                            {"type": "output_text", "text": "hi"}
                        ]
                    }
                }),
                expect_first: is_output,
                expected_len: 2,
            },
            TestCase {
                name: "unknown",
                event: json!({"type": "response.new_tool_event"}),
                expect_first: is_completed,
                expected_len: 1,
            },
        ];

        for case in cases {
            let mut evs = vec![case.event];
            evs.push(completed.clone());

            let provider = ModelProviderInfo {
                name: "test".to_string(),
                base_url: Some("https://test.com".to_string()),
                env_key: Some("TEST_API_KEY".to_string()),
                env_key_instructions: None,
                wire_api: WireApi::Responses,
                query_params: None,
                http_headers: None,
                env_http_headers: None,
                request_max_retries: Some(0),
                stream_max_retries: Some(0),
                stream_idle_timeout_ms: Some(1000),
                requires_auth: false,
            };

            let out = run_sse(evs, provider).await;
            assert_eq!(out.len(), case.expected_len, "case {}", case.name);
            assert!(
                (case.expect_first)(&out[0]),
                "first event mismatch in case {}",
                case.name
            );
        }
    }
}

/// Process a complete JSON response from Azure OpenAI and convert it to events
async fn process_complete_json_response(
    response_text: String,
    tx_event: mpsc::Sender<Result<ResponseEvent>>,
) -> Result<()> {
    use serde_json::Value;
    use crate::protocol::TokenUsage;

    trace!("=== Complete JSON Response ===");
    trace!("{}", response_text);
    trace!("==============================");

    // Parse the JSON response
    let response: Value = serde_json::from_str(&response_text)
        .map_err(|e| CodexErr::Stream(format!("Failed to parse JSON response: {}", e)))?;

    // Send created event
    let _ = tx_event.send(Ok(ResponseEvent::Created)).await;

    // Extract the text content from the response and create proper ResponseItem
    if let Some(output) = response.get("output").and_then(|o| o.as_array()) {
        trace!("Found output array with {} items", output.len());

        let mut collected_text = String::new();

        for item in output {
            if item.get("type").and_then(|t| t.as_str()) == Some("message") {
                if let Some(content) = item.get("content").and_then(|c| c.as_array()) {
                    for content_item in content {
                        if content_item.get("type").and_then(|t| t.as_str()) == Some("output_text") {
                            if let Some(text) = content_item.get("text").and_then(|t| t.as_str()) {
                                collected_text.push_str(text);

                                // Send the text as a delta event for streaming display
                                let event = ResponseEvent::OutputTextDelta(text.to_string());
                                if tx_event.send(Ok(event)).await.is_err() {
                                    return Ok(()); // Receiver dropped
                                }
                            }
                        }
                    }
                }
            }
        }

        // Create a complete ResponseItem::Message for conversation history
        if !collected_text.is_empty() {
            use crate::models::{ResponseItem, ContentItem};

            let message_item = ResponseItem::Message {
                id: response.get("id")
                    .and_then(|id| id.as_str())
                    .map(|s| s.to_string()),
                role: "assistant".to_string(),
                content: vec![ContentItem::OutputText { text: collected_text }],
            };

            // Send the complete message as OutputItemDone for conversation history
            let event = ResponseEvent::OutputItemDone(message_item);
            if tx_event.send(Ok(event)).await.is_err() {
                return Ok(()); // Receiver dropped
            }
        }
    } else {
        trace!("No output array found in response");
    }

    // Extract token usage
    let token_usage = response.get("usage").and_then(|usage| {
        let input_tokens = usage.get("input_tokens")?.as_u64()?;
        let output_tokens = usage.get("output_tokens")?.as_u64()?;
        let total_tokens = usage.get("total_tokens")?.as_u64()?;

        // Extract cached tokens if available
        let cached_input_tokens = usage.get("input_tokens_details")
            .and_then(|details| details.get("cached_tokens"))
            .and_then(|v| v.as_u64());

        // Extract reasoning tokens if available
        let reasoning_output_tokens = usage.get("output_tokens_details")
            .and_then(|details| details.get("reasoning_tokens"))
            .and_then(|v| v.as_u64());

        Some(TokenUsage {
            input_tokens,
            cached_input_tokens,
            output_tokens,
            reasoning_output_tokens,
            total_tokens,
        })
    });

    // Send completion event
    let response_id = response.get("id")
        .and_then(|id| id.as_str())
        .unwrap_or("unknown")
        .to_string();

    let completion_event = ResponseEvent::Completed {
        response_id,
        token_usage,
    };

    let _ = tx_event.send(Ok(completion_event)).await;
    Ok(())
}
