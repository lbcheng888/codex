use std::time::Duration;

use bytes::Bytes;
use eventsource_stream::Eventsource;
use futures::Stream;
use futures::StreamExt;
use futures::TryStreamExt;
use reqwest::StatusCode;
use serde_json::json;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;
use tokio::sync::mpsc;
use tokio::time::timeout;
use tracing::debug;
use tracing::trace;

use crate::ModelProviderInfo;
use crate::client_common::Prompt;
use crate::client_common::ResponseEvent;
use crate::client_common::ResponseStream;
use crate::error::CodexErr;
use crate::error::Result;
use crate::flags::OPENAI_REQUEST_MAX_RETRIES;
use crate::flags::OPENAI_STREAM_IDLE_TIMEOUT_MS;
use crate::models::ContentItem;
use crate::models::ResponseItem;
use crate::openai_tools::create_tools_json_for_chat_completions_api;
use crate::util::backoff;

/// Implementation for the classic Chat Completions API.
pub(crate) async fn stream_chat_completions(
    prompt: &Prompt,
    model: &str,
    client: &reqwest::Client,
    provider: &ModelProviderInfo,
) -> Result<ResponseStream> {
    // Build messages array
    let mut messages = Vec::<serde_json::Value>::new();

    let full_instructions = prompt.get_full_instructions(model);
    messages.push(json!({"role": "system", "content": full_instructions}));

    for item in &prompt.input {
        match item {
            ResponseItem::Message { role, content } => {
                let mut text = String::new();
                for c in content {
                    match c {
                        ContentItem::InputText { text: t }
                        | ContentItem::OutputText { text: t } => {
                            text.push_str(t);
                        }
                        _ => {}
                    }
                }
                messages.push(json!({"role": role, "content": text}));
            }
            ResponseItem::FunctionCall {
                name,
                arguments,
                call_id,
            } => {
                messages.push(json!({
                    "role": "assistant",
                    "content": null,
                    "tool_calls": [{
                        "id": call_id,
                        "type": "function",
                        "function": {
                            "name": name,
                            "arguments": arguments,
                        }
                    }]
                }));
            }
            ResponseItem::LocalShellCall {
                id,
                call_id: _,
                status,
                action,
            } => {
                // Confirm with API team.
                messages.push(json!({
                    "role": "assistant",
                    "content": null,
                    "tool_calls": [{
                        "id": id.clone().unwrap_or_else(|| "".to_string()),
                        "type": "local_shell_call",
                        "status": status,
                        "action": action,
                    }]
                }));
            }
            ResponseItem::FunctionCallOutput { call_id, output } => {
                messages.push(json!({
                    "role": "tool",
                    "tool_call_id": call_id,
                    "content": output.content,
                }));
            }
            ResponseItem::Reasoning { .. } | ResponseItem::Other => {
                // Omit these items from the conversation history.
                continue;
            }
        }
    }

    let tools_json = create_tools_json_for_chat_completions_api(prompt, model)?;
    let payload = json!({
        "model": model,
        "messages": messages,
        "stream": true,
        "tools": tools_json,
    });

    debug!(
        "POST to {}: {}",
        provider.get_full_url(),
        serde_json::to_string_pretty(&payload).unwrap_or_default()
    );
    
    crate::debug_log!("[CHAT] Sending request to {} with model: {}", provider.get_full_url(), model);
    crate::debug_log!("[CHAT] Message count: {}, Tools count: {}", messages.len(), tools_json.len());
    crate::debug_log!("[CHAT] Full request payload: {}", serde_json::to_string_pretty(&payload).unwrap_or_default());

    let mut attempt = 0;
    loop {
        attempt += 1;

        let req_builder = provider.create_request_builder(client)?;

        let res = req_builder
            .header(reqwest::header::ACCEPT, "text/event-stream")
            .json(&payload)
            .send()
            .await;

        match res {
            Ok(resp) if resp.status().is_success() => {
                crate::debug_log!("[CHAT] HTTP Response successful: {}", resp.status());
                let (tx_event, rx_event) = mpsc::channel::<Result<ResponseEvent>>(16);
                let stream = resp.bytes_stream().map_err(CodexErr::Reqwest);
                tokio::spawn(process_chat_sse(stream, tx_event));
                return Ok(ResponseStream { rx_event });
            }
            Ok(res) => {
                let status = res.status();
                crate::debug_log!("[CHAT] HTTP Response error status: {}", status);
                if !(status == StatusCode::TOO_MANY_REQUESTS || status.is_server_error()) {
                    let body = (res.text().await).unwrap_or_default();
                    crate::debug_log!("[CHAT] Error response body: {}", body);
                    return Err(CodexErr::UnexpectedStatus(status, body));
                }

                if attempt > *OPENAI_REQUEST_MAX_RETRIES {
                    if status == StatusCode::TOO_MANY_REQUESTS {
                        crate::debug_log!(
                            "[CHAT] Rate limit retry exhausted. Grok-4 has limits: 32K TPM, 120 RPM. Consider spacing requests further apart."
                        );
                    }
                    return Err(CodexErr::RetryLimit(status));
                }

                let retry_after_secs = res
                    .headers()
                    .get(reqwest::header::RETRY_AFTER)
                    .and_then(|v| v.to_str().ok())
                    .and_then(|s| s.parse::<u64>().ok());

                let delay = if status == StatusCode::TOO_MANY_REQUESTS {
                    // For 429 errors, ensure minimum 60 seconds delay for Grok API rate limits
                    let api_suggested_delay = retry_after_secs.unwrap_or(60);
                    let minimum_delay = std::cmp::max(api_suggested_delay, 60);
                    crate::debug_log!(
                        "[CHAT] Rate limit hit (429). API suggested: {}s, Using: {}s", 
                        retry_after_secs.unwrap_or(0), 
                        minimum_delay
                    );
                    Duration::from_secs(minimum_delay)
                } else {
                    // For server errors, use existing backoff logic
                    retry_after_secs
                        .map(|s| Duration::from_secs(s))
                        .unwrap_or_else(|| backoff(attempt))
                };
                
                crate::debug_log!("[CHAT] Retrying in {:?} (attempt {}/{})", delay, attempt, *OPENAI_REQUEST_MAX_RETRIES);
                tokio::time::sleep(delay).await;
            }
            Err(e) => {
                crate::debug_log!("[CHAT] HTTP Request failed: {}", e);
                if attempt > *OPENAI_REQUEST_MAX_RETRIES {
                    return Err(e.into());
                }
                let delay = backoff(attempt);
                tokio::time::sleep(delay).await;
            }
        }
    }
}

/// Lightweight SSE processor for the Chat Completions streaming format. The
/// output is mapped onto Codex's internal [`ResponseEvent`] so that the rest
/// of the pipeline can stay agnostic of the underlying wire format.
async fn process_chat_sse<S>(stream: S, tx_event: mpsc::Sender<Result<ResponseEvent>>)
where
    S: Stream<Item = Result<Bytes>> + Unpin,
{
    let mut stream = stream.eventsource();

    let idle_timeout = *OPENAI_STREAM_IDLE_TIMEOUT_MS;

    // State to accumulate a function call across streaming chunks.
    // OpenAI may split the `arguments` string over multiple `delta` events
    // until the chunk whose `finish_reason` is `tool_calls` is emitted. We
    // keep collecting the pieces here and forward a single
    // `ResponseItem::FunctionCall` once the call is complete.
    #[derive(Default)]
    struct FunctionCallState {
        name: Option<String>,
        arguments: String,
        call_id: Option<String>,
        active: bool,
    }

    let mut fn_call_state = FunctionCallState::default();

    loop {
        let sse = match timeout(idle_timeout, stream.next()).await {
            Ok(Some(Ok(ev))) => ev,
            Ok(Some(Err(e))) => {
                let _ = tx_event.send(Err(CodexErr::Stream(e.to_string()))).await;
                return;
            }
            Ok(None) => {
                crate::debug_log!("[CHAT] Stream closed gracefully - fn_call_state.active: {}", fn_call_state.active);
                
                // Check if we have an incomplete function call when stream ends
                if fn_call_state.active {
                    crate::debug_log!("[CHAT] Stream ended with active function call - emitting FunctionCall");
                    let item = ResponseItem::FunctionCall {
                        name: fn_call_state.name.clone().unwrap_or_else(|| "".to_string()),
                        arguments: fn_call_state.arguments.clone(),
                        call_id: fn_call_state.call_id.clone().unwrap_or_else(String::new),
                    };
                    
                    crate::debug_log!("[CHAT] Emitting FunctionCall on stream end: {:#?}", item);
                    let _ = tx_event.send(Ok(ResponseEvent::OutputItemDone(item))).await;
                }
                
                // Stream closed gracefully – emit Completed with dummy id.
                let _ = tx_event
                    .send(Ok(ResponseEvent::Completed {
                        response_id: String::new(),
                        token_usage: None,
                    }))
                    .await;
                return;
            }
            Err(_) => {
                let _ = tx_event
                    .send(Err(CodexErr::Stream("idle timeout waiting for SSE".into())))
                    .await;
                return;
            }
        };

        // OpenAI Chat streaming sends a literal string "[DONE]" when finished.
        if sse.data.trim() == "[DONE]" {
            crate::debug_log!("[CHAT] Received [DONE] - fn_call_state.active: {}", fn_call_state.active);
            
            // Check if we have an incomplete function call when [DONE] is received
            if fn_call_state.active {
                crate::debug_log!("[CHAT] [DONE] with active function call - emitting FunctionCall");
                let item = ResponseItem::FunctionCall {
                    name: fn_call_state.name.clone().unwrap_or_else(|| "".to_string()),
                    arguments: fn_call_state.arguments.clone(),
                    call_id: fn_call_state.call_id.clone().unwrap_or_else(String::new),
                };
                
                crate::debug_log!("[CHAT] Emitting FunctionCall on [DONE]: {:#?}", item);
                let _ = tx_event.send(Ok(ResponseEvent::OutputItemDone(item))).await;
            }
            
            let _ = tx_event
                .send(Ok(ResponseEvent::Completed {
                    response_id: String::new(),
                    token_usage: None,
                }))
                .await;
            return;
        }

        // Parse JSON chunk
        let chunk: serde_json::Value = match serde_json::from_str(&sse.data) {
            Ok(v) => v,
            Err(_) => continue,
        };
        trace!("chat_completions received SSE chunk: {chunk:?}");
        
        // DEBUG: Log raw SSE data for Grok debugging
        crate::debug_log!("[CHAT] Raw SSE data: {}", sse.data);
        if let Some(choices) = chunk.get("choices") {
            crate::debug_log!("[CHAT] Choices: {}", serde_json::to_string(choices).unwrap_or_default());
        }

        let choice_opt = chunk.get("choices").and_then(|c| c.get(0));
        
        // DEBUG: Check if this chunk has a finish_reason
        if let Some(choice) = choice_opt {
            if let Some(finish_reason) = choice.get("finish_reason") {
                crate::debug_log!("[CHAT] Found finish_reason in chunk: {:?}", finish_reason);
            } else {
                crate::debug_log!("[CHAT] No finish_reason in this chunk");
            }
        }

        if let Some(choice) = choice_opt {
            // Handle assistant content tokens.
            if let Some(content) = choice
                .get("delta")
                .and_then(|d| d.get("content"))
                .and_then(|c| c.as_str())
            {
                let item = ResponseItem::Message {
                    role: "assistant".to_string(),
                    content: vec![ContentItem::OutputText {
                        text: content.to_string(),
                    }],
                };

                let _ = tx_event.send(Ok(ResponseEvent::OutputItemDone(item))).await;
            }
            
            // Handle Grok's reasoning_content (appears in delta for Grok models)
            if let Some(reasoning_content) = choice
                .get("delta")
                .and_then(|d| d.get("reasoning_content"))
                .and_then(|c| c.as_str())
            {
                crate::debug_log!("[CHAT] Grok reasoning content: {}", reasoning_content);
                
                // For now, just log reasoning content without emitting it as a separate item
                // This avoids complexity while still supporting Grok's format
            }

            // Handle streaming function / tool calls.
            if let Some(tool_calls) = choice
                .get("delta")
                .and_then(|d| d.get("tool_calls"))
                .and_then(|tc| tc.as_array())
            {
                if let Some(tool_call) = tool_calls.first() {
                    // Mark that we have an active function call in progress.
                    fn_call_state.active = true;
                    crate::debug_log!("[CHAT] Tool call detected in stream");

                    // Extract call_id if present.
                    if let Some(id) = tool_call.get("id").and_then(|v| v.as_str()) {
                        fn_call_state.call_id.get_or_insert_with(|| id.to_string());
                        crate::debug_log!("[CHAT] Tool call ID: {}", id);
                    }

                    // Extract function details if present.
                    if let Some(function) = tool_call.get("function") {
                        if let Some(name) = function.get("name").and_then(|n| n.as_str()) {
                            fn_call_state.name.get_or_insert_with(|| name.to_string());
                            crate::debug_log!("[CHAT] Tool call function name: {}", name);
                        }

                        if let Some(args_fragment) =
                            function.get("arguments").and_then(|a| a.as_str())
                        {
                            fn_call_state.arguments.push_str(args_fragment);
                            crate::debug_log!("[CHAT] Tool call arguments fragment: {}", args_fragment);
                        }
                    }
                }
            }

            // Emit end-of-turn when finish_reason signals completion.
            if let Some(finish_reason) = choice.get("finish_reason").and_then(|v| v.as_str()) {
                crate::debug_log!("[CHAT] Finish reason: {}", finish_reason);
                match finish_reason {
                    "tool_calls" if fn_call_state.active => {
                        // Build the FunctionCall response item.
                        let item = ResponseItem::FunctionCall {
                            name: fn_call_state.name.clone().unwrap_or_else(|| "".to_string()),
                            arguments: fn_call_state.arguments.clone(),
                            call_id: fn_call_state.call_id.clone().unwrap_or_else(String::new),
                        };

                        crate::debug_log!("[CHAT] Emitting FunctionCall: {:#?}", item);

                        // Emit it downstream.
                        let _ = tx_event.send(Ok(ResponseEvent::OutputItemDone(item))).await;
                    }
                    // Handle providers that use empty string or other finish_reason values for tool calls
                    "" if fn_call_state.active => {
                        // Build the FunctionCall response item.
                        let item = ResponseItem::FunctionCall {
                            name: fn_call_state.name.clone().unwrap_or_else(|| "".to_string()),
                            arguments: fn_call_state.arguments.clone(),
                            call_id: fn_call_state.call_id.clone().unwrap_or_else(String::new),
                        };

                        // Emit it downstream.
                        let _ = tx_event.send(Ok(ResponseEvent::OutputItemDone(item))).await;
                    }
                    "stop" => {
                        // Regular turn without tool-call.
                    }
                    _ => {
                        // If we have an active function call but unknown finish_reason, 
                        // treat it as a tool call completion for compatibility with different providers
                        if fn_call_state.active {
                            let item = ResponseItem::FunctionCall {
                                name: fn_call_state.name.clone().unwrap_or_else(|| "".to_string()),
                                arguments: fn_call_state.arguments.clone(),
                                call_id: fn_call_state.call_id.clone().unwrap_or_else(String::new),
                            };

                            // Emit it downstream.
                            let _ = tx_event.send(Ok(ResponseEvent::OutputItemDone(item))).await;
                        }
                    }
                }

                // Emit Completed regardless of reason so the agent can advance.
                let _ = tx_event
                    .send(Ok(ResponseEvent::Completed {
                        response_id: String::new(),
                        token_usage: None,
                    }))
                    .await;

                // Prepare for potential next turn (should not happen in same stream).
                // fn_call_state = FunctionCallState::default();

                return; // End processing for this SSE stream.
            }
        }
    }
}

/// Optional client-side aggregation helper
///
/// Stream adapter that merges the incremental `OutputItemDone` chunks coming from
/// [`process_chat_sse`] into a *running* assistant message, **suppressing the
/// per-token deltas**.  The stream stays silent while the model is thinking
/// and only emits two events per turn:
///
///   1. `ResponseEvent::OutputItemDone` with the *complete* assistant message
///      (fully concatenated).
///   2. The original `ResponseEvent::Completed` right after it.
///
/// This mirrors the behaviour the TypeScript CLI exposes to its higher layers.
///
/// The adapter is intentionally *lossless*: callers who do **not** opt in via
/// [`AggregateStreamExt::aggregate()`] keep receiving the original unmodified
/// events.
pub(crate) struct AggregatedChatStream<S> {
    inner: S,
    cumulative: String,
    pending_completed: Option<ResponseEvent>,
}

impl<S> Stream for AggregatedChatStream<S>
where
    S: Stream<Item = Result<ResponseEvent>> + Unpin,
{
    type Item = Result<ResponseEvent>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let this = self.get_mut();

        // First, flush any buffered Completed event from the previous call.
        if let Some(ev) = this.pending_completed.take() {
            return Poll::Ready(Some(Ok(ev)));
        }

        loop {
            match Pin::new(&mut this.inner).poll_next(cx) {
                Poll::Pending => return Poll::Pending,
                Poll::Ready(None) => return Poll::Ready(None),
                Poll::Ready(Some(Err(e))) => return Poll::Ready(Some(Err(e))),
                Poll::Ready(Some(Ok(ResponseEvent::OutputItemDone(item)))) => {
                    // If this is an incremental assistant message chunk, accumulate but
                    // do NOT emit yet. Forward any other item (e.g. FunctionCall) right
                    // away so downstream consumers see it.

                    let is_assistant_delta = matches!(&item, crate::models::ResponseItem::Message { role, .. } if role == "assistant");

                    if is_assistant_delta {
                        if let crate::models::ResponseItem::Message { content, .. } = &item {
                            if let Some(text) = content.iter().find_map(|c| match c {
                                crate::models::ContentItem::OutputText { text } => Some(text),
                                _ => None,
                            }) {
                                this.cumulative.push_str(text);
                            }
                        }

                        // Swallow partial assistant chunk; keep polling.
                        continue;
                    }

                    // Not an assistant message – forward immediately.
                    return Poll::Ready(Some(Ok(ResponseEvent::OutputItemDone(item))));
                }
                Poll::Ready(Some(Ok(ResponseEvent::Completed {
                    response_id,
                    token_usage,
                }))) => {
                    if !this.cumulative.is_empty() {
                        let aggregated_item = crate::models::ResponseItem::Message {
                            role: "assistant".to_string(),
                            content: vec![crate::models::ContentItem::OutputText {
                                text: std::mem::take(&mut this.cumulative),
                            }],
                        };

                        // Buffer Completed so it is returned *after* the aggregated message.
                        this.pending_completed = Some(ResponseEvent::Completed {
                            response_id,
                            token_usage,
                        });

                        return Poll::Ready(Some(Ok(ResponseEvent::OutputItemDone(
                            aggregated_item,
                        ))));
                    }

                    // Nothing aggregated – forward Completed directly.
                    return Poll::Ready(Some(Ok(ResponseEvent::Completed {
                        response_id,
                        token_usage,
                    })));
                }
                Poll::Ready(Some(Ok(ResponseEvent::Created))) => {
                    // These events are exclusive to the Responses API and
                    // will never appear in a Chat Completions stream.
                    continue;
                }
            }
        }
    }
}

/// Extension trait that activates aggregation on any stream of [`ResponseEvent`].
pub(crate) trait AggregateStreamExt: Stream<Item = Result<ResponseEvent>> + Sized {
    /// Returns a new stream that emits **only** the final assistant message
    /// per turn instead of every incremental delta.  The produced
    /// `ResponseEvent` sequence for a typical text turn looks like:
    ///
    /// ```ignore
    ///     OutputItemDone(<full message>)
    ///     Completed
    /// ```
    ///
    /// No other `OutputItemDone` events will be seen by the caller.
    ///
    /// Usage:
    ///
    /// ```ignore
    /// let agg_stream = client.stream(&prompt).await?.aggregate();
    /// while let Some(event) = agg_stream.next().await {
    ///     // event now contains cumulative text
    /// }
    /// ```
    fn aggregate(self) -> AggregatedChatStream<Self> {
        AggregatedChatStream {
            inner: self,
            cumulative: String::new(),
            pending_completed: None,
        }
    }
}

impl<T> AggregateStreamExt for T where T: Stream<Item = Result<ResponseEvent>> + Sized {}
