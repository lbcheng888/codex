use std::collections::VecDeque;
use std::sync::Arc;

use super::Session;
use super::TurnContext;
use super::get_last_assistant_message_from_turn;
use super::is_context_window_exceeded_error;
use crate::Prompt;
use crate::client_common::ResponseEvent;
use crate::error::CodexErr;
use crate::error::Result as CodexResult;
use crate::protocol::AgentMessageEvent;
use crate::protocol::CompactedItem;
use crate::protocol::ErrorEvent;
use crate::protocol::Event;
use crate::protocol::EventMsg;
use crate::protocol::InputItem;
use crate::protocol::InputMessageKind;
use crate::protocol::TaskStartedEvent;
use crate::protocol::TurnContextItem;
use crate::truncate::truncate_middle;
use crate::util::backoff;
use askama::Template;
use codex_protocol::models::ContentItem;
use codex_protocol::models::ReasoningItemContent;
use codex_protocol::models::ReasoningItemReasoningSummary;
use codex_protocol::models::ResponseInputItem;
use codex_protocol::models::ResponseItem;
use codex_protocol::protocol::RolloutItem;
use futures::prelude::*;

pub const SUMMARIZATION_PROMPT: &str = include_str!("../../templates/compact/prompt.md");
const COMPACT_USER_MESSAGE_MAX_TOKENS: usize = 20_000;
const FALLBACK_SUMMARY_TEXT: &str =
    "Summary unavailable: older turns were truncated after exceeding the model's context window.";
const COMPACT_PROMPT_BUDGET_PERCENT: u64 = 80;
const STREAM_CLOSED_BEFORE_COMPLETED: &str = "stream closed before response.completed";

const MESSAGE_METADATA_TOKEN_ESTIMATE: u64 = 8;

#[derive(Template)]
#[template(path = "compact/history_bridge.md", escape = "none")]
struct HistoryBridgeTemplate<'a> {
    user_messages_text: &'a str,
    summary_text: &'a str,
}

pub(crate) async fn run_inline_auto_compact_task(
    sess: Arc<Session>,
    turn_context: Arc<TurnContext>,
) {
    let sub_id = sess.next_internal_sub_id();
    let input = vec![InputItem::Text {
        text: SUMMARIZATION_PROMPT.to_string(),
    }];
    run_compact_task_inner(sess, turn_context, sub_id, input).await;
}

pub(crate) async fn run_compact_task(
    sess: Arc<Session>,
    turn_context: Arc<TurnContext>,
    sub_id: String,
    input: Vec<InputItem>,
) -> Option<String> {
    let start_event = Event {
        id: sub_id.clone(),
        msg: EventMsg::TaskStarted(TaskStartedEvent {
            model_context_window: turn_context.client.get_model_context_window(),
        }),
    };
    sess.send_event(start_event).await;
    run_compact_task_inner(sess.clone(), turn_context, sub_id.clone(), input).await;
    None
}

async fn run_compact_task_inner(
    sess: Arc<Session>,
    turn_context: Arc<TurnContext>,
    sub_id: String,
    input: Vec<InputItem>,
) {
    let initial_input_for_turn: ResponseInputItem = ResponseInputItem::from(input);
    let turn_input = build_compact_prompt_input(
        sess.as_ref(),
        turn_context.as_ref(),
        initial_input_for_turn.clone(),
    )
    .await;

    let prompt = Prompt {
        input: turn_input,
        ..Default::default()
    };

    let max_retries = turn_context.client.get_provider().stream_max_retries();
    let mut retries = 0;
    let mut summary_override: Option<String> = None;

    let rollout_item = RolloutItem::TurnContext(TurnContextItem {
        cwd: turn_context.cwd.clone(),
        approval_policy: turn_context.approval_policy,
        sandbox_policy: turn_context.sandbox_policy.clone(),
        model: turn_context.client.get_model(),
        effort: turn_context.client.get_reasoning_effort(),
        summary: turn_context.client.get_reasoning_summary(),
    });
    sess.persist_rollout_items(&[rollout_item]).await;

    loop {
        let attempt_result =
            drain_to_completed(sess.as_ref(), turn_context.as_ref(), &sub_id, &prompt).await;

        match attempt_result {
            Ok(()) => break,
            Err(CodexErr::Interrupted) => return,
            Err(CodexErr::Stream(message, _)) if is_context_window_exceeded_error(&message) => {
                sess
                    .notify_background_event(
                        &sub_id,
                        "Automatic summarization failed because the conversation already exceeded the model's context window. Trimming older turns and continuing without a summary…",
                    )
                    .await;
                summary_override = Some(FALLBACK_SUMMARY_TEXT.to_string());
                break;
            }
            Err(CodexErr::Stream(message, _))
                if message.contains(STREAM_CLOSED_BEFORE_COMPLETED) =>
            {
                sess
                    .notify_background_event(
                        &sub_id,
                        "Automatic summarization stream closed unexpectedly. Trimming older turns and continuing with a synthetic summary…",
                    )
                    .await;
                summary_override = Some(FALLBACK_SUMMARY_TEXT.to_string());
                break;
            }
            Err(e) => {
                if retries < max_retries {
                    retries += 1;
                    let delay = backoff(retries);
                    sess.notify_stream_error(
                        &sub_id,
                        format!("stream error: {e}; retrying automatically…"),
                    )
                    .await;
                    tokio::time::sleep(delay).await;
                    continue;
                }
                let event = Event {
                    id: sub_id.clone(),
                    msg: EventMsg::Error(ErrorEvent {
                        message: e.to_string(),
                    }),
                };
                sess.send_event(event).await;
                return;
            }
        }
    }

    let history_snapshot = sess.history_snapshot().await;
    let summary_text = summary_override.unwrap_or_else(|| {
        get_last_assistant_message_from_turn(&history_snapshot).unwrap_or_default()
    });
    let user_messages = collect_user_messages(&history_snapshot);
    let initial_context = sess.build_initial_context(turn_context.as_ref());
    let new_history = build_compacted_history(initial_context, &user_messages, &summary_text);
    sess.replace_history(new_history).await;

    let rollout_item = RolloutItem::Compacted(CompactedItem {
        message: summary_text.clone(),
    });
    sess.persist_rollout_items(&[rollout_item]).await;

    let event = Event {
        id: sub_id.clone(),
        msg: EventMsg::AgentMessage(AgentMessageEvent {
            message: "Compact task completed".to_string(),
        }),
    };
    sess.send_event(event).await;
}

pub fn content_items_to_text(content: &[ContentItem]) -> Option<String> {
    let mut pieces = Vec::new();
    for item in content {
        match item {
            ContentItem::InputText { text } | ContentItem::OutputText { text } => {
                if !text.is_empty() {
                    pieces.push(text.as_str());
                }
            }
            ContentItem::InputImage { .. } => {}
        }
    }
    if pieces.is_empty() {
        None
    } else {
        Some(pieces.join("\n"))
    }
}

pub(crate) fn collect_user_messages(items: &[ResponseItem]) -> Vec<String> {
    items
        .iter()
        .filter_map(|item| match item {
            ResponseItem::Message { role, content, .. } if role == "user" => {
                content_items_to_text(content)
            }
            _ => None,
        })
        .filter(|text| !is_session_prefix_message(text))
        .collect()
}

pub fn is_session_prefix_message(text: &str) -> bool {
    matches!(
        InputMessageKind::from(("user", text)),
        InputMessageKind::UserInstructions | InputMessageKind::EnvironmentContext
    )
}

struct TruncationSummary {
    dropped_count: usize,
    dropped_tokens: u64,
}

async fn build_compact_prompt_input(
    sess: &Session,
    turn_context: &TurnContext,
    new_input: ResponseInputItem,
) -> Vec<ResponseItem> {
    let mut history = sess.history_snapshot().await;
    let mut truncation_summary: Option<TruncationSummary> = None;
    let new_input_item: ResponseItem = new_input.clone().into();
    let new_input_tokens = approximate_tokens_for_response_item(&new_input_item);

    if let Some(budget) = compute_compaction_budget(
        turn_context.client.get_model_context_window(),
        turn_context.client.get_auto_compact_token_limit(),
    ) {
        if budget <= new_input_tokens {
            let dropped_tokens = history
                .iter()
                .map(approximate_tokens_for_response_item)
                .sum();
            let dropped_count = history.len();
            history.clear();
            if dropped_tokens > 0 {
                truncation_summary = Some(TruncationSummary {
                    dropped_count,
                    dropped_tokens,
                });
            }
        } else {
            let history_budget = budget - new_input_tokens;
            let total_history_tokens = history
                .iter()
                .map(approximate_tokens_for_response_item)
                .sum::<u64>();
            if total_history_tokens > history_budget {
                let (trimmed, summary) = trim_history_to_budget(history, history_budget);
                history = trimmed;
                truncation_summary = summary;
            }
        }
    }

    if let Some(summary) = truncation_summary {
        if summary.dropped_count > 0 || summary.dropped_tokens > 0 {
            history.push(make_truncation_notice(&summary));
        }
    }

    history.push(new_input.into());
    history
}

fn compute_compaction_budget(context_window: Option<u64>, auto_limit: Option<i64>) -> Option<u64> {
    let window =
        context_window.or_else(|| auto_limit.and_then(|limit| (limit > 0).then_some(limit as u64)));
    window.map(|window| window.saturating_mul(COMPACT_PROMPT_BUDGET_PERCENT) / 100)
}

fn trim_history_to_budget(
    history: Vec<ResponseItem>,
    budget_tokens: u64,
) -> (Vec<ResponseItem>, Option<TruncationSummary>) {
    let mut prefix: Vec<ResponseItem> = Vec::new();
    let mut prefix_tokens: u64 = 0;
    let mut candidates: VecDeque<(ResponseItem, u64)> = VecDeque::new();
    for item in history {
        if is_prefix_response_item(&item) {
            let tokens = approximate_tokens_for_response_item(&item);
            prefix_tokens = prefix_tokens.saturating_add(tokens);
            prefix.push(item);
        } else {
            let tokens = approximate_tokens_for_response_item(&item);
            candidates.push_back((item, tokens));
        }
    }

    let mut total_tokens =
        prefix_tokens + candidates.iter().map(|(_, tokens)| *tokens).sum::<u64>();
    let mut dropped_count = 0usize;
    let mut dropped_tokens = 0u64;

    while total_tokens > budget_tokens {
        let Some((item, tokens)) = candidates.pop_front() else {
            break;
        };
        dropped_count += 1;
        dropped_tokens = dropped_tokens.saturating_add(tokens);
        total_tokens = total_tokens.saturating_sub(tokens);

        if let Some(call_id) = tool_call_id(&item) {
            drop_following_outputs(
                &mut candidates,
                &mut dropped_count,
                &mut dropped_tokens,
                &mut total_tokens,
                &call_id,
            );
        }
    }

    let mut trimmed = prefix;
    trimmed.extend(candidates.into_iter().map(|(item, _)| item));

    let summary = if dropped_count > 0 {
        Some(TruncationSummary {
            dropped_count,
            dropped_tokens,
        })
    } else {
        None
    };

    (trimmed, summary)
}

fn tool_call_id(item: &ResponseItem) -> Option<String> {
    match item {
        ResponseItem::FunctionCall { call_id, .. }
        | ResponseItem::CustomToolCall { call_id, .. }
        | ResponseItem::FunctionCallOutput { call_id, .. }
        | ResponseItem::CustomToolCallOutput { call_id, .. } => Some(call_id.clone()),
        ResponseItem::LocalShellCall { call_id, .. } => call_id.clone(),
        _ => None,
    }
}

fn drop_following_outputs(
    candidates: &mut VecDeque<(ResponseItem, u64)>,
    dropped_count: &mut usize,
    dropped_tokens: &mut u64,
    total_tokens: &mut u64,
    call_id: &str,
) {
    while let Some((next_item, _)) = candidates.front() {
        let should_drop = match next_item {
            ResponseItem::FunctionCallOutput {
                call_id: next_id, ..
            }
            | ResponseItem::CustomToolCallOutput {
                call_id: next_id, ..
            } => next_id == call_id,
            _ => false,
        };
        if !should_drop {
            break;
        }
        let (_, tokens) = candidates.pop_front().unwrap();
        *dropped_count += 1;
        *dropped_tokens = dropped_tokens.saturating_add(tokens);
        *total_tokens = total_tokens.saturating_sub(tokens);
    }
}

fn make_truncation_notice(summary: &TruncationSummary) -> ResponseItem {
    let text = format!(
        "Automatic compaction trimmed {} message(s) (~{} tokens) of older context to stay within the model's window. Use the remaining turns and summary below to recover any important details.",
        summary.dropped_count, summary.dropped_tokens
    );
    ResponseItem::Message {
        id: None,
        role: "system".to_string(),
        content: vec![ContentItem::InputText { text }],
    }
}

fn is_prefix_response_item(item: &ResponseItem) -> bool {
    match item {
        ResponseItem::Message { role, content, .. } if role == "user" => {
            content_items_to_text(content)
                .map(|text| is_session_prefix_message(&text))
                .unwrap_or(false)
        }
        _ => false,
    }
}

fn approximate_tokens_for_response_item(item: &ResponseItem) -> u64 {
    match item {
        ResponseItem::Message { content, .. } => content
            .iter()
            .map(approximate_tokens_for_content_item)
            .sum::<u64>()
            .saturating_add(MESSAGE_METADATA_TOKEN_ESTIMATE),
        ResponseItem::Reasoning {
            summary, content, ..
        } => {
            let summary_tokens = summary
                .iter()
                .map(approximate_tokens_for_reasoning_summary)
                .sum::<u64>();
            let content_tokens = content
                .as_ref()
                .map(|items| {
                    items
                        .iter()
                        .map(approximate_tokens_for_reasoning_content)
                        .sum()
                })
                .unwrap_or(0);
            summary_tokens
                .saturating_add(content_tokens)
                .saturating_add(MESSAGE_METADATA_TOKEN_ESTIMATE)
        }
        ResponseItem::FunctionCall {
            name, arguments, ..
        } => {
            approximate_tokens_for_text(name)
                + approximate_tokens_for_text(arguments)
                + MESSAGE_METADATA_TOKEN_ESTIMATE
        }
        ResponseItem::FunctionCallOutput { output, .. } => {
            approximate_tokens_for_text(&output.content)
                .saturating_add(MESSAGE_METADATA_TOKEN_ESTIMATE)
        }
        ResponseItem::CustomToolCall { name, input, .. } => {
            approximate_tokens_for_text(name)
                + approximate_tokens_for_text(input)
                + MESSAGE_METADATA_TOKEN_ESTIMATE
        }
        ResponseItem::CustomToolCallOutput { output, .. } => {
            approximate_tokens_for_text(output) + MESSAGE_METADATA_TOKEN_ESTIMATE
        }
        ResponseItem::LocalShellCall { action, .. } => approximate_tokens_for_serialized(action)
            .saturating_add(MESSAGE_METADATA_TOKEN_ESTIMATE),
        ResponseItem::WebSearchCall { action, .. } => approximate_tokens_for_serialized(action)
            .saturating_add(MESSAGE_METADATA_TOKEN_ESTIMATE),
        ResponseItem::Other => MESSAGE_METADATA_TOKEN_ESTIMATE,
    }
}

fn approximate_tokens_for_content_item(item: &ContentItem) -> u64 {
    match item {
        ContentItem::InputText { text }
        | ContentItem::OutputText { text }
        | ContentItem::InputImage { image_url: text } => approximate_tokens_for_text(text),
    }
}

fn approximate_tokens_for_reasoning_summary(summary: &ReasoningItemReasoningSummary) -> u64 {
    match summary {
        ReasoningItemReasoningSummary::SummaryText { text } => approximate_tokens_for_text(text),
    }
}

fn approximate_tokens_for_reasoning_content(content: &ReasoningItemContent) -> u64 {
    match content {
        ReasoningItemContent::ReasoningText { text } | ReasoningItemContent::Text { text } => {
            approximate_tokens_for_text(text)
        }
    }
}

fn approximate_tokens_for_serialized<T: serde::Serialize>(value: &T) -> u64 {
    serde_json::to_string(value)
        .map(|s| approximate_tokens_for_text(&s))
        .unwrap_or(0)
}

fn approximate_tokens_for_text(text: &str) -> u64 {
    (text.len() as u64).div_ceil(4)
}

pub(crate) fn build_compacted_history(
    initial_context: Vec<ResponseItem>,
    user_messages: &[String],
    summary_text: &str,
) -> Vec<ResponseItem> {
    let mut history = initial_context;
    let mut user_messages_text = if user_messages.is_empty() {
        "(none)".to_string()
    } else {
        user_messages.join("\n\n")
    };
    // Truncate the concatenated prior user messages so the bridge message
    // stays well under the context window (approx. 4 bytes/token).
    let max_bytes = COMPACT_USER_MESSAGE_MAX_TOKENS * 4;
    if user_messages_text.len() > max_bytes {
        user_messages_text = truncate_middle(&user_messages_text, max_bytes).0;
    }
    let summary_text = if summary_text.is_empty() {
        "(no summary available)".to_string()
    } else {
        summary_text.to_string()
    };
    let Ok(bridge) = HistoryBridgeTemplate {
        user_messages_text: &user_messages_text,
        summary_text: &summary_text,
    }
    .render() else {
        return vec![];
    };
    history.push(ResponseItem::Message {
        id: None,
        role: "user".to_string(),
        content: vec![ContentItem::InputText { text: bridge }],
    });
    history
}

async fn drain_to_completed(
    sess: &Session,
    turn_context: &TurnContext,
    sub_id: &str,
    prompt: &Prompt,
) -> CodexResult<()> {
    let mut stream = turn_context.client.clone().stream(prompt).await?;
    loop {
        let maybe_event = stream.next().await;
        let Some(event) = maybe_event else {
            return Err(CodexErr::Stream(
                "stream closed before response.completed".into(),
                None,
            ));
        };
        match event {
            Ok(ResponseEvent::OutputItemDone(item)) => {
                sess.record_into_history(std::slice::from_ref(&item)).await;
            }
            Ok(ResponseEvent::RateLimits(snapshot)) => {
                sess.update_rate_limits(sub_id, snapshot).await;
            }
            Ok(ResponseEvent::Completed { token_usage, .. }) => {
                sess.update_token_usage_info(sub_id, turn_context, token_usage.as_ref())
                    .await;
                return Ok(());
            }
            Ok(_) => continue,
            Err(e) => return Err(e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use codex_protocol::models::FunctionCallOutputPayload;
    use pretty_assertions::assert_eq;

    #[test]
    fn content_items_to_text_joins_non_empty_segments() {
        let items = vec![
            ContentItem::InputText {
                text: "hello".to_string(),
            },
            ContentItem::OutputText {
                text: String::new(),
            },
            ContentItem::OutputText {
                text: "world".to_string(),
            },
        ];

        let joined = content_items_to_text(&items);

        assert_eq!(Some("hello\nworld".to_string()), joined);
    }

    #[test]
    fn content_items_to_text_ignores_image_only_content() {
        let items = vec![ContentItem::InputImage {
            image_url: "file://image.png".to_string(),
        }];

        let joined = content_items_to_text(&items);

        assert_eq!(None, joined);
    }

    #[test]
    fn collect_user_messages_extracts_user_text_only() {
        let items = vec![
            ResponseItem::Message {
                id: Some("assistant".to_string()),
                role: "assistant".to_string(),
                content: vec![ContentItem::OutputText {
                    text: "ignored".to_string(),
                }],
            },
            ResponseItem::Message {
                id: Some("user".to_string()),
                role: "user".to_string(),
                content: vec![
                    ContentItem::InputText {
                        text: "first".to_string(),
                    },
                    ContentItem::OutputText {
                        text: "second".to_string(),
                    },
                ],
            },
            ResponseItem::Other,
        ];

        let collected = collect_user_messages(&items);

        assert_eq!(vec!["first\nsecond".to_string()], collected);
    }

    #[test]
    fn collect_user_messages_filters_session_prefix_entries() {
        let items = vec![
            ResponseItem::Message {
                id: None,
                role: "user".to_string(),
                content: vec![ContentItem::InputText {
                    text: "<user_instructions>do things</user_instructions>".to_string(),
                }],
            },
            ResponseItem::Message {
                id: None,
                role: "user".to_string(),
                content: vec![ContentItem::InputText {
                    text: "<ENVIRONMENT_CONTEXT>cwd=/tmp</ENVIRONMENT_CONTEXT>".to_string(),
                }],
            },
            ResponseItem::Message {
                id: None,
                role: "user".to_string(),
                content: vec![ContentItem::InputText {
                    text: "real user message".to_string(),
                }],
            },
        ];

        let collected = collect_user_messages(&items);

        assert_eq!(vec!["real user message".to_string()], collected);
    }

    #[test]
    fn build_compacted_history_truncates_overlong_user_messages() {
        // Prepare a very large prior user message so the aggregated
        // `user_messages_text` exceeds the truncation threshold used by
        // `build_compacted_history` (80k bytes).
        let big = "X".repeat(200_000);
        let history = build_compacted_history(Vec::new(), std::slice::from_ref(&big), "SUMMARY");

        // Expect exactly one bridge message added to history (plus any initial context we provided, which is none).
        assert_eq!(history.len(), 1);

        // Extract the text content of the bridge message.
        let bridge_text = match &history[0] {
            ResponseItem::Message { role, content, .. } if role == "user" => {
                content_items_to_text(content).unwrap_or_default()
            }
            other => panic!("unexpected item in history: {other:?}"),
        };

        // The bridge should contain the truncation marker and not the full original payload.
        assert!(
            bridge_text.contains("tokens truncated"),
            "expected truncation marker in bridge message"
        );
        assert!(
            !bridge_text.contains(&big),
            "bridge should not include the full oversized user text"
        );
        assert!(
            bridge_text.contains("SUMMARY"),
            "bridge should include the provided summary text"
        );
    }

    fn user_input(text: &str) -> ResponseItem {
        ResponseItem::Message {
            id: None,
            role: "user".to_string(),
            content: vec![ContentItem::InputText {
                text: text.to_string(),
            }],
        }
    }

    fn function_call(call_id: &str) -> ResponseItem {
        ResponseItem::FunctionCall {
            id: None,
            name: "tool".to_string(),
            arguments: "{}".to_string(),
            call_id: call_id.to_string(),
        }
    }

    fn function_output(call_id: &str) -> ResponseItem {
        ResponseItem::FunctionCallOutput {
            call_id: call_id.to_string(),
            output: FunctionCallOutputPayload {
                success: Some(true),
                content: "ok".to_string(),
            },
        }
    }

    #[test]
    fn trim_history_to_budget_retains_prefix_and_drops_oldest() {
        let prefix = user_input("<user_instructions>keep</user_instructions>");
        let first = user_input("first message");
        let second = user_input("second message");
        let third = user_input("third message");

        let history = vec![prefix.clone(), first.clone(), second.clone(), third.clone()];

        let prefix_tokens = approximate_tokens_for_response_item(&prefix);
        let third_tokens = approximate_tokens_for_response_item(&third);
        let budget = prefix_tokens + third_tokens + 1;

        let (trimmed, summary) = trim_history_to_budget(history, budget);

        assert_eq!(trimmed.first(), Some(&prefix));
        assert!(!trimmed.contains(&first));
        assert!(trimmed.contains(&third));
        assert!(!trimmed.contains(&second));

        let summary = summary.expect("expected truncation summary");
        assert_eq!(summary.dropped_count, 2);
        assert!(summary.dropped_tokens > 0);
    }

    #[test]
    fn compute_compaction_budget_uses_eighty_percent() {
        assert_eq!(compute_compaction_budget(None, None), None);
        assert_eq!(compute_compaction_budget(Some(100), Some(200)), Some(80));
        assert_eq!(compute_compaction_budget(None, Some(200)), Some(160));
    }

    #[test]
    fn make_truncation_notice_emits_system_message() {
        let summary = TruncationSummary {
            dropped_count: 3,
            dropped_tokens: 120,
        };
        let notice = make_truncation_notice(&summary);

        match notice {
            ResponseItem::Message { role, content, .. } => {
                assert_eq!(role, "system");
                let rendered = content_items_to_text(&content).unwrap();
                assert!(rendered.contains("3 message"));
                assert!(rendered.contains("120 tokens"));
            }
            other => panic!("expected system message, found {other:?}"),
        }
    }

    #[test]
    fn trim_history_to_budget_drops_function_call_pairs() {
        let call = function_call("call-1");
        let output = function_output("call-1");
        let message = user_input("after");
        let history = vec![call, output, message.clone()];

        let budget = approximate_tokens_for_response_item(&message);
        let (trimmed, summary) = trim_history_to_budget(history, budget);

        assert_eq!(trimmed, vec![message]);
        let summary = summary.expect("expected summary");
        assert_eq!(summary.dropped_count, 2);
        assert!(summary.dropped_tokens > 0);
    }
}
