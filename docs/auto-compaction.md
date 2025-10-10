# Automatic Conversation Compaction

Codex automatically summarizes an ongoing conversation whenever the tracked
token usage approaches the model’s context window. The goal is to keep the
session responsive without repeatedly asking the user to start over. This
document records the strategy the core implementation follows.

## When compaction triggers

1. Every turn we compute the blended `TokenUsage` (non-cached input + output).
2. Codex triggers compaction as soon as the conversation leaves at most 5% of
   the model's context window or the configured `auto_compact_token_limit`
   threshold (when present) is hit. If the context window is unknown we fall
   back to the configured token limit.

## Preparing the summarizer prompt

To avoid the summarizer itself exceeding the context window, the compaction
task now prepares the prompt in three stages:

1. **History snapshot** – capture the current conversation including user
   instructions and environment context.
2. **Token budget** – reserve at most 80% of the model’s context window for
   the summarizer turn. This leaves headroom for the summarization instructions
   and the model’s streamed response.
3. **History trimming** – drop the oldest, non-prefix messages until the
   estimated token count falls under the reserved budget. If trimming occurs,
   append a system note so the summarizer knows earlier context was omitted.

The trimming step only affects the prompt sent to the summarizer. The full
history remains intact until the summarizer produces the replacement summary.

## Summarization and history replacement

1. The compact prompt includes the standard summarization template plus the
   shortened history described above.
2. When the summarizer completes successfully, we lift the assistant’s reply
   into a bridge message that restates the remaining user instructions and the
   generated summary. The session history is replaced with that bridge.
3. If the summarizer stream fails specifically because the context window is
   still exceeded, we emit a background notice, skip the summarizer, and fall
   back to a canned summary explaining that older turns were truncated.

This guarantees that auto-compaction always produces a compacted history, even
when the summarizer cannot run, and that subsequent turns no longer repeatedly
fail with context-window errors.
