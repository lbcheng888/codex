//! Defines the protocol for a Codex session between a client and an agent.
//!
//! Uses a SQ (Submission Queue) / EQ (Event Queue) pattern to asynchronously communicate
//! between user and agent.

use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;
use std::time::Duration;

use mcp_types::CallToolResult;
use serde::Deserialize;
use serde::Serialize;
use strum_macros::Display;
use uuid::Uuid;

use crate::config_types::ReasoningEffort as ReasoningEffortConfig;
use crate::config_types::ReasoningSummary as ReasoningSummaryConfig;
use crate::message_history::HistoryEntry;
use crate::model_provider_info::ModelProviderInfo;
use crate::plan_tool::UpdatePlanArgs;

/// Sandbox execution type - simplified to only support no sandboxing
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SandboxType {
    None,
}

/// Sandbox policy - simplified to only support full access
#[derive(Debug, Clone, PartialEq)]
pub enum SandboxPolicy {
    DangerFullAccess,
    WorkspaceWrite {
        writable_roots: Vec<PathBuf>,
        network_access: bool,
    },
}

impl SandboxPolicy {
    pub fn new_read_only_policy() -> Self {
        Self::DangerFullAccess
    }

    pub fn new_workspace_write_policy() -> Self {
        Self::WorkspaceWrite {
            writable_roots: vec![],
            network_access: false,
        }
    }

    pub fn has_full_network_access(&self) -> bool {
        match self {
            Self::DangerFullAccess => true,
            Self::WorkspaceWrite { network_access, .. } => *network_access,
        }
    }
}

/// Submission Queue Entry - requests from user
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Submission {
    /// Unique id for this Submission to correlate with Events
    pub id: String,
    /// Payload
    pub op: Op,
}

/// Submission operation
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
#[serde(tag = "type", rename_all = "snake_case")]
#[allow(clippy::large_enum_variant)]
#[non_exhaustive]
pub enum Op {
    /// Configure the model session.
    ConfigureSession {
        /// Provider identifier ("openai", "openrouter", ...).
        provider: ModelProviderInfo,

        /// If not specified, server will use its default model.
        model: String,

        model_reasoning_effort: ReasoningEffortConfig,
        model_reasoning_summary: ReasoningSummaryConfig,

        /// Model instructions that are appended to the base instructions.
        user_instructions: Option<String>,

        /// Base instructions override.
        base_instructions: Option<String>,

        /// When to escalate for approval for execution
        approval_policy: AskForApproval,
        /// Disable server-side response storage (send full context each request)
        #[serde(default)]
        disable_response_storage: bool,

        /// Optional external notifier command tokens. Present only when the
        /// client wants the agent to spawn a program after each completed
        /// turn.
        #[serde(skip_serializing_if = "Option::is_none")]
        #[serde(default)]
        notify: Option<Vec<String>>,

        /// Working directory that should be treated as the *root* of the
        /// session. All relative paths supplied by the model as well as the
        /// execution sandbox are resolved against this directory **instead**
        /// of the process-wide current working directory. CLI front-ends are
        /// expected to expand this to an absolute path before sending the
        /// `ConfigureSession` operation so that the business-logic layer can
        /// operate deterministically.
        cwd: std::path::PathBuf,

        /// Path to a rollout file to resume from.
        #[serde(skip_serializing_if = "Option::is_none")]
        resume_path: Option<std::path::PathBuf>,
    },

    /// Abort current task.
    /// This server sends no corresponding Event
    Interrupt,

    /// Input from the user
    UserInput {
        /// User input items, see `InputItem`
        items: Vec<InputItem>,
    },

    /// Approve a command execution
    ExecApproval {
        /// The id of the submission we are approving
        id: String,
        /// The user's decision in response to the request.
        decision: ReviewDecision,
    },

    /// Approve a code patch
    PatchApproval {
        /// The id of the submission we are approving
        id: String,
        /// The user's decision in response to the request.
        decision: ReviewDecision,
    },

    /// Append an entry to the persistent cross-session message history.
    ///
    /// Note the entry is not guaranteed to be logged if the user has
    /// history disabled, it matches the list of "sensitive" patterns, etc.
    AddToHistory {
        /// The message text to be stored.
        text: String,
    },

    /// Request a single history entry identified by `log_id` + `offset`.
    GetHistoryEntryRequest { offset: usize, log_id: u64 },

    /// Request to shut down codex instance.
    Shutdown,
}

/// Determines the conditions under which the user is consulted to approve
/// running the command proposed by Codex.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Serialize, Deserialize, Display)]
#[serde(rename_all = "kebab-case")]
#[strum(serialize_all = "kebab-case")]
pub enum AskForApproval {
    /// Under this policy, only "known safe" commands—as determined by
    /// `is_safe_command()`—that **only read files** are auto‑approved.
    /// Everything else will ask the user to approve.
    #[default]
    #[serde(rename = "untrusted")]
    #[strum(serialize = "untrusted")]
    UnlessTrusted,

    /// *All* commands are auto‑approved, but they are expected to run inside a
    /// sandbox where network access is disabled and writes are confined to a
    /// specific set of paths. If the command fails, it will be escalated to
    /// the user to approve execution without a sandbox.
    OnFailure,

    /// Never ask the user to approve commands. Failures are immediately returned
    /// to the model, and never escalated to the user for approval.
    Never,
}

/// User input
#[non_exhaustive]
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum InputItem {
    Text {
        text: String,
    },
    /// Pre‑encoded data: URI image.
    Image {
        image_url: String,
    },

    /// Local image path provided by the user.  This will be converted to an
    /// `Image` variant (base64 data URL) during request serialization.
    LocalImage {
        path: std::path::PathBuf,
    },
}

/// Event Queue Entry - events from agent
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Event {
    /// Submission `id` that this event is correlated with.
    pub id: String,
    /// Payload
    pub msg: EventMsg,
}

/// Response event from the agent
#[derive(Debug, Clone, Deserialize, Serialize, Display)]
#[serde(tag = "type", rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum EventMsg {
    /// Error while executing a submission
    Error(ErrorEvent),

    /// Agent has started a task
    TaskStarted,

    /// Agent has completed all actions
    TaskComplete(TaskCompleteEvent),

    /// Token count event, sent periodically to report the number of tokens
    /// used in the current session.
    TokenCount(TokenUsage),

    /// Agent text output message
    AgentMessage(AgentMessageEvent),

    /// Agent text output delta message
    AgentMessageDelta(AgentMessageDeltaEvent),

    /// Reasoning event from agent.
    AgentReasoning(AgentReasoningEvent),

    /// Agent reasoning delta event from agent.
    AgentReasoningDelta(AgentReasoningDeltaEvent),

    /// Ack the client's configure message.
    SessionConfigured(SessionConfiguredEvent),

    McpToolCallBegin(McpToolCallBeginEvent),

    McpToolCallEnd(McpToolCallEndEvent),

    /// Notification that the server is about to execute a command.
    ExecCommandBegin(ExecCommandBeginEvent),

    ExecCommandEnd(ExecCommandEndEvent),

    ExecApprovalRequest(ExecApprovalRequestEvent),

    ApplyPatchApprovalRequest(ApplyPatchApprovalRequestEvent),

    BackgroundEvent(BackgroundEventEvent),

    /// Notification that the agent is about to apply a code patch. Mirrors
    /// `ExecCommandBegin` so front‑ends can show progress indicators.
    PatchApplyBegin(PatchApplyBeginEvent),

    /// Notification that a patch application has finished.
    PatchApplyEnd(PatchApplyEndEvent),

    /// Response to GetHistoryEntryRequest.
    GetHistoryEntryResponse(GetHistoryEntryResponseEvent),

    PlanUpdate(UpdatePlanArgs),

    /// Notification that the agent is shutting down.
    ShutdownComplete,
}

// Individual event payload types matching each `EventMsg` variant.

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ErrorEvent {
    pub message: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct TaskCompleteEvent {
    pub last_agent_message: Option<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct TokenUsage {
    pub input_tokens: u64,
    pub cached_input_tokens: Option<u64>,
    pub output_tokens: u64,
    pub reasoning_output_tokens: Option<u64>,
    pub total_tokens: u64,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct FinalOutput {
    pub token_usage: TokenUsage,
}

impl From<TokenUsage> for FinalOutput {
    fn from(token_usage: TokenUsage) -> Self {
        Self { token_usage }
    }
}

impl fmt::Display for FinalOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let u = &self.token_usage;
        write!(
            f,
            "Token usage: total={} input={}{} output={}{}",
            u.total_tokens,
            u.input_tokens,
            u.cached_input_tokens
                .map(|c| format!(" (cached {c})"))
                .unwrap_or_default(),
            u.output_tokens,
            u.reasoning_output_tokens
                .map(|r| format!(" (reasoning {r})"))
                .unwrap_or_default()
        )
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct AgentMessageEvent {
    pub message: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct AgentMessageDeltaEvent {
    pub delta: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct AgentReasoningEvent {
    pub text: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct AgentReasoningDeltaEvent {
    pub delta: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct McpInvocation {
    /// Name of the MCP server as defined in the config.
    pub server: String,
    /// Name of the tool as given by the MCP server.
    pub tool: String,
    /// Arguments to the tool call.
    pub arguments: Option<serde_json::Value>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct McpToolCallBeginEvent {
    /// Identifier so this can be paired with the McpToolCallEnd event.
    pub call_id: String,
    pub invocation: McpInvocation,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct McpToolCallEndEvent {
    /// Identifier for the corresponding McpToolCallBegin that finished.
    pub call_id: String,
    pub invocation: McpInvocation,
    pub duration: Duration,
    /// Result of the tool call. Note this could be an error.
    pub result: Result<CallToolResult, String>,
}

impl McpToolCallEndEvent {
    pub fn is_success(&self) -> bool {
        match &self.result {
            Ok(result) => !result.is_error.unwrap_or(false),
            Err(_) => false,
        }
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ExecCommandBeginEvent {
    /// Identifier so this can be paired with the ExecCommandEnd event.
    pub call_id: String,
    /// The command to be executed.
    pub command: Vec<String>,
    /// The command's working directory if not the default cwd for the agent.
    pub cwd: PathBuf,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ExecCommandEndEvent {
    /// Identifier for the ExecCommandBegin that finished.
    pub call_id: String,
    /// Captured stdout
    pub stdout: String,
    /// Captured stderr
    pub stderr: String,
    /// The command's exit code.
    pub exit_code: i32,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ExecApprovalRequestEvent {
    /// Identifier for the associated exec call, if available.
    pub call_id: String,
    /// The command to be executed.
    pub command: Vec<String>,
    /// The command's working directory.
    pub cwd: PathBuf,
    /// Optional human-readable reason for the approval (e.g. retry without sandbox).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ApplyPatchApprovalRequestEvent {
    /// Responses API call id for the associated patch apply call, if available.
    pub call_id: String,
    pub changes: HashMap<PathBuf, FileChange>,
    /// Optional explanatory reason (e.g. request for extra write access).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
    /// When set, the agent is asking the user to allow writes under this root for the remainder of the session.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub grant_root: Option<PathBuf>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct BackgroundEventEvent {
    pub message: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct PatchApplyBeginEvent {
    /// Identifier so this can be paired with the PatchApplyEnd event.
    pub call_id: String,
    /// If true, there was no ApplyPatchApprovalRequest for this patch.
    pub auto_approved: bool,
    /// The changes to be applied.
    pub changes: HashMap<PathBuf, FileChange>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct PatchApplyEndEvent {
    /// Identifier for the PatchApplyBegin that finished.
    pub call_id: String,
    /// Captured stdout (summary printed by apply_patch).
    pub stdout: String,
    /// Captured stderr (parser errors, IO failures, etc.).
    pub stderr: String,
    /// Whether the patch was applied successfully.
    pub success: bool,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct GetHistoryEntryResponseEvent {
    pub offset: usize,
    pub log_id: u64,
    /// The entry at the requested offset, if available and parseable.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub entry: Option<HistoryEntry>,
}

#[derive(Debug, Default, Clone, Deserialize, Serialize)]
pub struct SessionConfiguredEvent {
    /// Unique id for this session.
    pub session_id: Uuid,

    /// Tell the client what model is being queried.
    pub model: String,

    /// Identifier of the history log file (inode on Unix, 0 otherwise).
    pub history_log_id: u64,

    /// Current number of entries in the history log.
    pub history_entry_count: usize,
}

/// User's decision in response to an ExecApprovalRequest.
#[derive(Debug, Default, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum ReviewDecision {
    /// User has approved this command and the agent should execute it.
    Approved,

    /// User has approved this command and wants to automatically approve any
    /// future identical instances (`command` and `cwd` match exactly) for the
    /// remainder of the session.
    ApprovedForSession,

    /// User has denied this command and the agent should not execute it, but
    /// it should continue the session and try something else.
    #[default]
    Denied,

    /// User has denied this command and the agent should not do anything until
    /// the user's next command.
    Abort,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FileChange {
    Add {
        content: String,
    },
    Delete,
    Update {
        unified_diff: String,
        move_path: Option<PathBuf>,
    },
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Chunk {
    /// 1-based line index of the first line in the original file
    pub orig_index: u32,
    pub deleted_lines: Vec<String>,
    pub inserted_lines: Vec<String>,
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used)]
    use super::*;

    /// Serialize Event to verify that its JSON representation has the expected
    /// amount of nesting.
    #[test]
    fn serialize_event() {
        let session_id: Uuid = uuid::uuid!("67e55044-10b1-426f-9247-bb680e5fe0c8");
        let event = Event {
            id: "1234".to_string(),
            msg: EventMsg::SessionConfigured(SessionConfiguredEvent {
                session_id,
                model: "codex-mini-latest".to_string(),
                history_log_id: 0,
                history_entry_count: 0,
            }),
        };
        let serialized = serde_json::to_string(&event).unwrap();
        assert_eq!(
            serialized,
            r#"{"id":"1234","msg":{"type":"session_configured","session_id":"67e55044-10b1-426f-9247-bb680e5fe0c8","model":"codex-mini-latest","history_log_id":0,"history_entry_count":0}}"#
        );
    }
}
