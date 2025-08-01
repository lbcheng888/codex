// Improved error handling for mutex operations

use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::atomic::AtomicU64;
use std::time::Duration;

use async_channel::Receiver;
use async_channel::Sender;
use codex_apply_patch::ApplyPatchAction;
use codex_apply_patch::MaybeApplyPatchVerified;
use codex_apply_patch::maybe_parse_apply_patch_verified;
use codex_login::CodexAuth;
use futures::prelude::*;
use mcp_types::CallToolResult;
use serde::Serialize;
use serde_json;
use tokio::sync::Notify;
use tokio::sync::oneshot;
use tokio::task::AbortHandle;
use tracing::debug;
use tracing::error;
use tracing::info;
use tracing::trace;
use tracing::warn;
use uuid::Uuid;

use crate::apply_patch::convert_apply_patch_to_protocol;
use crate::apply_patch::get_writable_roots;
use crate::apply_patch::{self};
use crate::client::ModelClient;
use crate::client_common::Prompt;
use crate::client_common::ResponseEvent;
use crate::config::Config;
use crate::config_types::ShellEnvironmentPolicy;
use crate::conversation_history::ConversationHistory;
use crate::error::CodexErr;
use crate::error::Result as CodexResult;
use crate::exec::ExecParams;
use crate::exec::ExecToolCallOutput;
use crate::exec::process_exec_tool_call;
use crate::exec_env::create_env;
use crate::mcp_connection_manager::McpConnectionManager;
use crate::mcp_tool_call::handle_mcp_tool_call;
use crate::models::ContentItem;
use crate::models::FunctionCallOutputPayload;
use crate::models::LocalShellAction;
use crate::models::ReasoningItemReasoningSummary;
use crate::models::ResponseInputItem;
use crate::models::ResponseItem;
use crate::models::ShellToolCallParams;
use crate::plan_tool::handle_update_plan;
use crate::project_doc::get_user_instructions;
use crate::protocol::AgentMessageDeltaEvent;
use crate::protocol::AgentMessageEvent;
use crate::protocol::AgentReasoningDeltaEvent;
use crate::protocol::AgentReasoningEvent;
use crate::protocol::ApplyPatchApprovalRequestEvent;
use crate::protocol::AskForApproval;
use crate::protocol::BackgroundEventEvent;
use crate::protocol::ErrorEvent;
use crate::protocol::Event;
use crate::protocol::EventMsg;
use crate::protocol::ExecApprovalRequestEvent;
use crate::protocol::ExecCommandBeginEvent;
use crate::protocol::ExecCommandEndEvent;
use crate::protocol::InputItem;
use crate::protocol::Op;
use crate::protocol::ReviewDecision;
// Sandbox imports removed
use crate::protocol::SessionConfiguredEvent;
use crate::protocol::Submission;
use crate::protocol::TaskCompleteEvent;
use crate::rollout::RolloutRecorder;
use crate::safety::SafetyCheck;
use crate::safety::assess_command_safety;
use crate::shell;
use crate::user_notification::UserNotification;
use crate::util::backoff;

/// The high-level interface to the Codex system.
/// It operates as a queue pair where you send submissions and receive events.
pub struct Codex {
    next_id: AtomicU64,
    tx_sub: Sender<Submission>,
    rx_event: Receiver<Event>,
}

/// Wrapper returned by [`Codex::spawn`] containing the spawned [`Codex`],
/// the submission id for the initial `ConfigureSession` request and the
/// unique session id.
pub struct CodexSpawnOk {
    pub codex: Codex,
    pub init_id: String,
    pub session_id: Uuid,
}

impl Codex {
    /// Spawn a new [`Codex`] and initialize the session.
    pub async fn spawn(
        config: Config,
        auth: Option<CodexAuth>,
        ctrl_c: Arc<Notify>,
    ) -> CodexResult<CodexSpawnOk> {
        // experimental resume path (undocumented)
        let resume_path = config.experimental_resume.clone();
        info!("resume_path: {resume_path:?}");
        let (tx_sub, rx_sub) = async_channel::bounded(64);
        let (tx_event, rx_event) = async_channel::bounded(1600);

        let user_instructions = get_user_instructions(&config).await;

        let configure_session = Op::ConfigureSession {
            provider: config.model_provider.clone(),
            model: config.model.clone(),
            model_reasoning_effort: config.model_reasoning_effort,
            model_reasoning_summary: config.model_reasoning_summary,
            user_instructions,
            base_instructions: config.base_instructions.clone(),
            approval_policy: config.approval_policy,
            disable_response_storage: config.disable_response_storage,
            notify: config.notify.clone(),
            cwd: config.cwd.clone(),
            resume_path: resume_path.clone(),
        };

        let config = Arc::new(config);

        // Generate a unique ID for the lifetime of this Codex session.
        let session_id = Uuid::new_v4();
        tokio::spawn(submission_loop(
            session_id, config, auth, rx_sub, tx_event, ctrl_c,
        ));
        let codex = Codex {
            next_id: AtomicU64::new(0),
            tx_sub,
            rx_event,
        };
        let init_id = codex.submit(configure_session).await?;

        Ok(CodexSpawnOk {
            codex,
            init_id,
            session_id,
        })
    }

    /// Submit the `op` wrapped in a `Submission` with a unique ID.
    pub async fn submit(&self, op: Op) -> CodexResult<String> {
        let id = self
            .next_id
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst)
            .to_string();
        let sub = Submission { id: id.clone(), op };
        self.submit_with_id(sub).await?;
        Ok(id)
    }

    /// Use sparingly: prefer `submit()` so Codex is responsible for generating
    /// unique IDs for each submission.
    pub async fn submit_with_id(&self, sub: Submission) -> CodexResult<()> {
        self.tx_sub
            .send(sub)
            .await
            .map_err(|_| CodexErr::InternalAgentDied)?;
        Ok(())
    }

    pub async fn next_event(&self) -> CodexResult<Event> {
        let event = self
            .rx_event
            .recv()
            .await
            .map_err(|_| CodexErr::InternalAgentDied)?;
        Ok(event)
    }
}

/// Context for an initialized model agent
///
/// A session has at most 1 running task at a time, and can be interrupted by user input.
pub(crate) struct Session {
    client: ModelClient,
    pub(crate) tx_event: Sender<Event>,
    ctrl_c: Arc<Notify>,

    /// The session's current working directory. All relative paths provided by
    /// the model are resolved against this path instead of `std::env::current_dir()`.
    pub(crate) cwd: PathBuf,
    base_instructions: Option<String>,
    user_instructions: Option<String>,
    pub(crate) approval_policy: AskForApproval,
    shell_environment_policy: ShellEnvironmentPolicy,
    pub(crate) writable_roots: Mutex<Vec<PathBuf>>,
    disable_response_storage: bool,

    /// Manager for external MCP servers/tools.
    mcp_connection_manager: McpConnectionManager,

    /// External notifier command (will be passed as args to exec()). When
    /// `None` this feature is disabled.
    notify: Option<Vec<String>>,

    /// Optional rollout recorder for persisting the conversation transcript so
    /// sessions can be replayed or inspected later.
    rollout: Mutex<Option<RolloutRecorder>>,
    state: Mutex<State>,
    user_shell: shell::Shell,
}

impl Session {
    fn resolve_path(&self, path: Option<String>) -> PathBuf {
        path.as_ref()
            .map(PathBuf::from)
            .map_or_else(|| self.cwd.clone(), |p| self.cwd.join(p))
    }
}

/// Mutable state of the agent
#[derive(Default)]
struct State {
    approved_commands: HashSet<Vec<String>>,
    current_task: Option<AgentTask>,
    pending_approvals: HashMap<String, oneshot::Sender<ReviewDecision>>,
    pending_input: Vec<ResponseInputItem>,
    history: ConversationHistory,
}

impl Session {
    pub fn set_task(&self, task: AgentTask) {
        let mut state = self.state.lock().expect("Session state mutex was poisoned");
        if let Some(current_task) = state.current_task.take() {
            current_task.abort();
        }
        state.current_task = Some(task);
    }

    pub fn remove_task(&self, sub_id: &str) {
        let mut state = self.state.lock().expect("Session state mutex was poisoned");
        if let Some(task) = &state.current_task {
            if task.sub_id == sub_id {
                state.current_task.take();
            }
        }
    }

    pub fn add_approved_command(&self, command: Vec<String>) {
        let mut state = self.state.lock().expect("Session state mutex was poisoned");
        state.approved_commands.insert(command);
    }

    /// Sends the given event to the client and swallows the send event, if
    /// any, logging it as an error.
    pub(crate) async fn send_event(&self, event: Event) {
        if let Err(e) = self.tx_event.send(event).await {
            error!("failed to send tool call event: {e}");
        }
    }

    pub async fn request_command_approval(
        &self,
        sub_id: String,
        call_id: String,
        command: Vec<String>,
        cwd: PathBuf,
        reason: Option<String>,
    ) -> oneshot::Receiver<ReviewDecision> {
        let (tx_approve, rx_approve) = oneshot::channel();
        let event = Event {
            id: sub_id.clone(),
            msg: EventMsg::ExecApprovalRequest(ExecApprovalRequestEvent {
                call_id,
                command,
                cwd,
                reason,
            }),
        };
        let _ = self.tx_event.send(event).await;
        {
            let mut state = self.state.lock().unwrap();
            state.pending_approvals.insert(sub_id, tx_approve);
        }
        rx_approve
    }

    pub async fn request_patch_approval(
        &self,
        sub_id: String,
        call_id: String,
        action: &ApplyPatchAction,
        reason: Option<String>,
        grant_root: Option<PathBuf>,
    ) -> oneshot::Receiver<ReviewDecision> {
        let (tx_approve, rx_approve) = oneshot::channel();
        let event = Event {
            id: sub_id.clone(),
            msg: EventMsg::ApplyPatchApprovalRequest(ApplyPatchApprovalRequestEvent {
                call_id,
                changes: convert_apply_patch_to_protocol(action),
                reason,
                grant_root,
            }),
        };
        let _ = self.tx_event.send(event).await;
        {
            let mut state = self.state.lock().expect("Session state mutex was poisoned");
            state.pending_approvals.insert(sub_id, tx_approve);
        }
        rx_approve
    }

    pub fn notify_approval(&self, sub_id: &str, decision: ReviewDecision) {
        let mut state = self.state.lock().expect("Session state mutex was poisoned");
        if let Some(tx_approve) = state.pending_approvals.remove(sub_id) {
            tx_approve.send(decision).ok();
        }
    }

    /// Records items to both the rollout and the chat completions/ZDR
    /// transcript, if enabled.
    async fn record_conversation_items(&self, items: &[ResponseItem]) {
        debug!("Recording items for conversation: {items:?}");
        self.record_state_snapshot(items).await;

        self.state.lock().unwrap().history.record_items(items);
    }

    async fn record_state_snapshot(&self, items: &[ResponseItem]) {
        let snapshot = { crate::rollout::SessionStateSnapshot {} };

        let recorder = {
            let guard = self.rollout.lock().expect("Rollout mutex was poisoned");
            guard.as_ref().cloned()
        };

        if let Some(rec) = recorder {
            if let Err(e) = rec.record_state(snapshot).await {
                error!("failed to record rollout state: {e:#}");
            }
            if let Err(e) = rec.record_items(items).await {
                error!("failed to record rollout items: {e:#}");
            }
        }
    }

    async fn notify_exec_command_begin(&self, sub_id: &str, call_id: &str, params: &ExecParams) {
        let event = Event {
            id: sub_id.to_string(),
            msg: EventMsg::ExecCommandBegin(ExecCommandBeginEvent {
                call_id: call_id.to_string(),
                command: params.command.clone(),
                cwd: params.cwd.clone(),
            }),
        };
        let _ = self.tx_event.send(event).await;
    }

    async fn notify_exec_command_end(
        &self,
        sub_id: &str,
        call_id: &str,
        stdout: &str,
        stderr: &str,
        exit_code: i32,
    ) {
        const MAX_STREAM_OUTPUT: usize = 5 * 1024; // 5KiB
        let event = Event {
            id: sub_id.to_string(),
            // Because stdout and stderr could each be up to 100 KiB, we send
            // truncated versions.
            msg: EventMsg::ExecCommandEnd(ExecCommandEndEvent {
                call_id: call_id.to_string(),
                stdout: stdout.chars().take(MAX_STREAM_OUTPUT).collect(),
                stderr: stderr.chars().take(MAX_STREAM_OUTPUT).collect(),
                exit_code,
            }),
        };
        let _ = self.tx_event.send(event).await;
    }

    /// Helper that emits a BackgroundEvent with the given message. This keeps
    /// the call‑sites terse so adding more diagnostics does not clutter the
    /// core agent logic.
    async fn notify_background_event(&self, sub_id: &str, message: impl Into<String>) {
        let event = Event {
            id: sub_id.to_string(),
            msg: EventMsg::BackgroundEvent(BackgroundEventEvent {
                message: message.into(),
            }),
        };
        let _ = self.tx_event.send(event).await;
    }

    /// Returns the input if there was no task running to inject into
    pub fn inject_input(&self, input: Vec<InputItem>) -> Result<(), Vec<InputItem>> {
        let mut state = self.state.lock().unwrap();
        if state.current_task.is_some() {
            state.pending_input.push(input.into());
            Ok(())
        } else {
            Err(input)
        }
    }

    pub fn get_pending_input(&self) -> Vec<ResponseInputItem> {
        let mut state = self.state.lock().unwrap();
        if state.pending_input.is_empty() {
            Vec::with_capacity(0)
        } else {
            let mut ret = Vec::new();
            std::mem::swap(&mut ret, &mut state.pending_input);
            ret
        }
    }

    pub async fn call_tool(
        &self,
        server: &str,
        tool: &str,
        arguments: Option<serde_json::Value>,
        timeout: Option<Duration>,
    ) -> anyhow::Result<CallToolResult> {
        self.mcp_connection_manager
            .call_tool(server, tool, arguments, timeout)
            .await
    }

    pub fn abort(&self) {
        info!("Aborting existing session");
        let mut state = self.state.lock().unwrap();
        state.pending_approvals.clear();
        state.pending_input.clear();
        if let Some(task) = state.current_task.take() {
            task.abort();
        }
    }

    /// Spawn the configured notifier (if any) with the given JSON payload as
    /// the last argument. Failures are logged but otherwise ignored so that
    /// notification issues do not interfere with the main workflow.
    fn maybe_notify(&self, notification: UserNotification) {
        let Some(notify_command) = &self.notify else {
            return;
        };

        if notify_command.is_empty() {
            return;
        }

        let Ok(json) = serde_json::to_string(&notification) else {
            error!("failed to serialise notification payload");
            return;
        };

        let mut command = std::process::Command::new(&notify_command[0]);
        if notify_command.len() > 1 {
            command.args(&notify_command[1..]);
        }
        command.arg(json);

        // Fire-and-forget – we do not wait for completion.
        if let Err(e) = command.spawn() {
            warn!("failed to spawn notifier '{}': {e}", notify_command[0]);
        }
    }
}

impl Drop for Session {
    fn drop(&mut self) {
        self.abort();
    }
}

impl State {
    pub fn partial_clone(&self) -> Self {
        Self {
            approved_commands: self.approved_commands.clone(),
            history: self.history.clone(),
            ..Default::default()
        }
    }
}

/// A series of Turns in response to user input.
pub(crate) struct AgentTask {
    sess: Arc<Session>,
    sub_id: String,
    handle: AbortHandle,
}

impl AgentTask {
    fn spawn(sess: Arc<Session>, sub_id: String, input: Vec<InputItem>) -> Self {
        let handle =
            tokio::spawn(run_task(Arc::clone(&sess), sub_id.clone(), input)).abort_handle();
        Self {
            sess,
            sub_id,
            handle,
        }
    }

    fn abort(self) {
        if !self.handle.is_finished() {
            self.handle.abort();
            let event = Event {
                id: self.sub_id,
                msg: EventMsg::Error(ErrorEvent {
                    message: "Turn interrupted".to_string(),
                }),
            };
            let tx_event = self.sess.tx_event.clone();
            tokio::spawn(async move {
                tx_event.send(event).await.ok();
            });
        }
    }
}

async fn submission_loop(
    mut session_id: Uuid,
    config: Arc<Config>,
    auth: Option<CodexAuth>,
    rx_sub: Receiver<Submission>,
    tx_event: Sender<Event>,
    ctrl_c: Arc<Notify>,
) {
    let mut sess: Option<Arc<Session>> = None;
    // shorthand - send an event when there is no active session
    let send_no_session_event = |sub_id: String| async {
        let event = Event {
            id: sub_id,
            msg: EventMsg::Error(ErrorEvent {
                message: "No session initialized, expected 'ConfigureSession' as first Op"
                    .to_string(),
            }),
        };
        tx_event.send(event).await.ok();
    };

    loop {
        crate::debug_log!("[SUBMISSION_LOOP] Waiting for next submission...");
        let interrupted = ctrl_c.notified();
        let sub = tokio::select! {
            res = rx_sub.recv() => match res {
                Ok(sub) => {
                    crate::debug_log!("[SUBMISSION_LOOP] Received submission with id: {}", sub.id);
                    sub
                },
                Err(_) => {
                    crate::debug_log!("[SUBMISSION_LOOP] Submission channel closed, breaking");
                    break
                },
            },
            _ = interrupted => {
                crate::debug_log!("[SUBMISSION_LOOP] Interrupted by Ctrl+C");
                if let Some(sess) = sess.as_ref(){
                    sess.abort();
                }
                continue;
            },
        };

        debug!(?sub, "Submission");

        // DEBUG: Log Op processing
        if std::env::var("CODEX_DEBUG").is_ok() {
            match &sub.op {
                Op::UserInput { items } => {
                    eprintln!(
                        "[DEBUG CORE] Processing Op::UserInput with {} items",
                        items.len()
                    );
                    for (i, item) in items.iter().enumerate() {
                        match item {
                            crate::protocol::InputItem::Text { text } => {
                                eprintln!("[DEBUG CORE] Item {}: Text = '{}'", i, text);
                            }
                            crate::protocol::InputItem::LocalImage { path } => {
                                eprintln!("[DEBUG CORE] Item {}: LocalImage = {:?}", i, path);
                            }
                            crate::protocol::InputItem::Image { .. } => {
                                eprintln!("[DEBUG CORE] Item {}: Image", i);
                            }
                        }
                    }
                }
                op => {
                    eprintln!(
                        "[DEBUG CORE] Processing Op: {:?}",
                        std::mem::discriminant(op)
                    );
                }
            }
        }

        match sub.op {
            Op::Interrupt => {
                let sess = match sess.as_ref() {
                    Some(sess) => sess,
                    None => {
                        send_no_session_event(sub.id).await;
                        continue;
                    }
                };
                sess.abort();
            }
            Op::ConfigureSession {
                provider,
                model,
                model_reasoning_effort,
                model_reasoning_summary,
                user_instructions,
                base_instructions,
                approval_policy,
                disable_response_storage,
                notify,
                cwd,
                resume_path,
            } => {
                info!(
                    "Configuring session: model={model}; provider={provider:?}; resume={resume_path:?}"
                );
                if !cwd.is_absolute() {
                    let message = format!("cwd is not absolute: {cwd:?}");
                    error!(message);
                    let event = Event {
                        id: sub.id,
                        msg: EventMsg::Error(ErrorEvent { message }),
                    };
                    if let Err(e) = tx_event.send(event).await {
                        error!("failed to send error message: {e:?}");
                    }
                    return;
                }
                // Optionally resume an existing rollout.
                let mut restored_items: Option<Vec<ResponseItem>> = None;
                let rollout_recorder: Option<RolloutRecorder> =
                    if let Some(path) = resume_path.as_ref() {
                        match RolloutRecorder::resume(path, cwd.clone()).await {
                            Ok((rec, saved)) => {
                                session_id = saved.session_id;
                                if !saved.items.is_empty() {
                                    restored_items = Some(saved.items);
                                }
                                Some(rec)
                            }
                            Err(e) => {
                                warn!("failed to resume rollout from {path:?}: {e}");
                                None
                            }
                        }
                    } else {
                        None
                    };

                let rollout_recorder = match rollout_recorder {
                    Some(rec) => Some(rec),
                    None => {
                        match RolloutRecorder::new(&config, session_id, user_instructions.clone())
                            .await
                        {
                            Ok(r) => Some(r),
                            Err(e) => {
                                warn!("failed to initialise rollout recorder: {e}");
                                None
                            }
                        }
                    }
                };

                let client = ModelClient::new(
                    config.clone(),
                    auth.clone(),
                    provider.clone(),
                    model_reasoning_effort,
                    model_reasoning_summary,
                    session_id,
                );

                // abort any current running session and clone its state
                let state = match sess.take() {
                    Some(sess) => {
                        sess.abort();
                        sess.state.lock().unwrap().partial_clone()
                    }
                    None => State {
                        history: ConversationHistory::new(),
                        ..Default::default()
                    },
                };

                let writable_roots = Mutex::new(get_writable_roots(&cwd));

                // Error messages to dispatch after SessionConfigured is sent.
                let mut mcp_connection_errors = Vec::<Event>::new();
                let (mcp_connection_manager, failed_clients) =
                    match McpConnectionManager::new(config.mcp_servers.clone()).await {
                        Ok((mgr, failures)) => (mgr, failures),
                        Err(e) => {
                            let message = format!("Failed to create MCP connection manager: {e:#}");
                            error!("{message}");
                            mcp_connection_errors.push(Event {
                                id: sub.id.clone(),
                                msg: EventMsg::Error(ErrorEvent { message }),
                            });
                            (McpConnectionManager::default(), Default::default())
                        }
                    };

                // Surface individual client start-up failures to the user.
                if !failed_clients.is_empty() {
                    for (server_name, err) in failed_clients {
                        let message =
                            format!("MCP client for `{server_name}` failed to start: {err:#}");
                        error!("{message}");
                        mcp_connection_errors.push(Event {
                            id: sub.id.clone(),
                            msg: EventMsg::Error(ErrorEvent { message }),
                        });
                    }
                }
                let default_shell = shell::default_user_shell().await;
                sess = Some(Arc::new(Session {
                    client,
                    tx_event: tx_event.clone(),
                    ctrl_c: Arc::clone(&ctrl_c),
                    user_instructions,
                    base_instructions,
                    approval_policy,
                    shell_environment_policy: config.shell_environment_policy.clone(),
                    cwd,
                    writable_roots,
                    mcp_connection_manager,
                    notify,
                    state: Mutex::new(state),
                    rollout: Mutex::new(rollout_recorder),
                    disable_response_storage,
                    user_shell: default_shell,
                }));

                // Patch restored state into the newly created session.
                if let Some(sess_arc) = &sess {
                    if restored_items.is_some() {
                        let mut st = sess_arc.state.lock().unwrap();
                        st.history.record_items(restored_items.unwrap().iter());
                    }
                }

                // Gather history metadata for SessionConfiguredEvent.
                let (history_log_id, history_entry_count) =
                    crate::message_history::history_metadata(&config).await;

                // ack
                let events = std::iter::once(Event {
                    id: sub.id.clone(),
                    msg: EventMsg::SessionConfigured(SessionConfiguredEvent {
                        session_id,
                        model,
                        history_log_id,
                        history_entry_count,
                    }),
                })
                .chain(mcp_connection_errors.into_iter());
                for event in events {
                    if let Err(e) = tx_event.send(event).await {
                        error!("failed to send event: {e:?}");
                    }
                }
            }
            Op::UserInput { items } => {
                let sess = match sess.as_ref() {
                    Some(sess) => sess,
                    None => {
                        send_no_session_event(sub.id).await;
                        continue;
                    }
                };

                // attempt to inject input into current task
                if let Err(items) = sess.inject_input(items) {
                    // no current task, spawn a new one
                    if std::env::var("CODEX_DEBUG").is_ok() {
                        eprintln!(
                            "[DEBUG CORE] No current task, spawning new AgentTask for {} items",
                            items.len()
                        );
                    }
                    let task = AgentTask::spawn(Arc::clone(sess), sub.id, items);
                    sess.set_task(task);
                }
            }
            Op::ExecApproval { id, decision } => {
                let sess = match sess.as_ref() {
                    Some(sess) => sess,
                    None => {
                        send_no_session_event(sub.id).await;
                        continue;
                    }
                };
                match decision {
                    ReviewDecision::Abort => {
                        sess.abort();
                    }
                    other => sess.notify_approval(&id, other),
                }
            }
            Op::PatchApproval { id, decision } => {
                let sess = match sess.as_ref() {
                    Some(sess) => sess,
                    None => {
                        send_no_session_event(sub.id).await;
                        continue;
                    }
                };
                match decision {
                    ReviewDecision::Abort => {
                        sess.abort();
                    }
                    other => sess.notify_approval(&id, other),
                }
            }
            Op::AddToHistory { text } => {
                // TODO: What should we do if we got AddToHistory before ConfigureSession?
                // currently, if ConfigureSession has resume path, this history will be ignored
                let id = session_id;
                let config = config.clone();
                tokio::spawn(async move {
                    if let Err(e) = crate::message_history::append_entry(&text, &id, &config).await
                    {
                        warn!("failed to append to message history: {e}");
                    }
                });
            }

            Op::GetHistoryEntryRequest { offset, log_id } => {
                let config = config.clone();
                let tx_event = tx_event.clone();
                let sub_id = sub.id.clone();

                tokio::spawn(async move {
                    // Run lookup in blocking thread because it does file IO + locking.
                    let entry_opt = tokio::task::spawn_blocking(move || {
                        crate::message_history::lookup(log_id, offset, &config)
                    })
                    .await
                    .unwrap_or(None);

                    let event = Event {
                        id: sub_id,
                        msg: EventMsg::GetHistoryEntryResponse(
                            crate::protocol::GetHistoryEntryResponseEvent {
                                offset,
                                log_id,
                                entry: entry_opt,
                            },
                        ),
                    };

                    if let Err(e) = tx_event.send(event).await {
                        warn!("failed to send GetHistoryEntryResponse event: {e}");
                    }
                });
            }
            Op::Shutdown => {
                info!("Shutting down Codex instance");

                // Gracefully flush and shutdown rollout recorder on session end so tests
                // that inspect the rollout file do not race with the background writer.
                if let Some(sess_arc) = sess {
                    let recorder_opt = sess_arc.rollout.lock().unwrap().take();
                    if let Some(rec) = recorder_opt {
                        if let Err(e) = rec.shutdown().await {
                            warn!("failed to shutdown rollout recorder: {e}");
                            let event = Event {
                                id: sub.id.clone(),
                                msg: EventMsg::Error(ErrorEvent {
                                    message: "Failed to shutdown rollout recorder".to_string(),
                                }),
                            };
                            if let Err(e) = tx_event.send(event).await {
                                warn!("failed to send error message: {e:?}");
                            }
                        }
                    }
                }
                let event = Event {
                    id: sub.id.clone(),
                    msg: EventMsg::ShutdownComplete,
                };
                if let Err(e) = tx_event.send(event).await {
                    warn!("failed to send Shutdown event: {e}");
                }
                break;
            }
        }
    }
    debug!("Agent loop exited");
}

/// Takes a user message as input and runs a loop where, at each turn, the model
/// replies with either:
///
/// - requested function calls
/// - an assistant message
///
/// While it is possible for the model to return multiple of these items in a
/// single turn, in practice, we generally one item per turn:
///
/// - If the model requests a function call, we execute it and send the output
///   back to the model in the next turn.
/// - If the model sends only an assistant message, we record it in the
///   conversation history and consider the task complete.
async fn run_task(sess: Arc<Session>, sub_id: String, input: Vec<InputItem>) {
    crate::debug_log!("[RUN_TASK] run_task started with sub_id: {}", sub_id);

    if input.is_empty() {
        crate::debug_log!("[RUN_TASK] Input is empty, returning");
        return;
    }

    crate::debug_log!("[RUN_TASK] Processing {} input items", input.len());
    for (i, item) in input.iter().enumerate() {
        match item {
            InputItem::Text { text } => {
                crate::debug_log!("[RUN_TASK] Input {}: Text = '{}'", i, text);
            }
            InputItem::LocalImage { path } => {
                crate::debug_log!("[RUN_TASK] Input {}: LocalImage = {:?}", i, path);
            }
            InputItem::Image { .. } => {
                crate::debug_log!("[RUN_TASK] Input {}: Image", i);
            }
        }
    }

    let event = Event {
        id: sub_id.clone(),
        msg: EventMsg::TaskStarted,
    };
    if sess.tx_event.send(event).await.is_err() {
        return;
    }

    let initial_input_for_turn = ResponseInputItem::from(input);
    sess.record_conversation_items(&[initial_input_for_turn.clone().into()])
        .await;

    let last_agent_message: Option<String>;
    loop {
        // Note that pending_input would be something like a message the user
        // submitted through the UI while the model was running. Though the UI
        // may support this, the model might not.
        let pending_input = sess
            .get_pending_input()
            .into_iter()
            .map(ResponseItem::from)
            .collect::<Vec<ResponseItem>>();
        sess.record_conversation_items(&pending_input).await;

        // Construct the input that we will send to the model. When using the
        // Chat completions API (or ZDR clients), the model needs the full
        // conversation history on each turn. The rollout file, however, should
        // only record the new items that originated in this turn so that it
        // represents an append-only log without duplicates.
        let turn_input: Vec<ResponseItem> =
            [sess.state.lock().unwrap().history.contents(), pending_input].concat();

        let turn_input_messages: Vec<String> = turn_input
            .iter()
            .filter_map(|item| match item {
                ResponseItem::Message { content, .. } => Some(content),
                _ => None,
            })
            .flat_map(|content| {
                content.iter().filter_map(|item| match item {
                    ContentItem::OutputText { text } => Some(text.clone()),
                    _ => None,
                })
            })
            .collect();
        crate::debug_log!(
            "[RUN_TASK] Calling run_turn with {} items",
            turn_input.len()
        );

        match run_turn(&sess, sub_id.clone(), turn_input).await {
            Ok(turn_output) => {
                crate::debug_log!(
                    "[RUN_TASK] run_turn completed with {} output items",
                    turn_output.len()
                );
                let mut items_to_record_in_conversation_history = Vec::<ResponseItem>::new();
                let mut responses = Vec::<ResponseInputItem>::new();
                for processed_response_item in turn_output {
                    let ProcessedResponseItem { item, response } = processed_response_item;
                    match (&item, &response) {
                        (ResponseItem::Message { role, .. }, None) if role == "assistant" => {
                            // If the model returned a message, we need to record it.
                            items_to_record_in_conversation_history.push(item);
                        }
                        (
                            ResponseItem::LocalShellCall { .. },
                            Some(ResponseInputItem::FunctionCallOutput { call_id, output }),
                        ) => {
                            items_to_record_in_conversation_history.push(item);
                            items_to_record_in_conversation_history.push(
                                ResponseItem::FunctionCallOutput {
                                    call_id: call_id.clone(),
                                    output: output.clone(),
                                },
                            );
                        }
                        (
                            ResponseItem::FunctionCall { .. },
                            Some(ResponseInputItem::FunctionCallOutput { call_id, output }),
                        ) => {
                            items_to_record_in_conversation_history.push(item);
                            items_to_record_in_conversation_history.push(
                                ResponseItem::FunctionCallOutput {
                                    call_id: call_id.clone(),
                                    output: output.clone(),
                                },
                            );
                        }
                        (
                            ResponseItem::FunctionCall { .. },
                            Some(ResponseInputItem::McpToolCallOutput { call_id, result }),
                        ) => {
                            items_to_record_in_conversation_history.push(item);
                            let (content, success): (String, Option<bool>) = match result {
                                Ok(CallToolResult {
                                    content,
                                    is_error,
                                    structured_content: _,
                                }) => match serde_json::to_string(content) {
                                    Ok(content) => (content, *is_error),
                                    Err(e) => {
                                        warn!("Failed to serialize MCP tool call output: {e}");
                                        (e.to_string(), Some(true))
                                    }
                                },
                                Err(e) => (e.clone(), Some(true)),
                            };
                            items_to_record_in_conversation_history.push(
                                ResponseItem::FunctionCallOutput {
                                    call_id: call_id.clone(),
                                    output: FunctionCallOutputPayload { content, success },
                                },
                            );
                        }
                        (
                            ResponseItem::Reasoning {
                                id,
                                summary,
                                encrypted_content,
                            },
                            None,
                        ) => {
                            items_to_record_in_conversation_history.push(ResponseItem::Reasoning {
                                id: id.clone(),
                                summary: summary.clone(),
                                encrypted_content: encrypted_content.clone(),
                            });
                        }
                        _ => {
                            warn!("Unexpected response item: {item:?} with response: {response:?}");
                        }
                    };
                    if let Some(response) = response {
                        responses.push(response);
                    }
                }

                // Only attempt to take the lock if there is something to record.
                if !items_to_record_in_conversation_history.is_empty() {
                    sess.record_conversation_items(&items_to_record_in_conversation_history)
                        .await;
                }

                if responses.is_empty() {
                    debug!("Turn completed");
                    last_agent_message = get_last_assistant_message_from_turn(
                        &items_to_record_in_conversation_history,
                    );
                    sess.maybe_notify(UserNotification::AgentTurnComplete {
                        turn_id: sub_id.clone(),
                        input_messages: turn_input_messages,
                        last_assistant_message: last_agent_message.clone(),
                    });
                    break;
                }
            }
            Err(e) => {
                info!("Turn error: {e:#}");
                let event = Event {
                    id: sub_id.clone(),
                    msg: EventMsg::Error(ErrorEvent {
                        message: e.to_string(),
                    }),
                };
                sess.tx_event.send(event).await.ok();
                return;
            }
        }
    }
    sess.remove_task(&sub_id);
    let event = Event {
        id: sub_id,
        msg: EventMsg::TaskComplete(TaskCompleteEvent { last_agent_message }),
    };
    sess.tx_event.send(event).await.ok();
}

async fn run_turn(
    sess: &Session,
    sub_id: String,
    input: Vec<ResponseItem>,
) -> CodexResult<Vec<ProcessedResponseItem>> {
    let extra_tools = sess.mcp_connection_manager.list_all_tools();
    let prompt = Prompt {
        input,
        user_instructions: sess.user_instructions.clone(),
        store: !sess.disable_response_storage,
        extra_tools,
        base_instructions_override: sess.base_instructions.clone(),
    };

    let mut retries = 0;
    loop {
        match try_run_turn(sess, &sub_id, &prompt).await {
            Ok(output) => return Ok(output),
            Err(CodexErr::Interrupted) => return Err(CodexErr::Interrupted),
            Err(CodexErr::EnvVar(var)) => return Err(CodexErr::EnvVar(var)),
            Err(e) => {
                // Use the configured provider-specific stream retry budget.
                let max_retries = sess.client.get_provider().stream_max_retries();
                if retries < max_retries {
                    retries += 1;
                    let delay = backoff(retries);
                    warn!(
                        "stream disconnected - retrying turn ({retries}/{max_retries} in {delay:?})...",
                    );

                    // Surface retry information to any UI/front‑end so the
                    // user understands what is happening instead of staring
                    // at a seemingly frozen screen.
                    sess.notify_background_event(
                        &sub_id,
                        format!(
                            "stream error: {e}; retrying {retries}/{max_retries} in {delay:?}…"
                        ),
                    )
                    .await;

                    tokio::time::sleep(delay).await;
                } else {
                    return Err(e);
                }
            }
        }
    }
}

/// When the model is prompted, it returns a stream of events. Some of these
/// events map to a `ResponseItem`. A `ResponseItem` may need to be
/// "handled" such that it produces a `ResponseInputItem` that needs to be
/// sent back to the model on the next turn.
#[derive(Debug)]
struct ProcessedResponseItem {
    item: ResponseItem,
    response: Option<ResponseInputItem>,
}

async fn try_run_turn(
    sess: &Session,
    sub_id: &str,
    prompt: &Prompt,
) -> CodexResult<Vec<ProcessedResponseItem>> {
    crate::debug_log!(
        "[TRY_RUN_TURN] Starting try_run_turn for sub_id: {}",
        sub_id
    );
    crate::debug_log!(
        "[TRY_RUN_TURN] Prompt has {} input items",
        prompt.input.len()
    );

    // call_ids that are part of this response.
    let completed_call_ids = prompt
        .input
        .iter()
        .filter_map(|ri| match ri {
            ResponseItem::FunctionCallOutput { call_id, .. } => Some(call_id),
            ResponseItem::LocalShellCall {
                call_id: Some(call_id),
                ..
            } => Some(call_id),
            _ => None,
        })
        .collect::<Vec<_>>();

    // call_ids that were pending but are not part of this response.
    // This usually happens because the user interrupted the model before we responded to one of its tool calls
    // and then the user sent a follow-up message.
    let missing_calls = {
        prompt
            .input
            .iter()
            .filter_map(|ri| match ri {
                ResponseItem::FunctionCall { call_id, .. } => Some(call_id),
                ResponseItem::LocalShellCall {
                    call_id: Some(call_id),
                    ..
                } => Some(call_id),
                _ => None,
            })
            .filter_map(|call_id| {
                if completed_call_ids.contains(&call_id) {
                    None
                } else {
                    Some(call_id.clone())
                }
            })
            .map(|call_id| ResponseItem::FunctionCallOutput {
                call_id: call_id.clone(),
                output: FunctionCallOutputPayload {
                    content: "aborted".to_string(),
                    success: Some(false),
                },
            })
            .collect::<Vec<_>>()
    };
    let prompt: Cow<Prompt> = if missing_calls.is_empty() {
        Cow::Borrowed(prompt)
    } else {
        // Add the synthetic aborted missing calls to the beginning of the input to ensure all call ids have responses.
        let input = [missing_calls, prompt.input.clone()].concat();
        Cow::Owned(Prompt {
            input,
            ..prompt.clone()
        })
    };

    crate::debug_log!("[TRY_RUN_TURN] Creating stream for model API call...");
    crate::debug_log!("[TRY_RUN_TURN] About to call sess.client.stream()");

    let mut stream = sess.client.clone().stream(&prompt).await?;
    crate::debug_log!("[TRY_RUN_TURN] Stream created successfully");

    let mut output = Vec::new();
    loop {
        // Poll the next item from the model stream. We must inspect *both* Ok and Err
        // cases so that transient stream failures (e.g., dropped SSE connection before
        // `response.completed`) bubble up and trigger the caller's retry logic.
        let event = stream.next().await;
        let Some(event) = event else {
            // Channel closed without yielding a final Completed event or explicit error.
            // Treat as a disconnected stream so the caller can retry.
            return Err(CodexErr::Stream(
                "stream closed before response.completed".into(),
            ));
        };

        let event = match event {
            Ok(ev) => ev,
            Err(e) => {
                // Propagate the underlying stream error to the caller (run_turn), which
                // will apply the configured `stream_max_retries` policy.
                return Err(e);
            }
        };

        match event {
            ResponseEvent::Created => {}
            ResponseEvent::OutputItemDone(item) => {
                let response = handle_response_item(sess, sub_id, item.clone()).await?;

                output.push(ProcessedResponseItem { item, response });
            }
            ResponseEvent::Completed {
                response_id: _,
                token_usage,
            } => {
                if let Some(token_usage) = token_usage {
                    sess.tx_event
                        .send(Event {
                            id: sub_id.to_string(),
                            msg: EventMsg::TokenCount(token_usage),
                        })
                        .await
                        .ok();
                }

                return Ok(output);
            }
            ResponseEvent::OutputTextDelta(delta) => {
                let event = Event {
                    id: sub_id.to_string(),
                    msg: EventMsg::AgentMessageDelta(AgentMessageDeltaEvent { delta }),
                };
                sess.tx_event.send(event).await.ok();
            }
            ResponseEvent::ReasoningSummaryDelta(delta) => {
                let event = Event {
                    id: sub_id.to_string(),
                    msg: EventMsg::AgentReasoningDelta(AgentReasoningDeltaEvent { delta }),
                };
                sess.tx_event.send(event).await.ok();
            }
        }
    }
}

async fn handle_response_item(
    sess: &Session,
    sub_id: &str,
    item: ResponseItem,
) -> CodexResult<Option<ResponseInputItem>> {
    debug!(?item, "Output item");

    if std::env::var("CODEX_DEBUG").is_ok() {
        match &item {
            ResponseItem::Message { content, .. } => {
                eprintln!(
                    "[DEBUG CORE] handle_response_item: Message with {} content items",
                    content.len()
                );
            }
            ResponseItem::FunctionCall { name, call_id, .. } => {
                eprintln!(
                    "[DEBUG CORE] handle_response_item: FunctionCall '{}' (call_id: {})",
                    name, call_id
                );
            }
            ResponseItem::LocalShellCall { call_id, .. } => {
                eprintln!(
                    "[DEBUG CORE] handle_response_item: LocalShellCall (call_id: {:?})",
                    call_id
                );
            }
            _ => {
                eprintln!(
                    "[DEBUG CORE] handle_response_item: {:?}",
                    std::mem::discriminant(&item)
                );
            }
        }
    }

    let output = match item {
        ResponseItem::Message { content, .. } => {
            for item in content {
                if let ContentItem::OutputText { text } = item {
                    let event = Event {
                        id: sub_id.to_string(),
                        msg: EventMsg::AgentMessage(AgentMessageEvent { message: text }),
                    };
                    sess.tx_event.send(event).await.ok();
                }
            }
            None
        }
        ResponseItem::Reasoning { summary, .. } => {
            for item in summary {
                let text = match item {
                    ReasoningItemReasoningSummary::SummaryText { text } => text,
                };
                let event = Event {
                    id: sub_id.to_string(),
                    msg: EventMsg::AgentReasoning(AgentReasoningEvent { text }),
                };
                sess.tx_event.send(event).await.ok();
            }
            None
        }
        ResponseItem::FunctionCall {
            name,
            arguments,
            call_id,
            ..
        } => {
            info!("FunctionCall: {arguments}");
            Some(handle_function_call(sess, sub_id.to_string(), name, arguments, call_id).await)
        }
        ResponseItem::LocalShellCall {
            id,
            call_id,
            status: _,
            action,
        } => {
            let LocalShellAction::Exec(action) = action;
            tracing::info!("LocalShellCall: {action:?}");
            let params = ShellToolCallParams {
                command: action.command,
                workdir: action.working_directory,
                timeout_ms: action.timeout_ms,
            };
            let effective_call_id = match (call_id, id) {
                (Some(call_id), _) => call_id,
                (None, Some(id)) => id,
                (None, None) => {
                    error!("LocalShellCall without call_id or id");
                    return Ok(Some(ResponseInputItem::FunctionCallOutput {
                        call_id: "".to_string(),
                        output: FunctionCallOutputPayload {
                            content: "LocalShellCall without call_id or id".to_string(),
                            success: None,
                        },
                    }));
                }
            };

            let exec_params = to_exec_params(params, sess);
            Some(
                handle_container_exec_with_params(
                    exec_params,
                    sess,
                    sub_id.to_string(),
                    effective_call_id,
                )
                .await,
            )
        }
        ResponseItem::FunctionCallOutput { .. } => {
            debug!("unexpected FunctionCallOutput from stream");
            None
        }
        ResponseItem::Other => None,
    };
    Ok(output)
}

async fn handle_function_call(
    sess: &Session,
    sub_id: String,
    name: String,
    arguments: String,
    call_id: String,
) -> ResponseInputItem {
    // DEBUG: Log the function call details for compatibility analysis
    if std::env::var("CODEX_DEBUG").is_ok() {
        eprintln!(
            "[DEBUG CORE] Function call received - name: '{}', call_id: '{}'",
            name, call_id
        );
        eprintln!("[DEBUG CORE] Function arguments: {}", arguments);
    }

    match name.as_str() {
        // Support various common shell tool names for better compatibility
        "container.exec" | "shell" | "bash" | "run_shell" | "execute" | "exec" => {
            if std::env::var("CODEX_DEBUG").is_ok() {
                eprintln!(
                    "[DEBUG CORE] Routing to built-in shell handler (tool: '{}')",
                    name
                );
            }
            let params = match parse_container_exec_arguments(arguments, sess, &call_id) {
                Ok(params) => params,
                Err(output) => {
                    return *output;
                }
            };
            handle_container_exec_with_params(params, sess, sub_id, call_id).await
        }
        "update_plan" => handle_update_plan(sess, arguments, sub_id, call_id).await,
        _ => {
            match sess.mcp_connection_manager.parse_tool_name(&name) {
                Some((server, tool_name)) => {
                    if std::env::var("CODEX_DEBUG").is_ok() {
                        eprintln!(
                            "[DEBUG CORE] Routing to MCP tool - server: '{}', tool: '{}'",
                            server, tool_name
                        );
                    }
                    // TODO(mbolin): Determine appropriate timeout for tool call.
                    let timeout = None;
                    handle_mcp_tool_call(
                        sess, &sub_id, call_id, server, tool_name, arguments, timeout,
                    )
                    .await
                }
                None => {
                    if std::env::var("CODEX_DEBUG").is_ok() {
                        eprintln!(
                            "[DEBUG CORE] Unknown tool name '{}' - checking for fallback options",
                            name
                        );
                    }

                    // Try to handle unknown tool names with intelligent fallbacks
                    if name.contains("shell")
                        || name.contains("exec")
                        || name.contains("run")
                        || name.contains("command")
                    {
                        if std::env::var("CODEX_DEBUG").is_ok() {
                            eprintln!(
                                "[DEBUG CORE] Tool name '{}' looks like a shell command, attempting shell fallback",
                                name
                            );
                        }
                        let params = match parse_container_exec_arguments(arguments, sess, &call_id)
                        {
                            Ok(params) => params,
                            Err(output) => {
                                if std::env::var("CODEX_DEBUG").is_ok() {
                                    eprintln!(
                                        "[DEBUG CORE] Shell fallback argument parsing failed"
                                    );
                                }
                                return *output;
                            }
                        };
                        return handle_container_exec_with_params(params, sess, sub_id, call_id)
                            .await;
                    }

                    if std::env::var("CODEX_DEBUG").is_ok() {
                        eprintln!(
                            "[DEBUG CORE] No fallback available for tool '{}' - returning error",
                            name
                        );
                    }
                    // Unknown function: reply with structured failure so the model can adapt.
                    ResponseInputItem::FunctionCallOutput {
                        call_id,
                        output: FunctionCallOutputPayload {
                            content: format!(
                                "Unsupported tool: '{}'. Available tools: shell, container.exec, or MCP tools with fully qualified names (server.tool_name)",
                                name
                            ),
                            success: Some(false),
                        },
                    }
                }
            }
        }
    }
}

fn to_exec_params(params: ShellToolCallParams, sess: &Session) -> ExecParams {
    ExecParams {
        command: params.command,
        cwd: sess.resolve_path(params.workdir.clone()),
        timeout_ms: params.timeout_ms,
        env: create_env(&sess.shell_environment_policy),
    }
}

fn parse_container_exec_arguments(
    arguments: String,
    sess: &Session,
    call_id: &str,
) -> Result<ExecParams, Box<ResponseInputItem>> {
    // parse command
    match serde_json::from_str::<ShellToolCallParams>(&arguments) {
        Ok(shell_tool_call_params) => Ok(to_exec_params(shell_tool_call_params, sess)),
        Err(e) => {
            // allow model to re-sample
            let output = ResponseInputItem::FunctionCallOutput {
                call_id: call_id.to_string(),
                output: FunctionCallOutputPayload {
                    content: format!("failed to parse function arguments: {e}"),
                    success: None,
                },
            };
            Err(Box::new(output))
        }
    }
}

fn maybe_run_with_user_profile(params: ExecParams, sess: &Session) -> ExecParams {
    if sess.shell_environment_policy.use_profile {
        let command = sess
            .user_shell
            .format_default_shell_invocation(params.command.clone());
        if let Some(command) = command {
            return ExecParams { command, ..params };
        }
    }
    params
}

async fn handle_container_exec_with_params(
    params: ExecParams,
    sess: &Session,
    sub_id: String,
    call_id: String,
) -> ResponseInputItem {
    // check if this was a patch, and apply it if so
    match maybe_parse_apply_patch_verified(&params.command, &params.cwd) {
        MaybeApplyPatchVerified::Body(changes) => {
            return apply_patch::apply_patch(sess, sub_id, call_id, changes).await;
        }
        MaybeApplyPatchVerified::CorrectnessError(parse_error) => {
            // It looks like an invocation of `apply_patch`, but we
            // could not resolve it into a patch that would apply
            // cleanly. Return to model for resample.
            return ResponseInputItem::FunctionCallOutput {
                call_id,
                output: FunctionCallOutputPayload {
                    content: format!("error: {parse_error:#}"),
                    success: None,
                },
            };
        }
        MaybeApplyPatchVerified::ShellParseError(error) => {
            trace!("Failed to parse shell command, {error:?}");
        }
        MaybeApplyPatchVerified::NotApplyPatch => (),
    }

    // safety checks
    let safety = {
        let state = sess.state.lock().unwrap();
        assess_command_safety(
            &params.command,
            sess.approval_policy,
            &state.approved_commands,
        )
    };
    // Handle safety check - no sandboxing, just approval logic
    match safety {
        SafetyCheck::AutoApprove => {
            // Command is automatically approved, proceed directly
        }
        SafetyCheck::AskUser => {
            let rx_approve = sess
                .request_command_approval(
                    sub_id.clone(),
                    call_id.clone(),
                    params.command.clone(),
                    params.cwd.clone(),
                    None,
                )
                .await;
            match rx_approve.await.unwrap_or_default() {
                ReviewDecision::Approved => (),
                ReviewDecision::ApprovedForSession => {
                    sess.add_approved_command(params.command.clone());
                }
                ReviewDecision::Denied | ReviewDecision::Abort => {
                    return ResponseInputItem::FunctionCallOutput {
                        call_id,
                        output: FunctionCallOutputPayload {
                            content: "exec command rejected by user".to_string(),
                            success: None,
                        },
                    };
                }
            }
            // User has given explicit approval, proceed with execution
        }
        SafetyCheck::Reject { reason } => {
            return ResponseInputItem::FunctionCallOutput {
                call_id,
                output: FunctionCallOutputPayload {
                    content: format!("exec command rejected: {reason}"),
                    success: None,
                },
            };
        }
    }

    sess.notify_exec_command_begin(&sub_id, &call_id, &params)
        .await;

    let params = maybe_run_with_user_profile(params, sess);
    let output_result = process_exec_tool_call(params.clone(), sess.ctrl_c.clone()).await;

    match output_result {
        Ok(output) => {
            let ExecToolCallOutput {
                exit_code,
                stdout,
                stderr,
                duration_seconds,
            } = output;

            sess.notify_exec_command_end(&sub_id, &call_id, &stdout, &stderr, exit_code)
                .await;

            let is_success = exit_code == 0;
            let content = format_exec_output(
                if is_success { &stdout } else { &stderr },
                exit_code,
                duration_seconds,
            );

            ResponseInputItem::FunctionCallOutput {
                call_id,
                output: FunctionCallOutputPayload {
                    content,
                    success: Some(is_success),
                },
            }
        }
        Err(error) => {
            // Handle all error cases
            ResponseInputItem::FunctionCallOutput {
                call_id,
                output: FunctionCallOutputPayload {
                    content: format!("Command failed: {}", error),
                    success: Some(false),
                },
            }
        }
    }
}

/// Exec output is a pre-serialized JSON payload
fn format_exec_output(output: &str, exit_code: i32, duration_seconds: f64) -> String {
    #[derive(Serialize)]
    struct ExecMetadata {
        exit_code: i32,
        duration_seconds: f32,
    }

    #[derive(Serialize)]
    struct ExecOutput<'a> {
        output: &'a str,
        metadata: ExecMetadata,
    }

    // round to 1 decimal place
    let duration_secs_f32 = ((duration_seconds as f32) * 10.0).round() / 10.0;

    let payload = ExecOutput {
        output,
        metadata: ExecMetadata {
            exit_code,
            duration_seconds: duration_secs_f32,
        },
    };

    #[expect(clippy::expect_used)]
    serde_json::to_string(&payload).expect("serialize ExecOutput")
}

fn get_last_assistant_message_from_turn(responses: &[ResponseItem]) -> Option<String> {
    responses.iter().rev().find_map(|item| {
        if let ResponseItem::Message { role, content, .. } = item {
            if role == "assistant" {
                content.iter().rev().find_map(|ci| {
                    if let ContentItem::OutputText { text } = ci {
                        Some(text.clone())
                    } else {
                        None
                    }
                })
            } else {
                None
            }
        } else {
            None
        }
    })
}
