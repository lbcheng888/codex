use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use crate::continuous_mode::ContinuousModeSettings;
use codex_core::plan_tool::PlanItemArg;
use codex_core::plan_tool::StepStatus;
use codex_core::plan_tool::UpdatePlanArgs;
use codex_core::protocol::ApplyPatchApprovalRequestEvent;
use codex_core::protocol::Event;
use codex_core::protocol::EventMsg;
use codex_core::protocol::ExecApprovalRequestEvent;
use codex_core::protocol::ExecCommandEndEvent;
use codex_core::protocol::Op;
use codex_core::protocol::PatchApplyEndEvent;
use codex_core::protocol::ReviewDecision;
use codex_core::protocol::SessionConfiguredEvent;
use codex_protocol::ConversationId;
use color_eyre::eyre::Result;
use color_eyre::eyre::eyre;
use crossterm::event::KeyCode;
use crossterm::event::KeyEvent;
use crossterm::event::KeyEventKind;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::path::Path;
use std::path::PathBuf;
use tokio::fs;
use tokio::process::Command;

#[derive(Debug, Clone)]
pub enum ContinuousNotice {
    Info(String),
    Warn(String),
}

const STALL_LIMIT: u32 = 3;
const MAX_AUTO_RETRIES: u32 = 3;

#[derive(Default)]
pub struct ContinuousEventResult {
    pub consume_event: bool,
    pub notices: Vec<ContinuousNotice>,
}

pub struct ContinuousController {
    settings: ContinuousModeSettings,
    app_event_tx: AppEventSender,
    persistence: PersistenceState,
    session_id: Option<ConversationId>,
    steps_since_resume: u32,
    pending_approvals: VecDeque<PendingApproval>,
    pause: Option<PauseReason>,
    plan: PlanState,
    running_commands: HashMap<String, Vec<String>>,
    retry_counters: HashMap<String, u32>,
    workspace_snapshot: Option<Vec<String>>,
    stall_counter: u32,
    last_command_signature: Option<String>,
}

impl ContinuousController {
    pub fn new(
        settings: ContinuousModeSettings,
        codex_home: &Path,
        app_event_tx: AppEventSender,
    ) -> Result<Self> {
        let state_dir = codex_home.join("state").join("continuous");
        std::fs::create_dir_all(&state_dir)?;
        Ok(Self {
            settings,
            app_event_tx,
            persistence: PersistenceState {
                base_dir: state_dir,
                session_path: None,
            },
            session_id: None,
            steps_since_resume: 0,
            pending_approvals: VecDeque::new(),
            pause: None,
            plan: PlanState::default(),
            running_commands: HashMap::new(),
            retry_counters: HashMap::new(),
            workspace_snapshot: None,
            stall_counter: 0,
            last_command_signature: None,
        })
    }

    pub async fn on_codex_event(
        &mut self,
        event: &Event,
        cwd: &Path,
    ) -> Result<ContinuousEventResult> {
        let mut result = ContinuousEventResult::default();
        match &event.msg {
            EventMsg::SessionConfigured(ev) => {
                let resume_notices = self.on_session_configured(ev).await?;
                self.refresh_workspace_snapshot(cwd).await?;
                self.save_state().await?;
                result.notices.extend(resume_notices);
            }
            EventMsg::PlanUpdate(update) => {
                let summary = self.plan.apply_update(update);
                result
                    .notices
                    .push(ContinuousNotice::Info(summary.summary()));
                self.save_state().await?;
            }
            EventMsg::ExecApprovalRequest(ev) => {
                return self
                    .handle_exec_request(event.id.clone(), ev.clone(), cwd)
                    .await;
            }
            EventMsg::ApplyPatchApprovalRequest(ev) => {
                return self
                    .handle_patch_request(event.id.clone(), ev.clone(), cwd)
                    .await;
            }
            EventMsg::ExecCommandBegin(ev) => {
                self.running_commands
                    .insert(ev.call_id.clone(), ev.command.clone());
            }
            EventMsg::ExecCommandEnd(ev) => {
                let notices = self.on_exec_end(ev, cwd).await?;
                result.notices.extend(notices);
                self.save_state().await?;
            }
            EventMsg::PatchApplyEnd(ev) => {
                let notices = self.on_patch_end(ev, cwd).await?;
                result.notices.extend(notices);
                self.save_state().await?;
            }
            EventMsg::TaskComplete(ev) => {
                if let Some(message) = ev.last_agent_message.as_ref() {
                    result.notices.push(ContinuousNotice::Info(format!(
                        "Task complete: {}",
                        message
                    )));
                }
                self.steps_since_resume = 0;
                self.save_state().await?;
            }
            _ => {}
        }
        Ok(result)
    }

    pub async fn handle_key_event(
        &mut self,
        key_event: &KeyEvent,
        cwd: &Path,
    ) -> Result<Option<Vec<ContinuousNotice>>> {
        if key_event.kind != KeyEventKind::Press {
            return Ok(None);
        }
        match key_event.code {
            KeyCode::Char('c') | KeyCode::Char('C') => {
                if self.pause.is_none() && self.pending_approvals.is_empty() {
                    return Ok(Some(vec![ContinuousNotice::Info(
                        "连续模式已在运行".to_string(),
                    )]));
                }
                let notices = self.resume(cwd).await?;
                self.save_state().await?;
                Ok(Some(notices))
            }
            KeyCode::Char('p') | KeyCode::Char('P') => {
                if let Some(PauseReason::Manual) = self.pause {
                    let notices = self.resume(cwd).await?;
                    self.save_state().await?;
                    Ok(Some(notices))
                } else {
                    self.pause = Some(PauseReason::Manual);
                    self.save_state().await?;
                    Ok(Some(vec![ContinuousNotice::Info(
                        "连续模式已暂停".to_string(),
                    )]))
                }
            }
            _ => Ok(None),
        }
    }

    pub async fn finalize(&mut self) -> Result<()> {
        self.save_state().await
    }

    pub fn is_paused(&self) -> bool {
        self.pause.is_some()
    }

    pub fn summary_message(&self) -> Option<String> {
        if let Some(pause) = self.pause.as_ref() {
            Some(pause.summary())
        } else if !self.pending_approvals.is_empty() {
            Some(format!("排队待执行：{} 项", self.pending_approvals.len()))
        } else {
            self.plan.summary_notice().map(|notice| match notice {
                ContinuousNotice::Info(msg) | ContinuousNotice::Warn(msg) => msg,
            })
        }
    }

    async fn on_session_configured(
        &mut self,
        event: &SessionConfiguredEvent,
    ) -> Result<Vec<ContinuousNotice>> {
        self.session_id = Some(event.session_id.clone());
        let session_path = self
            .persistence
            .base_dir
            .join(format!("{}.json", event.session_id));
        self.persistence.session_path = Some(session_path.clone());
        let mut notices = Vec::new();
        if session_path.exists() {
            self.load_state(&session_path).await?;
            if let Some(summary) = self.plan.summary_notice() {
                notices.push(summary);
            }
            if let Some(pause) = self.pause.as_ref() {
                notices.push(ContinuousNotice::Warn(pause.summary()));
            }
        }
        Ok(notices)
    }

    async fn handle_exec_request(
        &mut self,
        id: String,
        ev: ExecApprovalRequestEvent,
        cwd: &Path,
    ) -> Result<ContinuousEventResult> {
        let mut result = ContinuousEventResult::default();
        let summary = format_command(&ev.command);
        if self.should_pause_for_confirmation() {
            self.pause = Some(PauseReason::AwaitingApproval {
                summary: summary.clone(),
            });
            self.pending_approvals
                .push_back(PendingApproval::exec(id, summary.clone()));
            result
                .notices
                .push(ContinuousNotice::Info(format!("等待确认命令：{}", summary)));
            self.save_state().await?;
            return Ok(result);
        }

        if let Some(notice) = self.check_workspace_guard(cwd).await?.map(|notice| {
            self.pause = Some(PauseReason::WorkspaceChanged);
            self.pending_approvals
                .push_back(PendingApproval::exec(id.clone(), summary.clone()));
            notice
        }) {
            result.notices.push(notice);
            self.save_state().await?;
            return Ok(result);
        }

        if self.steps_since_resume >= self.settings.max_steps {
            self.pause_for_step_limit(&summary);
            self.pending_approvals
                .push_back(PendingApproval::exec(id, summary.clone()));
            result.notices.push(ContinuousNotice::Info(format!(
                "达到最大步数 ({}), 已在执行前暂停：{}",
                self.settings.max_steps, summary
            )));
            self.save_state().await?;
            return Ok(result);
        }

        self.send_exec_approval(id.clone());
        self.steps_since_resume += 1;
        result.consume_event = true;
        result.notices.push(ContinuousNotice::Info(format!(
            "已自动批准命令：{}",
            summary
        )));
        self.save_state().await?;
        Ok(result)
    }

    async fn handle_patch_request(
        &mut self,
        id: String,
        ev: ApplyPatchApprovalRequestEvent,
        cwd: &Path,
    ) -> Result<ContinuousEventResult> {
        let mut result = ContinuousEventResult::default();
        let summary = format!("apply patch ({} files)", ev.changes.len());
        if self.should_pause_for_confirmation() {
            self.pause = Some(PauseReason::AwaitingApproval {
                summary: summary.clone(),
            });
            self.pending_approvals
                .push_back(PendingApproval::patch(id, summary.clone()));
            result
                .notices
                .push(ContinuousNotice::Info(format!("等待确认以{}", summary)));
            self.save_state().await?;
            return Ok(result);
        }

        if let Some(notice) = self.check_workspace_guard(cwd).await?.map(|notice| {
            self.pause = Some(PauseReason::WorkspaceChanged);
            self.pending_approvals
                .push_back(PendingApproval::patch(id.clone(), summary.clone()));
            notice
        }) {
            result.notices.push(notice);
            self.save_state().await?;
            return Ok(result);
        }

        if self.steps_since_resume >= self.settings.max_steps {
            self.pause_for_step_limit(&summary);
            self.pending_approvals
                .push_back(PendingApproval::patch(id, summary.clone()));
            result.notices.push(ContinuousNotice::Info(format!(
                "达到最大步数 ({}), 已在应用修改前暂停：{}",
                self.settings.max_steps, summary
            )));
            self.save_state().await?;
            return Ok(result);
        }

        self.send_patch_approval(id.clone());
        self.steps_since_resume += 1;
        result.consume_event = true;
        result
            .notices
            .push(ContinuousNotice::Info(format!("已自动批准{}", summary)));
        self.save_state().await?;
        Ok(result)
    }

    async fn on_exec_end(
        &mut self,
        event: &ExecCommandEndEvent,
        cwd: &Path,
    ) -> Result<Vec<ContinuousNotice>> {
        let mut notices = Vec::new();
        let command = self.running_commands.remove(&event.call_id);
        let summary = command
            .as_ref()
            .map(|cmd| format_command(cmd.as_slice()))
            .unwrap_or_else(|| "command".to_string());

        if event.exit_code != 0 {
            let count = self.retry_counters.entry(summary.clone()).or_insert(0);
            *count += 1;
            if *count >= MAX_AUTO_RETRIES {
                self.pause = Some(PauseReason::CommandFailed {
                    summary: format!("{summary} 退出码 {}", event.exit_code),
                });
                notices.push(ContinuousNotice::Warn(format!(
                    "命令多次失败（{} 次），已暂停：{}",
                    *count, summary
                )));
            } else {
                notices.push(ContinuousNotice::Warn(format!(
                    "命令失败，退出码 {}（已失败 {} 次）：{}",
                    event.exit_code, *count, summary
                )));
            }
            self.save_state().await?;
            return Ok(notices);
        }

        self.retry_counters.remove(&summary);

        let previous_snapshot = self.workspace_snapshot.clone();
        self.refresh_workspace_snapshot(cwd).await?;
        let workspace_changed = match (&previous_snapshot, &self.workspace_snapshot) {
            (Some(prev), Some(curr)) => prev != curr,
            (None, Some(_)) | (Some(_), None) => true,
            (None, None) => false,
        };

        let aggregated_clean = event.aggregated_output.trim();
        if aggregated_clean.is_empty() && !workspace_changed {
            if self
                .last_command_signature
                .as_ref()
                .is_some_and(|prev| prev == &summary)
            {
                self.stall_counter += 1;
            } else {
                self.stall_counter = 1;
            }
            self.last_command_signature = Some(summary.clone());
            if self.stall_counter >= STALL_LIMIT {
                self.pause = Some(PauseReason::Stalled {
                    summary: summary.clone(),
                });
                notices.push(ContinuousNotice::Warn(format!(
                    "疑似卡住：连续 {} 次执行 `{}` 无变化，已暂停",
                    STALL_LIMIT, summary
                )));
                self.save_state().await?;
                return Ok(notices);
            }
        } else {
            self.stall_counter = 0;
            self.last_command_signature = Some(summary.clone());
        }

        notices.push(ContinuousNotice::Info("命令已完成".to_string()));
        Ok(notices)
    }

    async fn on_patch_end(
        &mut self,
        event: &PatchApplyEndEvent,
        cwd: &Path,
    ) -> Result<Vec<ContinuousNotice>> {
        let mut notices = Vec::new();
        if !event.success {
            self.pause = Some(PauseReason::CommandFailed {
                summary: "Patch failed to apply".to_string(),
            });
            notices.push(ContinuousNotice::Warn("补丁应用失败".to_string()));
            self.save_state().await?;
            return Ok(notices);
        }
        self.refresh_workspace_snapshot(cwd).await?;
        notices.push(ContinuousNotice::Info("补丁已应用".to_string()));
        Ok(notices)
    }

    fn should_pause_for_confirmation(&self) -> bool {
        self.pause.is_some() || self.settings.confirm_on_breakpoint
    }

    fn pause_for_step_limit(&mut self, summary: &str) {
        self.pause = Some(PauseReason::StepLimit {
            limit: self.settings.max_steps,
            summary: summary.to_string(),
        });
        self.steps_since_resume = self.settings.max_steps;
        self.stall_counter = 0;
        self.last_command_signature = None;
    }

    fn send_exec_approval(&self, id: String) {
        self.app_event_tx.send(AppEvent::CodexOp(Op::ExecApproval {
            id,
            decision: ReviewDecision::Approved,
        }));
    }

    fn send_patch_approval(&self, id: String) {
        self.app_event_tx.send(AppEvent::CodexOp(Op::PatchApproval {
            id,
            decision: ReviewDecision::Approved,
        }));
    }

    async fn resume(&mut self, cwd: &Path) -> Result<Vec<ContinuousNotice>> {
        let mut notices = vec![ContinuousNotice::Info("连续模式已继续".to_string())];
        if matches!(self.pause, Some(PauseReason::StepLimit { .. })) {
            self.steps_since_resume = 0;
        }
        self.pause = None;
        self.stall_counter = 0;
        self.last_command_signature = None;
        let mut approvals = Vec::new();
        while let Some(pending) = self.pending_approvals.pop_front() {
            if self.steps_since_resume >= self.settings.max_steps {
                self.pause_for_step_limit(&pending.summary);
                self.pending_approvals.push_front(pending);
                notices.push(ContinuousNotice::Info(format!(
                    "已达到最大步数 ({}), 剩余操作已暂停",
                    self.settings.max_steps
                )));
                break;
            }
            if let Some(notice) = self.check_workspace_guard(cwd).await? {
                self.pause = Some(PauseReason::WorkspaceChanged);
                notices.push(notice);
                self.pending_approvals.push_front(pending);
                break;
            }
            approvals.push(pending);
        }

        for approval in approvals {
            match approval.kind {
                ApprovalKind::Exec => self.send_exec_approval(approval.id.clone()),
                ApprovalKind::Patch => self.send_patch_approval(approval.id.clone()),
            }
            self.steps_since_resume += 1;
            notices.push(ContinuousNotice::Info(format!(
                "已自动批准：{}",
                approval.summary
            )));
        }

        Ok(notices)
    }

    async fn refresh_workspace_snapshot(&mut self, cwd: &Path) -> Result<()> {
        if let Some(snapshot) = capture_git_status(cwd).await? {
            self.workspace_snapshot = Some(snapshot);
        }
        Ok(())
    }

    async fn check_workspace_guard(&mut self, cwd: &Path) -> Result<Option<ContinuousNotice>> {
        let Some(current) = capture_git_status(cwd).await? else {
            return Ok(None);
        };
        if let Some(previous) = self.workspace_snapshot.as_ref() {
            if *previous != current {
                self.workspace_snapshot = Some(current);
                return Ok(Some(ContinuousNotice::Warn(
                    "检测到工作区外部改动，已暂停".to_string(),
                )));
            }
        } else {
            self.workspace_snapshot = Some(current);
        }
        Ok(None)
    }

    async fn save_state(&self) -> Result<()> {
        let Some(path) = self.persistence.session_path.as_ref() else {
            return Ok(());
        };
        let snapshot = ContinuousStateFile {
            session_id: self
                .session_id
                .as_ref()
                .map(|id| id.to_string())
                .unwrap_or_default(),
            plan: self.plan.clone(),
            steps_since_resume: self.steps_since_resume,
            pause: self.pause.clone().map(PauseStateFile::from),
            workspace_snapshot: self.workspace_snapshot.clone(),
            settings: ContinuousSettingsSnapshot::from(&self.settings),
            pending_approvals: self
                .pending_approvals
                .iter()
                .map(PendingApprovalState::from)
                .collect(),
            stall_counter: self.stall_counter,
            last_command_signature: self.last_command_signature.clone(),
            retry_counters: self.retry_counters.clone(),
        };
        let data = serde_json::to_vec_pretty(&snapshot)?;
        fs::write(path, data).await?;
        Ok(())
    }

    async fn load_state(&mut self, path: &Path) -> Result<()> {
        let data = fs::read(path).await?;
        let snapshot: ContinuousStateFile = serde_json::from_slice(&data)?;
        self.plan = snapshot.plan;
        self.steps_since_resume = snapshot.steps_since_resume;
        self.pause = snapshot.pause.map(PauseReason::from);
        self.workspace_snapshot = snapshot.workspace_snapshot;
        self.pending_approvals = snapshot
            .pending_approvals
            .into_iter()
            .map(PendingApproval::from)
            .collect();
        self.stall_counter = snapshot.stall_counter;
        self.last_command_signature = snapshot.last_command_signature;
        self.retry_counters = snapshot.retry_counters;
        Ok(())
    }
}

fn format_command(parts: &[String]) -> String {
    shlex::try_join(parts.iter().map(String::as_str)).unwrap_or_else(|_| parts.join(" "))
}

#[derive(Clone, Serialize, Deserialize, Default)]
struct PlanState {
    tasks: Vec<PlanTask>,
}

impl PlanState {
    fn apply_update(&mut self, update: &UpdatePlanArgs) -> PlanSummary {
        self.tasks = update
            .plan
            .iter()
            .map(|PlanItemArg { step, status }| PlanTask {
                step: step.clone(),
                status: status.clone().into(),
            })
            .collect();
        PlanSummary::from_tasks(&self.tasks)
    }

    fn summary_notice(&self) -> Option<ContinuousNotice> {
        if self.tasks.is_empty() {
            None
        } else {
            Some(ContinuousNotice::Info(
                PlanSummary::from_tasks(&self.tasks).summary(),
            ))
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
struct PlanTask {
    step: String,
    status: PlanTaskStatus,
}

#[derive(Clone, Serialize, Deserialize)]
enum PlanTaskStatus {
    Pending,
    Active,
    Done,
}

impl From<StepStatus> for PlanTaskStatus {
    fn from(value: StepStatus) -> Self {
        match value {
            StepStatus::Pending => PlanTaskStatus::Pending,
            StepStatus::InProgress => PlanTaskStatus::Active,
            StepStatus::Completed => PlanTaskStatus::Done,
        }
    }
}

struct PlanSummary {
    pending: usize,
    active: usize,
    done: usize,
}

impl PlanSummary {
    fn from_tasks(tasks: &[PlanTask]) -> Self {
        let mut summary = Self {
            pending: 0,
            active: 0,
            done: 0,
        };
        for task in tasks {
            match task.status {
                PlanTaskStatus::Pending => summary.pending += 1,
                PlanTaskStatus::Active => summary.active += 1,
                PlanTaskStatus::Done => summary.done += 1,
            }
        }
        summary
    }

    fn summary(&self) -> String {
        format!(
            "计划更新 • 待处理 {} 项 • 进行中 {} 项 • 已完成 {} 项",
            self.pending, self.active, self.done
        )
    }
}

#[derive(Clone)]
struct PendingApproval {
    id: String,
    kind: ApprovalKind,
    summary: String,
}

impl PendingApproval {
    fn exec(id: String, summary: String) -> Self {
        Self {
            id,
            kind: ApprovalKind::Exec,
            summary,
        }
    }

    fn patch(id: String, summary: String) -> Self {
        Self {
            id,
            kind: ApprovalKind::Patch,
            summary,
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
enum ApprovalKind {
    Exec,
    Patch,
}

#[derive(Clone, Serialize, Deserialize)]
struct ContinuousStateFile {
    session_id: String,
    plan: PlanState,
    steps_since_resume: u32,
    pause: Option<PauseStateFile>,
    workspace_snapshot: Option<Vec<String>>,
    settings: ContinuousSettingsSnapshot,
    pending_approvals: Vec<PendingApprovalState>,
    stall_counter: u32,
    last_command_signature: Option<String>,
    retry_counters: HashMap<String, u32>,
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
enum PauseStateFile {
    Manual,
    StepLimit { limit: u32, summary: String },
    WorkspaceChanged,
    CommandFailed { summary: String },
    AwaitingApproval { summary: String },
    Stalled { summary: String },
}

impl From<PauseStateFile> for PauseReason {
    fn from(value: PauseStateFile) -> Self {
        match value {
            PauseStateFile::Manual => PauseReason::Manual,
            PauseStateFile::StepLimit { limit, summary } => {
                PauseReason::StepLimit { limit, summary }
            }
            PauseStateFile::WorkspaceChanged => PauseReason::WorkspaceChanged,
            PauseStateFile::CommandFailed { summary } => PauseReason::CommandFailed { summary },
            PauseStateFile::AwaitingApproval { summary } => {
                PauseReason::AwaitingApproval { summary }
            }
            PauseStateFile::Stalled { summary } => PauseReason::Stalled { summary },
        }
    }
}

impl From<PauseReason> for PauseStateFile {
    fn from(value: PauseReason) -> Self {
        match value {
            PauseReason::Manual => PauseStateFile::Manual,
            PauseReason::StepLimit { limit, summary } => {
                PauseStateFile::StepLimit { limit, summary }
            }
            PauseReason::WorkspaceChanged => PauseStateFile::WorkspaceChanged,
            PauseReason::CommandFailed { summary } => PauseStateFile::CommandFailed { summary },
            PauseReason::AwaitingApproval { summary } => {
                PauseStateFile::AwaitingApproval { summary }
            }
            PauseReason::Stalled { summary } => PauseStateFile::Stalled { summary },
        }
    }
}

#[derive(Clone)]
enum PauseReason {
    Manual,
    StepLimit { limit: u32, summary: String },
    WorkspaceChanged,
    CommandFailed { summary: String },
    AwaitingApproval { summary: String },
    Stalled { summary: String },
}

impl PauseReason {
    fn summary(&self) -> String {
        match self {
            PauseReason::Manual => "连续模式已手动暂停".to_string(),
            PauseReason::StepLimit { limit, summary } => {
                format!("达到步数上限 ({limit})，待处理：{summary}")
            }
            PauseReason::WorkspaceChanged => "检测到工作区外部改动，已暂停".to_string(),
            PauseReason::CommandFailed { summary } => {
                format!("命令失败：{summary}")
            }
            PauseReason::AwaitingApproval { summary } => {
                format!("等待确认：{summary}")
            }
            PauseReason::Stalled { summary } => {
                format!("疑似卡住：{summary}")
            }
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
struct ContinuousSettingsSnapshot {
    max_steps: u32,
    confirm_on_breakpoint: bool,
}

impl From<&ContinuousModeSettings> for ContinuousSettingsSnapshot {
    fn from(value: &ContinuousModeSettings) -> Self {
        Self {
            max_steps: value.max_steps,
            confirm_on_breakpoint: value.confirm_on_breakpoint,
        }
    }
}

struct PersistenceState {
    base_dir: PathBuf,
    session_path: Option<PathBuf>,
}

#[derive(Clone, Serialize, Deserialize)]
struct PendingApprovalState {
    id: String,
    summary: String,
    kind: ApprovalKind,
}

impl From<&PendingApproval> for PendingApprovalState {
    fn from(value: &PendingApproval) -> Self {
        Self {
            id: value.id.clone(),
            summary: value.summary.clone(),
            kind: value.kind.clone(),
        }
    }
}

impl From<PendingApprovalState> for PendingApproval {
    fn from(value: PendingApprovalState) -> Self {
        match value.kind {
            ApprovalKind::Exec => PendingApproval::exec(value.id, value.summary),
            ApprovalKind::Patch => PendingApproval::patch(value.id, value.summary),
        }
    }
}

async fn capture_git_status(cwd: &Path) -> Result<Option<Vec<String>>> {
    let output = Command::new("git")
        .arg("status")
        .arg("--short")
        .arg("--untracked-files=normal")
        .current_dir(cwd)
        .output()
        .await;

    let output = match output {
        Ok(output) => output,
        Err(_) => return Ok(None),
    };

    if !output.status.success() {
        return Ok(None);
    }

    let stdout = String::from_utf8(output.stdout).map_err(|e| eyre!(e))?;
    Ok(Some(stdout.lines().map(|line| line.to_string()).collect()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use codex_core::protocol::EventMsg;
    use codex_core::protocol::ExecCommandEndEvent;
    use codex_core::protocol::TaskCompleteEvent;
    use crossterm::event::KeyModifiers;
    use tokio::sync::mpsc::unbounded_channel;

    #[tokio::test]
    async fn auto_approves_when_not_paused() {
        let tmp = tempfile::tempdir().unwrap();
        let (tx, _rx) = unbounded_channel();
        let mut controller = ContinuousController::new(
            ContinuousModeSettings {
                max_steps: 2,
                confirm_on_breakpoint: false,
            },
            tmp.path(),
            AppEventSender::new(tx),
        )
        .unwrap();
        controller
            .on_session_configured(&SessionConfiguredEvent {
                session_id: ConversationId::new(),
                model: "model".into(),
                reasoning_effort: None,
                history_log_id: 0,
                history_entry_count: 0,
                initial_messages: None,
                rollout_path: PathBuf::new(),
            })
            .await
            .unwrap();
        let event = Event {
            id: "1".into(),
            msg: EventMsg::ExecApprovalRequest(ExecApprovalRequestEvent {
                call_id: "call".into(),
                command: vec!["echo".into(), "hi".into()],
                cwd: PathBuf::new(),
                reason: None,
            }),
        };
        let result = controller.on_codex_event(&event, tmp.path()).await.unwrap();
        assert!(result.consume_event);
    }

    #[tokio::test]
    async fn pauses_on_step_limit() {
        let tmp = tempfile::tempdir().unwrap();
        let (tx, _rx) = unbounded_channel();
        let mut controller = ContinuousController::new(
            ContinuousModeSettings {
                max_steps: 1,
                confirm_on_breakpoint: false,
            },
            tmp.path(),
            AppEventSender::new(tx),
        )
        .unwrap();
        controller
            .on_session_configured(&SessionConfiguredEvent {
                session_id: ConversationId::new(),
                model: "model".into(),
                reasoning_effort: None,
                history_log_id: 0,
                history_entry_count: 0,
                initial_messages: None,
                rollout_path: PathBuf::new(),
            })
            .await
            .unwrap();
        controller.steps_since_resume = 1;
        let event = Event {
            id: "1".into(),
            msg: EventMsg::ExecApprovalRequest(ExecApprovalRequestEvent {
                call_id: "call".into(),
                command: vec!["echo".into(), "hi".into()],
                cwd: PathBuf::new(),
                reason: None,
            }),
        };
        let result = controller.on_codex_event(&event, tmp.path()).await.unwrap();
        assert!(!result.consume_event);
        assert!(matches!(
            controller.pause,
            Some(PauseReason::StepLimit { .. })
        ));
    }

    #[tokio::test]
    async fn resume_clears_pause() {
        let tmp = tempfile::tempdir().unwrap();
        let (tx, _rx) = unbounded_channel();
        let mut controller = ContinuousController::new(
            ContinuousModeSettings {
                max_steps: 1,
                confirm_on_breakpoint: false,
            },
            tmp.path(),
            AppEventSender::new(tx),
        )
        .unwrap();
        controller.pause = Some(PauseReason::Manual);
        let notices = controller.resume(tmp.path()).await.unwrap();
        assert!(matches!(controller.pause, None));
        assert!(!notices.is_empty());
    }

    #[tokio::test]
    async fn handle_key_event_resume() {
        let tmp = tempfile::tempdir().unwrap();
        let (tx, _rx) = unbounded_channel();
        let mut controller = ContinuousController::new(
            ContinuousModeSettings {
                max_steps: 1,
                confirm_on_breakpoint: false,
            },
            tmp.path(),
            AppEventSender::new(tx),
        )
        .unwrap();
        controller.pause = Some(PauseReason::Manual);
        let key = KeyEvent::new(KeyCode::Char('c'), KeyModifiers::empty());
        let handled = controller.handle_key_event(&key, tmp.path()).await.unwrap();
        assert!(handled.is_some());
    }

    #[tokio::test]
    async fn handle_exec_end_failure_pauses() {
        let tmp = tempfile::tempdir().unwrap();
        let (tx, _rx) = unbounded_channel();
        let mut controller = ContinuousController::new(
            ContinuousModeSettings {
                max_steps: 1,
                confirm_on_breakpoint: false,
            },
            tmp.path(),
            AppEventSender::new(tx),
        )
        .unwrap();
        for _ in 0..MAX_AUTO_RETRIES {
            let notices = controller
                .on_exec_end(
                    &ExecCommandEndEvent {
                        call_id: "1".into(),
                        stdout: String::new(),
                        stderr: String::new(),
                        aggregated_output: String::new(),
                        exit_code: 1,
                        duration: std::time::Duration::default(),
                        formatted_output: String::new(),
                    },
                    tmp.path(),
                )
                .await
                .unwrap();
            assert!(!notices.is_empty());
        }
        assert!(matches!(
            controller.pause,
            Some(PauseReason::CommandFailed { .. })
        ));
    }

    #[tokio::test]
    async fn stalled_detection_triggers_pause() {
        let tmp = tempfile::tempdir().unwrap();
        let (tx, _rx) = unbounded_channel();
        let mut controller = ContinuousController::new(
            ContinuousModeSettings {
                max_steps: 5,
                confirm_on_breakpoint: false,
            },
            tmp.path(),
            AppEventSender::new(tx),
        )
        .unwrap();

        for idx in 0..STALL_LIMIT {
            let call_id = format!("call{idx}");
            controller
                .running_commands
                .insert(call_id.clone(), vec!["echo".into(), "hi".into()]);
            let notices = controller
                .on_exec_end(
                    &ExecCommandEndEvent {
                        call_id,
                        stdout: String::new(),
                        stderr: String::new(),
                        aggregated_output: String::new(),
                        exit_code: 0,
                        duration: std::time::Duration::default(),
                        formatted_output: String::new(),
                    },
                    tmp.path(),
                )
                .await
                .unwrap();
            if idx + 1 < STALL_LIMIT {
                assert!(
                    notices
                        .iter()
                        .any(|n| matches!(n, ContinuousNotice::Info(_)))
                );
            }
        }

        assert!(matches!(
            controller.pause,
            Some(PauseReason::Stalled { .. })
        ));
    }

    #[tokio::test]
    async fn task_complete_resets_steps() {
        let tmp = tempfile::tempdir().unwrap();
        let (tx, _rx) = unbounded_channel();
        let mut controller = ContinuousController::new(
            ContinuousModeSettings {
                max_steps: 5,
                confirm_on_breakpoint: false,
            },
            tmp.path(),
            AppEventSender::new(tx),
        )
        .unwrap();
        controller.steps_since_resume = 3;
        let event = Event {
            id: "1".into(),
            msg: EventMsg::TaskComplete(TaskCompleteEvent {
                last_agent_message: Some("done".into()),
            }),
        };
        controller.on_codex_event(&event, tmp.path()).await.unwrap();
        assert_eq!(controller.steps_since_resume, 0);
    }

    #[tokio::test]
    async fn load_state_restores_plan_and_pending() {
        let tmp = tempfile::tempdir().unwrap();
        let session_id = ConversationId::new();
        let (tx, _rx) = unbounded_channel();
        let mut controller = ContinuousController::new(
            ContinuousModeSettings {
                max_steps: 5,
                confirm_on_breakpoint: true,
            },
            tmp.path(),
            AppEventSender::new(tx),
        )
        .unwrap();

        controller
            .on_session_configured(&SessionConfiguredEvent {
                session_id,
                model: "model".into(),
                reasoning_effort: None,
                history_log_id: 0,
                history_entry_count: 0,
                initial_messages: None,
                rollout_path: PathBuf::new(),
            })
            .await
            .unwrap();

        controller.plan.tasks.push(PlanTask {
            step: "实现功能".into(),
            status: PlanTaskStatus::Active,
        });
        controller.pause = Some(PauseReason::Manual);
        controller
            .pending_approvals
            .push_back(PendingApproval::exec("id".into(), "echo".into()));
        controller.save_state().await.unwrap();
        drop(controller);

        let (tx2, _rx2) = unbounded_channel();
        let mut controller = ContinuousController::new(
            ContinuousModeSettings {
                max_steps: 5,
                confirm_on_breakpoint: true,
            },
            tmp.path(),
            AppEventSender::new(tx2),
        )
        .unwrap();

        let notices = controller
            .on_session_configured(&SessionConfiguredEvent {
                session_id,
                model: "model".into(),
                reasoning_effort: None,
                history_log_id: 0,
                history_entry_count: 0,
                initial_messages: None,
                rollout_path: PathBuf::new(),
            })
            .await
            .unwrap();

        assert!(
            notices
                .iter()
                .any(|n| matches!(n, ContinuousNotice::Info(_)))
        );
        assert!(
            notices
                .iter()
                .any(|n| matches!(n, ContinuousNotice::Warn(_)))
        );
        assert_eq!(controller.pending_approvals.len(), 1);
        assert!(matches!(controller.pause, Some(PauseReason::Manual)));
    }
}
