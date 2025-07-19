use codex_common::elapsed::format_elapsed;
use codex_core::WireApi;
use codex_core::config::Config;
use codex_core::model_supports_reasoning_summaries;
use codex_core::protocol::AgentMessageEvent;
use codex_core::protocol::BackgroundEventEvent;
use codex_core::protocol::ErrorEvent;
use codex_core::protocol::Event;
use codex_core::protocol::EventMsg;
use codex_core::protocol::ExecCommandBeginEvent;
use codex_core::protocol::ExecCommandEndEvent;
use codex_core::protocol::FileChange;
use codex_core::protocol::McpToolCallBeginEvent;
use codex_core::protocol::McpToolCallEndEvent;
use codex_core::protocol::PatchApplyBeginEvent;
use codex_core::protocol::PatchApplyEndEvent;
use codex_core::protocol::SessionConfiguredEvent;
use codex_core::protocol::TokenUsage;
use owo_colors::OwoColorize;
use owo_colors::Style;
use shlex::try_join;
use std::collections::HashMap;
use std::time::Instant;

/// Render a simple subset of Markdown to ANSI-styled text for CLI output.
/// Supports headings (#, ##, ###), inline code (`code`), and fenced code blocks (```).
fn render_markdown_to_ansi(
    input: &str,
    bold: Style,
    italic: Style,
    dimmed: Style,
    code_style: Style,
) -> String {
    let mut out_lines = Vec::new();
    let mut in_code_block = false;
    for line in input.lines() {
        let trimmed = line.trim_start();

        // Handle fenced code blocks (```)
        if trimmed.starts_with("```") {
            in_code_block = !in_code_block;
            // Skip the fence markers themselves so they are not printed.
            continue;
        }

        if in_code_block {
            // Keep code block lines indented and apply the dedicated style.
            out_lines.push(format!("  {}", line).style(code_style).to_string());
            continue;
        }

        // For non-code lines we perform a *very* small subset of markdown
        // rendering that is sufficient for our CLI output:
        //   * Headings (#, ##, ### …)  -> bold
        //   * Inline code              -> code_style
        //   * Bold (**text** or __text__)  -> bold
        //   * Italic (*text* or _text_)    -> italic
        //   * Blockquote (> line)      -> dimmed

        // Strip heading markers first so other inline formatting applies to
        // the heading content as well.
        let (mut content, heading_style) = if trimmed.starts_with('#') {
            (
                trimmed.trim_start_matches('#').trim_start().to_string(),
                Some(bold),
            )
        } else {
            (line.to_string(), None)
        };

        // If the line is a blockquote, drop the leading '>' and one optional
        // space so we can style the remainder with `dimmed` to visually
        // distinguish it.
        let mut apply_dimmed = false;
        let blockquote_trimmed = content.trim_start();
        if blockquote_trimmed.starts_with('>') {
            apply_dimmed = true;
            content = blockquote_trimmed[1..].trim_start().to_string();
        }

        // Helper to push a styled span into an output buffer.
        let mut rendered_line = String::new();
        let mut idx = 0;
        let bytes = content.as_bytes();
        while idx < bytes.len() {
            // Inline code span
            if bytes[idx] == b'`' {
                if let Some(end) = content[idx + 1..].find('`') {
                    let code_text = &content[idx + 1..idx + 1 + end];
                    rendered_line.push_str(&code_text.style(code_style).to_string());
                    idx += end + 2;
                    continue;
                }
            }

            // Bold (** or __)
            if idx + 1 < bytes.len() && (bytes[idx] == b'*' && bytes[idx + 1] == b'*') {
                if let Some(end) = content[idx + 2..].find("**") {
                    let bold_text = &content[idx + 2..idx + 2 + end];
                    rendered_line.push_str(&bold_text.style(bold).to_string());
                    idx += end + 4;
                    continue;
                }
            }
            if idx + 1 < bytes.len() && (bytes[idx] == b'_' && bytes[idx + 1] == b'_') {
                if let Some(end) = content[idx + 2..].find("__") {
                    let bold_text = &content[idx + 2..idx + 2 + end];
                    rendered_line.push_str(&bold_text.style(bold).to_string());
                    idx += end + 4;
                    continue;
                }
            }

            // Italic (* or _)
            if bytes[idx] == b'*' || bytes[idx] == b'_' {
                let marker = bytes[idx];
                if let Some(end_pos) = content[idx + 1..].find(marker as char) {
                    let italic_text = &content[idx + 1..idx + 1 + end_pos];
                    rendered_line.push_str(&italic_text.style(italic).to_string());
                    idx += end_pos + 2;
                    continue;
                }
            }

            // Otherwise, push current byte and advance.
            rendered_line.push(bytes[idx] as char);
            idx += 1;
        }

        // Apply heading *or* blockquote style if present. We prefer heading
        // style (bold) so blockquote > # Heading stays bold.
        if let Some(style) = heading_style {
            rendered_line = rendered_line.style(style).to_string();
        } else if apply_dimmed {
            rendered_line = rendered_line.style(dimmed).to_string();
        }

        out_lines.push(rendered_line);
    }
    out_lines.join("\n")
}

#[cfg(test)]
mod tests {
    use super::render_markdown_to_ansi;
    use owo_colors::Style;

    #[test]
    fn test_render_markdown_to_ansi_basic() {
        let bold = Style::new();
        let italic = Style::new();
        let dimmed = Style::new();
        let code_style = Style::new();
        let input = "# Heading\nSome `code` example.\n\n```rust\nfn main() {}\n```\n";
        let output = render_markdown_to_ansi(input, bold, italic, dimmed, code_style);
        // Markdown markers should be removed
        assert!(!output.contains('#'));
        assert!(!output.contains("```"));
        // Content should be preserved
        assert!(output.contains("Heading"));
        assert!(output.contains("code example."));
        assert!(output.contains("fn main() {}"));
    }
}

/// This should be configurable. When used in CI, users may not want to impose
/// a limit so they can see the full transcript.
const MAX_OUTPUT_LINES_FOR_EXEC_TOOL_CALL: usize = 20;

pub(crate) struct EventProcessor {
    call_id_to_command: HashMap<String, ExecCommandBegin>,
    call_id_to_patch: HashMap<String, PatchApplyBegin>,

    /// Tracks in-flight MCP tool calls so we can calculate duration and print
    /// a concise summary when the corresponding `McpToolCallEnd` event is
    /// received.
    call_id_to_tool_call: HashMap<String, McpToolCallBegin>,

    // To ensure that --color=never is respected, ANSI escapes _must_ be added
    // using .style() with one of these fields. If you need a new style, add a
    // new field here.
    bold: Style,
    italic: Style,
    dimmed: Style,

    magenta: Style,
    red: Style,
    green: Style,
    cyan: Style,

    /// Whether to include `AgentReasoning` events in the output.
    show_agent_reasoning: bool,
}

impl EventProcessor {
    pub(crate) fn create_with_ansi(with_ansi: bool, show_agent_reasoning: bool) -> Self {
        let call_id_to_command = HashMap::new();
        let call_id_to_patch = HashMap::new();
        let call_id_to_tool_call = HashMap::new();

        if with_ansi {
            Self {
                call_id_to_command,
                call_id_to_patch,
                bold: Style::new().bold(),
                italic: Style::new().italic(),
                dimmed: Style::new().dimmed(),
                magenta: Style::new().magenta(),
                red: Style::new().red(),
                green: Style::new().green(),
                cyan: Style::new().cyan(),
                call_id_to_tool_call,
                show_agent_reasoning,
            }
        } else {
            Self {
                call_id_to_command,
                call_id_to_patch,
                bold: Style::new(),
                italic: Style::new(),
                dimmed: Style::new(),
                magenta: Style::new(),
                red: Style::new(),
                green: Style::new(),
                cyan: Style::new(),
                call_id_to_tool_call,
                show_agent_reasoning,
            }
        }
    }
}

struct ExecCommandBegin {
    command: Vec<String>,
    start_time: Instant,
}

/// Metadata captured when an `McpToolCallBegin` event is received.
struct McpToolCallBegin {
    /// Formatted invocation string, e.g. `server.tool({"city":"sf"})`.
    invocation: String,
    /// Timestamp when the call started so we can compute duration later.
    start_time: Instant,
}

struct PatchApplyBegin {
    start_time: Instant,
    auto_approved: bool,
}

// Timestamped println helper. The timestamp is styled with self.dimmed.
#[macro_export]
macro_rules! ts_println {
    ($self:ident, $($arg:tt)*) => {{
        let now = chrono::Utc::now();
        let formatted = now.format("[%Y-%m-%dT%H:%M:%S]");
        print!("{} ", formatted.style($self.dimmed));
        println!($($arg)*);
    }};
}

impl EventProcessor {
    /// Print a concise summary of the effective configuration that will be used
    /// for the session. This mirrors the information shown in the TUI welcome
    /// screen.
    pub(crate) fn print_config_summary(&mut self, config: &Config, prompt: &str) {
        const VERSION: &str = env!("CARGO_PKG_VERSION");
        ts_println!(
            self,
            "OpenAI Codex v{} (research preview)\n--------",
            VERSION
        );

        let mut entries = vec![
            ("workdir", config.cwd.display().to_string()),
            ("model", config.model.clone()),
            ("provider", config.model_provider_id.clone()),
            ("approval", format!("{:?}", config.approval_policy)),
            ("sandbox", "unrestricted".to_string()),
        ];
        if config.model_provider.wire_api == WireApi::Responses
            && model_supports_reasoning_summaries(config)
        {
            entries.push((
                "reasoning effort",
                config.model_reasoning_effort.to_string(),
            ));
            entries.push((
                "reasoning summaries",
                config.model_reasoning_summary.to_string(),
            ));
        }

        for (key, value) in entries {
            println!("{} {}", format!("{key}:").style(self.bold), value);
        }

        println!("--------");

        // Echo the prompt that will be sent to the agent so it is visible in the
        // transcript/logs before any events come in. Note the prompt may have been
        // read from stdin, so it may not be visible in the terminal otherwise.
        // Render user prompt as Markdown with basic styling
        let rendered_prompt =
            render_markdown_to_ansi(prompt, self.bold, self.italic, self.dimmed, self.cyan);
        ts_println!(
            self,
            "{}\n{}",
            "User instructions:".style(self.bold).style(self.cyan),
            rendered_prompt
        );
    }

    pub(crate) fn process_event(&mut self, event: Event) {
        let Event { id: _, msg } = event;
        match msg {
            EventMsg::Error(ErrorEvent { message }) => {
                let prefix = "ERROR:".style(self.red);
                ts_println!(self, "{prefix} {message}");
            }
            EventMsg::BackgroundEvent(BackgroundEventEvent { message }) => {
                ts_println!(self, "{}", message.style(self.dimmed));
            }
            EventMsg::TaskStarted | EventMsg::TaskComplete(_) => {
                // Ignore.
            }
            EventMsg::TokenCount(TokenUsage { total_tokens, .. }) => {
                ts_println!(self, "tokens used: {total_tokens}");
            }
            EventMsg::AgentMessage(AgentMessageEvent { message }) => {
                // Render model response as Markdown with basic styling
                let rendered = render_markdown_to_ansi(
                    &message,
                    self.bold,
                    self.italic,
                    self.dimmed,
                    self.cyan,
                );
                ts_println!(
                    self,
                    "{}\n{}",
                    "codex".style(self.bold).style(self.magenta),
                    rendered
                );
            }
            EventMsg::ExecCommandBegin(ExecCommandBeginEvent {
                call_id,
                command,
                cwd,
            }) => {
                self.call_id_to_command.insert(
                    call_id.clone(),
                    ExecCommandBegin {
                        command: command.clone(),
                        start_time: Instant::now(),
                    },
                );
                ts_println!(
                    self,
                    "{} {} in {}",
                    "exec".style(self.magenta),
                    escape_command(&command).style(self.bold),
                    cwd.to_string_lossy(),
                );
            }
            EventMsg::ExecCommandEnd(ExecCommandEndEvent {
                call_id,
                stdout,
                stderr,
                exit_code,
            }) => {
                let exec_command = self.call_id_to_command.remove(&call_id);
                let (duration, call) = if let Some(ExecCommandBegin {
                    command,
                    start_time,
                }) = exec_command
                {
                    (
                        format!(" in {}", format_elapsed(start_time)),
                        format!("{}", escape_command(&command).style(self.bold)),
                    )
                } else {
                    ("".to_string(), format!("exec('{call_id}')"))
                };

                let output = if exit_code == 0 { stdout } else { stderr };
                let truncated_output = output
                    .lines()
                    .take(MAX_OUTPUT_LINES_FOR_EXEC_TOOL_CALL)
                    .collect::<Vec<_>>()
                    .join("\n");
                match exit_code {
                    0 => {
                        let title = format!("{call} succeeded{duration}:");
                        ts_println!(self, "{}", title.style(self.green));
                    }
                    _ => {
                        let title = format!("{call} exited {exit_code}{duration}:");
                        ts_println!(self, "{}", title.style(self.red));
                    }
                }
                println!("{}", truncated_output.style(self.dimmed));
            }
            EventMsg::McpToolCallBegin(McpToolCallBeginEvent {
                call_id,
                server,
                tool,
                arguments,
            }) => {
                // Build fully-qualified tool name: server.tool
                let fq_tool_name = format!("{server}.{tool}");

                // Format arguments as compact JSON so they fit on one line.
                let args_str = arguments
                    .as_ref()
                    .map(|v: &serde_json::Value| {
                        serde_json::to_string(v).unwrap_or_else(|_| v.to_string())
                    })
                    .unwrap_or_default();

                let invocation = if args_str.is_empty() {
                    format!("{fq_tool_name}()")
                } else {
                    format!("{fq_tool_name}({args_str})")
                };

                self.call_id_to_tool_call.insert(
                    call_id.clone(),
                    McpToolCallBegin {
                        invocation: invocation.clone(),
                        start_time: Instant::now(),
                    },
                );

                ts_println!(
                    self,
                    "{} {}",
                    "tool".style(self.magenta),
                    invocation.style(self.bold),
                );
            }
            EventMsg::McpToolCallEnd(tool_call_end_event) => {
                let is_success = tool_call_end_event.is_success();
                let McpToolCallEndEvent { call_id, result } = tool_call_end_event;
                // Retrieve start time and invocation for duration calculation and labeling.
                let info = self.call_id_to_tool_call.remove(&call_id);

                let (duration, invocation) = if let Some(McpToolCallBegin {
                    invocation,
                    start_time,
                    ..
                }) = info
                {
                    (format!(" in {}", format_elapsed(start_time)), invocation)
                } else {
                    (String::new(), format!("tool('{call_id}')"))
                };

                let status_str = if is_success { "success" } else { "failed" };
                let title_style = if is_success { self.green } else { self.red };
                let title = format!("{invocation} {status_str}{duration}:");

                ts_println!(self, "{}", title.style(title_style));

                if let Ok(res) = result {
                    let val: serde_json::Value = res.into();
                    let pretty =
                        serde_json::to_string_pretty(&val).unwrap_or_else(|_| val.to_string());

                    for line in pretty.lines().take(MAX_OUTPUT_LINES_FOR_EXEC_TOOL_CALL) {
                        println!("{}", line.style(self.dimmed));
                    }
                }
            }
            EventMsg::PatchApplyBegin(PatchApplyBeginEvent {
                call_id,
                auto_approved,
                changes,
            }) => {
                // Store metadata so we can calculate duration later when we
                // receive the corresponding PatchApplyEnd event.
                self.call_id_to_patch.insert(
                    call_id.clone(),
                    PatchApplyBegin {
                        start_time: Instant::now(),
                        auto_approved,
                    },
                );

                ts_println!(
                    self,
                    "{} auto_approved={}:",
                    "apply_patch".style(self.magenta),
                    auto_approved,
                );

                // Pretty-print the patch summary with colored diff markers so
                // it’s easy to scan in the terminal output.
                for (path, change) in changes.iter() {
                    match change {
                        FileChange::Add { content } => {
                            let header = format!(
                                "{} {}",
                                format_file_change(change),
                                path.to_string_lossy()
                            );
                            println!("{}", header.style(self.magenta));
                            for line in content.lines() {
                                println!("{}", line.style(self.green));
                            }
                        }
                        FileChange::Delete => {
                            let header = format!(
                                "{} {}",
                                format_file_change(change),
                                path.to_string_lossy()
                            );
                            println!("{}", header.style(self.magenta));
                        }
                        FileChange::Update {
                            unified_diff,
                            move_path,
                        } => {
                            let header = if let Some(dest) = move_path {
                                format!(
                                    "{} {} -> {}",
                                    format_file_change(change),
                                    path.to_string_lossy(),
                                    dest.to_string_lossy()
                                )
                            } else {
                                format!("{} {}", format_file_change(change), path.to_string_lossy())
                            };
                            println!("{}", header.style(self.magenta));

                            // Colorize diff lines. We keep file header lines
                            // (--- / +++) without extra coloring so they are
                            // still readable.
                            for diff_line in unified_diff.lines() {
                                if diff_line.starts_with('+') && !diff_line.starts_with("+++") {
                                    println!("{}", diff_line.style(self.green));
                                } else if diff_line.starts_with('-')
                                    && !diff_line.starts_with("---")
                                {
                                    println!("{}", diff_line.style(self.red));
                                } else {
                                    println!("{diff_line}");
                                }
                            }
                        }
                    }
                }
            }
            EventMsg::PatchApplyEnd(PatchApplyEndEvent {
                call_id,
                stdout,
                stderr,
                success,
            }) => {
                let patch_begin = self.call_id_to_patch.remove(&call_id);

                // Compute duration and summary label similar to exec commands.
                let (duration, label) = if let Some(PatchApplyBegin {
                    start_time,
                    auto_approved,
                }) = patch_begin
                {
                    (
                        format!(" in {}", format_elapsed(start_time)),
                        format!("apply_patch(auto_approved={auto_approved})"),
                    )
                } else {
                    (String::new(), format!("apply_patch('{call_id}')"))
                };

                let (exit_code, output, title_style) = if success {
                    (0, stdout, self.green)
                } else {
                    (1, stderr, self.red)
                };

                let title = format!("{label} exited {exit_code}{duration}:");
                ts_println!(self, "{}", title.style(title_style));
                for line in output.lines() {
                    println!("{}", line.style(self.dimmed));
                }
            }
            EventMsg::ExecApprovalRequest(_) => {
                // Should we exit?
            }
            EventMsg::ApplyPatchApprovalRequest(_) => {
                // Should we exit?
            }
            EventMsg::AgentReasoning(agent_reasoning_event) => {
                if self.show_agent_reasoning {
                    ts_println!(
                        self,
                        "{}\n{}",
                        "thinking".style(self.italic).style(self.magenta),
                        agent_reasoning_event.text
                    );
                }
            }
            EventMsg::SessionConfigured(session_configured_event) => {
                let SessionConfiguredEvent {
                    session_id,
                    model,
                    history_log_id: _,
                    history_entry_count: _,
                } = session_configured_event;

                ts_println!(
                    self,
                    "{} {}",
                    "codex session".style(self.magenta).style(self.bold),
                    session_id.to_string().style(self.dimmed)
                );

                ts_println!(self, "model: {}", model);
                println!();
            }
            EventMsg::GetHistoryEntryResponse(_) => {
                // Currently ignored in exec output.
            }
        }
    }
}

fn escape_command(command: &[String]) -> String {
    try_join(command.iter().map(|s| s.as_str())).unwrap_or_else(|_| command.join(" "))
}

fn format_file_change(change: &FileChange) -> &'static str {
    match change {
        FileChange::Add { .. } => "A",
        FileChange::Delete => "D",
        FileChange::Update {
            move_path: Some(_), ..
        } => "R",
        FileChange::Update {
            move_path: None, ..
        } => "M",
    }
}
