//! Native terminal interactive CLI without full-screen TUI.
//!
//! This mode prints all Codex events as ANSI text so they integrate with the
//! terminal’s scroll-back buffer and copy-&-paste behaviour.  User input is
//! captured with the `reedline` crate which provides a multi-line editable
//! prompt similar to modern shells.

use std::io::{self, IsTerminal, Write};
use std::path::PathBuf;
use std::sync::Arc;

use codex_core::codex_wrapper::{init_codex, CodexConversation};
use codex_core::config::Config;
use codex_core::config::ConfigOverrides;
use codex_core::protocol::{InputItem, Op};
use codex_core::protocol::{AskForApproval, Event};
use codex_core::util::is_inside_git_repo;
use reedline::{
    default_emacs_keybindings, FileBackedHistory, DefaultPrompt, EditCommand, Emacs, Reedline,
    ReedlineEvent, Signal,
};


// Re-exported from `reedline` so we avoid pulling in an extra version of the
// `crossterm` crate which would cause type mismatches.
use reedline::{KeyCode, KeyModifiers};

use std::sync::atomic::{AtomicBool, Ordering};
use tokio::sync::mpsc::unbounded_channel;
use tracing::error;

use codex_core::protocol::{EventMsg, TaskCompleteEvent};

use ratatui::style::{Color, Modifier, Style};
use tui_markdown; // already dependency via codex-tui

/// Convert a markdown string into ANSI-coloured text suitable for printing to a
/// normal terminal.  This performs **minimal** rendering: headings and block
/// formatting are kept as plain text while basic inline styles (bold, italic
/// and underline) are mapped to the corresponding SGR sequences.  All other
/// markup is stripped.
#[allow(dead_code)]
fn markdown_to_ansi(src: &str) -> String {
    // Parse the markdown into `ratatui::text::Text` using the same renderer
    // used by the fullscreen TUI implementation so we keep behaviour
    // consistent across both interactive modes.
    let text = tui_markdown::from_str(src);

    text.lines
        .iter()
        .map(|line| {
            line.spans
                .iter()
                .map(|span| {
                    let style_prefix = style_to_ansi(&span.style);
                    let reset = if style_prefix.is_empty() {
                        "".to_string()
                    } else {
                        "\x1b[0m".to_string()
                    };
                    format!("{style_prefix}{}{reset}", span.content)
                })
                .collect::<String>()
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Translate a `ratatui::style::Style` into an ANSI SGR prefix.  Only a subset
/// of style attributes are handled as required by the current markdown
/// renderer.
#[allow(dead_code)]
fn style_to_ansi(style: &Style) -> String {
    let mut codes: Vec<&str> = Vec::new();

    if let Some(color) = style.fg {
        if let Some(code) = color_to_ansi(color) {
            codes.push(code);
        }
    }

    let mods = style.add_modifier;
    if mods.contains(Modifier::BOLD) {
        codes.push("1");
    }
    if mods.contains(Modifier::ITALIC) {
        codes.push("3");
    }
    if mods.contains(Modifier::UNDERLINED) {
        codes.push("4");
    }

    if codes.is_empty() {
        String::new()
    } else {
        format!("\x1b[{}m", codes.join(";"))
    }
}

/// Map a limited set of `ratatui::style::Color` variants to ANSI colour codes.
/// Only colours that are actually produced by the markdown renderer are
/// considered; for everything else we fall back to None so the caller can skip
/// emitting a foreground colour.
#[allow(dead_code)]
fn color_to_ansi(color: Color) -> Option<&'static str> {
    match color {
        Color::Black => Some("30"),
        Color::Red => Some("31"),
        Color::Green => Some("32"),
        Color::Yellow => Some("33"),
        Color::Blue => Some("34"),
        Color::Magenta => Some("35"),
        Color::Cyan => Some("36"),
        Color::Gray => Some("90"),
        Color::DarkGray => Some("90"),
        Color::LightRed => Some("91"),
        Color::LightGreen => Some("92"),
        Color::LightYellow => Some("93"),
        Color::LightBlue => Some("94"),
        Color::LightMagenta => Some("95"),
        Color::LightCyan => Some("96"),
        Color::White => Some("97"),
        _ => None,
    }
}

use codex_tui::Cli as TuiCli;

/// Run the *native* interactive CLI (no full-screen TUI).
///
/// This is a best-effort implementation meant for environments where the
/// alternate-screen based TUI cannot be used (e.g. inside tmux/screen splits or
/// when the user explicitly opts out via `--no-tui`).
#[allow(dead_code)]
pub(crate) async fn run_native_cli(
    interactive_cli: TuiCli,
    _codex_linux_sandbox_exe: Option<PathBuf>,
) -> anyhow::Result<()> {
    if !std::io::stdout().is_terminal() || !std::io::stdin().is_terminal() {
        eprintln!("Native interactive mode requires both stdin and stdout to be TTYs.");
        std::process::exit(1);
    }

    // Build Config similar to TUI path.
    let approval_policy = interactive_cli
        .approval_policy
        .map(|a| AskForApproval::from(a))
        .unwrap_or(AskForApproval::Never);

    let overrides = ConfigOverrides {
        model: interactive_cli.model.clone(),
        config_profile: interactive_cli.config_profile.clone(),
        approval_policy: Some(approval_policy),
        cwd: interactive_cli.cwd.clone(),
        model_provider: None,
        codex_linux_sandbox_exe: None,
        base_instructions: None,
        include_plan_tool: None,
    };

    // Parse any `-c key=value` overrides.
    let cli_kv_overrides = match interactive_cli.config_overrides.parse_overrides() {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Error parsing -c overrides: {e}");
            std::process::exit(1);
        }
    };

    let config = Config::load_with_cli_overrides(cli_kv_overrides, overrides)?;

    // Simple git repo check to match exec CLI behaviour.
    if !interactive_cli.skip_git_repo_check && !is_inside_git_repo(&config) {
        eprintln!("Not inside a Git repo. Use --skip-git-repo-check to override.");
        std::process::exit(1);
    }

    // Initialize Codex backend.
    let CodexConversation {
        codex: codex_wrapper,
        session_configured: session_event,
        ctrl_c,
        ..
    } = init_codex(config.clone()).await?;
    println!("Codex session initialized: {:?}", session_event.msg);

    let codex = Arc::new(codex_wrapper);

    // Channel for submitting Ops to Codex from the input thread.
    let (op_tx, mut op_rx) = unbounded_channel::<Op>();

    // Spawn task that consumes Ops and submits them to Codex.
    {
        let codex = codex.clone();
        tokio::spawn(async move {
            while let Some(op) = op_rx.recv().await {
                let res = codex.submit(op).await;
                if let Err(e) = res {
                    error!("Failed to submit op: {e}");
                }
            }
        });
    }

    // Spawn task that listens for events from Codex and prints them.
    let (event_tx, mut event_rx) = unbounded_channel::<Event>();
    {
        let codex = codex.clone();
        tokio::spawn(async move {
            loop {
                let interrupted = ctrl_c.notified();
                tokio::select! {
                    _ = interrupted => {
                        // on ctrl_c, propagate interrupt op.
                        let _ = codex.submit(Op::Interrupt).await;
                    }
                    res = codex.next_event() => {
                        match res {
                            Ok(event) => {
                                let _ = event_tx.send(event);
                            }
                            Err(e) => {
                                error!("Error reading Codex event: {e}");
                                break;
                            }
                        }
                    }
                }
            }
        });
    }

    // Shared flags for coordination between tasks
    // `thinking_flag` – Codex is processing the last user input (used for spinner & Ctrl-C behaviour)
    // `exit_flag`     – User requested to terminate the REPL (e.g. Ctrl-C while idle or Ctrl-D)
    let thinking_flag = Arc::new(AtomicBool::new(false));
    let exit_flag = Arc::new(AtomicBool::new(false));

    // Spawn blocking task for reedline input loop. We send ops via op_tx.
    let input_handle = {
        let op_tx = op_tx.clone();
        let thinking_flag = thinking_flag.clone();
        let exit_flag = exit_flag.clone();
        std::thread::spawn(move || {
            // Build a Reedline instance with:
            // 1. File-backed history so users can navigate previous inputs just
            //    like in the full-screen TUI implementation.
            // 2. A key-binding (Alt+Enter) that inserts a newline into the
            //    current buffer, giving us basic multi-line editing support.

            // History file inside the user’s home directory
            let history_file = dirs::home_dir()
                .unwrap_or_else(|| PathBuf::from("."))
                .join(".codex_history");

            // Custom keybindings based on the default Emacs set
            let mut keybindings = default_emacs_keybindings();
            // Allow inserting a literal newline inside the prompt using a few
            // common key combinations so the feature works across different
            // terminal/OS setups.
            //
            // 1. Alt+Enter – the original binding that works well in many
            //    terminal emulators.
            // 2. Shift+Enter – familiar from chat/web UIs (e.g. Slack, Discord)
            //    where Enter submits and Shift+Enter inserts a newline.
            // 3. Ctrl+Enter – another popular variant found in some
            //    applications.  We map all three to the same `InsertNewline`
            //    action so users can pick whichever combination their
            //    environment reports.

            for mods in [KeyModifiers::ALT, KeyModifiers::SHIFT, KeyModifiers::CONTROL] {
                keybindings.add_binding(
                    mods,
                    KeyCode::Enter,
                    ReedlineEvent::Edit(vec![EditCommand::InsertNewline]),
                );
            }

            let edit_mode = Box::new(Emacs::new(keybindings));

            let mut rl = Reedline::create().with_edit_mode(edit_mode);

            if let Ok(history) = FileBackedHistory::with_file(1000, history_file) {
                rl = rl.with_history(Box::new(history));
            }

            let prompt = DefaultPrompt::default();

            loop {
                match rl.read_line(&prompt) {
                    Ok(Signal::Success(buffer)) => {
                        // Empty => ignore
                        if buffer.trim().is_empty() {
                            continue;
                        }
                        let items = vec![InputItem::Text { text: buffer }];
                        let op = Op::UserInput { items };
                        // Indicate that Codex is now processing the user
                        // input so the spinner can be shown.
                        thinking_flag.store(true, Ordering::SeqCst);

                        if op_tx.send(op).is_err() {
                            break; // channel closed
                        }
                    }
                    Ok(Signal::CtrlC) => {
                        // If the assistant is currently processing (spinner
                        // active), translate Ctrl-C into an interrupt signal
                        // for Codex so it cancels the current request.
                        //
                        // Otherwise, treat Ctrl-C as a request to exit the
                        // native CLI – matching the behaviour of many
                        // command-line applications where the first Ctrl-C
                        // aborts the running job and the next one terminates
                        // the program.  Here we simply exit immediately when
                        // nothing is running so users are not forced to
                        // remember the Ctrl-D fallback.
                        if thinking_flag.load(Ordering::SeqCst) {
                            let _ = op_tx.send(Op::Interrupt);
                        } else {
                            exit_flag.store(true, Ordering::SeqCst);
                            break; // exit REPL
                        }
                    }
                    Ok(Signal::CtrlD) => {
                        exit_flag.store(true, Ordering::SeqCst);
                        break; // exit loop
                    }
                    Err(err) => {
                        eprintln!("Readline error: {err}");
                        break;
                    }
                }
            }
        })
    };

    use tokio::time::{self, Duration};

    let spinner_frames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
    let mut spinner_idx = 0usize;

    loop {
        tokio::select! {
            maybe_event = event_rx.recv() => {
                match maybe_event {
                    Some(event) => {
                        // Stop spinner if active.
                        if thinking_flag.swap(false, Ordering::SeqCst) {
                            // Clear spinner line
                            print!("\r\x1b[2K\r");
                        }

                        print_event_to_stdout(&event);
                        let _ = io::stdout().flush();

                        if matches!(event.msg, EventMsg::TaskComplete(TaskCompleteEvent { .. })) {
                            break;
                        }
                    }
                    None => break,
                }
            }
            _ = time::sleep(Duration::from_millis(120)), if thinking_flag.load(Ordering::SeqCst) => {
                print!("\r[thinking] {}", spinner_frames[spinner_idx]);
                let _ = io::stdout().flush();
                spinner_idx = (spinner_idx + 1) % spinner_frames.len();
            }
            _ = time::sleep(Duration::from_millis(50)), if exit_flag.load(Ordering::SeqCst) => {
                break;
            }
        }
    }

    // Wait for input thread to finish.
    let _ = input_handle.join();

    Ok(())
}

/// Extremely small renderer that prints a subset of events.
#[allow(dead_code)]
fn print_event_to_stdout(event: &Event) {
    match &event.msg {
        EventMsg::AgentMessage(agent) => {
            let rendered = markdown_to_ansi(&agent.message);
            println!("\n[Model] {}\n", rendered);
        }
        EventMsg::AgentReasoning(reason) => {
            let rendered = markdown_to_ansi(&reason.text);
            println!("[Reasoning] {}", rendered);
        }
        EventMsg::Error(err) => {
            eprintln!("[Error] {}", err.message);
        }
        EventMsg::ExecCommandBegin(cmd) => {
            println!("[exec] {}", cmd.command.join(" "));
        }
        EventMsg::ExecCommandEnd(end) => {
            println!("[exec completed] exit={} stdout:\n{}", end.exit_code, end.stdout);
        }
        _ => {
            // Fallback – debug print
            println!("[event] {:?}", event.msg);
        }
    }
}
