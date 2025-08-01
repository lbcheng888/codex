use std::path::Path;

// Removed sandbox functionality
use codex_core::WireApi;
use codex_core::config::Config;
use codex_core::model_supports_reasoning_summaries;
use codex_core::protocol::Event;
use codex_core::protocol::FileChange;
use shlex::try_join;

pub(crate) enum CodexStatus {
    Running,
    InitiateShutdown,
    Shutdown,
}

pub(crate) trait EventProcessor {
    /// Print summary of effective configuration and user prompt.
    fn print_config_summary(&mut self, config: &Config, prompt: &str);

    /// Handle a single event emitted by the agent.
    fn process_event(&mut self, event: Event) -> CodexStatus;
}

#[allow(dead_code)]
pub(crate) fn escape_command(command: &[String]) -> String {
    try_join(command.iter().map(|s| s.as_str())).unwrap_or_else(|_| command.join(" "))
}

#[allow(dead_code)]
pub(crate) fn format_file_change(change: &FileChange) -> &'static str {
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

pub(crate) fn create_config_summary_entries(config: &Config) -> Vec<(&'static str, String)> {
    let mut entries = vec![
        ("workdir", config.cwd.display().to_string()),
        ("model", config.model.clone()),
        ("provider", config.model_provider_id.clone()),
        ("approval", config.approval_policy.to_string()),
        ("sandbox", "disabled".to_string()),
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

    entries
}

pub(crate) fn handle_last_message(
    last_agent_message: Option<&str>,
    last_message_path: Option<&Path>,
) {
    match (last_message_path, last_agent_message) {
        (Some(path), Some(msg)) => write_last_message_file(msg, Some(path)),
        (Some(path), None) => {
            write_last_message_file("", Some(path));
            eprintln!(
                "Warning: no last agent message; wrote empty content to {}",
                path.display()
            );
        }
        (None, _) => {
            // This is not an error - just an optional feature
            // Users can use --output-last-message to save the response to a file
        },
    }
}

fn write_last_message_file(contents: &str, last_message_path: Option<&Path>) {
    if let Some(path) = last_message_path {
        if let Err(e) = std::fs::write(path, contents) {
            eprintln!("Failed to write last message file {path:?}: {e}");
        }
    }
}
