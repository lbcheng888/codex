//! Types used to define the fields of [`crate::config::Config`].

// Note this file should generally be restricted to simple struct/enum
// definitions that do not contain business logic.

use std::collections::HashMap;
use strum_macros::Display;
use wildmatch::WildMatchPattern;

use serde::Deserialize;
use serde::Serialize;

#[derive(Deserialize, Debug, Clone, PartialEq)]
pub struct McpServerConfig {
    pub command: String,

    #[serde(default)]
    pub args: Vec<String>,

    #[serde(default)]
    pub env: Option<HashMap<String, String>>,
}

#[derive(Deserialize, Debug, Copy, Clone, PartialEq)]
pub enum UriBasedFileOpener {
    #[serde(rename = "vscode")]
    VsCode,

    #[serde(rename = "vscode-insiders")]
    VsCodeInsiders,

    #[serde(rename = "windsurf")]
    Windsurf,

    #[serde(rename = "cursor")]
    Cursor,

    /// Option to disable the URI-based file opener.
    #[serde(rename = "none")]
    None,
}

impl UriBasedFileOpener {
    pub fn get_scheme(&self) -> Option<&str> {
        match self {
            UriBasedFileOpener::VsCode => Some("vscode"),
            UriBasedFileOpener::VsCodeInsiders => Some("vscode-insiders"),
            UriBasedFileOpener::Windsurf => Some("windsurf"),
            UriBasedFileOpener::Cursor => Some("cursor"),
            UriBasedFileOpener::None => None,
        }
    }
}

/// Settings that govern if and what will be written to `~/.codex/history.jsonl`.
#[derive(Deserialize, Debug, Clone, PartialEq, Default)]
pub struct History {
    /// If true, history entries will not be written to disk.
    pub persistence: HistoryPersistence,

    /// If set, the maximum size of the history file in bytes.
    /// TODO(mbolin): Not currently honored.
    pub max_bytes: Option<usize>,
}

#[derive(Deserialize, Debug, Copy, Clone, PartialEq, Default)]
#[serde(rename_all = "kebab-case")]
pub enum HistoryPersistence {
    /// Save all history entries to disk.
    #[default]
    SaveAll,
    /// Do not write history to disk.
    None,
}

/// Collection of settings that are specific to the TUI.
#[derive(Deserialize, Debug, Clone, PartialEq)]
pub struct Tui {
    /// By default, mouse capture is enabled in the TUI so that it is possible
    /// to scroll the conversation history with a mouse. This comes at the cost
    /// of not being able to use the mouse to select text in the TUI.
    /// (Most terminals support a modifier key to allow this. For example,
    /// text selection works in iTerm if you hold down the `Option` key while
    /// clicking and dragging.)
    ///
    /// Setting this option to `true` disables mouse capture, so scrolling with
    /// the mouse is not possible, though the keyboard shortcuts e.g. `b` and
    /// `space` still work. This allows the user to select text in the TUI
    /// using the mouse without needing to hold down a modifier key.
    #[serde(default)]
    pub disable_mouse_capture: bool,

    /// By default the TUI switches the terminal into the so-called
    /// "alternate screen" buffer (via the ANSI **smcup** / **rmcup**
    /// capabilities).  While this provides a clean, full-screen
    /// interface, it has a number of downsides:
    ///
    ///   * The normal scroll-back buffer is hidden, meaning the user cannot
    ///     simply scroll up in their terminal to review previous output.
    ///   * Selecting text with the mouse for copy-and-paste is often
    ///     clunkier because the alternate screen captures the selection and
    ///     many terminals disable the native copy shortcut.
    ///
    /// Setting this flag to `true` prevents the TUI from entering the
    /// alternate screen, allowing the terminal to retain its native
    /// scroll-back buffer and selection behaviour.
    #[serde(default)]
    pub disable_alternate_screen: bool,
}

// Manual `Default` implementation so we can enable a sensible default for
// `disable_alternate_screen`.  Leaving the decision to the plain `derive`
// implementation would default the bool to `false`, which gives users the
// traditional full-screen *alternate* buffer.  That mode breaks native
// scroll-back and interferes with mouse-based text selection in most
// terminals – two behaviours that many people expect from a modern CLI.
//
// Switching the default to **true** keeps the UI functional while preserving
// the terminal emulator’s built-in scroll-back buffer and “click-and-drag to
// copy” interaction, matching the behaviour requested by users who prefer a
// more *Claude-style* experience.
impl Default for Tui {
    fn default() -> Self {
        // Use the platform's native scroll-back buffer and allow the user to
        // select/copy text with the mouse by default.  These two behaviours
        // require that we (a) avoid switching to the *alternate* screen and
        // (b) keep mouse capture disabled so the terminal emulator continues
        // to handle click-and-drag selection itself.

        Self {
            // Disabling mouse capture ensures that "click-and-drag" text
            // selection works out-of-the-box without the user needing to hold
            // down a modifier key (e.g. `Shift` in many terminals).
            disable_mouse_capture: true,

            // Render directly to the main screen so that the normal
            // scroll-back buffer remains available.
            disable_alternate_screen: true,
        }
    }
}


#[derive(Deserialize, Debug, Clone, PartialEq, Default)]
#[serde(rename_all = "kebab-case")]
pub enum ShellEnvironmentPolicyInherit {
    /// "Core" environment variables for the platform. On UNIX, this would
    /// include HOME, LOGNAME, PATH, SHELL, and USER, among others.
    #[default]
    Core,

    /// Inherits the full environment from the parent process.
    All,

    /// Do not inherit any environment variables from the parent process.
    None,
}

/// Policy for building the `env` when spawning a process via either the
/// `shell` or `local_shell` tool.
#[derive(Deserialize, Debug, Clone, PartialEq, Default)]
pub struct ShellEnvironmentPolicyToml {
    pub inherit: Option<ShellEnvironmentPolicyInherit>,

    pub ignore_default_excludes: Option<bool>,

    /// List of regular expressions.
    pub exclude: Option<Vec<String>>,

    pub r#set: Option<HashMap<String, String>>,

    /// List of regular expressions.
    pub include_only: Option<Vec<String>>,
}

pub type EnvironmentVariablePattern = WildMatchPattern<'*', '?'>;

/// Deriving the `env` based on this policy works as follows:
/// 1. Create an initial map based on the `inherit` policy.
/// 2. If `ignore_default_excludes` is false, filter the map using the default
///    exclude pattern(s), which are: `"*KEY*"` and `"*TOKEN*"`.
/// 3. If `exclude` is not empty, filter the map using the provided patterns.
/// 4. Insert any entries from `r#set` into the map.
/// 5. If non-empty, filter the map using the `include_only` patterns.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct ShellEnvironmentPolicy {
    /// Starting point when building the environment.
    pub inherit: ShellEnvironmentPolicyInherit,

    /// True to skip the check to exclude default environment variables that
    /// contain "KEY" or "TOKEN" in their name.
    pub ignore_default_excludes: bool,

    /// Environment variable names to exclude from the environment.
    pub exclude: Vec<EnvironmentVariablePattern>,

    /// (key, value) pairs to insert in the environment.
    pub r#set: HashMap<String, String>,

    /// Environment variable names to retain in the environment.
    pub include_only: Vec<EnvironmentVariablePattern>,
}

impl From<ShellEnvironmentPolicyToml> for ShellEnvironmentPolicy {
    fn from(toml: ShellEnvironmentPolicyToml) -> Self {
        let inherit = toml.inherit.unwrap_or(ShellEnvironmentPolicyInherit::Core);
        let ignore_default_excludes = toml.ignore_default_excludes.unwrap_or(false);
        let exclude = toml
            .exclude
            .unwrap_or_default()
            .into_iter()
            .map(|s| EnvironmentVariablePattern::new_case_insensitive(&s))
            .collect();
        let r#set = toml.r#set.unwrap_or_default();
        let include_only = toml
            .include_only
            .unwrap_or_default()
            .into_iter()
            .map(|s| EnvironmentVariablePattern::new_case_insensitive(&s))
            .collect();

        Self {
            inherit,
            ignore_default_excludes,
            exclude,
            r#set,
            include_only,
        }
    }
}

/// See https://platform.openai.com/docs/guides/reasoning?api-mode=responses#get-started-with-reasoning
#[derive(Debug, Serialize, Deserialize, Default, Clone, Copy, PartialEq, Eq, Display)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
pub enum ReasoningEffort {
    Low,
    #[default]
    Medium,
    High,
    /// Option to disable reasoning.
    None,
}

/// A summary of the reasoning performed by the model. This can be useful for
/// debugging and understanding the model's reasoning process.
/// See https://platform.openai.com/docs/guides/reasoning?api-mode=responses#reasoning-summaries
#[derive(Debug, Serialize, Deserialize, Default, Clone, Copy, PartialEq, Eq, Display)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
pub enum ReasoningSummary {
    #[default]
    Auto,
    Concise,
    Detailed,
    /// Option to disable reasoning summaries.
    None,
}
