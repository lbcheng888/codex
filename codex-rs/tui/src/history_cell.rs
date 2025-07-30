use crate::cell_widget::CellWidget;
use crate::exec_command::escape_command;
use crate::markdown::append_markdown;
use crate::text_block::TextBlock;
use crate::text_formatting::format_and_truncate_tool_result;
use crate::theme::Theme;
use base64::Engine;
use codex_ansi_escape::ansi_escape_line;
use codex_common::elapsed::format_duration;
use codex_core::WireApi;
use codex_core::config::Config;
use codex_core::model_supports_reasoning_summaries;
use codex_core::protocol::FileChange;
use codex_core::protocol::SessionConfiguredEvent;
use image::DynamicImage;
use image::GenericImageView;
use image::ImageReader;
use lazy_static::lazy_static;
use mcp_types::EmbeddedResourceResource;
use mcp_types::ResourceLink;
use ratatui::prelude::*;
use ratatui::style::Modifier;
use ratatui::style::Style;
use ratatui::text::Line as RtLine;
use ratatui::text::Span as RtSpan;
use ratatui_image::Image as TuiImage;
use ratatui_image::Resize as ImgResize;
use ratatui_image::picker::ProtocolType;
use std::collections::HashMap;
use std::io::Cursor;
use std::path::PathBuf;
use std::time::Duration;
use std::time::Instant;
use tracing::error;

pub(crate) struct CommandOutput {
    pub(crate) exit_code: i32,
    pub(crate) stdout: String,
    pub(crate) stderr: String,
    pub(crate) duration: Duration,
}

pub(crate) enum PatchEventType {
    ApprovalRequest,
    ApplyBegin { auto_approved: bool },
}

/// Represents an event to display in the conversation history. Returns its
/// `Vec<Line<'static>>` representation to make it easier to display in a
/// scrollable list.
pub(crate) enum HistoryCell {
    /// Welcome message.
    WelcomeMessage { view: TextBlock },

    /// Message from the user.
    UserPrompt { view: TextBlock },

    /// Message from the agent.
    AgentMessage { view: TextBlock },

    /// Reasoning event from the agent.
    AgentReasoning { view: TextBlock },

    /// An exec tool call that has not finished yet.
    ActiveExecCommand {
        call_id: String,
        /// The shell command, escaped and formatted.
        command: String,
        start: Instant,
        view: TextBlock,
    },

    /// Completed exec tool call.
    CompletedExecCommand { view: TextBlock },

    /// An MCP tool call that has not finished yet.
    ActiveMcpToolCall {
        call_id: String,
        /// Formatted line that shows the command name and arguments
        invocation: Line<'static>,
        start: Instant,
        view: TextBlock,
    },

    /// Completed MCP tool call where we show the result serialized as JSON.
    CompletedMcpToolCall { view: TextBlock },

    /// Completed MCP tool call where the result is an image.
    /// Admittedly, [mcp_types::CallToolResult] can have multiple content types,
    /// which could be a mix of text and images, so we need to tighten this up.
    // NOTE: For image output we keep the *original* image around and lazily
    // compute a resized copy that fits the available cell width.  Caching the
    // resized version avoids doing the potentially expensive rescale twice
    // because the scroll-view first calls `height()` for layouting and then
    // `render_window()` for painting.
    CompletedMcpToolCallWithImageOutput {
        image: DynamicImage,
        /// Cached data derived from the current terminal width.  The cache is
        /// invalidated whenever the width changes (e.g. when the user
        /// resizes the window).
        render_cache: std::cell::RefCell<Option<ImageRenderCache>>,
    },

    /// Background event.
    BackgroundEvent { view: TextBlock },

    /// Output from the `/diff` command.
    GitDiffOutput { view: TextBlock },

    /// Error event from the backend.
    ErrorEvent { view: TextBlock },

    /// Info describing the newly-initialized session.
    SessionInfo { view: TextBlock },

    /// A pending code patch that is awaiting user approval. Mirrors the
    /// behaviour of `ActiveExecCommand` so the user sees *what* patch the
    /// model wants to apply before being prompted to approve or deny it.
    PendingPatch { view: TextBlock },
}

const TOOL_CALL_MAX_LINES: usize = 5;

/// Force all spans in a line to use white foreground color for maximum readability
#[allow(dead_code)]
fn force_white_foreground(line: Line<'static>) -> Line<'static> {
    let white_spans: Vec<_> = line.spans.into_iter().map(|span| {
        // Preserve background and modifiers, but force white foreground
        let mut new_style = span.style;
        new_style.fg = Some(Color::Rgb(255, 255, 255));
        Span::styled(span.content, new_style)
    }).collect();
    Line::from(white_spans).style(line.style)
}

/// Create a modern message bubble with rounded corners and proper padding
fn create_message_bubble(
    theme: &Theme,
    header_text: String,
    content_lines: Vec<String>,
    is_user: bool,
) -> Vec<Line<'static>> {
    let mut lines: Vec<Line<'static>> = Vec::new();
    
    // Clean styling without borders
    let header_style = theme.message_header_style(is_user);
    // Use pure terminal white to ensure maximum brightness even on 16-color
    // terminals where custom RGB values are approximated to dull gray.
    let content_style = Style::default().fg(Color::White);
    
    // Simple header line
    lines.push(Line::from(vec![
        Span::styled(header_text, header_style),
    ]));
    
    // Content lines without borders
    for line in content_lines {
        lines.push(Line::from(vec![
            Span::styled(line, content_style),
        ]));
    }
    
    // Add spacing after message
    lines.push(Line::from(""));
    
    lines
}

/// Create a modern message bubble for agent messages with markdown support
fn create_agent_message_bubble(
    theme: &Theme,
    header_text: String,
    message: &str,
    config: &Config,
) -> Vec<Line<'static>> {
    let mut lines: Vec<Line<'static>> = Vec::new();
    
    let header_style = theme.message_header_style(false); // Claude's orange for assistant messages
    
    // Process markdown content first
    let mut content_lines: Vec<Line<'static>> = Vec::new();
    append_markdown(message, &mut content_lines, config);
    
    // Simple header line
    lines.push(Line::from(vec![
        Span::styled(header_text, header_style),
    ]));
    
    // Content lines without borders
    for line in content_lines {
        // Use bright white for content with proper markdown styling
        let bright_spans: Vec<_> = line.spans.into_iter().map(|span| {
            let mut new_style = span.style;
            // Use bright white for text content, preserve other styling
            if new_style.fg.is_none() || 
               (new_style.fg == Some(Color::Rgb(255, 255, 255))) {
                new_style.fg = Some(Color::White);
            }
            Span::styled(span.content, new_style)
        }).collect();
        lines.push(Line::from(bright_spans));
    }
    
    // Add spacing after message
    lines.push(Line::from(""));
    
    lines
}

/// Create a special reasoning message bubble with distinctive styling
fn create_reasoning_message_bubble(
    theme: &Theme,
    header_text: String,
    text: &str,
    config: &Config,
) -> Vec<Line<'static>> {
    let mut lines: Vec<Line<'static>> = Vec::new();
    
    // Clean styling for reasoning content
    let header_style = Style::default()
        .fg(Color::Rgb(180, 180, 200))  // Subtle gray-blue for thinking header
        .add_modifier(Modifier::ITALIC); // Italic thinking header
    
    // Process markdown content first
    let mut content_lines: Vec<Line<'static>> = Vec::new();
    append_markdown(text, &mut content_lines, config);
    
    // Simple header line
    lines.push(Line::from(vec![
        Span::styled(header_text, header_style),
    ]));
    
    // Content lines without borders
    for line in content_lines {
        let bright_spans: Vec<Span> = line
            .spans
            .into_iter()
            .map(|span| {
                let mut new_style = span.style;
                // Use bright white for content, preserve other styling
                if new_style.fg.is_none() || 
                   (new_style.fg == Some(Color::Rgb(255, 255, 255))) {
                new_style.fg = Some(Color::White);
                }
                Span::styled(span.content, new_style)
            })
            .collect();
        lines.push(Line::from(bright_spans));
    }
    
    // Add spacing after message
    lines.push(Line::from(""));
    
    lines
}

impl HistoryCell {
    pub(crate) fn new_session_info(
        config: &Config,
        event: SessionConfiguredEvent,
        is_first_event: bool,
    ) -> Self {
        let theme = Theme::default();
        let SessionConfiguredEvent {
            model,
            session_id,
            history_log_id: _,
            history_entry_count: _,
        } = event;
        if is_first_event {
            const VERSION: &str = env!("CARGO_PKG_VERSION");

            let mut lines: Vec<Line<'static>> = vec![
                Line::from(vec![
                    Span::raw("OpenAI "),
                    Span::styled("Codex", Style::default().add_modifier(Modifier::BOLD)),
                    Span::raw(format!(" v{VERSION}")),
                    Span::styled(" (research preview)", theme.dim_style()),
                ]),
                Line::from(""),
                Line::from(vec![
                    Span::styled("codex session", theme.emphasis_style().fg(theme.status.info)),
                    Span::raw(" "),
                    Span::styled(session_id.to_string(), theme.dim_style()),
                ]),
            ];

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
                lines.push(Line::from(vec![
                    Span::styled(format!("{key}: "), theme.emphasis_style()),
                    Span::raw(value),
                ]));
            }
            lines.push(Line::from(""));
            HistoryCell::WelcomeMessage {
                view: TextBlock::new(lines),
            }
        } else if config.model == model {
            HistoryCell::SessionInfo {
                view: TextBlock::new(Vec::new()),
            }
        } else {
            let lines = vec![
                Line::styled("model changed:", theme.emphasis_style().fg(theme.status.info)),
                Line::from(format!("requested: {}", config.model)),
                Line::from(format!("used: {model}")),
                Line::from(""),
            ];
            HistoryCell::SessionInfo {
                view: TextBlock::new(lines),
            }
        }
    }

    pub(crate) fn new_user_prompt(message: String) -> Self {
        use ratatui::text::{Line as RtLine, Span as RtSpan};

        let theme = Theme::default();

        // --- Parse the message as Markdown so users can leverage rich formatting ---
        // We intentionally do *not* attempt to rewrite file citations (the `【F:`
        // syntax) for user-authored prompts because keeping the raw text makes
        // it easier to copy/paste.  Therefore we can bypass the helper that
        // requires a `Config` instance and directly feed the string to
        // `tui_markdown`.

        let mut markdown_lines: Vec<RtLine<'static>> = Vec::new();

        // Render markdown using `tui_markdown` then clone the borrowed spans so
        // they can live for the full duration of the `HistoryCell`.
        let rendered = tui_markdown::from_str(&message);

        for borrowed_line in rendered.lines {
            let mut owned_spans = Vec::with_capacity(borrowed_line.spans.len());
            for span in &borrowed_line.spans {
                // Force bright white foreground for maximum readability
                let mut style = span.style;
                style.fg = Some(Color::White);
                owned_spans.push(RtSpan::styled(span.content.to_string(), style));
            }

            let mut owned_line: RtLine<'static> = RtLine::from(owned_spans).style(borrowed_line.style);
            owned_line.style.fg = Some(Color::White);
            let owned_line = match borrowed_line.alignment {
                Some(alignment) => owned_line.alignment(alignment),
                None => owned_line,
            };

            markdown_lines.push(owned_line);
        }

        // Build the bubble with a coloured header followed by the rendered
        // markdown lines.
        let header_style = theme.message_header_style(true);

        let mut lines: Vec<RtLine<'static>> = Vec::new();
        // Header line (e.g. "user")
        lines.push(RtLine::from(vec![RtSpan::styled("user", header_style)]));
        // Content lines.
        lines.extend(markdown_lines);
        // Trailing blank line to add spacing after the bubble.
        lines.push(RtLine::from(""));

        HistoryCell::UserPrompt {
            view: TextBlock::new(lines),
        }
    }

    pub(crate) fn new_agent_message(config: &Config, message: String) -> Self {
        let theme = Theme::default();
        
        // For assistant messages, we need to handle markdown specially
        // Create a modern bubble with markdown content
        let mut lines = create_agent_message_bubble(&theme, "codex".to_string(), &message, config);
        
        // Ensure all text is white
        for line in &mut lines {
            for span in &mut line.spans {
                span.style.fg = Some(Color::Rgb(245, 245, 245));
            }
        }

        HistoryCell::AgentMessage {
            view: TextBlock::new(lines),
        }
    }

    pub(crate) fn new_agent_reasoning(config: &Config, text: String) -> Self {
        let theme = Theme::default();
        
        // Create a special reasoning bubble with distinctive styling
        let mut lines = create_reasoning_message_bubble(&theme, "thinking".to_string(), &text, config);
        
        // Ensure all text is white
        for line in &mut lines {
            for span in &mut line.spans {
                span.style.fg = Some(Color::Rgb(245, 245, 245));
            }
        }

        HistoryCell::AgentReasoning {
            view: TextBlock::new(lines),
        }
    }

    pub(crate) fn new_active_exec_command(call_id: String, command: Vec<String>) -> Self {
        let command_escaped = escape_command(&command);
        let start = Instant::now();

        let theme = Theme::default();
        let lines: Vec<Line<'static>> = vec![
            Line::from(vec![
                Span::styled("command", Style::default().fg(theme.status.info)),
                Span::styled(" running...", theme.dim_style()),
            ]),
            Line::from(format!("$ {command_escaped}")),
            Line::from(""),
        ];

        HistoryCell::ActiveExecCommand {
            call_id,
            command: command_escaped,
            start,
            view: TextBlock::new(lines),
        }
    }

    pub(crate) fn new_completed_exec_command(command: String, output: CommandOutput) -> Self {
        let CommandOutput {
            exit_code,
            stdout,
            stderr,
            duration,
        } = output;

        let mut lines: Vec<Line<'static>> = Vec::new();

        let theme = Theme::default();
        // Title depends on whether we have output yet.
        let title_line = Line::from(vec![
            Span::styled("command", Style::default().fg(theme.status.info)),
            Span::styled(
                format!(
                    " (code: {}, duration: {})",
                    exit_code,
                    format_duration(duration)
                ),
                theme.dim_style(),
            ),
        ]);
        lines.push(title_line);

        let src = if exit_code == 0 { stdout } else { stderr };

        // Add command with better styling
        lines.push(Line::from(vec![
            Span::styled("$ ", theme.code_style()),
            Span::styled(command, theme.code_style().add_modifier(Modifier::BOLD)),
        ]));

        // Add output with proper formatting
        if !src.is_empty() {
            let mut lines_iter = src.lines();
            let mut _line_count = 0;

            for raw in lines_iter.by_ref().take(TOOL_CALL_MAX_LINES) {
                _line_count += 1;
                let styled_line = if exit_code == 0 {
                    ansi_escape_line(raw).fg(theme.status.success)
                } else {
                    ansi_escape_line(raw).fg(theme.status.error)
                };
                lines.push(styled_line);
            }

            let remaining = lines_iter.count();
            if remaining > 0 {
                lines.push(Line::from(vec![
                    Span::styled("... ", theme.dim_style()),
                    Span::styled(format!("{remaining} additional lines"), theme.dim_style()),
                ]));
            }
        }
        lines.push(Line::from(""));

        HistoryCell::CompletedExecCommand {
            view: TextBlock::new(lines),
        }
    }

    pub(crate) fn new_active_mcp_tool_call(
        call_id: String,
        server: String,
        tool: String,
        arguments: Option<serde_json::Value>,
    ) -> Self {
        // Format the arguments as compact JSON so they roughly fit on one
        // line. If there are no arguments we keep it empty so the invocation
        // mirrors a function-style call.
        let args_str = arguments
            .as_ref()
            .map(|v| {
                // Use compact form to keep things short but readable.
                serde_json::to_string(v).unwrap_or_else(|_| v.to_string())
            })
            .unwrap_or_default();

        let theme = Theme::default();
        let invocation_spans = vec![
            Span::styled(server, Style::default().fg(theme.ui.selection)),
            Span::raw("."),
            Span::styled(tool, Style::default().fg(theme.ui.selection)),
            Span::raw("("),
            Span::styled(args_str, theme.dim_style()),
            Span::raw(")"),
        ];
        let invocation = Line::from(invocation_spans);

        let start = Instant::now();
        let title_line = Line::from(vec![
            Span::styled("tool", Style::default().fg(theme.status.info)),
            Span::styled(" running...", theme.dim_style()),
        ]);
        let lines: Vec<Line<'static>> = vec![title_line, invocation.clone(), Line::from("")];

        HistoryCell::ActiveMcpToolCall {
            call_id,
            invocation,
            start,
            view: TextBlock::new(lines),
        }
    }

    /// If the first content is an image, return a new cell with the image.
    /// TODO(rgwood-dd): Handle images properly even if they're not the first result.
    fn try_new_completed_mcp_tool_call_with_image_output(
        result: &Result<mcp_types::CallToolResult, String>,
    ) -> Option<Self> {
        match result {
            Ok(mcp_types::CallToolResult { content, .. }) => {
                if let Some(mcp_types::ContentBlock::ImageContent(image)) = content.first() {
                    let raw_data =
                        match base64::engine::general_purpose::STANDARD.decode(&image.data) {
                            Ok(data) => data,
                            Err(e) => {
                                error!("Failed to decode image data: {e}");
                                return None;
                            }
                        };
                    let reader = match ImageReader::new(Cursor::new(raw_data)).with_guessed_format()
                    {
                        Ok(reader) => reader,
                        Err(e) => {
                            error!("Failed to guess image format: {e}");
                            return None;
                        }
                    };

                    let image = match reader.decode() {
                        Ok(image) => image,
                        Err(e) => {
                            error!("Image decoding failed: {e}");
                            return None;
                        }
                    };

                    Some(HistoryCell::CompletedMcpToolCallWithImageOutput {
                        image,
                        render_cache: std::cell::RefCell::new(None),
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub(crate) fn new_completed_mcp_tool_call(
        num_cols: u16,
        invocation: Line<'static>,
        start: Instant,
        success: bool,
        result: Result<mcp_types::CallToolResult, String>,
    ) -> Self {
        if let Some(cell) = Self::try_new_completed_mcp_tool_call_with_image_output(&result) {
            return cell;
        }

        let theme = Theme::default();
        let duration = format_duration(start.elapsed());
        let status_str = if success { "success" } else { "failed" };
        let title_line = Line::from(vec![
            Span::styled("tool", Style::default().fg(theme.status.info)),
            Span::raw(" "),
            if success {
                Span::styled(status_str, Style::default().fg(theme.status.success))
            } else {
                Span::styled(status_str, Style::default().fg(theme.status.error))
            },
            Span::styled(format!(", duration: {duration}"), theme.dim_style()),
        ]);

        let mut lines: Vec<Line<'static>> = Vec::new();
        lines.push(title_line);
        lines.push(invocation);

        match result {
            Ok(mcp_types::CallToolResult { content, .. }) => {
                if !content.is_empty() {
                    lines.push(Line::from(""));

                    for tool_call_result in content {
                        let line_text = match tool_call_result {
                            mcp_types::ContentBlock::TextContent(text) => {
                                format_and_truncate_tool_result(
                                    &text.text,
                                    TOOL_CALL_MAX_LINES,
                                    num_cols as usize,
                                )
                            }
                            mcp_types::ContentBlock::ImageContent(_) => {
                                // TODO show images even if they're not the first result, will require a refactor of `CompletedMcpToolCall`
                                "<image content>".to_string()
                            }
                            mcp_types::ContentBlock::AudioContent(_) => {
                                "<audio content>".to_string()
                            }
                            mcp_types::ContentBlock::EmbeddedResource(resource) => {
                                let uri = match resource.resource {
                                    EmbeddedResourceResource::TextResourceContents(text) => {
                                        text.uri
                                    }
                                    EmbeddedResourceResource::BlobResourceContents(blob) => {
                                        blob.uri
                                    }
                                };
                                format!("embedded resource: {uri}")
                            }
                            mcp_types::ContentBlock::ResourceLink(ResourceLink { uri, .. }) => {
                                format!("link: {uri}")
                            }
                        };
                        lines.push(Line::styled(line_text, theme.dim_style()));
                    }
                }

                lines.push(Line::from(""));
            }
            Err(e) => {
                lines.push(Line::from(vec![
                    Span::styled(
                        "Error: ",
                        theme.emphasis_style().fg(theme.status.error),
                    ),
                    Span::raw(e),
                ]));
            }
        };

        HistoryCell::CompletedMcpToolCall {
            view: TextBlock::new(lines),
        }
    }

    /// Create a background event entry. The `message` supports basic markdown
    /// styling so that links and inline formatting render consistently with
    /// normal user/assistant messages. We intentionally keep the rendering
    /// logic **self-contained** to avoid requiring a `Config` instance (which
    /// is not available at the call-sites generating background events).
    pub(crate) fn new_background_event(message: String) -> Self {
        use ratatui::text::{Line as RtLine, Span as RtSpan};

        let mut lines: Vec<RtLine<'static>> = Vec::new();

        // First line – small "event" header so users can visually distinguish
        // background information from regular assistant responses.
        lines.push(RtLine::styled("event", Style::default().fg(Color::Rgb(245, 245, 245))));

        // Render the *content* using the same markdown renderer employed for
        // user & assistant messages.  We bypass citation-rewriting because the
        // helper requires the full `Config`; background events rarely contain
        // the specialised citation syntax and, even when they do, displaying
        // the raw text is acceptable.
        let rendered = tui_markdown::from_str(&message);

        for borrowed_line in rendered.lines {
            let owned_spans: Vec<RtSpan<'static>> = borrowed_line
                .spans
                .iter()
                .map(|span| {
                    // Clone content and force bright-white foreground for
                    // maximum readability inside the dark TUI theme.
                    let mut style = span.style;
                    style.fg = Some(Color::White);
                    RtSpan::styled(span.content.to_string(), style)
                })
                .collect();

            let mut owned_line: RtLine<'static> = RtLine::from(owned_spans).style(borrowed_line.style);
            owned_line.style.fg = Some(Color::White);
            let owned_line = match borrowed_line.alignment {
                Some(alignment) => owned_line.alignment(alignment),
                None => owned_line,
            };

            lines.push(owned_line);
        }

        // Blank spacer after the message bubble.
        lines.push(RtLine::from(""));

        HistoryCell::BackgroundEvent {
            view: TextBlock::new(lines),
        }
    }

    pub(crate) fn new_diff_output(message: String) -> Self {
        let theme = Theme::default();
        let mut lines: Vec<Line<'static>> = Vec::new();
        lines.push(Line::styled("/diff", Style::default().fg(theme.status.info)));

        if message.trim().is_empty() {
            lines.push(Line::styled("No changes detected.", theme.dim_style().add_modifier(Modifier::ITALIC)));
        } else {
            lines.extend(message.lines().map(ansi_escape_line));
        }

        lines.push(Line::from(""));
        HistoryCell::GitDiffOutput {
            view: TextBlock::new(lines),
        }
    }

    pub(crate) fn new_error_event(message: String) -> Self {
        let theme = Theme::default();
        let lines: Vec<Line<'static>> = vec![
            Line::from(vec![
                Span::styled("ERROR: ", theme.emphasis_style().fg(theme.status.error)),
                Span::styled(message, Style::default().fg(Color::Rgb(245, 245, 245))),
            ]),
            Line::from(""),
        ];
        HistoryCell::ErrorEvent {
            view: TextBlock::new(lines),
        }
    }

    /// Create a new `PendingPatch` cell that lists the file‑level summary of
    /// a proposed patch. The summary lines should already be formatted (e.g.
    /// "A path/to/file.rs").
    pub(crate) fn new_patch_event(
        event_type: PatchEventType,
        changes: HashMap<PathBuf, FileChange>,
    ) -> Self {
        let title = match event_type {
            PatchEventType::ApprovalRequest => "proposed patch",
            PatchEventType::ApplyBegin {
                auto_approved: true,
            } => "applying patch",
            PatchEventType::ApplyBegin {
                auto_approved: false,
            } => {
                let theme = Theme::default();
                let lines = vec![Line::styled("patch applied", theme.emphasis_style().fg(theme.status.success))];
                return Self::PendingPatch {
                    view: TextBlock::new(lines),
                };
            }
        };

        let summary_lines = create_diff_summary(changes);

        let theme = Theme::default();
        let mut lines: Vec<Line<'static>> = Vec::new();

        // Header similar to the command formatter so patches are visually
        // distinct while still fitting the overall colour scheme.
        lines.push(Line::styled(title, theme.emphasis_style().fg(theme.status.info)));

        for line in summary_lines {
            if line.starts_with('+') {
                lines.push(Line::styled(line, Style::default().fg(theme.status.success)));
            } else if line.starts_with('-') {
                lines.push(Line::styled(line, Style::default().fg(theme.status.error)));
            } else if let Some(space_idx) = line.find(' ') {
                let kind_owned = line[..space_idx].to_string();
                let rest_owned = line[space_idx + 1..].to_string();

                let theme = Theme::default();

                let styled_kind = match kind_owned.as_str() {
                    "A" => RtSpan::styled(kind_owned.clone(), theme.emphasis_style().fg(theme.status.success)),
                    "D" => RtSpan::styled(kind_owned.clone(), theme.emphasis_style().fg(theme.status.error)),
                    "M" => RtSpan::styled(kind_owned.clone(), theme.emphasis_style().fg(theme.status.warning)),
                    "R" | "C" => RtSpan::styled(kind_owned.clone(), theme.emphasis_style().fg(theme.status.info)),
                    _ => RtSpan::raw(kind_owned.clone()),
                };

                let styled_line =
                    RtLine::from(vec![styled_kind, RtSpan::raw(" "), RtSpan::raw(rest_owned)]);
                lines.push(styled_line);
            } else {
                lines.push(Line::from(line));
            }
        }

        lines.push(Line::from(""));

        HistoryCell::PendingPatch {
            view: TextBlock::new(lines),
        }
    }
}

// ---------------------------------------------------------------------------
// `CellWidget` implementation – most variants delegate to their internal
// `TextBlock`.  Variants that need custom painting can add their own logic in
// the match arms.
// ---------------------------------------------------------------------------

impl CellWidget for HistoryCell {
    fn height(&self, width: u16) -> usize {
        match self {
            HistoryCell::WelcomeMessage { view }
            | HistoryCell::UserPrompt { view }
            | HistoryCell::AgentMessage { view }
            | HistoryCell::AgentReasoning { view }
            | HistoryCell::BackgroundEvent { view }
            | HistoryCell::GitDiffOutput { view }
            | HistoryCell::ErrorEvent { view }
            | HistoryCell::SessionInfo { view }
            | HistoryCell::CompletedExecCommand { view }
            | HistoryCell::CompletedMcpToolCall { view }
            | HistoryCell::PendingPatch { view }
            | HistoryCell::ActiveExecCommand { view, .. }
            | HistoryCell::ActiveMcpToolCall { view, .. } => view.height(width),
            HistoryCell::CompletedMcpToolCallWithImageOutput {
                image,
                render_cache,
            } => ensure_image_cache(image, width, render_cache),
        }
    }

    fn render_window(&self, first_visible_line: usize, area: Rect, buf: &mut Buffer) {
        match self {
            HistoryCell::WelcomeMessage { view }
            | HistoryCell::UserPrompt { view }
            | HistoryCell::AgentMessage { view }
            | HistoryCell::AgentReasoning { view }
            | HistoryCell::BackgroundEvent { view }
            | HistoryCell::GitDiffOutput { view }
            | HistoryCell::ErrorEvent { view }
            | HistoryCell::SessionInfo { view }
            | HistoryCell::CompletedExecCommand { view }
            | HistoryCell::CompletedMcpToolCall { view }
            | HistoryCell::PendingPatch { view }
            | HistoryCell::ActiveExecCommand { view, .. }
            | HistoryCell::ActiveMcpToolCall { view, .. } => {
                view.render_window(first_visible_line, area, buf)
            }
            HistoryCell::CompletedMcpToolCallWithImageOutput {
                image,
                render_cache,
            } => {
                // Ensure we have a cached, resized copy that matches the current width.
                // `height()` should have prepared the cache, but if something invalidated it
                // (e.g. the first `render_window()` call happens *before* `height()` after a
                // resize) we rebuild it here.

                let width_cells = area.width;

                // Ensure the cache is up-to-date and extract the scaled image.
                let _ = ensure_image_cache(image, width_cells, render_cache);

                let Some(resized) = render_cache
                    .borrow()
                    .as_ref()
                    .map(|c| c.scaled_image.clone())
                else {
                    return;
                };

                let picker = &*TERMINAL_PICKER;

                if let Ok(protocol) = picker.new_protocol(resized, area, ImgResize::Fit(None)) {
                    let img_widget = TuiImage::new(&protocol);
                    img_widget.render(area, buf);
                }
            }
        }
    }
}

fn create_diff_summary(changes: HashMap<PathBuf, FileChange>) -> Vec<String> {
    // Build a concise, human‑readable summary list similar to the
    // `git status` short format so the user can reason about the
    // patch without scrolling.
    let mut summaries: Vec<String> = Vec::new();
    for (path, change) in &changes {
        use codex_core::protocol::FileChange::*;
        match change {
            Add { content } => {
                let added = content.lines().count();
                summaries.push(format!("A {} (+{added})", path.display()));
            }
            Delete => {
                summaries.push(format!("D {}", path.display()));
            }
            Update {
                unified_diff,
                move_path,
            } => {
                if let Some(new_path) = move_path {
                    summaries.push(format!("R {} → {}", path.display(), new_path.display(),));
                } else {
                    summaries.push(format!("M {}", path.display(),));
                }
                summaries.extend(unified_diff.lines().map(|s| s.to_string()));
            }
        }
    }

    summaries
}

// -------------------------------------
// Helper types for image rendering
// -------------------------------------

/// Cached information for rendering an image inside a conversation cell.
///
/// The cache ties the resized image to a *specific* content width (in
/// terminal cells).  Whenever the terminal is resized and the width changes
/// we need to re-compute the scaled variant so that it still fits the
/// available space.  Keeping the resized copy around saves a costly rescale
/// between the back-to-back `height()` and `render_window()` calls that the
/// scroll-view performs while laying out the UI.
pub(crate) struct ImageRenderCache {
    /// Width in *terminal cells* the cached image was generated for.
    width_cells: u16,
    /// Height in *terminal rows* that the conversation cell must occupy so
    /// the whole image becomes visible.
    height_rows: usize,
    /// The resized image that fits the given width / height constraints.
    scaled_image: DynamicImage,
}

lazy_static! {
    static ref TERMINAL_PICKER: ratatui_image::picker::Picker = {
        use ratatui_image::picker::Picker;
        use ratatui_image::picker::cap_parser::QueryStdioOptions;

        // Ask the terminal for capabilities and explicit font size.  Request the
        // Kitty *text-sizing protocol* as a fallback mechanism for terminals
        // (like iTerm2) that do not reply to the standard CSI 16/18 queries.
        match Picker::from_query_stdio_with_options(QueryStdioOptions {
            text_sizing_protocol: true,
        }) {
            Ok(picker) => picker,
            Err(err) => {
                // Fall back to the conservative default that assumes ~8×16 px cells.
                // Still better than breaking the build in a headless test run.
                tracing::warn!("terminal capability query failed: {err:?}; using default font size");
                Picker::from_fontsize((8, 16))
            }
        }
    };
}

/// Resize `image` to fit into `width_cells`×10-rows keeping the original aspect
/// ratio. The function updates `render_cache` and returns the number of rows
/// (<= 10) the picture will occupy.
fn ensure_image_cache(
    image: &DynamicImage,
    width_cells: u16,
    render_cache: &std::cell::RefCell<Option<ImageRenderCache>>,
) -> usize {
    if let Some(cache) = render_cache.borrow().as_ref() {
        if cache.width_cells == width_cells {
            return cache.height_rows;
        }
    }

    let picker = &*TERMINAL_PICKER;
    let (char_w_px, char_h_px) = picker.font_size();

    // Heuristic to compensate for Hi-DPI terminals (iTerm2 on Retina Mac) that
    // report logical pixels (≈ 8×16) while the iTerm2 graphics protocol
    // expects *device* pixels.  Empirically the device-pixel-ratio is almost
    // always 2 on macOS Retina panels.
    let hidpi_scale = if picker.protocol_type() == ProtocolType::Iterm2 {
        2.0f64
    } else {
        1.0
    };

    // The fallback Halfblocks protocol encodes two pixel rows per cell, so each
    // terminal *row* represents only half the (possibly scaled) font height.
    let effective_char_h_px: f64 = if picker.protocol_type() == ProtocolType::Halfblocks {
        (char_h_px as f64) * hidpi_scale / 2.0
    } else {
        (char_h_px as f64) * hidpi_scale
    };

    let char_w_px_f64 = (char_w_px as f64) * hidpi_scale;

    const MAX_ROWS: f64 = 10.0;
    let max_height_px: f64 = effective_char_h_px * MAX_ROWS;

    let (orig_w_px, orig_h_px) = {
        let (w, h) = image.dimensions();
        (w as f64, h as f64)
    };

    if orig_w_px == 0.0 || orig_h_px == 0.0 || width_cells == 0 {
        *render_cache.borrow_mut() = None;
        return 0;
    }

    let max_w_px = char_w_px_f64 * width_cells as f64;
    let scale_w = max_w_px / orig_w_px;
    let scale_h = max_height_px / orig_h_px;
    let scale = scale_w.min(scale_h).min(1.0);

    use image::imageops::FilterType;
    let scaled_w_px = (orig_w_px * scale).round().max(1.0) as u32;
    let scaled_h_px = (orig_h_px * scale).round().max(1.0) as u32;

    let scaled_image = image.resize(scaled_w_px, scaled_h_px, FilterType::Lanczos3);

    let height_rows = ((scaled_h_px as f64 / effective_char_h_px).ceil()) as usize;

    let new_cache = ImageRenderCache {
        width_cells,
        height_rows,
        scaled_image,
    };
    *render_cache.borrow_mut() = Some(new_cache);

    height_rows
}
