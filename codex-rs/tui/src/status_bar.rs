//! Modern status bar component for Claude Code-style UI
//!
//! Provides a clean status bar with session info, model details, and connection status

use crate::theme::Theme;
use ratatui::prelude::*;
use ratatui::widgets::Block;
use ratatui::widgets::BorderType;
use ratatui::widgets::Borders;
use ratatui::widgets::Paragraph;
use ratatui::widgets::Widget;
use ratatui::widgets::WidgetRef;

/// Modern status bar widget inspired by Claude Code
#[allow(dead_code)]
pub struct StatusBar {
    theme: Theme,
    model_name: String,
    session_status: SessionStatus,
    message_count: usize,
    show_details: bool,
}

#[derive(Clone, Copy)]
#[allow(dead_code)]
pub enum SessionStatus {
    Connected,
    Connecting,
    Disconnected,
    Error,
}

impl StatusBar {
    /// Create a new status bar with the given theme
    #[allow(dead_code)]
    pub fn new(theme: Theme) -> Self {
        Self {
            theme,
            model_name: "Claude".to_string(),
            session_status: SessionStatus::Connected,
            message_count: 0,
            show_details: false,
        }
    }

    /// Update the model name displayed in the status bar
    #[allow(dead_code)]
    pub fn set_model_name(&mut self, name: String) {
        self.model_name = name;
    }

    /// Update the session status
    #[allow(dead_code)]
    pub fn set_session_status(&mut self, status: SessionStatus) {
        self.session_status = status;
    }

    /// Update the message count
    #[allow(dead_code)]
    pub fn set_message_count(&mut self, count: usize) {
        self.message_count = count;
    }

    /// Toggle showing detailed information
    #[allow(dead_code)]
    pub fn toggle_details(&mut self) {
        self.show_details = !self.show_details;
    }

    /// Get the status indicator text and style
    fn status_indicator(&self) -> (String, Style) {
        match self.session_status {
            SessionStatus::Connected => ("●".to_string(), self.theme.success_indicator_style()),
            SessionStatus::Connecting => ("●".to_string(), self.theme.loading_style()),
            SessionStatus::Disconnected => (
                "●".to_string(),
                Style::default().fg(Color::Rgb(120, 120, 120)),
            ),
            SessionStatus::Error => (
                "●".to_string(),
                Style::default().fg(self.theme.status.error),
            ),
        }
    }

    /// Create the status text content
    fn create_status_text(&self) -> Text {
        let (indicator, indicator_style) = self.status_indicator();

        let status_text = match self.session_status {
            SessionStatus::Connected => "Connected",
            SessionStatus::Connecting => "Connecting...",
            SessionStatus::Disconnected => "Disconnected",
            SessionStatus::Error => "Error",
        };

        let mut lines = vec![];

        if self.show_details {
            // Detailed view with multiple lines - Claude Code style
            lines.push(Line::from(vec![
                Span::styled(indicator, indicator_style),
                Span::raw(" "),
                Span::styled(&self.model_name, self.theme.emphasis_style()),
                Span::styled(" • ", self.theme.separator_style()),
                Span::styled(status_text, self.theme.inactive_item_style()),
            ]));

            lines.push(Line::from(vec![
                Span::styled(
                    format!("{} messages", self.message_count),
                    self.theme.dim_style(),
                ),
                Span::styled(" • ", self.theme.separator_style()),
                Span::styled("F2 to collapse", self.theme.help_text_style()),
            ]));
        } else {
            // Compact view in a single line - Claude Code minimal style
            let left_part = vec![
                Span::styled(indicator, indicator_style),
                Span::raw(" "),
                Span::styled(&self.model_name, self.theme.emphasis_style()),
            ];

            let right_part = vec![
                Span::styled(
                    format!("{} msgs", self.message_count),
                    self.theme.dim_style(),
                ),
                Span::styled(" • ", self.theme.separator_style()),
                Span::styled(status_text, self.theme.inactive_item_style()),
                Span::styled(" • F2", self.theme.help_text_style()),
            ];

            // Combine with proper spacing
            let mut combined = left_part;
            combined.extend(right_part);
            lines.push(Line::from(combined));
        }

        Text::from(lines)
    }
}

impl Widget for StatusBar {
    fn render(self, area: Rect, buf: &mut Buffer) {
        (&self).render_ref(area, buf);
    }
}

impl WidgetRef for StatusBar {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        // Create a subtle border for the status bar
        let block = Block::default()
            .borders(Borders::TOP)
            .border_type(BorderType::Plain)
            .border_style(self.theme.separator_style());

        let inner = block.inner(area);
        block.render(area, buf);

        // Render the status text
        let status_text = self.create_status_text();
        let paragraph = Paragraph::new(status_text)
            .style(self.theme.inactive_item_style())
            .wrap(ratatui::widgets::Wrap { trim: true });

        paragraph.render(inner, buf);
    }
}
