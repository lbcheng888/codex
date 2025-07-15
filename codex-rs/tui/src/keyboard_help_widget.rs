//! Keyboard shortcuts help overlay widget
//! Displays all available keyboard shortcuts in a categorized format

use ratatui::buffer::Buffer;
use ratatui::layout::{Alignment, Rect};
use ratatui::style::Style;
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, BorderType, Borders, Clear, Paragraph, Widget, WidgetRef};

use crate::theme::Theme;

/// Widget that displays a comprehensive keyboard shortcuts help overlay
pub struct KeyboardHelpWidget {
    theme: Theme,
}

impl KeyboardHelpWidget {
    pub fn new() -> Self {
        Self {
            theme: Theme::default(),
        }
    }

    /// Create help content with all available shortcuts organized by category
    fn create_help_content(&self) -> Vec<Line<'static>> {
        let mut lines = vec![];

        // Title
        lines.push(Line::from(vec![
            Span::styled(
                "Keyboard Shortcuts",
                self.theme.help_section_header_style(),
            ),
        ]));
        lines.push(Line::from(""));

        // Navigation category
        lines.push(Line::from(vec![
            Span::styled("Navigation", self.theme.help_category_style()),
        ]));
        lines.push(self.create_shortcut_line("Tab", "Switch between history ⇄ input panes"));
        lines.push(self.create_shortcut_line("Esc", "Reset focus to input pane"));
        lines.push(self.create_shortcut_line("↑/↓ or j/k", "Scroll conversation history"));
        lines.push(self.create_shortcut_line("PageUp/Down", "Scroll by page"));
        lines.push(Line::from(""));

        // Input category
        lines.push(Line::from(vec![
            Span::styled("Input & Editing", self.theme.help_category_style()),
        ]));
        lines.push(self.create_shortcut_line("Enter", "Send message"));
        lines.push(self.create_shortcut_line("Ctrl+J/Shift+Enter", "Insert newline"));
        lines.push(self.create_shortcut_line("↑/↓", "Navigate message history"));
        lines.push(self.create_shortcut_line("Ctrl+C", "Copy selected text / Interrupt task"));
        lines.push(Line::from(""));
        
        // Selection category
        lines.push(Line::from(vec![
            Span::styled("Text Selection (History Pane)", self.theme.help_category_style()),
        ]));
        lines.push(self.create_shortcut_line("Shift+↑/↓/←/→", "Select text"));
        lines.push(self.create_shortcut_line("Ctrl+A", "Select all"));
        lines.push(self.create_shortcut_line("Ctrl+C", "Copy selected text"));
        lines.push(Line::from(""));

        // Commands category
        lines.push(Line::from(vec![
            Span::styled("Commands & Actions", self.theme.help_category_style()),
        ]));
        lines.push(self.create_shortcut_line("/", "Start slash command"));
        lines.push(self.create_shortcut_line("@", "Reference files"));
        lines.push(self.create_shortcut_line("/diff", "Show git changes"));
        lines.push(self.create_shortcut_line("/new", "Start new conversation"));
        lines.push(Line::from(""));

        // File operations
        lines.push(Line::from(vec![
            Span::styled("File Operations", self.theme.help_category_style()),
        ]));
        lines.push(self.create_shortcut_line("@filename", "Reference specific file"));
        lines.push(self.create_shortcut_line("Tab", "Autocomplete suggestions"));
        lines.push(self.create_shortcut_line("Esc", "Cancel operation"));
        lines.push(Line::from(""));

        // System category
        lines.push(Line::from(vec![
            Span::styled("System", self.theme.help_category_style()),
        ]));
        lines.push(self.create_shortcut_line("F1/?", "Toggle this help"));
        lines.push(self.create_shortcut_line("Ctrl+D", "Quit application"));
        lines.push(self.create_shortcut_line("F2", "Toggle status details"));
        lines.push(Line::from(""));

        // Contextual tips
        lines.push(Line::from(vec![
            Span::styled("Pro Tips", self.theme.help_category_style()),
        ]));
        lines.push(Line::from(vec![
            Span::raw("  • Use "),
            Span::styled("Ctrl+K", self.theme.shortcut_key_style()),
            Span::raw(" to clear input"),
        ]));
        lines.push(Line::from(vec![
            Span::raw("  • Press "),
            Span::styled("Tab", self.theme.shortcut_key_style()),
            Span::raw(" twice for more completions"),
        ]));
        lines.push(Line::from(""));

        // Footer
        lines.push(Line::from(vec![
            Span::styled("Press ", self.theme.help_text_style()),
            Span::styled("F1", self.theme.shortcut_key_style()),
            Span::styled(" or ", self.theme.help_text_style()),
            Span::styled("?", self.theme.shortcut_key_style()),
            Span::styled(" to close this help", self.theme.help_text_style()),
        ]));

        lines
    }

    /// Create a formatted line for a keyboard shortcut
    fn create_shortcut_line(&self, key: &str, description: &str) -> Line<'static> {
        Line::from(vec![
            Span::styled("  ", Style::default()), // Indentation
            Span::styled(format!("{:12}", key), self.theme.shortcut_key_style()),
            Span::styled(description.to_string(), self.theme.help_text_style()),
        ])
    }
}

impl WidgetRef for KeyboardHelpWidget {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        // Clear the area first
        Clear.render(area, buf);

        // Calculate centered position for the help overlay
        let help_width = (area.width * 3 / 4).min(80); // 75% of screen width, max 80 chars
        let help_height = (area.height * 3 / 4).min(25); // 75% of screen height, max 25 lines
        
        let horizontal_margin = (area.width.saturating_sub(help_width)) / 2;
        let vertical_margin = (area.height.saturating_sub(help_height)) / 2;

        let help_area = Rect {
            x: area.x + horizontal_margin,
            y: area.y + vertical_margin,
            width: help_width,
            height: help_height,
        };

        // Create the help content
        let help_content = self.create_help_content();
        
        // Create the help widget
        let help_widget = Paragraph::new(help_content)
            .block(
                Block::default()
                    .title(" Keyboard Shortcuts Help ")
                    .title_alignment(Alignment::Center)
                    .borders(Borders::ALL)
                    .border_type(BorderType::Rounded)
                    .border_style(self.theme.popup_border_style())
                    .style(self.theme.help_overlay_background()),
            )
            .alignment(Alignment::Left)
            .wrap(ratatui::widgets::Wrap { trim: true });

        // Render the help widget
        help_widget.render(help_area, buf);
    }
}