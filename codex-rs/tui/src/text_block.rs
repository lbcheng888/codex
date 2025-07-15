use crate::cell_widget::CellWidget;
use ratatui::prelude::*;

/// A simple widget that just displays a list of `Line`s via a `Paragraph`.
/// This is the default rendering backend for most `HistoryCell` variants.
#[derive(Clone)]
pub(crate) struct TextBlock {
    pub(crate) lines: Vec<Line<'static>>,
}

impl TextBlock {
    pub(crate) fn new(lines: Vec<Line<'static>>) -> Self {
        Self { lines }
    }
}

impl CellWidget for TextBlock {
    fn height(&self, width: u16) -> usize {
        // Use the same wrapping configuration as ConversationHistoryWidget so
        // measurement stays in sync with rendering.
        ratatui::widgets::Paragraph::new(self.lines.clone())
            .wrap(crate::conversation_history_widget::wrap_cfg())
            .line_count(width)
    }

    fn render_window(&self, first_visible_line: usize, area: Rect, buf: &mut Buffer) {
        // Create a new Paragraph with bright white text as default
        let mut styled_lines = Vec::new();
        for line in &self.lines {
            let mut styled_spans = Vec::new();
            for span in &line.spans {
                let mut style = span.style;
                // Force bright white text for all content
                style.fg = Some(Color::Rgb(245, 245, 245));
                styled_spans.push(Span::styled(span.content.clone(), style));
            }
            styled_lines.push(Line::from(styled_spans));
        }

        ratatui::widgets::Paragraph::new(styled_lines)
            .wrap(crate::conversation_history_widget::wrap_cfg())
            .scroll((first_visible_line as u16, 0))
            .render(area, buf);
    }
}
