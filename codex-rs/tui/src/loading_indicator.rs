//! Modern loading indicator for Claude Code-style UI
//!
//! Provides smooth loading animations and progress indicators

use ratatui::prelude::*;
use ratatui::widgets::{Block, Borders, BorderType, Paragraph, Widget, WidgetRef};
use crate::theme::Theme;
use std::time::{Duration, Instant};

/// Modern loading indicator with smooth animations
pub struct LoadingIndicator {
    theme: Theme,
    message: String,
    start_time: Instant,
    animation_frames: Vec<char>,
    frame_index: usize,
}

impl LoadingIndicator {
    /// Create a new loading indicator
    pub fn new(theme: Theme, message: impl Into<String>) -> Self {
        Self {
            theme,
            message: message.into(),
            start_time: Instant::now(),
            animation_frames: vec!['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'],
            frame_index: 0,
        }
    }

    /// Update animation frame based on elapsed time
    pub fn update(&mut self) {
        let elapsed = self.start_time.elapsed();
        let fps = 8; // 8 frames per second
        let frame_duration = Duration::from_millis(1000 / fps);
        self.frame_index = (elapsed.as_millis() / frame_duration.as_millis()) as usize % self.animation_frames.len();
    }

    /// Set a custom message
    pub fn set_message(&mut self, message: String) {
        self.message = message;
    }

    /// Get current animation frame
    fn current_frame(&self) -> char {
        self.animation_frames[self.frame_index]
    }

    /// Create styled loading text
    fn create_loading_text(&self) -> Text {
        let frame = self.current_frame();
        let dots = ".".repeat(((self.start_time.elapsed().as_millis() / 500) % 4) as usize);
        
        Text::from(Line::from(vec![
            Span::styled(
                format!(" {} ", frame),
                self.theme.loading_style(),
            ),
            Span::styled(
                format!("{}{}", self.message, dots),
                self.theme.help_text_style(),
            ),
        ]))
    }
}

impl Widget for LoadingIndicator {
    fn render(self, area: Rect, buf: &mut Buffer) {
        (&self).render_ref(area, buf);
    }
}

impl WidgetRef for LoadingIndicator {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        let loading_text = self.create_loading_text();
        let paragraph = Paragraph::new(loading_text)
            .style(self.theme.inactive_item_style())
            .centered();

        paragraph.render(area, buf);
    }
}

/// Helper function to create a typing indicator
pub fn create_typing_indicator(theme: &Theme) -> Paragraph<'static> {
    Paragraph::new("💭 Claude is thinking...")
        .style(theme.loading_style())
}

/// Helper function to create a progress bar
pub fn create_progress_bar(theme: &Theme, progress: f32, label: &str) -> Paragraph<'static> {
    let width = 20;
    let filled = (progress * width as f32) as usize;
    let empty = width - filled;
    
    let bar = format!(
        "[{}{}{}] {}",
        "█".repeat(filled),
        "░".repeat(empty),
        format!("{:.0}%", progress * 100.0),
        label
    );
    
    Paragraph::new(bar).style(theme.loading_style())
}

/// Modern spinner widget for background tasks
pub struct Spinner {
    theme: Theme,
    frames: Vec<char>,
    current_frame: usize,
}

impl Spinner {
    pub fn new(theme: Theme) -> Self {
        Self {
            theme,
            frames: vec!['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'],
            current_frame: 0,
        }
    }

    pub fn next_frame(&mut self) {
        self.current_frame = (self.current_frame + 1) % self.frames.len();
    }

    pub fn render(&self, area: Rect, buf: &mut Buffer) {
        if area.width > 0 && area.height > 0 {
            let frame = self.frames[self.current_frame];
            let span = Span::styled(format!(" {}", frame), self.theme.loading_style());
            Paragraph::new(span).render(area, buf);
        }
    }
}