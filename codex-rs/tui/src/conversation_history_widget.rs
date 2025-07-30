use crate::cell_widget::CellWidget;
use crate::history_cell::CommandOutput;
use crate::history_cell::HistoryCell;
use crate::history_cell::PatchEventType;
use crate::theme::Theme;
use codex_core::config::Config;
use codex_core::protocol::FileChange;
use codex_core::protocol::SessionConfiguredEvent;
use crossterm::event::KeyCode;
use crossterm::event::KeyEvent;
use crossterm::event::KeyModifiers;
use crossterm::event::MouseButton;
use crossterm::event::MouseEvent;
use crossterm::event::MouseEventKind;
use ratatui::prelude::*;
use ratatui::style::Style;
use ratatui::widgets::*;
use serde_json::Value as JsonValue;
use std::cell::Cell as StdCell;
use std::cell::Cell;
use std::collections::HashMap;
use std::path::PathBuf;

/// A single history entry plus its cached wrapped-line count.
struct Entry {
    cell: HistoryCell,
    line_count: Cell<usize>,
}

/// Text selection state
#[derive(Clone, Debug)]
struct TextSelection {
    /// Start position (entry_index, line_within_entry, column)
    start: (usize, usize, usize),
    /// End position (entry_index, line_within_entry, column)
    end: (usize, usize, usize),
    /// Whether selection is active
    active: bool,
}

/// Direction for selection updates
enum SelectionDirection {
    Up,
    Down,
    Left,
    Right,
}

impl TextSelection {
    fn new() -> Self {
        Self {
            start: (0, 0, 0),
            end: (0, 0, 0),
            active: false,
        }
    }

    fn clear(&mut self) {
        self.active = false;
        self.start = (0, 0, 0);
        self.end = (0, 0, 0);
    }
}

pub struct ConversationHistoryWidget {
    entries: Vec<Entry>,
    /// The width (in terminal cells/columns) that [`Entry::line_count`] was
    /// computed for. When the available width changes we recompute counts.
    cached_width: StdCell<u16>,
    scroll_position: usize,
    /// Number of lines the last time render_ref() was called
    num_rendered_lines: StdCell<usize>,
    /// The height of the viewport last time render_ref() was called
    last_viewport_height: StdCell<usize>,
    has_input_focus: bool,
    /// Theme for consistent styling
    theme: Theme,
    /// Text selection state
    selection: TextSelection,
    /// Mouse state for drag selection
    mouse_dragging: bool,
    /// Last rendered area for mouse coordinate conversion
    last_render_area: StdCell<Rect>,

    /// Remember the last observed mouse coordinates while dragging so we can
    /// continue updating the selection even when the user scrolls beyond the
    /// current viewport ("autoscroll while selecting").
    last_mouse_x: StdCell<u16>,
    last_mouse_y: StdCell<u16>,
}

impl ConversationHistoryWidget {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self::with_theme(Theme::default())
    }

    pub fn with_theme(theme: Theme) -> Self {
        Self {
            entries: Vec::new(),
            cached_width: StdCell::new(0),
            scroll_position: usize::MAX,
            num_rendered_lines: StdCell::new(0),
            last_viewport_height: StdCell::new(0),
            has_input_focus: false,
            theme,
            selection: TextSelection::new(),
            mouse_dragging: false,
            last_render_area: StdCell::new(Rect::default()),

            last_mouse_x: StdCell::new(0),
            last_mouse_y: StdCell::new(0),
        }
    }

    pub(crate) fn set_input_focus(&mut self, has_input_focus: bool) {
        self.has_input_focus = has_input_focus;
        // Clear selection when losing focus
        if !has_input_focus {
            self.selection.clear();
        }
    }

    /// Get the selected text content
    pub(crate) fn get_selected_text(&self) -> Option<String> {
        if !self.selection.active {
            return None;
        }

        let mut selected_text = String::new();
        let width = self.cached_width.get();
        
        // Normalize selection bounds
        let (start_entry, start_line, start_col) = self.selection.start;
        let (end_entry, end_line, end_col) = self.selection.end;
        
        let (start_entry, start_line, start_col, end_entry, end_line, end_col) = 
            if (start_entry, start_line, start_col) <= (end_entry, end_line, end_col) {
                (start_entry, start_line, start_col, end_entry, end_line, end_col)
            } else {
                (end_entry, end_line, end_col, start_entry, start_line, start_col)
            };

        // Iterate through entries and extract selected text
        for (entry_idx, entry) in self.entries.iter().enumerate() {
            if entry_idx < start_entry || entry_idx > end_entry {
                continue;
            }

            // Extract text from the cell
            let cell_text = self.extract_text_from_cell(&entry.cell);
            
            // Split into lines considering wrapping
            let wrapped_lines = self.wrap_text(&cell_text, width);
            
            for (line_idx, line) in wrapped_lines.iter().enumerate() {
                let in_range = (entry_idx == start_entry && line_idx >= start_line) ||
                              (entry_idx > start_entry && entry_idx < end_entry) ||
                              (entry_idx == end_entry && line_idx <= end_line);
                
                if !in_range {
                    continue;
                }
                
                let line_start = if entry_idx == start_entry && line_idx == start_line {
                    start_col.min(line.len())
                } else {
                    0
                };
                
                let line_end = if entry_idx == end_entry && line_idx == end_line {
                    end_col.min(line.len())
                } else {
                    line.len()
                };
                
                if line_start < line_end {
                    selected_text.push_str(&line[line_start..line_end]);
                    if line_idx < wrapped_lines.len() - 1 || entry_idx < end_entry {
                        selected_text.push('\n');
                    }
                }
            }
        }

        if selected_text.is_empty() {
            None
        } else {
            Some(selected_text)
        }
    }

    /// Extract plain text from a HistoryCell
    fn extract_text_from_cell(&self, cell: &HistoryCell) -> String {
        match cell {
            HistoryCell::WelcomeMessage { view } |
            HistoryCell::UserPrompt { view } |
            HistoryCell::AgentMessage { view } |
            HistoryCell::AgentReasoning { view } |
            HistoryCell::CompletedExecCommand { view } |
            HistoryCell::CompletedMcpToolCall { view } => {
                // Extract text from TextBlock
                view.lines.iter()
                    .map(|line| {
                        line.spans.iter()
                            .map(|span| span.content.as_ref())
                            .collect::<String>()
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
            },
            HistoryCell::ActiveExecCommand { command, .. } => {
                format!("Running: {}", command)
            },
            HistoryCell::ActiveMcpToolCall { invocation, .. } => {
                invocation.spans.iter()
                    .map(|span| span.content.as_ref())
                    .collect::<String>()
            },
            _ => String::new(),
        }
    }

    /// Wrap text into lines based on width
    fn wrap_text(&self, text: &str, width: u16) -> Vec<String> {
        if width == 0 {
            return vec![text.to_string()];
        }
        
        let mut wrapped = Vec::new();
        for line in text.lines() {
            if line.len() <= width as usize {
                wrapped.push(line.to_string());
            } else {
                // Simple word wrapping
                let mut current = String::new();
                for word in line.split_whitespace() {
                    if current.is_empty() {
                        current = word.to_string();
                    } else if current.len() + 1 + word.len() <= width as usize {
                        current.push(' ');
                        current.push_str(word);
                    } else {
                        wrapped.push(current);
                        current = word.to_string();
                    }
                }
                if !current.is_empty() {
                    wrapped.push(current);
                }
            }
        }
        wrapped
    }

    /// Start or update text selection
    fn start_selection(&mut self) {
        if !self.selection.active {
            // Start new selection at current position
            let pos = self.get_cursor_position();
            self.selection.start = pos;
            self.selection.end = pos;
            self.selection.active = true;
        }
    }

    /// Get current cursor position based on scroll
    fn get_cursor_position(&self) -> (usize, usize, usize) {
        // For simplicity, return position at top of viewport
        // In a full implementation, we'd track actual cursor position
        let mut line_count = 0;
        for (idx, entry) in self.entries.iter().enumerate() {
            let entry_lines = entry.line_count.get();
            if line_count + entry_lines > self.scroll_position {
                let line_in_entry = self.scroll_position.saturating_sub(line_count);
                return (idx, line_in_entry, 0);
            }
            line_count += entry_lines;
        }
        (0, 0, 0)
    }

    /// Update selection end position
    fn update_selection_end(&mut self, direction: SelectionDirection) {
        if !self.selection.active {
            return;
        }

        let (entry, line, col) = self.selection.end;
        match direction {
            SelectionDirection::Up => {
                if line > 0 {
                    self.selection.end.1 = line - 1;
                } else if entry > 0 {
                    self.selection.end.0 = entry - 1;
                    self.selection.end.1 = self.entries[entry - 1].line_count.get().saturating_sub(1);
                }
            }
            SelectionDirection::Down => {
                let current_entry_lines = self.entries.get(entry).map(|e| e.line_count.get()).unwrap_or(0);
                if line < current_entry_lines.saturating_sub(1) {
                    self.selection.end.1 = line + 1;
                } else if entry < self.entries.len().saturating_sub(1) {
                    self.selection.end.0 = entry + 1;
                    self.selection.end.1 = 0;
                }
            }
            SelectionDirection::Left => {
                if col > 0 {
                    self.selection.end.2 = col - 1;
                }
            }
            SelectionDirection::Right => {
                self.selection.end.2 = col + 1;
            }
        }
    }

    /// Handle mouse events for selection
    pub(crate) fn handle_mouse_event(&mut self, mouse_event: MouseEvent) -> bool {
        let area = self.last_render_area.get();
        if area.width == 0 || area.height == 0 {
            return false;
        }

        match mouse_event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                // Start selection
                if let Some(pos) = self.mouse_to_text_position(mouse_event.column, mouse_event.row, area) {
                    self.selection.start = pos;
                    self.selection.end = pos;
                    self.selection.active = true;
                    self.mouse_dragging = true;

                    // Remember coordinates so we can keep updating the
                    // selection while the user scrolls with the wheel.
                    self.last_mouse_x.set(mouse_event.column);
                    self.last_mouse_y.set(mouse_event.row);
                    return true;
                }
            }
            MouseEventKind::Drag(MouseButton::Left) => {
                // Update selection end
                if self.mouse_dragging {
                    // Check if we need to auto-scroll
                    if mouse_event.row < area.y {
                        // Mouse is above the viewport - scroll up
                        let distance = (area.y - mouse_event.row) as u32;
                        let scroll_speed = distance.min(5).max(1); // Faster scroll when further from edge
                        self.scroll_up(scroll_speed);
                    } else if mouse_event.row >= area.y + area.height {
                        // Mouse is below the viewport - scroll down
                        let distance = (mouse_event.row - (area.y + area.height - 1)) as u32;
                        let scroll_speed = distance.min(5).max(1); // Faster scroll when further from edge
                        self.scroll_down(scroll_speed);
                    }

                    if let Some(pos) = self.mouse_to_text_position(mouse_event.column, mouse_event.row, area) {
                        self.selection.end = pos;

                        // Update last known coordinates so autoscroll keeps
                        // using the latest pointer location.
                        self.last_mouse_x.set(mouse_event.column);
                        self.last_mouse_y.set(mouse_event.row);
                        return true;
                    }
                }
            }
            MouseEventKind::Up(MouseButton::Left) => {
                // End selection
                self.mouse_dragging = false;

                // When the user finishes a drag-selection with the left mouse
                // button, automatically copy the selected text to the system
                // clipboard.  This matches the behaviour of many terminal
                // emulators where releasing the mouse button after selecting
                // text implicitly places the text into the clipboard.
                if self.selection.active {
                    if let Some(selected_text) = self.get_selected_text() {
                        if let Ok(mut clipboard) = arboard::Clipboard::new() {
                            let _ = clipboard.set_text(selected_text);
                        }
                    }
                }
            }
            _ => {}
        }
        false
    }

    /// Convert mouse coordinates to text position
    fn mouse_to_text_position(&self, x: u16, y: u16, area: Rect) -> Option<(usize, usize, usize)> {
        // Calculate relative Y position, allowing for coordinates outside the viewport
        let relative_y = if y < area.y {
            // Mouse is above the viewport - use negative offset
            let offset = (area.y - y) as i32;
            self.scroll_position.saturating_sub(offset as usize)
        } else {
            // Mouse is at or below the viewport top
            self.scroll_position + (y - area.y) as usize
        };
        
        // Calculate relative X position, clamping to widget width
        let relative_x = if x < area.x {
            0
        } else {
            (x - area.x).min(area.width.saturating_sub(1)) as usize
        };
        
        // Use relative_y directly as the target line
        let target_line = relative_y;
        
        // Find which entry and line within entry
        let mut accumulated_lines = 0;
        for (entry_idx, entry) in self.entries.iter().enumerate() {
            let entry_lines = entry.line_count.get();
            if accumulated_lines + entry_lines > target_line {
                let line_within_entry = target_line - accumulated_lines;
                return Some((entry_idx, line_within_entry, relative_x));
            }
            accumulated_lines += entry_lines;
        }
        
        None
    }

    /// Returns true if it needs a redraw.
    pub(crate) fn handle_key_event(&mut self, key_event: KeyEvent) -> bool {
        // Handle Ctrl+C for copy
        if key_event.modifiers.contains(KeyModifiers::CONTROL) && key_event.code == KeyCode::Char('c') {
            if let Some(selected_text) = self.get_selected_text() {
                // Copy to clipboard
                if let Ok(mut clipboard) = arboard::Clipboard::new() {
                    let _ = clipboard.set_text(selected_text);
                }
            }
            return true;
        }

        // Handle Ctrl+A for select all
        if key_event.modifiers.contains(KeyModifiers::CONTROL) && key_event.code == KeyCode::Char('a') {
            if !self.entries.is_empty() {
                self.selection.active = true;
                self.selection.start = (0, 0, 0);
                let last_entry_idx = self.entries.len() - 1;
                let last_entry_lines = self.entries[last_entry_idx].line_count.get();
                self.selection.end = (last_entry_idx, last_entry_lines.saturating_sub(1), usize::MAX);
            }
            return true;
        }

        // Handle selection with Shift
        if key_event.modifiers.contains(KeyModifiers::SHIFT) {
            match key_event.code {
                KeyCode::Up => {
                    self.start_selection();
                    self.update_selection_end(SelectionDirection::Up);
                    self.scroll_up(1);
                    return true;
                }
                KeyCode::Down => {
                    self.start_selection();
                    self.update_selection_end(SelectionDirection::Down);
                    self.scroll_down(1);
                    return true;
                }
                KeyCode::Left => {
                    self.start_selection();
                    self.update_selection_end(SelectionDirection::Left);
                    return true;
                }
                KeyCode::Right => {
                    self.start_selection();
                    self.update_selection_end(SelectionDirection::Right);
                    return true;
                }
                _ => {}
            }
        }

        // Clear selection on any non-shift movement
        if !key_event.modifiers.contains(KeyModifiers::SHIFT) {
            self.selection.clear();
        }

        match key_event.code {
            KeyCode::Up | KeyCode::Char('k') => {
                self.scroll_up(1);
                true
            }
            KeyCode::Down | KeyCode::Char('j') => {
                self.scroll_down(1);
                true
            }
            KeyCode::PageUp | KeyCode::Char('b') => {
                self.scroll_page_up();
                true
            }
            KeyCode::PageDown | KeyCode::Char(' ') => {
                self.scroll_page_down();
                true
            }
            _ => false,
        }
    }

    /// Negative delta scrolls up; positive delta scrolls down.
    pub(crate) fn scroll(&mut self, delta: i32) {
        match delta.cmp(&0) {
            std::cmp::Ordering::Less => self.scroll_up(-delta as u32),
            std::cmp::Ordering::Greater => self.scroll_down(delta as u32),
            std::cmp::Ordering::Equal => {}
        }

        // While the user is dragging a selection, keep the selection’s end
        // anchored to the cursor’s (potentially off-screen) position so they
        // can extend the selection by scrolling.
        if self.mouse_dragging && self.selection.active {
            let area = self.last_render_area.get();
            let x = self.last_mouse_x.get();
            let y = self.last_mouse_y.get();

            if let Some(pos) = self.mouse_to_text_position(x, y, area) {
                self.selection.end = pos;
            }
        }
    }

    fn scroll_up(&mut self, num_lines: u32) {
        // If a user is scrolling up from the "stick to bottom" mode, we need to
        // map this to a specific scroll position so we can calculate the delta.
        // This requires us to care about how tall the screen is.
        if self.scroll_position == usize::MAX {
            self.scroll_position = self
                .num_rendered_lines
                .get()
                .saturating_sub(self.last_viewport_height.get());
        }

        self.scroll_position = self.scroll_position.saturating_sub(num_lines as usize);
    }

    fn scroll_down(&mut self, num_lines: u32) {
        // If we're already pinned to the bottom there's nothing to do.
        if self.scroll_position == usize::MAX {
            return;
        }

        let viewport_height = self.last_viewport_height.get().max(1);
        let num_rendered_lines = self.num_rendered_lines.get();

        // Compute the maximum explicit scroll offset that still shows a full
        // viewport. This mirrors the calculation in `scroll_page_down()` and
        // in the render path.
        let max_scroll = num_rendered_lines.saturating_sub(viewport_height);

        let new_pos = self.scroll_position.saturating_add(num_lines as usize);

        if new_pos >= max_scroll {
            // Reached (or passed) the bottom – switch to stick‑to‑bottom mode
            // so that additional output keeps the view pinned automatically.
            self.scroll_position = usize::MAX;
        } else {
            self.scroll_position = new_pos;
        }
    }

    /// Scroll up by one full viewport height (Page Up).
    fn scroll_page_up(&mut self) {
        let viewport_height = self.last_viewport_height.get().max(1);

        // If we are currently in the "stick to bottom" mode, first convert the
        // implicit scroll position (`usize::MAX`) into an explicit offset that
        // represents the very bottom of the scroll region.  This mirrors the
        // logic from `scroll_up()`.
        if self.scroll_position == usize::MAX {
            self.scroll_position = self
                .num_rendered_lines
                .get()
                .saturating_sub(viewport_height);
        }

        // Move up by a full page.
        self.scroll_position = self.scroll_position.saturating_sub(viewport_height);
    }

    /// Scroll down by one full viewport height (Page Down).
    fn scroll_page_down(&mut self) {
        // Nothing to do if we're already stuck to the bottom.
        if self.scroll_position == usize::MAX {
            return;
        }

        let viewport_height = self.last_viewport_height.get().max(1);
        let num_lines = self.num_rendered_lines.get();

        // Calculate the maximum explicit scroll offset that is still within
        // range. This matches the logic in `scroll_down()` and the render
        // method.
        let max_scroll = num_lines.saturating_sub(viewport_height);

        // Attempt to move down by a full page.
        let new_pos = self.scroll_position.saturating_add(viewport_height);

        if new_pos >= max_scroll {
            // We have reached (or passed) the bottom – switch back to
            // automatic stick‑to‑bottom mode so that subsequent output keeps
            // the viewport pinned.
            self.scroll_position = usize::MAX;
        } else {
            self.scroll_position = new_pos;
        }
    }

    pub fn scroll_to_bottom(&mut self) {
        self.scroll_position = usize::MAX;
    }

    /// Note `model` could differ from `config.model` if the agent decided to
    /// use a different model than the one requested by the user.
    pub fn add_session_info(&mut self, config: &Config, event: SessionConfiguredEvent) {
        // In practice, SessionConfiguredEvent should always be the first entry
        // in the history, but it is possible that an error could be sent
        // before the session info.
        let has_welcome_message = self
            .entries
            .iter()
            .any(|entry| matches!(entry.cell, HistoryCell::WelcomeMessage { .. }));
        self.add_to_history(HistoryCell::new_session_info(
            config,
            event,
            !has_welcome_message,
        ));
    }

    pub fn add_user_message(&mut self, message: String) {
        self.add_to_history(HistoryCell::new_user_prompt(message));
    }

    pub fn add_agent_message(&mut self, config: &Config, message: String) {
        self.add_to_history(HistoryCell::new_agent_message(config, message));
    }

    pub fn add_agent_reasoning(&mut self, config: &Config, text: String) {
        self.add_to_history(HistoryCell::new_agent_reasoning(config, text));
    }

    pub fn replace_prev_agent_reasoning(&mut self, config: &Config, text: String) {
        self.replace_last_agent_reasoning(config, text);
    }

    pub fn replace_prev_agent_message(&mut self, config: &Config, text: String) {
        self.replace_last_agent_message(config, text);
    }

    pub fn add_background_event(&mut self, message: String) {
        self.add_to_history(HistoryCell::new_background_event(message));
    }

    pub fn add_diff_output(&mut self, diff_output: String) {
        self.add_to_history(HistoryCell::new_diff_output(diff_output));
    }

    pub fn add_error(&mut self, message: String) {
        self.add_to_history(HistoryCell::new_error_event(message));
    }

    /// Add a pending patch entry (before user approval).
    pub fn add_patch_event(
        &mut self,
        event_type: PatchEventType,
        changes: HashMap<PathBuf, FileChange>,
    ) {
        self.add_to_history(HistoryCell::new_patch_event(event_type, changes));
    }

    pub fn add_active_exec_command(&mut self, call_id: String, command: Vec<String>) {
        self.add_to_history(HistoryCell::new_active_exec_command(call_id, command));
    }

    pub fn add_active_mcp_tool_call(
        &mut self,
        call_id: String,
        server: String,
        tool: String,
        arguments: Option<JsonValue>,
    ) {
        self.add_to_history(HistoryCell::new_active_mcp_tool_call(
            call_id, server, tool, arguments,
        ));
    }

    fn add_to_history(&mut self, cell: HistoryCell) {
        let width = self.cached_width.get();
        let count = if width > 0 { cell.height(width) } else { 0 };

        self.entries.push(Entry {
            cell,
            line_count: Cell::new(count),
        });
    }

    pub fn replace_last_agent_reasoning(&mut self, config: &Config, text: String) {
        if let Some(idx) = self
            .entries
            .iter()
            .rposition(|entry| matches!(entry.cell, HistoryCell::AgentReasoning { .. }))
        {
            let width = self.cached_width.get();
            let entry = &mut self.entries[idx];
            entry.cell = HistoryCell::new_agent_reasoning(config, text);
            let height = if width > 0 {
                entry.cell.height(width)
            } else {
                0
            };
            entry.line_count.set(height);
        }
    }

    pub fn replace_last_agent_message(&mut self, config: &Config, text: String) {
        if let Some(idx) = self
            .entries
            .iter()
            .rposition(|entry| matches!(entry.cell, HistoryCell::AgentMessage { .. }))
        {
            let width = self.cached_width.get();
            let entry = &mut self.entries[idx];
            entry.cell = HistoryCell::new_agent_message(config, text);
            let height = if width > 0 {
                entry.cell.height(width)
            } else {
                0
            };
            entry.line_count.set(height);
        }
    }

    pub fn record_completed_exec_command(
        &mut self,
        call_id: String,
        stdout: String,
        stderr: String,
        exit_code: i32,
    ) {
        let width = self.cached_width.get();
        for entry in self.entries.iter_mut() {
            let cell = &mut entry.cell;
            if let HistoryCell::ActiveExecCommand {
                call_id: history_id,
                command,
                start,
                ..
            } = cell
            {
                if &call_id == history_id {
                    *cell = HistoryCell::new_completed_exec_command(
                        command.clone(),
                        CommandOutput {
                            exit_code,
                            stdout,
                            stderr,
                            duration: start.elapsed(),
                        },
                    );

                    // Update cached line count.
                    if width > 0 {
                        entry.line_count.set(cell.height(width));
                    }
                    break;
                }
            }
        }
    }

    pub fn record_completed_mcp_tool_call(
        &mut self,
        call_id: String,
        success: bool,
        result: Result<mcp_types::CallToolResult, String>,
    ) {
        let width = self.cached_width.get();
        for entry in self.entries.iter_mut() {
            if let HistoryCell::ActiveMcpToolCall {
                call_id: history_id,
                invocation,
                start,
                ..
            } = &entry.cell
            {
                if &call_id == history_id {
                    let completed = HistoryCell::new_completed_mcp_tool_call(
                        width,
                        invocation.clone(),
                        *start,
                        success,
                        result,
                    );
                    entry.cell = completed;

                    if width > 0 {
                        entry.line_count.set(entry.cell.height(width));
                    }

                    break;
                }
            }
        }
    }

    /// Get selection info for a specific entry
    fn get_selection_info_for_entry(&self, entry_idx: usize, skip_lines: usize, visible_lines: usize) -> Option<SelectionInfo> {
        let (start_entry, start_line, start_col) = self.selection.start;
        let (end_entry, end_line, end_col) = self.selection.end;
        
        // Normalize selection bounds
        let (start_entry, start_line, start_col, end_entry, end_line, end_col) = 
            if (start_entry, start_line, start_col) <= (end_entry, end_line, end_col) {
                (start_entry, start_line, start_col, end_entry, end_line, end_col)
            } else {
                (end_entry, end_line, end_col, start_entry, start_line, start_col)
            };
        
        if entry_idx < start_entry || entry_idx > end_entry {
            return None;
        }
        
        Some(SelectionInfo {
            start_line: if entry_idx == start_entry { start_line } else { 0 },
            start_col: if entry_idx == start_entry { start_col } else { 0 },
            end_line: if entry_idx == end_entry { end_line } else { usize::MAX },
            end_col: if entry_idx == end_entry { end_col } else { usize::MAX },
            skip_lines,
            visible_lines,
        })
    }

    /// Render a cell with selection highlighting
    fn render_cell_with_selection(&self, cell: &HistoryCell, skip_lines: usize, area: Rect, buf: &mut Buffer, selection_info: Option<SelectionInfo>) {
        // First render the cell normally
        cell.render_window(skip_lines, area, buf);
        
        // Then apply selection highlighting
        if let Some(info) = selection_info {
            for y in 0..area.height {
                let line_idx = skip_lines + y as usize;
                
                // Check if this line is within selection
                if line_idx < info.start_line || line_idx > info.end_line {
                    continue;
                }
                
                let start_x = if line_idx == info.start_line {
                    info.start_col.min(area.width as usize)
                } else {
                    0
                };
                
                let end_x = if line_idx == info.end_line {
                    info.end_col.min(area.width as usize)
                } else {
                    area.width as usize
                };
                
                // Apply selection style - use inverted colors instead of background
                for x in start_x..end_x {
                    let pos = (area.x + x as u16, area.y + y);
                    if let Some(cell) = buf.cell_mut(pos) {
                        let current_style = cell.style();
                        // Invert the colors for selection
                        let new_fg = current_style.bg.unwrap_or(self.theme.primary.background);
                        let new_bg = current_style.fg.unwrap_or(self.theme.primary.foreground);
                        cell.set_style(
                            Style::default()
                                .fg(new_fg)
                                .bg(new_bg)
                                .add_modifier(current_style.add_modifier)
                        );
                    }
                }
            }
        }
    }
}

/// Selection info for rendering
struct SelectionInfo {
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
    skip_lines: usize,
    visible_lines: usize,
}

impl WidgetRef for ConversationHistoryWidget {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        // Save the render area for mouse coordinate conversion
        self.last_render_area.set(area);
        
        // No border for conversation history - use the full area
        let inner = area;
        let viewport_height = inner.height as usize;

        // Cache (and if necessary recalculate) the wrapped line counts for every
        // [`HistoryCell`] so that our scrolling math accounts for text
        // wrapping.  We always reserve one column on the right-hand side for the
        // scrollbar so that the content never renders "under" the scrollbar.
        let effective_width = inner.width.saturating_sub(1);

        if effective_width == 0 {
            return; // Nothing to draw – avoid division by zero.
        }

        // Recompute cache if the effective width changed.
        let num_lines: usize = if self.cached_width.get() != effective_width {
            self.cached_width.set(effective_width);

            let mut num_lines: usize = 0;
            for entry in &self.entries {
                let count = entry.cell.height(effective_width);
                num_lines += count;
                entry.line_count.set(count);
            }
            num_lines
        } else {
            self.entries.iter().map(|e| e.line_count.get()).sum()
        };

        // Determine the scroll position. Note the existing value of
        // `self.scroll_position` could exceed the maximum scroll offset if the
        // user made the window wider since the last render.
        let max_scroll = num_lines.saturating_sub(viewport_height);
        let scroll_pos = if self.scroll_position == usize::MAX {
            max_scroll
        } else {
            self.scroll_position.min(max_scroll)
        };

        // ------------------------------------------------------------------
        // Render order:
        //   1. Clear full widget area (avoid artifacts from prior frame).
        //   2. Render *each* visible HistoryCell into its own sub-Rect while
        //      respecting partial visibility at the top and bottom.
        //   3. Draw the scrollbar track / thumb in the reserved column.
        // ------------------------------------------------------------------

        // Clear entire widget area first.
        Clear.render(area, buf);
        
        // Ensure the background uses the theme's background color
        // Avoid painting a solid background so the widget inherits the
        // terminal’s default colours.  This prevents the “full-screen blue
        // backdrop” effect and lets users copy text without the coloured
        // cells bleeding into their clipboard in some terminals.
        Block::default().render(area, buf);

        // No border to render - proceed directly to content

        // ------------------------------------------------------------------
        // Calculate which cells are visible for the current scroll position
        // and paint them one by one.
        // ------------------------------------------------------------------

        let mut y_cursor = inner.y; // first line inside viewport
        let mut remaining_height = inner.height as usize;
        let mut lines_to_skip = scroll_pos; // number of wrapped lines to skip (above viewport)

        for entry in &self.entries {
            let cell_height = entry.line_count.get();

            // Completely above viewport? Skip whole cell.
            if lines_to_skip >= cell_height {
                lines_to_skip -= cell_height;
                continue;
            }

            // Determine how much of this cell is visible.
            let visible_height = (cell_height - lines_to_skip).min(remaining_height);

            if visible_height == 0 {
                break; // no space left
            }

            let cell_rect = Rect {
                x: inner.x,
                y: y_cursor,
                width: effective_width,
                height: visible_height as u16,
            };

            // Check if this entry has any selection
            let entry_idx = self.entries.iter().position(|e| std::ptr::eq(e, entry)).unwrap_or(0);
            let selection_info = if self.selection.active {
                self.get_selection_info_for_entry(entry_idx, lines_to_skip, visible_height)
            } else {
                None
            };
            
            // Render the cell with selection info
            self.render_cell_with_selection(&entry.cell, lines_to_skip, cell_rect, buf, selection_info);

            // Advance cursor inside viewport.
            y_cursor += visible_height as u16;
            remaining_height -= visible_height;

            // After the first (possibly partially skipped) cell, we no longer
            // need to skip lines at the top.
            lines_to_skip = 0;

            if remaining_height == 0 {
                break; // viewport filled
            }
        }

        // Always render a scrollbar *track* so the reserved column is filled.
        let overflow = num_lines.saturating_sub(viewport_height);

        let mut scroll_state = ScrollbarState::default()
            // The Scrollbar widget expects the *content* height minus the
            // viewport height.  When there is no overflow we still provide 0
            // so that the widget renders only the track without a thumb.
            .content_length(overflow)
            .position(scroll_pos);

        {
            // Choose a thumb color that stands out only when this pane has focus so that the
            // user's attention is naturally drawn to the active viewport. When unfocused we show
            // a low-contrast thumb so the scrollbar fades into the background without becoming
            // invisible.
            let thumb_style = if self.has_input_focus {
                Style::reset().fg(self.theme.ui.highlight)
            } else {
                Style::reset().fg(self.theme.ui.border_unfocused)
            };

            // By default the Scrollbar widget inherits any style that was
            // present in the underlying buffer cells. That means if a colored
            // line happens to be underneath the scrollbar, the track (and
            // potentially the thumb) adopt that color. Explicitly setting the
            // track/thumb styles ensures we always draw the scrollbar with a
            // consistent palette regardless of what content is behind it.
            StatefulWidget::render(
                Scrollbar::new(ScrollbarOrientation::VerticalRight)
                    .begin_symbol(Some("↑"))
                    .end_symbol(Some("↓"))
                    .begin_style(Style::reset().fg(self.theme.ui.border_unfocused))
                    .end_style(Style::reset().fg(self.theme.ui.border_unfocused))
                    .thumb_symbol("█")
                    .thumb_style(thumb_style)
                    .track_symbol(Some("│"))
                    .track_style(Style::reset().fg(self.theme.ui.border_unfocused)),
                inner,
                buf,
                &mut scroll_state,
            );
        }

        // Update auxiliary stats that the scroll handlers rely on.
        self.num_rendered_lines.set(num_lines);
        self.last_viewport_height.set(viewport_height);
    }
}

/// Common [`Wrap`] configuration used for both measurement and rendering so
/// they stay in sync.
#[inline]
pub(crate) const fn wrap_cfg() -> ratatui::widgets::Wrap {
    ratatui::widgets::Wrap { trim: true }
}
