use color_eyre::eyre::Result;
use crossterm::event::KeyCode;
use crossterm::event::KeyEvent;
use crossterm::event::KeyEventKind;
use crossterm::event::KeyModifiers;
use ratatui::layout::Constraint;
use ratatui::layout::Layout;
use ratatui::layout::Rect;
use ratatui::style::Stylize as _;
use ratatui::text::Line;
use ratatui::widgets::Paragraph;
use ratatui::widgets::Widget;
use textwrap::Options as WrapOptions;
use textwrap::wrap;
use tokio_stream::StreamExt;

use crate::cli::ContinuousCliArgs;
use crate::public_widgets::composer_input::ComposerAction;
use crate::public_widgets::composer_input::ComposerInput;
use crate::tui::Tui;
use crate::tui::TuiEvent;

pub const DEFAULT_MAX_STEPS: u32 = 20;
pub const MIN_MAX_STEPS: u32 = 1;
pub const MAX_MAX_STEPS: u32 = 200;
pub const DEFAULT_CONFIRM_ON_BREAKPOINT: bool = true;

#[derive(Debug, Clone)]
pub struct ContinuousModeSettings {
    pub max_steps: u32,
    pub confirm_on_breakpoint: bool,
}

impl ContinuousModeSettings {
    pub fn from_cli(args: &ContinuousCliArgs) -> Self {
        Self {
            max_steps: args.max_steps.unwrap_or(DEFAULT_MAX_STEPS),
            confirm_on_breakpoint: args
                .confirm_on_breakpoint
                .unwrap_or(DEFAULT_CONFIRM_ON_BREAKPOINT),
        }
    }

    pub(crate) fn clamp(&mut self) {
        if self.max_steps < MIN_MAX_STEPS {
            self.max_steps = MIN_MAX_STEPS;
        } else if self.max_steps > MAX_MAX_STEPS {
            self.max_steps = MAX_MAX_STEPS;
        }
    }

    fn adjust_steps(&mut self, delta: i32) {
        let next = self.max_steps as i32 + delta;
        self.max_steps = next.clamp(MIN_MAX_STEPS as i32, MAX_MAX_STEPS as i32) as u32;
    }
}

pub enum PreviewAction {
    Start {
        prompt: String,
        settings: ContinuousModeSettings,
    },
    Abort,
}

struct PreviewState {
    prompt: String,
    settings: ContinuousModeSettings,
    editing: bool,
    show_help: bool,
    composer: ComposerInput,
}

impl PreviewState {
    fn new(prompt: String, settings: ContinuousModeSettings) -> Self {
        let mut composer = ComposerInput::new();
        composer.set_hint_items(vec![
            ("⏎", "save"),
            ("Shift+⏎", "newline"),
            ("Esc", "cancel"),
        ]);
        Self {
            prompt,
            settings,
            editing: false,
            show_help: false,
            composer,
        }
    }

    fn begin_edit(&mut self) {
        self.editing = true;
        self.composer.set_text(self.prompt.clone());
    }

    fn cancel_edit(&mut self) {
        self.editing = false;
        self.composer.clear();
    }

    fn toggle_help(&mut self) {
        self.show_help = !self.show_help;
    }

    fn handle_key(&mut self, key: KeyEvent, tui: &mut Tui) -> Option<PreviewAction> {
        if matches!(key.kind, KeyEventKind::Release) {
            return None;
        }

        if self.editing {
            match key.code {
                KeyCode::Esc => {
                    self.cancel_edit();
                }
                _ => {
                    if let ComposerAction::Submitted(new_prompt) = self.composer.input(key) {
                        self.prompt = new_prompt;
                        self.editing = false;
                        self.composer.clear();
                    }
                    tui.frame_requester().schedule_frame();
                }
            }
            return None;
        }

        match key.code {
            KeyCode::Enter => Some(PreviewAction::Start {
                prompt: self.prompt.clone(),
                settings: self.settings.clone(),
            }),
            KeyCode::Char('c') | KeyCode::Char('C') => {
                if key.modifiers.contains(KeyModifiers::CONTROL) {
                    Some(PreviewAction::Abort)
                } else {
                    Some(PreviewAction::Start {
                        prompt: self.prompt.clone(),
                        settings: self.settings.clone(),
                    })
                }
            }
            KeyCode::Char('q') | KeyCode::Char('Q') | KeyCode::Esc => Some(PreviewAction::Abort),
            KeyCode::Char('e') | KeyCode::Char('E') => {
                self.begin_edit();
                tui.frame_requester().schedule_frame();
                None
            }
            KeyCode::Char('h') | KeyCode::Char('H') => {
                self.toggle_help();
                None
            }
            KeyCode::Up | KeyCode::Char('+') => {
                self.settings.adjust_steps(1);
                None
            }
            KeyCode::Down | KeyCode::Char('-') => {
                self.settings.adjust_steps(-1);
                None
            }
            KeyCode::Char('b') | KeyCode::Char('B') => {
                self.settings.confirm_on_breakpoint = !self.settings.confirm_on_breakpoint;
                None
            }
            _ => None,
        }
    }

    fn handle_paste(&mut self, pasted: String) -> bool {
        if self.editing {
            self.composer.handle_paste(pasted)
        } else {
            false
        }
    }

    fn handle_draw(&mut self, tui: &mut Tui) {
        if self.editing {
            let _ = self.composer.flush_paste_burst_if_due();
            if self.composer.is_in_paste_burst() {
                tui.frame_requester()
                    .schedule_frame_in(ComposerInput::recommended_flush_delay());
            }
        }
    }
}

pub async fn run_preview(
    tui: &mut Tui,
    initial_prompt: Option<String>,
    args: &ContinuousCliArgs,
) -> Result<PreviewAction> {
    let mut state = PreviewState::new(initial_prompt.unwrap_or_default(), {
        let mut settings = ContinuousModeSettings::from_cli(args);
        settings.clamp();
        settings
    });

    let mut events = tui.event_stream();
    let mut needs_redraw = true;

    loop {
        if needs_redraw {
            draw_preview(tui, &state)?;
            needs_redraw = false;
        }

        tokio::select! {
            Some(event) = events.next() => {
                match event {
                    TuiEvent::Draw => {
                        state.handle_draw(tui);
                        needs_redraw = true;
                    }
                    TuiEvent::Paste(pasted) => {
                        if state.handle_paste(pasted) {
                            needs_redraw = true;
                        }
                    }
                    TuiEvent::Key(key) => {
                        if let Some(action) = state.handle_key(key, tui) {
                            return Ok(action);
                        }
                        needs_redraw = true;
                    }
                }
            }
        }
    }
}

fn draw_preview(tui: &mut Tui, state: &PreviewState) -> std::io::Result<()> {
    let height = tui.terminal.size()?.height;
    tui.draw(height, |frame| {
        let area = frame.area();
        let [header, status, body, footer] = Layout::vertical([
            Constraint::Length(1),
            Constraint::Length(1),
            Constraint::Min(area.height.saturating_sub(3)),
            Constraint::Length(2),
        ])
        .areas(area);

        frame.render_widget_ref(
            Line::from(vec!["Continuous mode preview".bold().cyan()]),
            header,
        );

        let status_line: Line = vec![
            format!("Max steps: {}", state.settings.max_steps).bold(),
            "  •  ".dim(),
            if state.settings.confirm_on_breakpoint {
                "Confirm on breakpoints".into()
            } else {
                "Auto-continue breakpoints".into()
            },
            "  •  ".dim(),
            "Press H for help".into(),
        ]
        .into();
        frame.render_widget_ref(status_line, status);

        render_body(frame, body, state);

        let hint_line: Line = if state.editing {
            vec![
                "⏎".bold(),
                " save".into(),
                "  •  ".dim(),
                "Esc".bold(),
                " cancel".into(),
            ]
            .into()
        } else {
            vec![
                "Enter".bold(),
                " start".into(),
                "  •  ".dim(),
                "E".bold(),
                " edit prompt".into(),
                "  •  ".dim(),
                "↑/↓".bold(),
                " adjust steps".into(),
                "  •  ".dim(),
                "B".bold(),
                " toggle breakpoint confirm".into(),
                "  •  ".dim(),
                "Q".bold(),
                " abort".into(),
            ]
            .into()
        };
        frame.render_widget_ref(hint_line, footer);
    })
}

fn render_body(frame: &mut crate::custom_terminal::Frame, area: Rect, state: &PreviewState) {
    if area.height == 0 {
        return;
    }

    if state.editing {
        render_editor(frame, area, &state.composer);
        return;
    }

    let width = area.width.max(1) as usize;
    let options = WrapOptions::new(width);
    let mut lines: Vec<Line<'static>> = if state.prompt.trim().is_empty() {
        vec!["No starting prompt provided.".dim().into()]
    } else {
        wrap(state.prompt.as_str(), options)
            .into_iter()
            .map(|cow| Line::from(cow.into_owned()))
            .collect()
    };

    lines.push("".into());

    if state.show_help {
        lines.push(
            "Codex will draft a plan, auto-execute each step, and stop when limits trigger.".into(),
        );
        lines.push(
            "Modify the starting prompt or adjust limits above before entering continuous mode."
                .dim()
                .into(),
        );
    } else {
        lines.push(
            "The final plan preview appears after you continue."
                .dim()
                .into(),
        );
        lines.push("Press H to view detailed help.".dim().into());
    }

    {
        let buffer = frame.buffer_mut();
        Paragraph::new(lines).render(area, buffer);
    }
}

fn render_editor(frame: &mut crate::custom_terminal::Frame, area: Rect, composer: &ComposerInput) {
    {
        let buffer = frame.buffer_mut();
        composer.render_ref(area, buffer);
    }
    if let Some((x, y)) = composer.cursor_pos(area) {
        frame.set_cursor_position((x, y));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn clamp_enforces_bounds() {
        let mut settings = ContinuousModeSettings {
            max_steps: 0,
            confirm_on_breakpoint: true,
        };
        settings.clamp();
        assert_eq!(settings.max_steps, MIN_MAX_STEPS);

        settings.max_steps = MAX_MAX_STEPS + 10;
        settings.clamp();
        assert_eq!(settings.max_steps, MAX_MAX_STEPS);
    }

    #[test]
    fn adjust_steps_stays_within_limits() {
        let mut settings = ContinuousModeSettings {
            max_steps: 10,
            confirm_on_breakpoint: true,
        };
        settings.adjust_steps(-15);
        assert_eq!(settings.max_steps, MIN_MAX_STEPS);

        settings.adjust_steps(500);
        assert_eq!(settings.max_steps, MAX_MAX_STEPS);
    }
}
