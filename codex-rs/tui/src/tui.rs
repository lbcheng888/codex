use std::io::stdout;
use std::io::Result;
use std::io::Stdout;
use std::sync::atomic::{AtomicBool, Ordering};

use codex_core::config::Config;
use crossterm::event::{DisableBracketedPaste, DisableMouseCapture, EnableBracketedPaste};
use ratatui::backend::CrosstermBackend;
use ratatui::crossterm::execute;
use ratatui::crossterm::terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen};
use ratatui::Terminal;

use crate::mouse_capture::MouseCapture;

/// Tracks whether the application has entered the terminal's *alternate screen*
/// buffer (`smcup`).  This lets us symmetrically call the corresponding
/// `rmcup` escape *only* if we entered it in the first place.  We use a global
/// to avoid plumbing this bit of state through every call site.
static ALTERNATE_SCREEN_ENTERED: AtomicBool = AtomicBool::new(false);

/// A type alias for the terminal type used in this application.
pub type Tui = Terminal<CrosstermBackend<Stdout>>;

/// Initialise the terminal.
///
/// Behaviour is controlled by `config.tui`:
///   * When `disable_alternate_screen` is **false** (the default) we switch to
///     the alternate screen for a traditional full-screen TUI.
///   * When it is **true** we render directly to the main screen so that the
///     user retains native scroll-back as well as "click-and-drag to copy"
///     functionality provided by the terminal emulator.
pub fn init(config: &Config) -> Result<(Tui, MouseCapture)> {
    if !config.tui.disable_alternate_screen {
        execute!(stdout(), EnterAlternateScreen)?;
        ALTERNATE_SCREEN_ENTERED.store(true, Ordering::Relaxed);
    }

    // Enable bracketed paste unconditionally – this setting does not interfere
    // with the main/alternate screen choice.
    execute!(stdout(), EnableBracketedPaste)?;

    // Honour the `disable_mouse_capture` knob exactly as before.
    let mouse_capture = MouseCapture::new_with_capture(!config.tui.disable_mouse_capture)?;

    enable_raw_mode()?;

    set_panic_hook();

    let tui = Terminal::new(CrosstermBackend::new(stdout()))?;
    Ok((tui, mouse_capture))
}

fn set_panic_hook() {
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |panic_info| {
        let _ = restore(); // best-effort – we are already panicking
        hook(panic_info);
    }));
}

/// Restore the terminal to its original state.
pub fn restore() -> Result<()> {
    // Always attempt to disable mouse capture; duplicate calls are harmless.
    if execute!(stdout(), DisableMouseCapture).is_err() {
        // Multiple disable sequences can fail on some terminals; ignore.
    }

    execute!(stdout(), DisableBracketedPaste)?;

    if ALTERNATE_SCREEN_ENTERED.load(Ordering::Relaxed) {
        // Only leave the alternate screen if we previously entered it.
        let _ = execute!(stdout(), LeaveAlternateScreen);
    }

    disable_raw_mode()?;
    Ok(())
}

