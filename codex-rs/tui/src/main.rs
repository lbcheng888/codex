use clap::Parser;
use codex_common::CliConfigOverrides;
use codex_tui::Cli;
use codex_tui::run_main;

#[derive(Parser, Debug)]
struct TopCli {
    #[clap(flatten)]
    config_overrides: CliConfigOverrides,

    #[clap(flatten)]
    inner: Cli,
}

fn main() -> anyhow::Result<()> {
    // DEBUG: Early TUI entry point logging
    if std::env::var("CODEX_DEBUG").is_ok() {
        eprintln!("[DEBUG TUI] TUI main() starting");
        eprintln!("[DEBUG TUI] Environment variables:");
        eprintln!("[DEBUG TUI]   CODEX_DEBUG = {:?}", std::env::var("CODEX_DEBUG"));
        eprintln!("[DEBUG TUI]   XAI_API_KEY = {:?}", std::env::var("XAI_API_KEY").map(|_| "***SET***"));
        eprintln!("[DEBUG TUI]   HOME = {:?}", std::env::var("HOME"));
    }
    
    let top_cli = TopCli::parse();
    let mut inner = top_cli.inner;
    inner
        .config_overrides
        .raw_overrides
        .splice(0..0, top_cli.config_overrides.raw_overrides);
    
    if std::env::var("CODEX_DEBUG").is_ok() {
        eprintln!("[DEBUG TUI] About to call run_main()");
    }
    
    run_main(inner, None)?;
    Ok(())
}
