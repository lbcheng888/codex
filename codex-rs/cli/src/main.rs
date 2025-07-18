use clap::CommandFactory;
use clap::Parser;
use clap_complete::Shell;
use clap_complete::generate;
use codex_chatgpt::apply_command::ApplyCommand;
use codex_chatgpt::apply_command::run_apply_command;
use codex_cli::LandlockCommand;
use codex_cli::SeatbeltCommand;
use codex_cli::login::run_login_with_chatgpt;
use codex_cli::proto;
use codex_common::CliConfigOverrides;
use codex_exec::Cli as ExecCli;
use codex_file_search::Cli as FileSearchCli;
use codex_tui::Cli as TuiCli;
use std::path::PathBuf;

use crate::proto::ProtoCli;

/// Codex CLI
///
/// If no subcommand is specified, options will be forwarded to the interactive CLI.
#[derive(Debug, Parser)]
#[clap(
    author,
    version,
    // If a sub‑command is given, ignore requirements of the default args.
    subcommand_negates_reqs = true
)]
struct MultitoolCli {
    #[clap(flatten)]
    pub config_overrides: CliConfigOverrides,

    #[clap(flatten)]
    interactive: TuiCli,

    #[clap(subcommand)]
    subcommand: Option<Subcommand>,
}

#[derive(Debug, clap::Subcommand)]
enum Subcommand {
    /// Run Codex non-interactively.
    #[clap(visible_alias = "e")]
    Exec(ExecCli),

    /// Login with ChatGPT.
    Login(LoginCommand),

    /// Experimental: run Codex as an MCP server.
    Mcp,

    /// Run the Protocol stream via stdin/stdout
    #[clap(visible_alias = "p")]
    Proto(ProtoCli),

    /// Generate shell completion scripts.
    Completion(CompletionCommand),

    /// Internal debugging commands.
    Debug(DebugArgs),

    /// Apply the latest diff produced by Codex agent as a `git apply` to your local working tree.
    #[clap(visible_alias = "a")]
    Apply(ApplyCommand),

    /// Search for files in the current directory.
    #[clap(visible_alias = "s")]
    Search(FileSearchCli),
}

#[derive(Debug, Parser)]
struct CompletionCommand {
    /// Shell to generate completions for
    #[clap(value_enum, default_value_t = Shell::Bash)]
    shell: Shell,
}

#[derive(Debug, Parser)]
struct DebugArgs {
    #[command(subcommand)]
    cmd: DebugCommand,
}

#[derive(Debug, clap::Subcommand)]
enum DebugCommand {
    /// Run a command under Seatbelt (macOS only).
    Seatbelt(SeatbeltCommand),

    /// Run a command under Landlock+seccomp (Linux only).
    Landlock(LandlockCommand),
}

#[derive(Debug, Parser)]
struct LoginCommand {
    #[clap(skip)]
    config_overrides: CliConfigOverrides,
}

fn main() -> anyhow::Result<()> {
    tokio::runtime::Runtime::new()?.block_on(async {
        cli_main(None).await?;
        Ok(())
    })
}

async fn cli_main(codex_linux_sandbox_exe: Option<PathBuf>) -> anyhow::Result<()> {
    let cli = MultitoolCli::parse();

    match cli.subcommand {
        None => {
            let mut tui_cli = cli.interactive;
            prepend_config_flags(&mut tui_cli.config_overrides, cli.config_overrides);
            codex_tui::run_main(tui_cli, codex_linux_sandbox_exe)?;
        }
        Some(Subcommand::Exec(mut exec_cli)) => {
            prepend_config_flags(&mut exec_cli.config_overrides, cli.config_overrides);
            codex_exec::run_main(exec_cli, codex_linux_sandbox_exe).await?;
        }
        Some(Subcommand::Mcp) => {
            codex_mcp_server::run_main(codex_linux_sandbox_exe).await?;
        }
        Some(Subcommand::Login(mut login_cli)) => {
            prepend_config_flags(&mut login_cli.config_overrides, cli.config_overrides);
            run_login_with_chatgpt(login_cli.config_overrides).await;
        }
        Some(Subcommand::Proto(mut proto_cli)) => {
            prepend_config_flags(&mut proto_cli.config_overrides, cli.config_overrides);
            proto::run_main(proto_cli).await?;
        }
        Some(Subcommand::Completion(completion_cli)) => {
            print_completion(completion_cli);
        }
        Some(Subcommand::Debug(debug_args)) => match debug_args.cmd {
            DebugCommand::Seatbelt(mut seatbelt_cli) => {
                prepend_config_flags(&mut seatbelt_cli.config_overrides, cli.config_overrides);
                // Seatbelt functionality has been removed
            }
            DebugCommand::Landlock(mut landlock_cli) => {
                prepend_config_flags(&mut landlock_cli.config_overrides, cli.config_overrides);
                // Landlock functionality has been removed
            }
        },
        Some(Subcommand::Apply(mut apply_cli)) => {
            prepend_config_flags(&mut apply_cli.config_overrides, cli.config_overrides);
            run_apply_command(apply_cli).await?;
        }
        Some(Subcommand::Search(search_cli)) => {
            use codex_file_search::{run_main, Reporter, FileMatch};
            use std::io::IsTerminal;
            use codex_common::json_utils;
            use serde_json::json;

            // Create a simple reporter for CLI output
            struct CliReporter {
                json_output: bool,
                show_indices: bool,
            }

            impl Reporter for CliReporter {
                fn report_match(&self, file_match: &FileMatch) {
                    if self.json_output {
                        match json_utils::to_json_string(&file_match) {
                            Ok(json_str) => println!("{}", json_str),
                            Err(e) => eprintln!("Error serializing file match to JSON: {}", e),
                        }
                    } else {
                        println!("{}", file_match.path);
                    }
                }

                fn warn_matches_truncated(&self, total_match_count: usize, shown_match_count: usize) {
                    if self.json_output {
                        let value = json!({"matches_truncated": true});
                        match json_utils::to_json_string(&value) {
                            Ok(json_str) => println!("{}", json_str),
                            Err(e) => eprintln!("Error serializing warning to JSON: {}", e),
                        }
                    } else {
                        eprintln!(
                            "Warning: showing {shown_match_count} out of {total_match_count} results."
                        );
                    }
                }

                fn warn_no_search_pattern(&self, search_directory: &std::path::Path) {
                    eprintln!(
                        "No search pattern specified. Showing contents of: {}",
                        search_directory.to_string_lossy()
                    );
                }
            }

            let reporter = CliReporter {
                json_output: search_cli.json,
                show_indices: search_cli.compute_indices && std::io::stdout().is_terminal(),
            };

            run_main(search_cli, reporter).await?;
        }
    }

    Ok(())
}

/// Prepend root-level overrides so they have lower precedence than
/// CLI-specific ones specified after the subcommand (if any).
fn prepend_config_flags(
    subcommand_config_overrides: &mut CliConfigOverrides,
    cli_config_overrides: CliConfigOverrides,
) {
    subcommand_config_overrides
        .raw_overrides
        .splice(0..0, cli_config_overrides.raw_overrides);
}

fn print_completion(cmd: CompletionCommand) {
    let mut app = MultitoolCli::command();
    let name = "codex";
    generate(cmd.shell, &mut app, name, &mut std::io::stdout());
}
