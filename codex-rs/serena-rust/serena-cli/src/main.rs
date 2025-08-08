use clap::{Parser, Subcommand};
use serena_core::{observability::{init_tracing, init_tracing_silent}, RuntimeConfig};
use serena_mcp::{McpOptions, McpServer};
use serena_tools::{FileRead, FileWrite, FileExists, FileList, Replace, Search, ListDir, FindFile, DeleteLines, ReplaceLines, InsertAtLine};
use serena_tools::{UserCreate, UserGet};
use serena_tools::{ReadMemory, WriteMemory, ListMemories, DeleteMemory};
use serena_tools::{GetSymbolsOverview, FindSymbol, FindReferencingSymbols, ReplaceSymbolBody, InsertAfterSymbol, InsertBeforeSymbol, RestartLanguageServerAll, RestartLanguageServerLanguage};
use serena_tools::{ActivateProject, SwitchModes, CheckOnboardingPerformed, Onboarding, RemoveProject, GetCurrentConfig};
// TODO: Enable these tools once registration issue is resolved
// use serena_tools::{ExecuteShellCommand, RestartLanguageServer};
// use serena_tools::{ThinkAboutCollectedInformation, ThinkAboutTaskAdherence, ThinkAboutWhetherYouAreDone, SummarizeChanges};
// use serena_tools::{PrepareForNewConversation, InitialInstructions, ManageWorkflowState};
use serena_core::tools::ToolRegistry;
use std::path::PathBuf;
use std::sync::Arc;
use tracing::info;

/// Serena Rust Port CLI
#[derive(Parser, Debug)]
#[command(
    name = "serena-cli",
    version,
    about = "Serena Rust Port CLI\n\nEnvironment variables:\n  SERENA_DISABLE_LSP=1   Disable LSP backends during MCP startup (useful for tests or fast boot)\n  RUST_LOG=info|debug    Standard Rust tracing filter\n\nCommon troubleshooting:\n  • If MCP stdio handshake reports 'missing field id', ensure your client sends JSON-RPC lines with an 'id'.\n  • For local E2E runs, prefer running from repo root so target/ paths resolve correctly.",
    long_about = None
)]
struct Cli {
    /// Config file path (yaml/json)
    #[arg(long, value_name="PATH", help="Path to runtime config file (yaml/json)")]
    config: Option<PathBuf>,
    /// Log level (overrides config)
    #[arg(long, value_name="LEVEL", help="Override log level (e.g. error|warn|info|debug|trace)")]
    log_level: Option<String>,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Starts the Serena MCP server (stdio transport).
    ///
    /// Examples:
    ///   serena-cli start-mcp-server --context ide-assistant --transport stdio --cwd /path/to/project
    ///   RUST_LOG=info SERENA_DISABLE_LSP=1 serena-cli start-mcp-server
    StartMcpServer {
        /// Context for the MCP server (ide-assistant, desktop-app, agent)
        #[arg(long, default_value = "desktop-app", value_name="CONTEXT")]
        context: String,
        /// Modes to activate (editing, interactive, planning, one-shot). Can be repeated.
        #[arg(long, action = clap::ArgAction::Append, value_name="MODE")]
        mode: Vec<String>,
        /// Project path to activate (equivalent to activating workspace/project)
        #[arg(long, value_name="PATH")]
        project: Option<PathBuf>,
        /// Working directory for the MCP server (server process chdir before start)
        #[arg(long, value_name="PATH")]
        cwd: Option<PathBuf>,
        /// Transport type (stdio, sse)
        #[arg(long, default_value = "stdio", value_name="TRANSPORT")]
        transport: String,
        /// Port for SSE transport (only used when --transport sse)
        #[arg(long, default_value = "8080", value_name="PORT")]
        port: u16,
    },
    /// Lists all available tools with brief descriptions.
    ///
    /// Example:
    ///   serena-cli list-tools
    ListTools,
    /// Export tool schemas to docs/tool_schemas.md (overwrites file).
    ///
    /// Example:
    ///   serena-cli export-tool-schemas
    ExportToolSchemas,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    

    let cli = Cli::parse();
    let mut cfg = if let Some(path) = cli.config.clone() {
        RuntimeConfig::load_from_file(&path).unwrap_or_default()
    } else {
        RuntimeConfig::default()
    };
    if let Some(l) = cli.log_level.clone() {
        cfg.log_level = l;
    }
    cfg.apply_env_overrides()?;

    // 在 stdio 与 content-length 两种传输下都保持静默，避免任何 stderr 污染。
    // 强制写 /tmp/serena_mcp_augment.log（不依赖环境变量），确保总能采集日志。
    let is_mcp = cli.command.as_ref()
        .map(|cmd| matches!(cmd, Commands::StartMcpServer { .. }))
        .unwrap_or(true);

    if is_mcp {
        let trace_path = "/tmp/serena_mcp_augment.log".to_string();
        // 强制将关键启动信息写入文件，禁用 stderr 输出（不污染协议流）
        let _ = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&trace_path)
            .map(|file| {
                use std::io::Write as IoWrite;
                let mut f = file;
                let _ = writeln!(f, "[mcp][trace] start serena-cli (forced file), trace_file={}, level={}", trace_path, cfg.log_level);
            });
        // 静默初始化，保持 stdout/stderr 无非协议输出
        init_tracing_silent(&cfg);
    } else {
        init_tracing(&cfg);
        info!("starting with config: level={} transport=stdio", cfg.log_level);
    }

    match cli.command.unwrap_or(Commands::StartMcpServer {
        context: "desktop-app".to_string(),
        mode: vec![],
        project: None,
        cwd: None,
        transport: "stdio".to_string(),
        port: 8080,
    }) {
        Commands::StartMcpServer { context: _context, mode: _mode, project, cwd, transport, port: _ } => {
            // 如果指定了工作目录，切换到该目录
            if let Some(cwd_path) = &cwd {
                std::env::set_current_dir(cwd_path)
                    .map_err(|e| anyhow::anyhow!("Failed to change directory to {:?}: {}", cwd_path, e))?;
                info!("Changed working directory to: {:?}", cwd_path);
            }

            // MCP 模式下一律不输出启动日志，完全静默（防止某些 wrapper 误判）
            let cfg_arc = Arc::new(cfg);
            let reg = ToolRegistry::new();
            reg.register(FileRead);
            reg.register(FileWrite);
            reg.register(Search);
            reg.register(FileExists);
            reg.register(Replace);
            reg.register(ListDir);
            reg.register(FileList);
            reg.register(FindFile);
            reg.register(DeleteLines);
            reg.register(ReplaceLines);
            reg.register(InsertAtLine);
            reg.register(UserCreate);
            reg.register(UserGet);
            
            // Memory tools
            reg.register(ReadMemory);
            reg.register(WriteMemory);
            reg.register(ListMemories);
            reg.register(DeleteMemory);
            
            // Symbol tools with LSP integration
            reg.register(GetSymbolsOverview);
            reg.register(FindSymbol);
            reg.register(FindReferencingSymbols);
            reg.register(ReplaceSymbolBody);
            reg.register(InsertAfterSymbol);
            reg.register(InsertBeforeSymbol);
            
            // Configuration tools
            reg.register(ActivateProject);
            reg.register(SwitchModes);
            reg.register(CheckOnboardingPerformed);
            reg.register(Onboarding);
            reg.register(RemoveProject);
            reg.register(GetCurrentConfig);

            // LSP restart tools
            reg.register(RestartLanguageServerAll);
            reg.register(RestartLanguageServerLanguage);

            // TODO: Enable new tools once registration issue is resolved
            // reg.register(ExecuteShellCommand);
            // reg.register(RestartLanguageServer);
            // reg.register(ThinkAboutCollectedInformation);
            // reg.register(ThinkAboutTaskAdherence);
            // reg.register(ThinkAboutWhetherYouAreDone);
            // reg.register(SummarizeChanges);
            // reg.register(PrepareForNewConversation);
            // reg.register(InitialInstructions);
            // reg.register(ManageWorkflowState);

            let project_root_str = project
                .as_ref()
                .map(|p| p.to_string_lossy().to_string());

            let opts = McpOptions {
                ready_line: false,                // 禁用ready消息，符合标准MCP协议
                transport: Some(transport),       // 传输模式
                project_root: project_root_str,   // 项目根路径
            };
            let server = McpServer::new(cfg_arc.clone(), reg, opts);
            server.run_stdio().await?;
        }
        Commands::ListTools => {
            println!("DEBUG: Starting ListTools command");
            let _cfg = Arc::new(cfg);
            let reg = ToolRegistry::new();
            println!("DEBUG: ToolRegistry created");

            // Register all tools (same as above)
            reg.register(FileRead);
            reg.register(FileWrite);
            reg.register(Search);
            reg.register(FileExists);
            reg.register(Replace);
            reg.register(ListDir);
            reg.register(FileList);
            reg.register(FindFile);
            reg.register(DeleteLines);
            reg.register(ReplaceLines);
            reg.register(InsertAtLine);
            reg.register(UserCreate);
            reg.register(UserGet);
            reg.register(ReadMemory);
            reg.register(WriteMemory);
            reg.register(ListMemories);
            reg.register(DeleteMemory);
            reg.register(GetSymbolsOverview);
            reg.register(FindSymbol);
            reg.register(FindReferencingSymbols);
            reg.register(ReplaceSymbolBody);
            reg.register(InsertAfterSymbol);
            reg.register(InsertBeforeSymbol);
            reg.register(ActivateProject);
            reg.register(SwitchModes);
            reg.register(CheckOnboardingPerformed);
            reg.register(Onboarding);
            reg.register(RemoveProject);
            reg.register(GetCurrentConfig);

            // LSP restart tools
            reg.register(RestartLanguageServerAll);
            reg.register(RestartLanguageServerLanguage);

            // TODO: Enable new tools once registration issue is resolved
            // reg.register(ExecuteShellCommand);

            // List all tools
            let tools = reg.list_mcp_format();
            println!("Available tools ({}):", tools.len());

            // Check if ExecuteShellCommand is registered
            let has_execute = tools.iter().any(|t|
                t.get("name").and_then(|n| n.as_str()) == Some("execute_shell_command")
            );
            println!("ExecuteShellCommand found: {}", has_execute);

            for tool in tools {
                let name = tool.get("name").and_then(|n| n.as_str()).unwrap_or("unknown");
                let desc = tool.get("description").and_then(|d| d.as_str()).unwrap_or("no description");
                println!("  - {} ({})", name, desc);
            }
        }
        Commands::ExportToolSchemas => {
            // Initialize config and registry similar to ListTools to ensure identical tool set
            let cfg = RuntimeConfig::default();
            let _cfg_arc = Arc::new(cfg);
            let reg = ToolRegistry::new();

            // Register all tools (keep in sync with StartMcpServer and ListTools)
            reg.register(FileRead);
            reg.register(FileWrite);
            reg.register(Search);
            reg.register(FileExists);
            reg.register(Replace);
            reg.register(ListDir);
            reg.register(FileList);
            reg.register(FindFile);
            reg.register(DeleteLines);
            reg.register(ReplaceLines);
            reg.register(InsertAtLine);
            reg.register(UserCreate);
            reg.register(UserGet);
            reg.register(ReadMemory);
            reg.register(WriteMemory);
            reg.register(ListMemories);
            reg.register(DeleteMemory);
            reg.register(GetSymbolsOverview);
            reg.register(FindSymbol);
            reg.register(FindReferencingSymbols);
            reg.register(ReplaceSymbolBody);
            reg.register(InsertAfterSymbol);
            reg.register(InsertBeforeSymbol);
            reg.register(ActivateProject);
            reg.register(SwitchModes);
            reg.register(CheckOnboardingPerformed);
            reg.register(Onboarding);
            reg.register(RemoveProject);
            reg.register(RestartLanguageServerAll);
            reg.register(RestartLanguageServerLanguage);
            reg.register(GetCurrentConfig);
            // TODO: Enable new tools once registration issue is resolved
            // reg.register(ExecuteShellCommand);

            // Render Markdown
            let tools = reg.list();
            let mut md = String::new();
            md.push_str("# Serena Tool Schemas\n\n");
            md.push_str("> This file is generated by `serena-cli export-tool-schemas`. Manual edits may be overwritten.\n\n");
            md.push_str(&format!("Total tools: {}\n\n", tools.len()));
            for (tool_name, schema) in tools {
                md.push_str(&format!("## {}\n\n", tool_name));
                if let Some(desc) = schema.get("description").and_then(|d| d.as_str()) {
                    md.push_str(&format!("{}  \n\n", desc));
                }
                md.push_str("```json\n");
                md.push_str(&serde_json::to_string_pretty(&schema).unwrap_or_else(|_| "{}".to_string()));
                md.push_str("\n```\n\n");
            }

            // Ensure docs directory exists and write file
            let out_path = std::path::Path::new("docs").join("tool_schemas.md");
            if let Some(parent) = out_path.parent() {
                std::fs::create_dir_all(parent)
                    .map_err(|e| anyhow::anyhow!("Failed to create docs directory {:?}: {}", parent, e))?;
            }
            std::fs::write(&out_path, md)
                .map_err(|e| anyhow::anyhow!("Failed to write {:?}: {}", out_path, e))?;
            println!("Wrote tool schemas to {:?}", out_path);
        }
    }
    Ok(())
}