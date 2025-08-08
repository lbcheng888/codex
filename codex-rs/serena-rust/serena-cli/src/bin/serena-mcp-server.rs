use std::path::PathBuf;
use std::sync::Arc;
use clap::Parser;
use serena_core::{observability::init_tracing_silent, RuntimeConfig};
use serena_core::tools::ToolRegistry;
use serena_mcp::{McpOptions, McpServer};
use serena_tools::{
    FileRead, FileWrite, FileExists, FileList, Replace, Search, ListDir, FindFile, DeleteLines, ReplaceLines, InsertAtLine,
    UserCreate, UserGet,
    ReadMemory, WriteMemory, ListMemories, DeleteMemory,
    GetSymbolsOverview, FindSymbol, FindReferencingSymbols, ReplaceSymbolBody, InsertAfterSymbol, InsertBeforeSymbol,
    ActivateProject, SwitchModes, CheckOnboardingPerformed, Onboarding, RemoveProject, GetCurrentConfig,
    RestartLanguageServerAll, RestartLanguageServerLanguage,
    ProxyPythonTool, SleepTool,
};

/// Minimal CLI to start the MCP server using the full ToolRegistry-backed implementation.
#[derive(Parser, Debug)]
#[command(name = "serena-mcp-server", version, about = "Serena MCP Server (ToolRegistry-backed stdio)")]
struct Args {
    /// Project root (optional)
    #[arg(long)]
    project: Option<PathBuf>,
    /// Working directory (chdir before start, optional)
    #[arg(long)]
    cwd: Option<PathBuf>,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // Optional chdir
    if let Some(cwd) = &args.cwd {
        std::env::set_current_dir(cwd)
            .map_err(|e| anyhow::anyhow!("Failed to change directory to {:?}: {}", cwd, e))?;
    }

    // Silent tracing to avoid protocol pollution
    let mut cfg = RuntimeConfig::default();
    cfg.apply_env_overrides()?;
    init_tracing_silent(&cfg);

    // Build full registry with real tools
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
    // Symbol tools (LSP)
    reg.register(GetSymbolsOverview);
    reg.register(FindSymbol);
    reg.register(FindReferencingSymbols);
    reg.register(ReplaceSymbolBody);
    reg.register(InsertAfterSymbol);
    reg.register(InsertBeforeSymbol);
    // Config tools
    reg.register(ActivateProject);
    reg.register(SwitchModes);
    reg.register(CheckOnboardingPerformed);
    reg.register(Onboarding);
    reg.register(RemoveProject);
    reg.register(GetCurrentConfig);
    // LSP restart tools
    reg.register(RestartLanguageServerAll);
    reg.register(RestartLanguageServerLanguage);

    // Proxy tools
    reg.register(ProxyPythonTool);

    // Deterministic testing utility tools
    reg.register(SleepTool);

    let project_root_str = args.project.as_ref().map(|p| p.to_string_lossy().to_string());

    let opts = McpOptions {
        ready_line: false,
        transport: Some("stdio".to_string()),
        project_root: project_root_str,
    };

    let cfg_arc = Arc::new(cfg);
    let server = McpServer::new(cfg_arc, reg, opts);
    server.run_stdio().await?;
    Ok(())
}
