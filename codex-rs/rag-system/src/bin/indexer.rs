use anyhow::Result;
use clap::Parser;
use std::path::PathBuf;
use std::time::Instant;

use rag_system::RagSystem;
use rag_system::config::RagConfig;

#[derive(Parser)]
#[command(name = "rag-indexer")]
#[command(about = "Index codebase for RAG system")]
#[command(version = "1.0")]
struct Cli {
    /// Path to codebase directory
    #[arg(short, long)]
    path: PathBuf,

    /// Configuration file path
    #[arg(short, long, default_value = "rag_config.json")]
    config: PathBuf,

    /// Verbose output
    #[arg(short, long)]
    verbose: bool,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize logging
    tracing_subscriber::fmt::init();

    // Load configuration
    let config = if cli.config.exists() {
        RagConfig::from_file(&cli.config)?
    } else {
        let default_config = RagConfig::default();
        default_config.save_to_file(&cli.config)?;
        tracing::info!("Created default configuration file: {:?}", cli.config);
        default_config
    };

    // Validate path
    if !cli.path.exists() {
        anyhow::bail!("Path does not exist: {:?}", cli.path);
    }

    if !cli.path.is_dir() {
        anyhow::bail!("Path is not a directory: {:?}", cli.path);
    }

    // Initialize RAG system
    let start_time = Instant::now();
    tracing::info!("Initializing RAG system...");
    
    let rag_system = RagSystem::new(config).await?;

    // Run indexing
    tracing::info!("Starting indexing...");
    rag_system.index_codebase(&cli.path).await?;

    let duration = start_time.elapsed();
    tracing::info!("Indexing completed in {:?}", duration);

    Ok(())
}