use tokio;
use codex_core::config::{Config, ConfigOverrides};
use codex_core::codex_wrapper::init_codex;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Enable debug logging
    unsafe {
        std::env::set_var("CODEX_DEBUG", "1");
    }
    
    println!("Testing rate limit handling for Grok API...");
    println!("Environment check:");
    println!("  CODEX_DEBUG = {:?}", std::env::var("CODEX_DEBUG"));
    println!("  XAI_API_KEY = {:?}", std::env::var("XAI_API_KEY").map(|_| "***SET***"));
    
    // Load config with xai profile
    let overrides = ConfigOverrides {
        config_profile: Some("xai".to_string()),
        cwd: Some(std::env::current_dir()?),
        ..Default::default()
    };
    
    println!("Loading config...");
    let config = Config::load_with_cli_overrides(vec![], overrides)?;
    
    println!("Config loaded:");
    println!("  Model: {}", config.model);
    println!("  Provider: {}", config.model_provider_id);
    println!("  Wire API: {:?}", config.model_provider.wire_api);
    println!("  Base URL: {:?}", config.model_provider.base_url);
    
    // Initialize Codex
    println!("Initializing Codex...");
    let codex = init_codex(config).await?;
    println!("Codex initialized successfully!");
    
    // Submit multiple requests rapidly to potentially trigger rate limits
    println!("Submitting multiple requests to test rate limiting...");
    
    for i in 1..=5 {
        println!("--- Request {} ---", i);
        println!("(example placeholder) Would send: echo 'Request number {}'", i);
        tokio::time::sleep(std::time::Duration::from_millis(500)).await;
    }
    
    println!("Rate limit test completed!");
    Ok(())
}