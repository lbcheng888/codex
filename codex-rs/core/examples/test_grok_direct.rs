use tokio;
use codex_core::config::{Config, ConfigOverrides};
use codex_core::codex_wrapper::init_codex;
use codex_core::protocol::{Op, InputItem};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Enable debug logging
    unsafe {
        std::env::set_var("CODEX_DEBUG", "1");
    }
    
    println!("Starting direct Grok test...");
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
    println!("  Base URL: {}", config.model_provider.base_url);
    
    // Initialize Codex
    println!("Initializing Codex...");
    let (codex, session_event, _ctrl_c) = init_codex(config).await?;
    println!("Codex initialized successfully!");
    println!("Session event: {:?}", session_event);
    
    // Submit a simple text command
    println!("Submitting 'ls' command...");
    let op = Op::UserInput {
        items: vec![InputItem::Text {
            text: "ls".to_string(),
        }],
    };
    
    let submission_id = codex.submit(op).await?;
    println!("Submission ID: {}", submission_id);
    
    // Listen for events with timeout
    println!("Waiting for events...");
    let mut event_count = 0;
    let timeout_duration = std::time::Duration::from_secs(30);
    let start_time = std::time::Instant::now();
    
    while start_time.elapsed() < timeout_duration {
        match tokio::time::timeout(std::time::Duration::from_secs(5), codex.next_event()).await {
            Ok(Ok(event)) => {
                event_count += 1;
                println!("Event {}: {:?}", event_count, event);
                
                // If we see a TaskComplete or error, break
                match &event.msg {
                    codex_core::protocol::EventMsg::TaskComplete(_) => {
                        println!("Task completed!");
                        break;
                    }
                    codex_core::protocol::EventMsg::Error(err) => {
                        println!("Error received: {}", err.message);
                        break;
                    }
                    _ => {}
                }
            }
            Ok(Err(e)) => {
                println!("Event error: {}", e);
                break;
            }
            Err(_) => {
                println!("Timeout waiting for event");
            }
        }
    }
    
    println!("Test completed. Total events received: {}", event_count);
    Ok(())
}