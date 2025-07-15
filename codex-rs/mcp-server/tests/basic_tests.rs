//! Basic tests for the MCP server
//!
//! These tests ensure basic functionality without complex protocol interactions.

use std::path::PathBuf;
use tokio::sync::mpsc;
use mcp_types::JSONRPCMessage;
use codex_mcp_server::message_processor::MessageProcessor;

#[test]
fn test_message_processor_creation() {
    let (outgoing_tx, _outgoing_rx) = mpsc::channel::<JSONRPCMessage>(128);
    let processor = MessageProcessor::new(outgoing_tx, None);
    
    // Just ensure we can create the processor without panicking
    drop(processor);
}

#[test]
fn test_message_processor_with_sandbox_exe() {
    let (outgoing_tx, _outgoing_rx) = mpsc::channel::<JSONRPCMessage>(128);
    let sandbox_exe = Some(PathBuf::from("/usr/bin/test"));
    let processor = MessageProcessor::new(outgoing_tx, sandbox_exe);
    
    // Just ensure we can create the processor with sandbox exe without panicking
    drop(processor);
}

#[tokio::test]
async fn test_channel_communication() {
    let (outgoing_tx, mut outgoing_rx) = mpsc::channel::<JSONRPCMessage>(128);
    
    // Test that we can send messages through the channel
    tokio::spawn(async move {
        // Create a simple message to test channel functionality
        let test_message = JSONRPCMessage::Response(mcp_types::JSONRPCResponse {
            jsonrpc: "2.0".to_string(),
            id: mcp_types::RequestId::String("test".to_string()),
            result: serde_json::json!({"test": "value"}),
        });
        
        let _ = outgoing_tx.send(test_message).await;
    });
    
    // Receive the message
    let received = outgoing_rx.recv().await;
    assert!(received.is_some());
    
    match received.unwrap() {
        JSONRPCMessage::Response(response) => {
            assert_eq!(response.jsonrpc, "2.0");
            assert_eq!(response.id, mcp_types::RequestId::String("test".to_string()));
        }
        _ => panic!("Expected response message"),
    }
}

#[test]
fn test_constants() {
    // Test that basic constants are available
    assert_eq!(mcp_types::JSONRPC_VERSION, "2.0");
    assert_eq!(mcp_types::MCP_SCHEMA_VERSION, "2025-03-26");
}

#[tokio::test]
async fn test_library_exports() {
    // Test that we can access the main run function
    // (without actually running it, since it would block on stdin)
    
    // Just test that the function signature is available
    let result = std::panic::catch_unwind(|| {
        // This won't actually run but tests compilation
        let _: fn(Option<PathBuf>) -> _ = codex_mcp_server::run_main;
    });
    
    assert!(result.is_ok());
}