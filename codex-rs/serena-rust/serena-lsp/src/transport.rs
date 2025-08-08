use std::collections::HashMap;
use std::process::Stdio;
use std::sync::Arc;
use tokio::io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader};
use tokio::process::{Child, Command};
use tokio::sync::{mpsc, oneshot};
use serde_json::{json, Value};
use tracing::{error, info, warn};
use crate::types::{LspError, LspResult};

/// JSON-RPC message types
#[derive(Debug, Clone)]
pub enum JsonRpcMessage {
    Request {
        id: i64,
        method: String,
        params: Option<Value>,
    },
    Response {
        id: i64,
        result: Option<Value>,
        error: Option<Value>,
    },
    Notification {
        method: String,
        params: Option<Value>,
    },
}

/// Transport layer for JSON-RPC over stdio communication with language servers
pub struct LspTransport {
    process: Child,
    request_id: i64,
    pending_requests: Arc<tokio::sync::Mutex<HashMap<i64, oneshot::Sender<Value>>>>,
    sender: mpsc::UnboundedSender<JsonRpcMessage>,
    #[allow(dead_code)]
    receiver: mpsc::UnboundedReceiver<JsonRpcMessage>,
}

impl LspTransport {
    /// Start a new language server process and establish communication
    pub async fn start(command: Vec<String>, working_dir: Option<String>, env: Option<HashMap<String, String>>) -> LspResult<Self> {
        if command.is_empty() {
            return Err(LspError::InvalidRequest("Empty command".to_string()));
        }
        
        let mut cmd = Command::new(&command[0]);
        cmd.args(&command[1..])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
            
        if let Some(dir) = working_dir {
            cmd.current_dir(dir);
        }
        
        if let Some(env_vars) = env {
            for (key, value) in env_vars {
                cmd.env(key, value);
            }
        }
        
        let mut process = cmd.spawn().map_err(|e| {
            LspError::InitializationFailed(format!("Failed to start language server: {}", e))
        })?;
        
        let stdin = process.stdin.take().ok_or_else(|| {
            LspError::InitializationFailed("Failed to get stdin".to_string())
        })?;
        
        let stdout = process.stdout.take().ok_or_else(|| {
            LspError::InitializationFailed("Failed to get stdout".to_string())
        })?;
        
        let (sender, receiver) = mpsc::unbounded_channel();
        let (response_sender, mut response_receiver) = mpsc::unbounded_channel();
        
        // Start the read task
        let read_sender = response_sender.clone();
        tokio::spawn(async move {
            Self::read_task(stdout, read_sender).await;
        });
        
        // Start the write task
        let write_sender = sender.clone();
        let write_receiver = receiver;
        tokio::spawn(async move {
            Self::write_task(stdin, write_receiver).await;
        });
        
        let pending_requests = Arc::new(tokio::sync::Mutex::new(HashMap::<i64, oneshot::Sender<Value>>::new()));
        let pending_requests_clone = pending_requests.clone();
        
        // Start the response routing task
        tokio::spawn(async move {
            while let Some(message) = response_receiver.recv().await {
                match message {
                    JsonRpcMessage::Response { id, result, error } => {
                        let mut pending = pending_requests_clone.lock().await;
                        if let Some(sender) = pending.remove(&id) {
                            let response = if let Some(error) = error {
                                // For now, we'll send the error as the result
                                // In a full implementation, we might want to handle errors differently
                                error
                            } else {
                                result.unwrap_or(json!(null))
                            };
                            let _ = sender.send(response);
                        }
                    }
                    JsonRpcMessage::Notification { method: _, params: _ } => {
                        // Handle notifications if needed (for now, ignore)
                    }
                    JsonRpcMessage::Request { id: _, method: _, params: _ } => {
                        // Handle requests from language server if needed (for now, ignore)
                    }
                }
            }
        });
        
        Ok(Self {
            process,
            request_id: 0,
            pending_requests,
            sender: write_sender,
            receiver: mpsc::unbounded_channel().1, // Dummy receiver, not used anymore
        })
    }
    
    /// Send a request and wait for response
    pub async fn send_request(&mut self, method: String, params: Option<Value>) -> LspResult<Value> {
        self.request_id += 1;
        let id = self.request_id;
        
        let (response_sender, response_receiver) = oneshot::channel();
        {
            let mut pending = self.pending_requests.lock().await;
            pending.insert(id, response_sender);
        }
        
        let message = JsonRpcMessage::Request { id, method, params };
        self.sender.send(message).map_err(|_| {
            LspError::CommunicationError("Failed to send request".to_string())
        })?;
        
        // Wait for response with timeout
        let result = tokio::time::timeout(
            std::time::Duration::from_secs(30),
            response_receiver
        ).await;
        
        match result {
            Ok(Ok(response)) => Ok(response),
            Ok(Err(_)) => Err(LspError::CommunicationError("Response channel closed".to_string())),
            Err(_) => {
                // Remove the pending request on timeout
                let mut pending = self.pending_requests.lock().await;
                pending.remove(&id);
                Err(LspError::CommunicationError("Request timeout".to_string()))
            }
        }
    }
    
    /// Send a notification (no response expected)
    pub async fn send_notification(&self, method: String, params: Option<Value>) -> LspResult<()> {
        let message = JsonRpcMessage::Notification { method, params };
        self.sender.send(message).map_err(|_| {
            LspError::CommunicationError("Failed to send notification".to_string())
        })?;
        Ok(())
    }
    
    /// Check if the language server process is still running
    pub fn is_alive(&mut self) -> bool {
        match self.process.try_wait() {
            Ok(Some(_)) => false, // Process has exited
            Ok(None) => true,     // Process is still running
            Err(_) => false,      // Error checking process status
        }
    }
    
    /// Gracefully shutdown the language server
    pub async fn shutdown(&mut self) -> LspResult<()> {
        // Send shutdown request
        let _ = self.send_request("shutdown".to_string(), None).await;
        
        // Send exit notification
        let _ = self.send_notification("exit".to_string(), None).await;
        
        // Wait for process to exit with timeout
        let result = tokio::time::timeout(
            std::time::Duration::from_secs(5),
            self.process.wait()
        ).await;
        
        match result {
            Ok(Ok(status)) => {
                info!("Language server exited with status: {}", status);
                Ok(())
            }
            Ok(Err(e)) => {
                error!("Error waiting for language server to exit: {}", e);
                Err(LspError::CommunicationError(format!("Exit error: {}", e)))
            }
            Err(_) => {
                warn!("Language server did not exit within timeout, killing process");
                let _ = self.process.kill().await;
                Ok(())
            }
        }
    }
    
    /// Task to read messages from language server stdout
    async fn read_task(stdout: tokio::process::ChildStdout, sender: mpsc::UnboundedSender<JsonRpcMessage>) {
        let mut reader = BufReader::new(stdout);
        let mut buffer = String::new();
        
        loop {
            buffer.clear();
            
            // Read Content-Length header
            match reader.read_line(&mut buffer).await {
                Ok(0) => break, // EOF
                Ok(_) => {
                    if buffer.trim().is_empty() {
                        continue;
                    }
                    
                    if let Some(content_length) = Self::parse_content_length(&buffer) {
                        // Read the empty line after the header
                        buffer.clear();
                        if reader.read_line(&mut buffer).await.is_err() {
                            break;
                        }
                        
                        // Read the JSON content
                        let mut content = vec![0u8; content_length];
                        if reader.read_exact(&mut content).await.is_ok() {
                            if let Ok(content_str) = String::from_utf8(content) {
                                if let Ok(json_value) = serde_json::from_str::<Value>(&content_str) {
                                    if let Some(message) = Self::parse_json_rpc_message(json_value) {
                                        if sender.send(message).is_err() {
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                Err(_) => break,
            }
        }
    }
    
    /// Task to write messages to language server stdin
    async fn write_task(mut stdin: tokio::process::ChildStdin, mut receiver: mpsc::UnboundedReceiver<JsonRpcMessage>) {
        while let Some(message) = receiver.recv().await {
            if let Ok(json_str) = Self::format_json_rpc_message(message) {
                let content = format!("Content-Length: {}\r\n\r\n{}", json_str.len(), json_str);
                if stdin.write_all(content.as_bytes()).await.is_err() {
                    break;
                }
                if stdin.flush().await.is_err() {
                    break;
                }
            }
        }
    }
    
    /// Parse Content-Length header
    fn parse_content_length(header: &str) -> Option<usize> {
        if header.starts_with("Content-Length:") {
            header
                .trim_start_matches("Content-Length:")
                .trim()
                .parse::<usize>()
                .ok()
        } else {
            None
        }
    }
    
    /// Parse JSON-RPC message from JSON value
    fn parse_json_rpc_message(value: Value) -> Option<JsonRpcMessage> {
        let obj = value.as_object()?;
        
        if let Some(id) = obj.get("id") {
            let id = id.as_i64()?;
            
            if obj.contains_key("method") {
                // Request
                let method = obj.get("method")?.as_str()?.to_string();
                let params = obj.get("params").cloned();
                Some(JsonRpcMessage::Request { id, method, params })
            } else {
                // Response
                let result = obj.get("result").cloned();
                let error = obj.get("error").cloned();
                Some(JsonRpcMessage::Response { id, result, error })
            }
        } else if let Some(method) = obj.get("method") {
            // Notification
            let method = method.as_str()?.to_string();
            let params = obj.get("params").cloned();
            Some(JsonRpcMessage::Notification { method, params })
        } else {
            None
        }
    }
    
    /// Format JSON-RPC message to string
    fn format_json_rpc_message(message: JsonRpcMessage) -> Result<String, serde_json::Error> {
        let json_obj = match message {
            JsonRpcMessage::Request { id, method, params } => {
                let mut obj = json!({
                    "jsonrpc": "2.0",
                    "id": id,
                    "method": method
                });
                if let Some(params) = params {
                    obj["params"] = params;
                }
                obj
            }
            JsonRpcMessage::Response { id, result, error } => {
                let mut obj = json!({
                    "jsonrpc": "2.0",
                    "id": id
                });
                if let Some(result) = result {
                    obj["result"] = result;
                }
                if let Some(error) = error {
                    obj["error"] = error;
                }
                obj
            }
            JsonRpcMessage::Notification { method, params } => {
                let mut obj = json!({
                    "jsonrpc": "2.0",
                    "method": method
                });
                if let Some(params) = params {
                    obj["params"] = params;
                }
                obj
            }
        };
        
        serde_json::to_string(&json_obj)
    }

    /// Stop the language server process (alias for shutdown)
    pub async fn stop(&mut self) -> LspResult<()> {
        self.shutdown().await
    }
}