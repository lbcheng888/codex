use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::RwLock;

use rag_system::{RagSystem, config::RagConfig};

#[derive(Debug, Serialize, Deserialize)]
struct MCPRequest {
    method: String,
    params: serde_json::Value,
}

#[derive(Debug, Serialize, Deserialize)]
struct MCPResponse {
    result: Option<serde_json::Value>,
    error: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct SearchParams {
    query: String,
    limit: Option<usize>,
    language: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct IndexParams {
    path: String,
}

struct MCPServer {
    rag_system: Arc<RagSystem>,
}

impl MCPServer {
    async fn new() -> Result<Self> {
        let config = RagConfig::default();
        let rag_system = Arc::new(RagSystem::new(config).await?);
        
        Ok(Self { rag_system })
    }

    async fn handle_request(&self, request: MCPRequest) -> MCPResponse {
        match request.method.as_str() {
            "search_code" => {
                let params: SearchParams = serde_json::from_value(request.params)
                    .unwrap_or_else(|_| SearchParams {
                        query: String::new(),
                        limit: Some(5),
                        language: None,
                    });
                
                match self.search_code(params).await {
                    Ok(results) => MCPResponse {
                        result: Some(serde_json::json!(results)),
                        error: None,
                    },
                    Err(e) => MCPResponse {
                        result: None,
                        error: Some(e.to_string()),
                    },
                }
            },
            "index_directory" => {
                let params: IndexParams = serde_json::from_value(request.params)
                    .unwrap_or_else(|_| IndexParams {
                        path: String::new(),
                    });
                
                match self.index_directory(params).await {
                    Ok(_) => MCPResponse {
                        result: Some(serde_json::json!({"status": "success"})),
                        error: None,
                    },
                    Err(e) => MCPResponse {
                        result: None,
                        error: Some(e.to_string()),
                    },
                }
            },
            "get_stats" => {
                match self.get_stats().await {
                    Ok(stats) => MCPResponse {
                        result: Some(serde_json::json!(stats)),
                        error: None,
                    },
                    Err(e) => MCPResponse {
                        result: None,
                        error: Some(e.to_string()),
                    },
                }
            },
            _ => MCPResponse {
                result: None,
                error: Some("Unknown method".to_string()),
            },
        }
    }

    async fn search_code(&self, params: SearchParams) -> Result<Vec<serde_json::Value>> {
        let mut results = self.rag_system.query(&params.query, params.limit.unwrap_or(5)).await?;
        
        // 限制返回内容长度
        results.iter_mut().for_each(|r| {
            if r.content.len() > 500 {
                r.content = format!("{}...", &r.content[..500]);
            }
        });
        
        let json_results: Vec<serde_json::Value> = results.into_iter()
            .map(|r| serde_json::json!({
                "file_path": r.file_path.to_string_lossy().to_string(),
                "language": format!("{:?}", r.language),
                "content": r.content,
                "dependencies": r.dependencies,
                "imports": r.imports,
                "lines": r.content.lines().count(),
            }))
            .collect();
        
        Ok(json_results)
    }

    async fn index_directory(&self, params: IndexParams) -> Result<()> {
        let path = std::path::Path::new(&params.path);
        self.rag_system.index_codebase(path).await?;
        Ok(())
    }

    async fn get_stats(&self) -> Result<serde_json::Value> {
        // 返回简单的统计信息
        Ok(serde_json::json!({
            "status": "running",
            "indexed_files": 0, // 实际统计需要数据库支持
            "languages": ["Rust", "Kotlin", "Java"],
            "version": "1.0.0"
        }))
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    // 启动MCP服务器
    println!("Content-Type: application/json");
    println!();
    
    let server = MCPServer::new().await?;
    
    // 读取标准输入作为MCP请求
    let mut input = String::new();
    std::io::stdin().read_line(&mut input)?;
    
    let request: MCPRequest = serde_json::from_str(&input)?;
    let response = server.handle_request(request).await;
    
    println!("{}", serde_json::to_string(&response)?);
    
    Ok(())
}