use anyhow::Result;
use axum::{
    routing::{get, post},
    Router, Json, extract::Query,
    http::StatusCode,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tower_http::cors::CorsLayer;

use rag_system::RagSystem;
use rag_system::config::RagConfig;

#[derive(Deserialize)]
struct IndexRequest {
    path: String,
}

#[derive(Serialize)]
struct ApiResponse<T> {
    success: bool,
    data: Option<T>,
    error: Option<String>,
}

#[derive(Deserialize)]
struct SearchQuery {
    q: String,
    limit: Option<usize>,
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize logging
    tracing_subscriber::fmt::init();

    // Load configuration
    let config = load_config().await?;
    
    // Initialize RAG system
    let rag_system = Arc::new(RagSystem::new(config.clone()).await?);

    // Build application
    let app = create_app(rag_system);

    // Start server
    let addr = "0.0.0.0:3001";
    let listener = tokio::net::TcpListener::bind(addr).await?;
    
    tracing::info!("RAG server starting on {}", addr);
    axum::serve(listener, app).await?;

    Ok(())
}

fn create_app(rag_system: Arc<RagSystem>) -> Router {
    Router::new()
        .route("/", get(health_check))
        .route("/health", get(health_check))
        .route("/index", post(index_codebase))
        .route("/search", get(search_code))
        .layer(CorsLayer::permissive())
        .with_state(rag_system)
}

async fn health_check() -> Json<ApiResponse<String>> {
    Json(ApiResponse {
        success: true,
        data: Some("RAG server is running".to_string()),
        error: None,
    })
}

async fn index_codebase(
    State(rag_system): State<Arc<RagSystem>>,
    Json(request): Json<IndexRequest>,
) -> Result<Json<ApiResponse<String>>, (StatusCode, String)> {
    let path = std::path::Path::new(&request.path);
    
    if !path.exists() {
        return Err((
            StatusCode::BAD_REQUEST,
            "Path does not exist".to_string(),
        ));
    }

    match rag_system.index_codebase(path).await {
        Ok(_) => Ok(Json(ApiResponse {
            success: true,
            data: Some(format!("Successfully indexed {}", request.path)),
            error: None,
        })),
        Err(e) => Err((
            StatusCode::INTERNAL_SERVER_ERROR,
            e.to_string(),
        )),
    }
}

async fn search_code(
    State(rag_system): State<Arc<RagSystem>>,
    Query(params): Query<SearchQuery>,
) -> Result<Json<ApiResponse<Vec<String>>>, (StatusCode, String)> {
    let limit = params.limit.unwrap_or(10);
    
    match rag_system.query(&params.q, limit).await {
        Ok(results) => {
            let file_paths: Vec<String> = results
                .into_iter()
                .map(|r| r.file_path.to_string_lossy().to_string())
                .collect();
            
            Ok(Json(ApiResponse {
                success: true,
                data: Some(file_paths),
                error: None,
            }))
        }
        Err(e) => Err((
            StatusCode::INTERNAL_SERVER_ERROR,
            e.to_string(),
        )),
    }
}

async fn load_config() -> Result<RagConfig> {
    let config_path = std::env::current_dir()?.join("rag_config.json");
    
    if config_path.exists() {
        RagConfig::from_file(&config_path)
    } else {
        let config = RagConfig::default();
        config.save_to_file(&config_path)?;
        Ok(config)
    }
}

use axum::extract::State;