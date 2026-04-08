mod page;
mod service;
mod webdriver;
mod ws;

use axum::Json;
use axum::extract::Path;
use axum::extract::State;
use axum::http::StatusCode;
use axum::response::Html;
use serde_json::json;
use std::sync::Arc;

pub(crate) use service::RateLimitDashboardService;
use webdriver::BrowserRateLimitDashboardFetcher;
pub(crate) use ws::dashboard_websocket_handler;

#[derive(Clone)]
pub(crate) struct RateLimitDashboardRouterState {
    pub service: Arc<RateLimitDashboardService>,
}

pub(crate) fn new_service(
    env_path: std::path::PathBuf,
    cache_path: std::path::PathBuf,
    auth_root: std::path::PathBuf,
    chatgpt_base_url: String,
    forced_workspace_id: Option<String>,
) -> Arc<RateLimitDashboardService> {
    Arc::new(RateLimitDashboardService::new(
        env_path,
        cache_path,
        Arc::new(BrowserRateLimitDashboardFetcher::new(
            auth_root,
            chatgpt_base_url,
            forced_workspace_id,
        )),
    ))
}

pub(crate) async fn dashboard_page_handler() -> Html<&'static str> {
    Html(page::DASHBOARD_PAGE)
}

pub(crate) async fn rate_limits_handler(
    State(state): State<RateLimitDashboardRouterState>,
) -> Result<Json<service::RateLimitDashboardResponse>, (StatusCode, Json<serde_json::Value>)> {
    state
        .service
        .snapshot()
        .await
        .map(Json)
        .map_err(internal_error)
}

pub(crate) async fn refresh_all_handler(
    State(state): State<RateLimitDashboardRouterState>,
) -> Result<Json<service::RateLimitDashboardResponse>, (StatusCode, Json<serde_json::Value>)> {
    state
        .service
        .refresh_all()
        .await
        .map(Json)
        .map_err(internal_error)
}

pub(crate) async fn refresh_one_handler(
    State(state): State<RateLimitDashboardRouterState>,
    Path(account): Path<String>,
) -> Result<Json<service::RateLimitDashboardResponse>, (StatusCode, Json<serde_json::Value>)> {
    state
        .service
        .refresh_account(&account)
        .await
        .map(Json)
        .map_err(|err| {
            if err.kind() == std::io::ErrorKind::NotFound {
                (
                    StatusCode::NOT_FOUND,
                    Json(json!({ "error": err.to_string() })),
                )
            } else {
                internal_error(err)
            }
        })
}

pub(crate) async fn start_manual_login_handler(
    State(state): State<RateLimitDashboardRouterState>,
    Path(account): Path<String>,
) -> Result<Html<String>, (StatusCode, Json<serde_json::Value>)> {
    state
        .service
        .start_manual_login(&account)
        .await
        .map(|manual_login| Html(manual_login.html))
        .map_err(|err| {
            if err.kind() == std::io::ErrorKind::NotFound {
                (
                    StatusCode::NOT_FOUND,
                    Json(json!({ "error": err.to_string() })),
                )
            } else {
                internal_error(err)
            }
        })
}

fn internal_error(err: std::io::Error) -> (StatusCode, Json<serde_json::Value>) {
    (
        StatusCode::INTERNAL_SERVER_ERROR,
        Json(json!({ "error": err.to_string() })),
    )
}
