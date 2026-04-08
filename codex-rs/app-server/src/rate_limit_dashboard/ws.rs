use super::RateLimitDashboardRouterState;
use super::service::RateLimitDashboardResponse;
use super::service::RateLimitDashboardService;
use axum::extract::State;
use axum::extract::ws::Message;
use axum::extract::ws::WebSocket;
use axum::extract::ws::WebSocketUpgrade;
use axum::response::IntoResponse;
use futures::SinkExt;
use futures::StreamExt;
use serde::Deserialize;
use serde::Serialize;
use std::sync::Arc;
use tokio::sync::mpsc;
use tracing::warn;

#[derive(Debug, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
enum DashboardClientMessage {
    Snapshot,
    RefreshAll,
    RefreshAccount { email: String },
}

#[derive(Debug, Serialize)]
#[serde(tag = "type", rename_all = "camelCase")]
enum DashboardServerMessage {
    Snapshot { data: RateLimitDashboardResponse },
    Error { message: String },
}

pub(crate) async fn dashboard_websocket_handler(
    websocket: WebSocketUpgrade,
    State(state): State<RateLimitDashboardRouterState>,
) -> impl IntoResponse {
    websocket.on_upgrade(move |socket| async move {
        run_dashboard_websocket(socket, state.service).await;
    })
}

async fn run_dashboard_websocket(socket: WebSocket, service: Arc<RateLimitDashboardService>) {
    let (mut sender, mut receiver) = socket.split();
    let mut updates = service.subscribe();
    let (command_error_tx, mut command_error_rx) = mpsc::unbounded_channel::<String>();

    match service.snapshot().await {
        Ok(_) => {
            let initial = updates.borrow_and_update().clone();
            if send_server_message(
                &mut sender,
                DashboardServerMessage::Snapshot { data: initial },
            )
            .await
            .is_err()
            {
                return;
            }
        }
        Err(err) => {
            let _ = send_server_message(
                &mut sender,
                DashboardServerMessage::Error {
                    message: err.to_string(),
                },
            )
            .await;
            return;
        }
    }

    loop {
        tokio::select! {
            incoming = receiver.next() => {
                match incoming {
                    Some(Ok(Message::Text(text))) => {
                        match serde_json::from_str::<DashboardClientMessage>(text.as_ref()) {
                            Ok(command) => dispatch_command(command, service.clone(), command_error_tx.clone()),
                            Err(err) => {
                                if send_server_message(
                                    &mut sender,
                                    DashboardServerMessage::Error {
                                        message: format!("invalid dashboard websocket message: {err}"),
                                    },
                                )
                                .await
                                .is_err()
                                {
                                    break;
                                }
                            }
                        }
                    }
                    Some(Ok(Message::Ping(payload))) => {
                        if sender.send(Message::Pong(payload)).await.is_err() {
                            break;
                        }
                    }
                    Some(Ok(Message::Pong(_))) => {}
                    Some(Ok(Message::Close(_))) | None => break,
                    Some(Ok(Message::Binary(_))) => {
                        warn!("ignoring unsupported binary dashboard websocket message");
                    }
                    Some(Err(err)) => {
                        warn!("dashboard websocket receive error: {err}");
                        break;
                    }
                }
            }
            changed = updates.changed() => {
                if changed.is_err() {
                    break;
                }
                let snapshot = updates.borrow_and_update().clone();
                if send_server_message(
                    &mut sender,
                    DashboardServerMessage::Snapshot { data: snapshot },
                )
                .await
                .is_err()
                {
                    break;
                }
            }
            error_message = command_error_rx.recv() => {
                let Some(message) = error_message else {
                    break;
                };
                if send_server_message(
                    &mut sender,
                    DashboardServerMessage::Error { message },
                )
                .await
                .is_err()
                {
                    break;
                }
            }
        }
    }
}

fn dispatch_command(
    command: DashboardClientMessage,
    service: Arc<RateLimitDashboardService>,
    command_error_tx: mpsc::UnboundedSender<String>,
) {
    match command {
        DashboardClientMessage::Snapshot => {
            tokio::spawn(async move {
                if let Err(err) = service.snapshot().await {
                    let _ = command_error_tx.send(err.to_string());
                }
            });
        }
        DashboardClientMessage::RefreshAll => {
            tokio::spawn(async move {
                if let Err(err) = service.refresh_all().await {
                    let _ = command_error_tx.send(err.to_string());
                }
            });
        }
        DashboardClientMessage::RefreshAccount { email } => {
            tokio::spawn(async move {
                if let Err(err) = service.refresh_account(&email).await {
                    let _ = command_error_tx.send(err.to_string());
                }
            });
        }
    }
}

async fn send_server_message(
    sender: &mut futures::stream::SplitSink<WebSocket, Message>,
    message: DashboardServerMessage,
) -> Result<(), ()> {
    let serialized = serde_json::to_string(&message).map_err(|err| {
        warn!("failed to serialize dashboard websocket message: {err}");
    })?;
    sender
        .send(Message::Text(serialized.into()))
        .await
        .map_err(|err| {
            warn!("failed to send dashboard websocket message: {err}");
        })
}
