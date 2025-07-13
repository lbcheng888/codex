use std::sync::Arc;

use crate::Codex;
use crate::config::Config;
use crate::protocol::Event;
use crate::protocol::EventMsg;
use crate::util::notify_on_sigint;
use tokio::sync::Notify;

/// Spawn a new [`Codex`] and initialize the session.
///
/// Returns the wrapped [`Codex`] **and** the `SessionInitialized` event that
/// is received as a response to the initial `ConfigureSession` submission so
/// that callers can surface the information to the UI.
pub async fn init_codex(config: Config) -> anyhow::Result<(Codex, Event, Arc<Notify>)> {
    crate::debug_log!("[WRAPPER] init_codex() called");
    crate::debug_log!("[WRAPPER] Config model: {}", config.model);
    crate::debug_log!("[WRAPPER] Config provider: {}", config.model_provider_id);
    crate::debug_log!("[WRAPPER] Config wire_api: {:?}", config.model_provider.wire_api);
    crate::debug_log!("[WRAPPER] Config base_url: {}", config.model_provider.base_url);
    
    let ctrl_c = notify_on_sigint();
    crate::debug_log!("[WRAPPER] About to spawn Codex");
    let (codex, init_id) = Codex::spawn(config, ctrl_c.clone()).await?;
    crate::debug_log!("[WRAPPER] Codex spawned successfully with init_id: {}", init_id);

    // The first event must be `SessionInitialized`. Validate and forward it to
    // the caller so that they can display it in the conversation history.
    crate::debug_log!("[WRAPPER] Waiting for first event...");
    let event = codex.next_event().await?;
    crate::debug_log!("[WRAPPER] Received first event: {:?}", event);
    
    if event.id != init_id
        || !matches!(
            &event,
            Event {
                id: _id,
                msg: EventMsg::SessionConfigured(_),
            }
        )
    {
        crate::debug_log!("[WRAPPER] ERROR: Expected SessionConfigured but got different event");
        return Err(anyhow::anyhow!(
            "expected SessionInitialized but got {event:?}"
        ));
    }

    crate::debug_log!("[WRAPPER] init_codex() completed successfully");
    Ok((codex, event, ctrl_c))
}
