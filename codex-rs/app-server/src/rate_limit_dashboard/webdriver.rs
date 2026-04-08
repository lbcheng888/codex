mod launcher;
mod mail163;
mod manual_login;

use super::service::ManualLoginStart;
use super::service::RateLimitDashboardFetcher;
use async_trait::async_trait;
use codex_app_server_protocol::RateLimitSnapshot as AppRateLimitSnapshot;
use codex_app_server_protocol::RateLimitSnapshot;
use codex_backend_client::Client as BackendClient;
use codex_backend_client::RequestError;
use codex_config::types::AuthCredentialsStoreMode;
use codex_login::AuthManager;
use codex_login::CLIENT_ID;
use codex_login::CodexAuth;
use codex_login::RefreshTokenError;
use codex_login::ServerOptions;
use codex_login::load_auth_dot_json;
use codex_login::run_login_server;
use codex_login::save_auth;
use launcher::WebDriverBrowser;
use launcher::resolve_webdriver_launch_spec;
use launcher::spawn_local_driver;
use launcher::uses_external_webdriver;
use reqwest::Method;
use serde_json::Value;
use serde_json::json;
use sha2::Digest;
use sha2::Sha256;
use std::collections::HashMap;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;
use tempfile::TempDir;
use tokio::sync::Mutex;
use tokio::time::Instant;
use tokio::time::sleep;
use tokio::time::timeout;
use tracing::warn;

const AUTH_PAGE_WAIT_TIMEOUT: Duration = Duration::from_secs(45);
const CONSENT_PAGE_WAIT_TIMEOUT: Duration = Duration::from_secs(20);
const LOGIN_COMPLETION_TIMEOUT: Duration = Duration::from_secs(120);
const OPENAI_AUTH_ERROR_MAX_LOGIN_ATTEMPTS: usize = 3;
const OPENAI_IDENTITY_MAX_RELOADS: usize = 5;
const POLL_INTERVAL: Duration = Duration::from_millis(500);
const FILL_AND_SUBMIT_SCRIPT: &str = r#"
const [selectors, submitSelectors, text] = arguments;
const isVisible = (element) => {
  if (!(element instanceof Element)) {
    return false;
  }
  const style = window.getComputedStyle(element);
  if (
    style.display === "none" ||
    style.visibility === "hidden" ||
    style.pointerEvents === "none"
  ) {
    return false;
  }
  if ("disabled" in element && element.disabled) {
    return false;
  }
  const rect = element.getBoundingClientRect();
  return rect.width > 0 && rect.height > 0;
};

let input = null;
for (const selector of selectors) {
  for (const candidate of document.querySelectorAll(selector)) {
    if (isVisible(candidate)) {
      input = candidate;
      break;
    }
  }
  if (input) {
    break;
  }
}

if (!input) {
  return { status: "missing" };
}

input.scrollIntoView({ block: "center", inline: "center" });
input.focus();

const inputSetter =
  window.HTMLInputElement &&
  Object.getOwnPropertyDescriptor(window.HTMLInputElement.prototype, "value")?.set;
const textareaSetter =
  window.HTMLTextAreaElement &&
  Object.getOwnPropertyDescriptor(window.HTMLTextAreaElement.prototype, "value")?.set;

if (input instanceof HTMLInputElement && inputSetter) {
  inputSetter.call(input, text);
} else if (input instanceof HTMLTextAreaElement && textareaSetter) {
  textareaSetter.call(input, text);
} else {
  input.value = text;
}

input.dispatchEvent(new Event("input", { bubbles: true, composed: true }));
input.dispatchEvent(new Event("change", { bubbles: true }));

const form = input.form || input.closest("form");
let visibleSubmitButton = null;
for (const selector of submitSelectors) {
  for (const candidate of document.querySelectorAll(selector)) {
    if (isVisible(candidate)) {
      visibleSubmitButton = candidate;
      break;
    }
  }
  if (visibleSubmitButton) {
    break;
  }
}

const submitButton = form?.querySelector('button[type="submit"], input[type="submit"]');
if (!visibleSubmitButton && form) {
  visibleSubmitButton = Array.from(
    form.querySelectorAll('button:not([type="button"]), input[type="submit"]')
  ).find(isVisible);
}

if (visibleSubmitButton instanceof HTMLElement) {
  visibleSubmitButton.click();
  return { status: "submitted" };
}

if (form?.requestSubmit) {
  form.requestSubmit();
  return { status: "submitted" };
}

if (submitButton instanceof HTMLElement) {
  submitButton.click();
  return { status: "submitted" };
}

input.dispatchEvent(new KeyboardEvent("keydown", {
  key: "Enter",
  code: "Enter",
  keyCode: 13,
  which: 13,
  bubbles: true,
}));
input.dispatchEvent(new KeyboardEvent("keypress", {
  key: "Enter",
  code: "Enter",
  keyCode: 13,
  which: 13,
  bubbles: true,
}));
input.dispatchEvent(new KeyboardEvent("keyup", {
  key: "Enter",
  code: "Enter",
  keyCode: 13,
  which: 13,
  bubbles: true,
}));
return { status: "submitted" };
"#;
const CLICK_VISIBLE_TEXT_SCRIPT: &str = r#"
const [selectors, texts] = arguments;
const isVisible = (element) => {
  if (!(element instanceof Element)) {
    return false;
  }
  const style = window.getComputedStyle(element);
  if (
    style.display === "none" ||
    style.visibility === "hidden" ||
    style.pointerEvents === "none"
  ) {
    return false;
  }
  if ("disabled" in element && element.disabled) {
    return false;
  }
  const rect = element.getBoundingClientRect();
  return rect.width > 0 && rect.height > 0;
};
const collapse = (value) => value.replace(/\s+/g, " ").trim();
const loweredTexts = texts.map((text) => text.toLowerCase());
for (const selector of selectors) {
  for (const candidate of document.querySelectorAll(selector)) {
    if (!isVisible(candidate)) {
      continue;
    }
    const text = collapse(
      candidate.innerText ||
        candidate.textContent ||
        candidate.value ||
        candidate.getAttribute("aria-label") ||
        ""
    ).toLowerCase();
    if (!text) {
      continue;
    }
    if (!loweredTexts.some((needle) => text.includes(needle))) {
      continue;
    }
    candidate.scrollIntoView({ block: "center", inline: "center" });
    if (candidate instanceof HTMLElement) {
      candidate.click();
      return { status: "clicked", text };
    }
  }
}
return { status: "missing" };
"#;
const HAS_VISIBLE_INPUT_SCRIPT: &str = r#"
const [selectors] = arguments;
const isVisible = (element) => {
  if (!(element instanceof Element)) {
    return false;
  }
  const style = window.getComputedStyle(element);
  if (
    style.display === "none" ||
    style.visibility === "hidden" ||
    style.pointerEvents === "none"
  ) {
    return false;
  }
  if ("disabled" in element && element.disabled) {
    return false;
  }
  const rect = element.getBoundingClientRect();
  return rect.width > 0 && rect.height > 0;
};
return selectors.some((selector) =>
  Array.from(document.querySelectorAll(selector)).some(isVisible)
);
"#;
const FILL_VISIBLE_INPUT_SCRIPT: &str = r#"
const [selectors, text] = arguments;
const isVisible = (element) => {
  if (!(element instanceof Element)) {
    return false;
  }
  const style = window.getComputedStyle(element);
  if (
    style.display === "none" ||
    style.visibility === "hidden" ||
    style.pointerEvents === "none"
  ) {
    return false;
  }
  if ("disabled" in element && element.disabled) {
    return false;
  }
  const rect = element.getBoundingClientRect();
  return rect.width > 0 && rect.height > 0;
};
for (const selector of selectors) {
  for (const candidate of document.querySelectorAll(selector)) {
    if (!isVisible(candidate)) {
      continue;
    }
    candidate.scrollIntoView({ block: "center", inline: "center" });
    candidate.focus();
    const proto =
      candidate instanceof HTMLInputElement
        ? window.HTMLInputElement?.prototype
        : window.HTMLTextAreaElement?.prototype;
    const setter = proto && Object.getOwnPropertyDescriptor(proto, "value")?.set;
    if (setter) {
      setter.call(candidate, text);
    } else {
      candidate.value = text;
    }
    candidate.dispatchEvent(new Event("input", { bubbles: true, composed: true }));
    candidate.dispatchEvent(new Event("change", { bubbles: true }));
    return { status: "filled" };
  }
}
return { status: "missing" };
"#;
const FIND_FIRST_VISIBLE_ELEMENT_SCRIPT: &str = r#"
const [selectors] = arguments;
const isVisible = (element) => {
  if (!(element instanceof Element)) {
    return false;
  }
  const style = window.getComputedStyle(element);
  if (
    style.display === "none" ||
    style.visibility === "hidden" ||
    style.pointerEvents === "none"
  ) {
    return false;
  }
  if ("disabled" in element && element.disabled) {
    return false;
  }
  const rect = element.getBoundingClientRect();
  return rect.width > 0 && rect.height > 0;
};
for (const selector of selectors) {
  for (const candidate of document.querySelectorAll(selector)) {
    if (isVisible(candidate)) {
      return candidate;
    }
  }
}
return null;
"#;
const SIMPLE_PAGE_STATE_SCRIPT: &str = r#"
const collapse = (value) => value.replace(/\s+/g, " ").trim();
return {
  url: window.location.href || "",
  title: document.title ? collapse(document.title) : "",
  bodyText: document.body?.innerText ? collapse(document.body.innerText) : "",
};
"#;
const PAGE_DEBUG_SCRIPT: &str = r#"
const collapse = (value) => value.replace(/\s+/g, " ").trim();
const isVisible = (element) => {
  if (!(element instanceof Element)) {
    return false;
  }
  const style = window.getComputedStyle(element);
  if (style.display === "none" || style.visibility === "hidden") {
    return false;
  }
  const rect = element.getBoundingClientRect();
  return rect.width > 0 && rect.height > 0;
};
const describeVisibleFields = (documentLike) =>
  Array.from(
    documentLike.querySelectorAll("input, textarea, button, [role='button']")
  )
    .filter(isVisible)
    .slice(0, 8)
    .map((element) => ({
      tag: element.tagName,
      type: "type" in element ? element.type || null : null,
      name: "name" in element ? element.name || null : null,
      id: element.id || null,
      placeholder: "placeholder" in element ? element.placeholder || null : null,
      text:
        element instanceof HTMLInputElement
          ? element.value || null
          : element.innerText
            ? collapse(element.innerText).slice(0, 80)
            : null,
    }));
const title = document.title ? collapse(document.title) : null;
const heading = document.querySelector("h1, h2, [role='heading']");
const headingText = heading?.innerText ? collapse(heading.innerText) : null;
const bodyText = document.body?.innerText ? collapse(document.body.innerText).slice(0, 240) : null;
return {
  title,
  url: window.location.href || null,
  heading: headingText,
  bodyText,
  visibleFields: describeVisibleFields(document),
  frames: Array.from(document.querySelectorAll("iframe")).slice(0, 4).map((frame, index) => {
    try {
      const doc = frame.contentDocument;
      return {
        index,
        id: frame.id || null,
        name: frame.name || null,
        src: frame.src || null,
        title: doc?.title ? collapse(doc.title) : null,
        bodyText: doc?.body?.innerText ? collapse(doc.body.innerText).slice(0, 240) : null,
        visibleFields: doc ? describeVisibleFields(doc) : [],
      };
    } catch (error) {
      return {
        index,
        id: frame.id || null,
        name: frame.name || null,
        src: frame.src || null,
        error: String(error),
      };
    }
  }),
};
"#;

pub(crate) struct BrowserRateLimitDashboardFetcher {
    auth_root: PathBuf,
    chatgpt_base_url: String,
    forced_workspace_id: Option<String>,
    manual_login_sessions: Mutex<HashMap<String, manual_login::ManualLoginSession>>,
}

impl BrowserRateLimitDashboardFetcher {
    pub(crate) fn new(
        auth_root: PathBuf,
        chatgpt_base_url: String,
        forced_workspace_id: Option<String>,
    ) -> Self {
        Self {
            auth_root,
            chatgpt_base_url,
            forced_workspace_id,
            manual_login_sessions: Mutex::new(HashMap::new()),
        }
    }

    fn account_auth_home(&self, email: &str) -> PathBuf {
        let slug = email
            .chars()
            .map(|ch| {
                if ch.is_ascii_alphanumeric() {
                    ch.to_ascii_lowercase()
                } else {
                    '_'
                }
            })
            .collect::<String>();
        let digest = format!("{:x}", Sha256::digest(email.as_bytes()));
        self.auth_root.join(format!("{slug}-{}", &digest[..12]))
    }

    fn auth_manager(&self, auth_home: &Path) -> AuthManager {
        let auth_manager = AuthManager::new(
            auth_home.to_path_buf(),
            /*enable_codex_api_key_env*/ false,
            AuthCredentialsStoreMode::File,
        );
        auth_manager.set_forced_chatgpt_workspace_id(self.forced_workspace_id.clone());
        auth_manager
    }

    async fn fetch_rate_limits_with_auth(
        &self,
        auth: &CodexAuth,
    ) -> Result<Vec<AppRateLimitSnapshot>, RequestError> {
        let client = BackendClient::from_auth(self.chatgpt_base_url.clone(), auth)
            .map_err(RequestError::Other)?;
        let snapshots = client.get_rate_limits_many_detailed().await?;
        if snapshots.is_empty() {
            return Err(RequestError::Other(anyhow::anyhow!(
                "backend returned no rate limit snapshots"
            )));
        }
        Ok(snapshots.into_iter().map(Into::into).collect())
    }

    fn validate_account_email(&self, auth: &CodexAuth, email: &str) -> io::Result<()> {
        if let Some(auth_email) = auth.get_account_email()
            && !auth_email.eq_ignore_ascii_case(email)
        {
            return Err(io::Error::other(format!(
                "cached auth email mismatch for {email}: got {auth_email}"
            )));
        }
        Ok(())
    }

    async fn fetch_with_cached_auth(
        &self,
        email: &str,
    ) -> io::Result<Option<Vec<RateLimitSnapshot>>> {
        let auth_home = self.account_auth_home(email);
        let auth_manager = self.auth_manager(&auth_home);
        let Some(auth) = auth_manager.auth().await else {
            return Ok(None);
        };
        self.validate_account_email(&auth, email)?;
        match self.fetch_rate_limits_with_auth(&auth).await {
            Ok(rate_limits) => Ok(Some(rate_limits)),
            Err(err) if err.is_unauthorized() => {
                self.fetch_after_refresh(&auth_manager, email).await
            }
            Err(err) => Err(io::Error::other(format!(
                "failed to fetch rate limits with cached auth: {err}"
            ))),
        }
    }

    async fn fetch_after_refresh(
        &self,
        auth_manager: &AuthManager,
        email: &str,
    ) -> io::Result<Option<Vec<RateLimitSnapshot>>> {
        match auth_manager.refresh_token().await {
            Ok(()) => {}
            Err(RefreshTokenError::Permanent(_)) => return Ok(None),
            Err(RefreshTokenError::Transient(err)) => {
                return Err(io::Error::other(format!(
                    "failed to refresh cached auth for {email}: {err}"
                )));
            }
        }

        let Some(refreshed_auth) = auth_manager.auth().await else {
            return Ok(None);
        };
        self.validate_account_email(&refreshed_auth, email)?;
        match self.fetch_rate_limits_with_auth(&refreshed_auth).await {
            Ok(rate_limits) => Ok(Some(rate_limits)),
            Err(err) if err.is_unauthorized() => Ok(None),
            Err(err) => Err(io::Error::other(format!(
                "failed to fetch rate limits with refreshed auth: {err}"
            ))),
        }
    }

    async fn login_and_persist_auth(
        &self,
        auth_home: &Path,
        email: &str,
        password: &str,
        mailbox_password: &str,
    ) -> io::Result<CodexAuth> {
        let webdriver = DashboardWebDriver::ensure_ready().await?;
        let mut last_auth_error: Option<io::Error> = None;

        for attempt in 1..=OPENAI_AUTH_ERROR_MAX_LOGIN_ATTEMPTS {
            let temp_dir = TempDir::new()?;
            let mut server_options = ServerOptions::new(
                temp_dir.path().to_path_buf(),
                CLIENT_ID.to_string(),
                self.forced_workspace_id.clone(),
                AuthCredentialsStoreMode::File,
            );
            server_options.open_browser = false;
            server_options.port = 0;

            let login_server = run_login_server(server_options)?;
            let auth_url = login_server.auth_url.clone();
            let cancel_handle = login_server.cancel_handle();
            let mut login_task = Some(tokio::spawn(async move {
                login_server.block_until_done().await
            }));
            let session_id = webdriver.create_session().await?;

            let automation_result = webdriver
                .complete_openai_login(&session_id, &auth_url, email, password, mailbox_password)
                .await;

            match automation_result {
                Err(err) if is_openai_auth_error(&err) => {
                    cancel_handle.shutdown();
                    if let Some(login_task) = login_task.take() {
                        let _ = timeout(Duration::from_secs(3), login_task).await;
                    }
                    if let Err(delete_err) = webdriver.delete_session(&session_id).await {
                        warn!("failed to close webdriver session: {delete_err}");
                    }
                    last_auth_error = Some(io::Error::other(format!(
                        "OpenAI auth error page for {email} on login attempt {attempt}/{OPENAI_AUTH_ERROR_MAX_LOGIN_ATTEMPTS}: {err}"
                    )));
                    if attempt < OPENAI_AUTH_ERROR_MAX_LOGIN_ATTEMPTS {
                        continue;
                    }
                    return Err(last_auth_error.take().unwrap_or_else(|| {
                        io::Error::other(format!(
                            "failed to log in {email}: OpenAI auth error retries were exhausted"
                        ))
                    }));
                }
                Err(err) => {
                    cancel_handle.shutdown();
                    if let Some(login_task) = login_task.take() {
                        let _ = timeout(Duration::from_secs(3), login_task).await;
                    }
                    if let Err(delete_err) = webdriver.delete_session(&session_id).await {
                        warn!("failed to close webdriver session: {delete_err}");
                    }
                    return Err(err);
                }
                Ok(()) => {}
            }

            let login_result = match timeout(
                LOGIN_COMPLETION_TIMEOUT,
                login_task
                    .take()
                    .ok_or_else(|| io::Error::other("login task was already consumed"))?,
            )
            .await
            {
                Ok(Ok(Ok(()))) => Ok(()),
                Ok(Ok(Err(err))) => Err(err),
                Ok(Err(err)) => Err(io::Error::other(format!("login task join failed: {err}"))),
                Err(_) => {
                    cancel_handle.shutdown();
                    Err(io::Error::new(
                        io::ErrorKind::TimedOut,
                        format!(
                            "timed out waiting for OAuth callback completion ({})",
                            webdriver.page_debug_summary(&session_id).await
                        ),
                    ))
                }
            };

            if let Err(err) = webdriver.delete_session(&session_id).await {
                warn!("failed to close webdriver session: {err}");
            }

            login_result?;

            let auth_dot_json =
                load_auth_dot_json(temp_dir.path(), AuthCredentialsStoreMode::File)?.ok_or_else(
                    || io::Error::other("login completed but no auth.json was written"),
                )?;
            save_auth(auth_home, &auth_dot_json, AuthCredentialsStoreMode::File)?;

            let auth = CodexAuth::from_auth_storage(auth_home, AuthCredentialsStoreMode::File)?
                .ok_or_else(|| io::Error::other("persisted auth could not be reloaded"))?;
            self.validate_account_email(&auth, email)?;
            return Ok(auth);
        }

        Err(last_auth_error.unwrap_or_else(|| {
            io::Error::other(format!(
                "failed to log in {email}: OpenAI auth error retries were exhausted"
            ))
        }))
    }
}

#[async_trait]
impl RateLimitDashboardFetcher for BrowserRateLimitDashboardFetcher {
    async fn fetch_account_rate_limits(
        &self,
        email: &str,
        password: &str,
        mailbox_password: Option<&str>,
    ) -> io::Result<Vec<RateLimitSnapshot>> {
        if let Some(rate_limits) = self.fetch_with_cached_auth(email).await? {
            return Ok(rate_limits);
        }

        let auth_home = self.account_auth_home(email);
        let auth = self
            .login_and_persist_auth(
                &auth_home,
                email,
                password,
                mailbox_password.unwrap_or(password),
            )
            .await?;
        self.fetch_rate_limits_with_auth(&auth)
            .await
            .map_err(|err| {
                io::Error::other(format!(
                    "failed to fetch rate limits after fresh login for {email}: {err}"
                ))
            })
    }

    async fn start_manual_login(&self, email: &str) -> io::Result<ManualLoginStart> {
        BrowserRateLimitDashboardFetcher::start_manual_login(self, email).await
    }
}

struct DashboardWebDriver {
    base_url: String,
    browser: WebDriverBrowser,
    http: reqwest::Client,
}

impl DashboardWebDriver {
    async fn ensure_ready() -> io::Result<Self> {
        let launch_spec = resolve_webdriver_launch_spec()?;
        let unavailable_message = launch_spec.unavailable_message();
        let http = reqwest::Client::builder()
            .build()
            .map_err(|err| io::Error::other(format!("failed to create webdriver client: {err}")))?;
        let driver = Self {
            base_url: launch_spec.base_url,
            browser: launch_spec.browser,
            http,
        };
        if driver.is_ready().await {
            return Ok(driver);
        }

        if uses_external_webdriver() {
            return Err(io::Error::other(format!(
                "{} webdriver at {} is not reachable",
                driver.browser.label(),
                driver.base_url,
            )));
        }

        spawn_local_driver(driver.browser).await?;

        let deadline = Instant::now() + launch_spec.ready_timeout;
        while Instant::now() < deadline {
            if driver.is_ready().await {
                return Ok(driver);
            }
            sleep(POLL_INTERVAL).await;
        }

        Err(io::Error::other(unavailable_message))
    }

    async fn create_session(&self) -> io::Result<String> {
        let body = json!({
            "capabilities": {
                "alwaysMatch": self.browser.session_capabilities()
            }
        });
        let value = self
            .request_json(Method::POST, "/session", Some(body))
            .await?;
        value
            .get("sessionId")
            .and_then(Value::as_str)
            .map(str::to_string)
            .ok_or_else(|| io::Error::other("webdriver did not return a session id"))
    }

    async fn delete_session(&self, session_id: &str) -> io::Result<()> {
        let _ = self
            .request_json(
                Method::DELETE,
                &format!("/session/{session_id}"),
                Option::<Value>::None,
            )
            .await?;
        Ok(())
    }

    async fn complete_openai_login(
        &self,
        session_id: &str,
        auth_url: &str,
        email: &str,
        password: &str,
        mailbox_password: &str,
    ) -> io::Result<()> {
        self.complete_openai_login_inner(
            session_id,
            auth_url,
            email,
            password,
            mailbox_password,
            /*allow_email_verification_restart*/ true,
        )
        .await
    }

    async fn complete_openai_login_inner(
        &self,
        session_id: &str,
        auth_url: &str,
        email: &str,
        password: &str,
        mailbox_password: &str,
        allow_email_verification_restart: bool,
    ) -> io::Result<()> {
        let password = strip_wrapping_quotes(password);
        let mailbox_password = strip_wrapping_quotes(mailbox_password);
        self.request_json(
            Method::POST,
            &format!("/session/{session_id}/url"),
            Some(json!({ "url": auth_url })),
        )
        .await?;

        self.fill_visible_input_and_submit_once_native(
            session_id,
            &[
                "input[type='email']",
                "input[name='email']",
                "input[name='username']",
                "input#username",
            ],
            &[
                "button[type='submit']",
                "input[type='submit']",
                "#identifierNext button",
                "#identifierNext [role='button']",
            ],
            email,
            "email",
            AUTH_PAGE_WAIT_TIMEOUT,
        )
        .await?;
        self.settle_openai_identity_step_after_email_submit(session_id, auth_url, email)
            .await?;
        self.wait_for_openai_password_step(session_id, auth_url, email)
            .await?;

        self.fill_visible_input_and_submit_once_native(
            session_id,
            &[
                "input[type='password']",
                "input[name='password']",
                "input[name='Passwd']",
                "input[autocomplete='current-password']",
            ],
            &[
                "#passwordNext",
                "button[type='submit']",
                "input[type='submit']",
                "#passwordNext button",
                "#passwordNext [role='button']",
            ],
            password,
            "password",
            AUTH_PAGE_WAIT_TIMEOUT,
        )
        .await?;
        self.complete_163_email_verification_if_needed(session_id, email, mailbox_password)
            .await?;
        if allow_email_verification_restart
            && self
                .wait_for_email_verification_success_page(session_id)
                .await?
        {
            return Box::pin(self.complete_openai_login_inner(
                session_id,
                auth_url,
                email,
                password,
                mailbox_password,
                /*allow_email_verification_restart*/ false,
            ))
            .await;
        }
        self.confirm_openai_consent_if_needed(session_id).await?;
        Ok(())
    }

    async fn settle_openai_identity_step_after_email_submit(
        &self,
        session_id: &str,
        auth_url: &str,
        email: &str,
    ) -> io::Result<()> {
        let deadline = Instant::now() + AUTH_PAGE_WAIT_TIMEOUT;
        let email_selectors = [
            "input[type='email']",
            "input[name='email']",
            "input[name='username']",
            "input#username",
        ];
        let password_selectors = [
            "input[type='password']",
            "input[name='password']",
            "input[name='Passwd']",
            "input[autocomplete='current-password']",
        ];
        let google_identifier_selectors = [
            "input#identifierId",
            "input[name='identifier']",
            "input[type='email']",
        ];
        let action_selectors = [
            "button[type='submit']",
            "input[type='submit']",
            "button",
            "a",
            "[role='button']",
        ];
        let mut resubmitted_email = false;
        let mut reload_attempts = 0;

        while Instant::now() < deadline {
            if self
                .has_visible_input(session_id, &password_selectors)
                .await?
                || self
                    .has_visible_input(session_id, &google_identifier_selectors)
                    .await?
            {
                return Ok(());
            }

            if self.openai_identity_page_needs_reload(session_id).await? {
                if reload_attempts >= OPENAI_IDENTITY_MAX_RELOADS {
                    break;
                }
                self.request_json(
                    Method::POST,
                    &format!("/session/{session_id}/url"),
                    Some(json!({ "url": auth_url })),
                )
                .await?;
                self.fill_visible_input_and_submit_once_native(
                    session_id,
                    &email_selectors,
                    &["button[type='submit']", "input[type='submit']"],
                    email,
                    "email",
                    AUTH_PAGE_WAIT_TIMEOUT,
                )
                .await?;
                reload_attempts += 1;
                continue;
            }

            if self
                .click_visible_text(
                    session_id,
                    &action_selectors,
                    &["重试", "Retry", "登录", "Log in"],
                )
                .await?
            {
                sleep(POLL_INTERVAL).await;
                continue;
            }

            if !resubmitted_email && self.has_visible_input(session_id, &email_selectors).await? {
                self.fill_visible_input_and_submit_once_native(
                    session_id,
                    &email_selectors,
                    &["button[type='submit']", "input[type='submit']"],
                    email,
                    "email",
                    AUTH_PAGE_WAIT_TIMEOUT,
                )
                .await?;
                resubmitted_email = true;
                continue;
            }

            sleep(POLL_INTERVAL).await;
        }

        Ok(())
    }

    async fn wait_for_openai_password_step(
        &self,
        session_id: &str,
        auth_url: &str,
        email: &str,
    ) -> io::Result<()> {
        let deadline = Instant::now() + AUTH_PAGE_WAIT_TIMEOUT;
        let email_selectors = [
            "input[type='email']",
            "input[name='email']",
            "input[name='username']",
            "input#username",
        ];
        let google_identifier_selectors = [
            "input#identifierId",
            "input[name='identifier']",
            "input[type='email']",
        ];
        let password_selectors = [
            "input[type='password']",
            "input[name='password']",
            "input[name='Passwd']",
            "input[autocomplete='current-password']",
        ];
        let action_selectors = [
            "button[type='submit']",
            "input[type='submit']",
            "button",
            "a",
            "[role='button']",
        ];
        let mut reload_attempts = 0;

        while Instant::now() < deadline {
            if self
                .has_visible_input(session_id, &password_selectors)
                .await?
            {
                return Ok(());
            }

            if self
                .has_visible_input(session_id, &google_identifier_selectors)
                .await?
            {
                self.fill_visible_input_and_submit_once_native(
                    session_id,
                    &google_identifier_selectors,
                    &[
                        "#identifierNext",
                        "#identifierNext button",
                        "#identifierNext [role='button']",
                        "button[type='submit']",
                        "input[type='submit']",
                    ],
                    email,
                    "google identifier",
                    AUTH_PAGE_WAIT_TIMEOUT,
                )
                .await?;
                sleep(POLL_INTERVAL).await;
                continue;
            }

            if self.openai_identity_page_needs_reload(session_id).await? {
                if reload_attempts >= OPENAI_IDENTITY_MAX_RELOADS {
                    break;
                }
                self.request_json(
                    Method::POST,
                    &format!("/session/{session_id}/url"),
                    Some(json!({ "url": auth_url })),
                )
                .await?;
                self.fill_visible_input_and_submit_once_native(
                    session_id,
                    &email_selectors,
                    &["button[type='submit']", "input[type='submit']"],
                    email,
                    "email",
                    AUTH_PAGE_WAIT_TIMEOUT,
                )
                .await?;
                self.settle_openai_identity_step_after_email_submit(session_id, auth_url, email)
                    .await?;
                reload_attempts += 1;
                continue;
            }

            if self
                .click_visible_text(
                    session_id,
                    &action_selectors,
                    &["重试", "Retry", "登录", "Log in"],
                )
                .await?
            {
                sleep(POLL_INTERVAL).await;
                continue;
            }

            if self.has_visible_input(session_id, &email_selectors).await? {
                self.fill_visible_input_and_submit_once_native(
                    session_id,
                    &email_selectors,
                    &["button[type='submit']", "input[type='submit']"],
                    email,
                    "email",
                    AUTH_PAGE_WAIT_TIMEOUT,
                )
                .await?;
                self.settle_openai_identity_step_after_email_submit(session_id, auth_url, email)
                    .await?;
                continue;
            }

            sleep(POLL_INTERVAL).await;
        }

        Err(io::Error::new(
            io::ErrorKind::TimedOut,
            format!(
                "timed out waiting for visible password input ({})",
                self.page_debug_summary(session_id).await
            ),
        ))
    }

    async fn fill_visible_input_and_submit_once(
        &self,
        session_id: &str,
        selectors: &[&str],
        submit_selectors: &[&str],
        text: &str,
        field_name: &str,
        wait_timeout: Duration,
    ) -> io::Result<()> {
        let deadline = Instant::now() + wait_timeout;
        while Instant::now() < deadline {
            let outcome = self
                .execute_script(
                    session_id,
                    FILL_AND_SUBMIT_SCRIPT,
                    json!([selectors, submit_selectors, text]),
                )
                .await?;
            if outcome.get("status").and_then(Value::as_str) == Some("submitted") {
                return Ok(());
            }
            sleep(POLL_INTERVAL).await;
        }

        Err(io::Error::new(
            io::ErrorKind::TimedOut,
            format!(
                "timed out waiting for visible {field_name} input ({})",
                self.page_debug_summary(session_id).await
            ),
        ))
    }

    async fn fill_visible_input_and_submit_once_native(
        &self,
        session_id: &str,
        selectors: &[&str],
        submit_selectors: &[&str],
        text: &str,
        field_name: &str,
        wait_timeout: Duration,
    ) -> io::Result<()> {
        let deadline = Instant::now() + wait_timeout;
        let mut last_interaction_error: Option<String> = None;
        while Instant::now() < deadline {
            if self.openai_identity_page_is_auth_error(session_id).await? {
                return Err(io::Error::other(format!(
                    "OpenAI returned an auth error page before the {field_name} step ({})",
                    self.page_debug_summary(session_id).await
                )));
            }
            let outcome = self
                .execute_script(
                    session_id,
                    FILL_VISIBLE_INPUT_SCRIPT,
                    json!([selectors, text]),
                )
                .await?;
            if outcome.get("status").and_then(Value::as_str) != Some("filled") {
                sleep(POLL_INTERVAL).await;
                continue;
            }
            let Some(submit_button) = self
                .find_first_visible_element(session_id, submit_selectors)
                .await?
            else {
                sleep(POLL_INTERVAL).await;
                continue;
            };
            match self.click_element(session_id, &submit_button).await {
                Ok(()) => return Ok(()),
                Err(err) if is_transient_interactability_error(&err) => {
                    last_interaction_error = Some(err.to_string());
                    sleep(POLL_INTERVAL).await;
                    continue;
                }
                Err(err) => return Err(err),
            }
        }

        if let Some(last_interaction_error) = last_interaction_error {
            return Err(io::Error::new(
                io::ErrorKind::TimedOut,
                format!(
                    "timed out clicking {field_name} submit control after transient webdriver errors: {last_interaction_error} ({})",
                    self.page_debug_summary(session_id).await
                ),
            ));
        }

        Err(io::Error::new(
            io::ErrorKind::TimedOut,
            format!(
                "timed out waiting for visible {field_name} input ({})",
                self.page_debug_summary(session_id).await
            ),
        ))
    }

    async fn wait_for_visible_input(
        &self,
        session_id: &str,
        selectors: &[&str],
        wait_timeout: Duration,
    ) -> io::Result<bool> {
        let deadline = Instant::now() + wait_timeout;
        while Instant::now() < deadline {
            if self.has_visible_input(session_id, selectors).await? {
                return Ok(true);
            }
            sleep(POLL_INTERVAL).await;
        }

        Ok(false)
    }

    async fn has_visible_input(&self, session_id: &str, selectors: &[&str]) -> io::Result<bool> {
        self.execute_script(session_id, HAS_VISIBLE_INPUT_SCRIPT, json!([selectors]))
            .await?
            .as_bool()
            .ok_or_else(|| io::Error::other("webdriver visible-input response was not a boolean"))
    }

    async fn click_visible_text(
        &self,
        session_id: &str,
        selectors: &[&str],
        texts: &[&str],
    ) -> io::Result<bool> {
        Ok(self
            .execute_script(
                session_id,
                CLICK_VISIBLE_TEXT_SCRIPT,
                json!([selectors, texts]),
            )
            .await?
            .get("status")
            .and_then(Value::as_str)
            == Some("clicked"))
    }

    async fn find_first_visible_element(
        &self,
        session_id: &str,
        selectors: &[&str],
    ) -> io::Result<Option<Value>> {
        let value = self
            .execute_script(
                session_id,
                FIND_FIRST_VISIBLE_ELEMENT_SCRIPT,
                json!([selectors]),
            )
            .await?;
        if value.is_null() {
            Ok(None)
        } else {
            Ok(Some(value))
        }
    }

    async fn click_element(&self, session_id: &str, element: &Value) -> io::Result<()> {
        let element_id = element_reference_id(element)?;
        self.request_json(
            Method::POST,
            &format!("/session/{session_id}/element/{element_id}/click"),
            Some(json!({})),
        )
        .await?;
        Ok(())
    }

    async fn openai_identity_page_needs_reload(&self, session_id: &str) -> io::Result<bool> {
        let state = self
            .execute_script(
                session_id,
                SIMPLE_PAGE_STATE_SCRIPT,
                Value::Array(Vec::new()),
            )
            .await?;
        let url = state
            .get("url")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_ascii_lowercase();
        let title = state
            .get("title")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_ascii_lowercase();
        let body_text = state
            .get("bodyText")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_ascii_lowercase();
        Ok(openai_identity_page_looks_timed_out(
            &url, &title, &body_text,
        ))
    }

    async fn openai_identity_page_is_auth_error(&self, session_id: &str) -> io::Result<bool> {
        let state = self
            .execute_script(
                session_id,
                SIMPLE_PAGE_STATE_SCRIPT,
                Value::Array(Vec::new()),
            )
            .await?;
        let url = state
            .get("url")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_ascii_lowercase();
        let title = state
            .get("title")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_ascii_lowercase();
        let body_text = state
            .get("bodyText")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_ascii_lowercase();
        Ok(openai_identity_page_is_auth_error(&url, &title, &body_text))
    }

    async fn wait_for_email_verification_success_page(&self, session_id: &str) -> io::Result<bool> {
        let deadline = Instant::now() + Duration::from_secs(8);
        while Instant::now() < deadline {
            if self.is_email_verification_success_page(session_id).await? {
                return Ok(true);
            }
            sleep(POLL_INTERVAL).await;
        }
        Ok(false)
    }

    async fn is_email_verification_success_page(&self, session_id: &str) -> io::Result<bool> {
        let state = self
            .execute_script(
                session_id,
                SIMPLE_PAGE_STATE_SCRIPT,
                Value::Array(Vec::new()),
            )
            .await?;
        let url = state
            .get("url")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_ascii_lowercase();
        let title = state
            .get("title")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_ascii_lowercase();
        let body_text = state
            .get("bodyText")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_ascii_lowercase();
        Ok(url.contains("auth.openai.com/email-verification")
            && (title.contains("电子邮件地址已验证")
                || title.contains("email address verified")
                || body_text.contains("电子邮件地址已验证")
                || body_text.contains("email address has been verified")))
    }

    async fn confirm_openai_consent_if_needed(&self, session_id: &str) -> io::Result<()> {
        let deadline = Instant::now() + CONSENT_PAGE_WAIT_TIMEOUT;
        let action_selectors = [
            "button[type='submit']",
            "input[type='submit']",
            "button",
            "a",
            "[role='button']",
        ];
        let mut saw_consent_page = false;

        while Instant::now() < deadline {
            if self.is_openai_consent_page(session_id).await? {
                saw_consent_page = true;
                if self
                    .click_visible_text(
                        session_id,
                        &action_selectors,
                        &["继续", "Continue", "允许", "Allow", "Authorize", "批准"],
                    )
                    .await?
                {
                    return Ok(());
                }
            } else if saw_consent_page {
                return Ok(());
            }
            sleep(POLL_INTERVAL).await;
        }

        if saw_consent_page {
            return Err(io::Error::new(
                io::ErrorKind::TimedOut,
                format!(
                    "timed out confirming OpenAI consent page ({})",
                    self.page_debug_summary(session_id).await
                ),
            ));
        }

        Ok(())
    }

    async fn is_openai_consent_page(&self, session_id: &str) -> io::Result<bool> {
        let state = self
            .execute_script(
                session_id,
                SIMPLE_PAGE_STATE_SCRIPT,
                Value::Array(Vec::new()),
            )
            .await?;
        let url = state
            .get("url")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_ascii_lowercase();
        let title = state
            .get("title")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_ascii_lowercase();
        let body_text = state
            .get("bodyText")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_ascii_lowercase();
        Ok(url.contains("auth.openai.com")
            && (title.contains("登录到 codex")
                || title.contains("sign in to codex")
                || body_text.contains("chatgpt 将向 codex 提供")
                || body_text.contains("chatgpt will share")
                || body_text.contains("不会收到你的聊天历史记录")
                || body_text.contains("won't receive your chat history")))
    }

    async fn execute_script(
        &self,
        session_id: &str,
        script: &str,
        args: Value,
    ) -> io::Result<Value> {
        self.request_json(
            Method::POST,
            &format!("/session/{session_id}/execute/sync"),
            Some(json!({
                "script": script,
                "args": args,
            })),
        )
        .await
    }

    async fn page_debug_summary(&self, session_id: &str) -> String {
        match self
            .execute_script(session_id, PAGE_DEBUG_SCRIPT, Value::Array(Vec::new()))
            .await
        {
            Ok(summary) => {
                let title = summary
                    .get("title")
                    .and_then(Value::as_str)
                    .unwrap_or("unknown");
                let url = summary
                    .get("url")
                    .and_then(Value::as_str)
                    .unwrap_or("unknown");
                let heading = summary
                    .get("heading")
                    .and_then(Value::as_str)
                    .unwrap_or("none");
                let body_text = summary
                    .get("bodyText")
                    .and_then(Value::as_str)
                    .unwrap_or("none");
                let visible_fields = summary
                    .get("visibleFields")
                    .and_then(Value::as_array)
                    .map(|items| {
                        items
                            .iter()
                            .filter_map(|item| {
                                Some(format!(
                                    "{}#{}:{}:{}:{}",
                                    item.get("tag")?.as_str().unwrap_or("?"),
                                    item.get("id").and_then(Value::as_str).unwrap_or("-"),
                                    item.get("type").and_then(Value::as_str).unwrap_or("-"),
                                    item.get("name").and_then(Value::as_str).unwrap_or("-"),
                                    item.get("placeholder")
                                        .and_then(Value::as_str)
                                        .unwrap_or("-"),
                                ))
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    })
                    .filter(|items| !items.is_empty())
                    .unwrap_or_else(|| "none".to_string());
                let frame_summaries = summary
                    .get("frames")
                    .and_then(Value::as_array)
                    .map(|frames| {
                        frames
                            .iter()
                            .map(|frame| {
                                let title =
                                    frame.get("title").and_then(Value::as_str).unwrap_or("none");
                                let id = frame.get("id").and_then(Value::as_str).unwrap_or("-");
                                let fields = frame
                                    .get("visibleFields")
                                    .and_then(Value::as_array)
                                    .map(std::vec::Vec::len)
                                    .unwrap_or(0);
                                format!("{id}:{title}:fields={fields}")
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    })
                    .filter(|items| !items.is_empty())
                    .unwrap_or_else(|| "none".to_string());
                format!(
                    "title={title}; heading={heading}; url={url}; body={body_text}; fields={visible_fields}; frames={frame_summaries}"
                )
            }
            Err(err) => format!("failed to inspect page: {err}"),
        }
    }

    async fn is_ready(&self) -> bool {
        match self
            .http
            .get(format!("{}/status", self.base_url))
            .send()
            .await
        {
            Ok(response) => response.status().is_success(),
            Err(_) => false,
        }
    }

    async fn request_json(
        &self,
        method: Method,
        path: &str,
        body: Option<Value>,
    ) -> io::Result<Value> {
        let url = format!("{}{}", self.base_url, path);
        let request = self.http.request(method, &url);
        let request = if let Some(body) = body {
            request.json(&body)
        } else {
            request
        };
        let response = request
            .send()
            .await
            .map_err(|err| io::Error::other(format!("webdriver request failed: {err}")))?;
        let status = response.status();
        let payload = response.json::<Value>().await.map_err(|err| {
            io::Error::other(format!("failed to decode webdriver response: {err}"))
        })?;
        let value = payload.get("value").cloned().unwrap_or(Value::Null);
        if let Some(error) = value.get("error").and_then(Value::as_str) {
            let message = value
                .get("message")
                .and_then(Value::as_str)
                .unwrap_or("webdriver command failed");
            if message.contains("Allow remote automation") {
                return Err(io::Error::other(
                    "Safari WebDriver requires Safari Settings > Developer > Allow Remote Automation",
                ));
            }
            return Err(io::Error::other(format!("webdriver {error}: {message}")));
        }
        if !status.is_success() {
            return Err(io::Error::other(format!(
                "webdriver command failed with status {status}"
            )));
        }
        Ok(value)
    }
}

pub(super) fn strip_wrapping_quotes(value: &str) -> &str {
    if value.len() >= 2
        && let Some(quote) = value.chars().next()
        && (quote == '"' || quote == '\'')
        && value.ends_with(quote)
    {
        return &value[1..value.len() - 1];
    }
    value
}

fn element_reference_id(element: &Value) -> io::Result<&str> {
    element
        .get("element-6066-11e4-a52e-4f735466cecf")
        .and_then(Value::as_str)
        .or_else(|| element.get("ELEMENT").and_then(Value::as_str))
        .ok_or_else(|| io::Error::other("webdriver did not return an element reference"))
}

fn is_transient_interactability_error(error: &io::Error) -> bool {
    let message = error.to_string();
    message.contains("invalid element state")
        || message.contains("not currently interactable")
        || message.contains("element click intercepted")
}

fn openai_identity_page_looks_timed_out(url: &str, title: &str, body_text: &str) -> bool {
    let url = url.to_ascii_lowercase();
    let title = title.to_ascii_lowercase();
    let body_text = body_text.to_ascii_lowercase();
    url.contains("auth.openai.com/log-in")
        && (title.contains("糟糕，出错了")
            || body_text.contains("operation timed out")
            || body_text.contains("did not match the expected pattern"))
}

fn openai_identity_page_is_auth_error(url: &str, title: &str, body_text: &str) -> bool {
    let url = url.to_ascii_lowercase();
    let title = title.to_ascii_lowercase();
    let body_text = body_text.to_ascii_lowercase();
    url.contains("auth.openai.com/error")
        && (title.contains("身份验证错误")
            || title.contains("authentication error")
            || body_text.contains("unknown_error")
            || body_text.contains("验证过程中出错")
            || body_text.contains("something went wrong during verification"))
}

fn is_openai_auth_error(error: &io::Error) -> bool {
    error
        .to_string()
        .contains("OpenAI returned an auth error page before the")
}

#[cfg(test)]
mod tests {
    use super::element_reference_id;
    use super::is_openai_auth_error;
    use super::is_transient_interactability_error;
    use super::openai_identity_page_is_auth_error;
    use super::openai_identity_page_looks_timed_out;
    use super::strip_wrapping_quotes;
    use pretty_assertions::assert_eq;
    use serde_json::json;
    use std::io;

    #[test]
    fn strip_wrapping_quotes_removes_matching_quotes_only() {
        assert_eq!("secret", strip_wrapping_quotes("\"secret\""));
        assert_eq!("secret", strip_wrapping_quotes("'secret'"));
        assert_eq!("secret", strip_wrapping_quotes("secret"));
        assert_eq!("\"secret", strip_wrapping_quotes("\"secret"));
    }

    #[test]
    fn element_reference_id_supports_w3c_and_legacy_keys() {
        assert_eq!(
            "w3c-id",
            element_reference_id(&json!({
                "element-6066-11e4-a52e-4f735466cecf": "w3c-id"
            }))
            .expect("w3c key should parse"),
        );
        assert_eq!(
            "legacy-id",
            element_reference_id(&json!({ "ELEMENT": "legacy-id" }))
                .expect("legacy key should parse"),
        );
    }

    #[test]
    fn transient_interactability_errors_are_retryable() {
        assert!(is_transient_interactability_error(&io::Error::other(
            "webdriver invalid element state: Element is not currently interactable",
        )));
        assert!(!is_transient_interactability_error(&io::Error::other(
            "webdriver no such window",
        )));
    }

    #[test]
    fn openai_identity_error_page_is_detected() {
        assert!(openai_identity_page_is_auth_error(
            "https://auth.openai.com/error?payload=...",
            "身份验证错误 - OpenAI",
            "验证过程中出错 (unknown_error)。请重试。",
        ));
        assert!(!openai_identity_page_is_auth_error(
            "https://auth.openai.com/log-in",
            "Sign in",
            "Continue with email",
        ));
    }

    #[test]
    fn openai_auth_error_marker_is_precise() {
        assert!(is_openai_auth_error(&io::Error::other(
            "OpenAI returned an auth error page before the email step (title=身份验证错误)",
        )));
        assert!(!is_openai_auth_error(&io::Error::other(
            "timed out waiting for visible email input",
        )));
    }

    #[test]
    fn openai_timed_out_identity_page_is_reloadable() {
        assert!(openai_identity_page_looks_timed_out(
            "https://auth.openai.com/log-in",
            "糟糕，出错了！",
            "Operation timed out",
        ));
        assert!(!openai_identity_page_looks_timed_out(
            "https://auth.openai.com/email-verification",
            "电子邮件地址已验证",
            "email address has been verified",
        ));
    }
}
