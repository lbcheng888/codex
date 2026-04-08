use super::BrowserRateLimitDashboardFetcher;
use crate::rate_limit_dashboard::service::ManualLoginStart;
use codex_config::types::AuthCredentialsStoreMode;
use codex_login::CLIENT_ID;
use codex_login::CodexAuth;
use codex_login::DeviceCode;
use codex_login::ServerOptions;
use codex_login::complete_device_code_login;
use codex_login::request_device_code;
use std::io;
use tokio::task::JoinHandle;
use tracing::warn;

const MANUAL_LOGIN_PAGE_HEAD: &str = r#"<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>Manual Login</title>
  <style>
    :root {
      --bg: #ffffff;
      --fg: #111111;
      --line: #dddddd;
      --muted: #666666;
    }

    * {
      box-sizing: border-box;
    }

    body {
      margin: 0;
      background: var(--bg);
      color: var(--fg);
      font: 16px/1.5 -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
    }

    main {
      max-width: 560px;
      margin: 0 auto;
      padding: 32px 20px 40px;
    }

    h1 {
      margin: 0 0 8px;
      font-size: 28px;
      line-height: 1.2;
    }

    p {
      margin: 0 0 16px;
    }

    .code {
      margin: 20px 0;
      padding: 18px 20px;
      border: 1px solid var(--line);
      font: 700 32px/1.2 ui-monospace, SFMono-Regular, Menlo, monospace;
      letter-spacing: 0.08em;
      text-align: center;
    }

    .actions {
      display: flex;
      gap: 10px;
      flex-wrap: wrap;
      margin: 20px 0;
    }

    a,
    button {
      display: inline-flex;
      align-items: center;
      justify-content: center;
      min-height: 42px;
      padding: 0 14px;
      border: 1px solid var(--line);
      background: #fff;
      color: var(--fg);
      font: inherit;
      text-decoration: none;
      cursor: pointer;
    }

    ol {
      padding-left: 22px;
      margin: 18px 0;
    }

    .muted {
      color: var(--muted);
    }
  </style>
</head>
<body>
  <main>
"#;

const MANUAL_LOGIN_PAGE_TAIL: &str = r#"
  </main>
  <script>
    const copyButton = document.getElementById("copy-code");
    if (copyButton) {
      copyButton.addEventListener("click", async () => {
        try {
          await navigator.clipboard.writeText(copyButton.dataset.code || "");
          copyButton.textContent = "Copied";
        } catch (error) {
          copyButton.textContent = "Copy failed";
        }
      });
    }
  </script>
</body>
</html>
"#;

pub(super) struct ManualLoginSession {
    pub(super) task: JoinHandle<()>,
}

impl BrowserRateLimitDashboardFetcher {
    pub(crate) async fn start_manual_login(&self, email: &str) -> io::Result<ManualLoginStart> {
        if let Some(existing_session) = self.manual_login_sessions.lock().await.remove(email) {
            existing_session.task.abort();
        }

        let auth_home = self.account_auth_home(email);
        let mut server_options = ServerOptions::new(
            auth_home.clone(),
            CLIENT_ID.to_string(),
            self.forced_workspace_id.clone(),
            AuthCredentialsStoreMode::File,
        );
        server_options.open_browser = false;

        let device_code = request_device_code(&server_options).await?;
        let verification_url = device_code.verification_url.clone();
        let user_code = device_code.user_code.clone();
        let email = email.to_string();
        let email_key = email.clone();
        let login_page_email = email.clone();
        let task = tokio::spawn(async move {
            let result = complete_manual_login(server_options, device_code, &email).await;
            if let Err(err) = result {
                warn!("manual dashboard login failed for {email}: {err}");
            }
        });
        self.manual_login_sessions
            .lock()
            .await
            .insert(email_key, ManualLoginSession { task });

        Ok(ManualLoginStart {
            verification_url: verification_url.clone(),
            user_code: user_code.clone(),
            html: render_manual_login_page(
                login_page_email.as_str(),
                &verification_url,
                &user_code,
            ),
        })
    }
}

async fn complete_manual_login(
    server_options: ServerOptions,
    device_code: DeviceCode,
    email: &str,
) -> io::Result<()> {
    complete_device_code_login(server_options.clone(), device_code).await?;
    let auth = CodexAuth::from_auth_storage(
        &server_options.codex_home,
        server_options.cli_auth_credentials_store_mode,
    )?
    .ok_or_else(|| {
        io::Error::other("manual device-code login completed but auth storage is empty")
    })?;
    validate_account_email(&auth, email)
}

fn validate_account_email(auth: &CodexAuth, email: &str) -> io::Result<()> {
    if let Some(auth_email) = auth.get_account_email()
        && !auth_email.eq_ignore_ascii_case(email)
    {
        return Err(io::Error::other(format!(
            "cached auth email mismatch for {email}: got {auth_email}"
        )));
    }
    Ok(())
}

fn render_manual_login_page(email: &str, verification_url: &str, user_code: &str) -> String {
    let escaped_email = html_escape(email);
    let escaped_verification_url = html_escape(verification_url);
    let escaped_user_code = html_escape(user_code);
    format!(
        "{MANUAL_LOGIN_PAGE_HEAD}<h1>Manual login for {escaped_email}</h1>\
<p>OpenAI browser authorize is unstable here, so this page now uses the official device-code flow.</p>\
<div class=\"code\">{escaped_user_code}</div>\
<div class=\"actions\">\
<a href=\"{escaped_verification_url}\" target=\"_blank\" rel=\"noopener\">Open OpenAI verification page</a>\
<button id=\"copy-code\" type=\"button\" data-code=\"{escaped_user_code}\">Copy code</button>\
</div>\
<ol>\
<li>Open the OpenAI verification page.</li>\
<li>Sign in with <strong>{escaped_email}</strong>.</li>\
<li>Enter the one-time code shown above.</li>\
<li>After OpenAI confirms the login, return to the dashboard and click <strong>Refresh</strong>.</li>\
</ol>\
<p class=\"muted\">This code expires in about 15 minutes. If it expires, close this tab and click Login again.</p>{MANUAL_LOGIN_PAGE_TAIL}"
    )
}

fn html_escape(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&#39;")
}
