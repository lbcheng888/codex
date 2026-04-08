use super::AUTH_PAGE_WAIT_TIMEOUT;
use super::DashboardWebDriver;
use super::POLL_INTERVAL;
use super::strip_wrapping_quotes;
use reqwest::Method;
use serde_json::Value;
use serde_json::json;
use std::collections::HashSet;
use std::io;
use std::time::Duration;
use tokio::time::Instant;
use tokio::time::sleep;

const MAIL_163_URL: &str = "https://mail.163.com/";
const EMAIL_VERIFICATION_TIMEOUT: Duration = Duration::from_secs(15);
const MAILBOX_LOGIN_TIMEOUT: Duration = Duration::from_secs(45);
const MAILBOX_CODE_TIMEOUT: Duration = Duration::from_secs(60);
const MAILBOX_LOGIN_ERROR_TIMEOUT: Duration = Duration::from_secs(15);

enum EmailVerificationSubmissionOutcome {
    Accepted,
    Rejected,
    AttemptsExhausted,
}

const FRAME_AWARE_CLICK_TEXT_SCRIPT: &str = r#"
const [selectors, texts] = arguments;
const collectDocuments = (root) => {
  const docs = [root];
  for (const frame of root.querySelectorAll("iframe")) {
    try {
      const frameDocument = frame.contentDocument;
      if (frameDocument) {
        docs.push(...collectDocuments(frameDocument));
      }
    } catch (error) {}
  }
  return docs;
};
const collapse = (value) => value.replace(/\s+/g, " ").trim();
const isVisible = (element) => {
  if (!(element instanceof Element)) {
    return false;
  }
  const view = element.ownerDocument?.defaultView || window;
  const style = view.getComputedStyle(element);
  if (
    style.display === "none" ||
    style.visibility === "hidden" ||
    style.pointerEvents === "none"
  ) {
    return false;
  }
  const rect = element.getBoundingClientRect();
  return rect.width > 0 && rect.height > 0;
};
const lowerTexts = texts.map((text) => text.toLowerCase());
const clickableAncestor = (element) =>
  element.closest("button, a, [role='button'], li, tr, td, div") || element;
for (const doc of collectDocuments(document)) {
  for (const selector of selectors) {
    for (const element of doc.querySelectorAll(selector)) {
      if (!isVisible(element)) {
        continue;
      }
      const text = collapse(
        element.innerText ||
          element.textContent ||
          element.value ||
          element.getAttribute("aria-label") ||
          ""
      ).toLowerCase();
      if (!text) {
        continue;
      }
      if (!lowerTexts.some((needle) => text.includes(needle))) {
        continue;
      }
      const target = clickableAncestor(element);
      target.scrollIntoView({ block: "center", inline: "center" });
      if (target instanceof HTMLElement) {
        target.click();
        return { status: "clicked", text };
      }
    }
  }
}
return { status: "missing" };
"#;

const FRAME_AWARE_EXTRACT_CODES_SCRIPT: &str = r#"
const collectDocuments = (root) => {
  const docs = [root];
  for (const frame of root.querySelectorAll("iframe")) {
    try {
      const frameDocument = frame.contentDocument;
      if (frameDocument) {
        docs.push(...collectDocuments(frameDocument));
      }
    } catch (error) {}
  }
  return docs;
};
const collapse = (value) => value.replace(/\s+/g, " ").trim();
const texts = collectDocuments(document)
  .map((doc) => collapse(doc.body?.innerText || ""))
  .filter(Boolean);
const sourceTexts = texts.filter((text) =>
  /(openai|chatgpt|codex|verification code|one-time code|验证码)/i.test(text)
);
const uniqueCodes = [];
const pushCode = (value) => {
  if (!value || uniqueCodes.includes(value)) {
    return;
  }
  uniqueCodes.push(value);
};
for (const text of (sourceTexts.length ? sourceTexts : texts)) {
  for (const pattern of [
    /(?:验证码|verification code|code is|one-time code|openai|chatgpt|codex)[^0-9]{0,160}([0-9]{6})/ig,
    /\b([0-9]{6})\b(?=[^0-9]{0,80}(?:验证码|verification code|one-time code|openai|chatgpt|codex))/ig,
  ]) {
    for (const matched of text.matchAll(pattern)) {
      pushCode(matched?.[1] || null);
    }
  }
}
return uniqueCodes;
"#;

const OPEN_LATEST_OPENAI_MAIL_FROM_UNREAD_LIST_SCRIPT: &str = r#"
const keywords = [
  "openai",
  "chatgpt",
  "codex",
  "verification code",
  "验证码",
];
const collectDocuments = (root) => {
  const docs = [root];
  for (const frame of root.querySelectorAll("iframe")) {
    try {
      const frameDocument = frame.contentDocument;
      if (frameDocument) {
        docs.push(...collectDocuments(frameDocument));
      }
    } catch (error) {}
  }
  return docs;
};
const collapse = (value) => value.replace(/\s+/g, " ").trim();
const isVisible = (element) => {
  if (!(element instanceof Element)) {
    return false;
  }
  const view = element.ownerDocument?.defaultView || window;
  const style = view.getComputedStyle(element);
  if (
    style.display === "none" ||
    style.visibility === "hidden" ||
    style.pointerEvents === "none"
  ) {
    return false;
  }
  const rect = element.getBoundingClientRect();
  return rect.width > 0 && rect.height > 0;
};
const candidateSelectors = [
  "[data-mid]",
  "[role='row']",
  "tr",
  "li",
  "a",
  "div",
];
const clickableAncestor = (element) =>
  element.closest("[data-mid], [role='row'], tr, li, a, div") || element;
for (const doc of collectDocuments(document)) {
  for (const selector of candidateSelectors) {
    for (const element of doc.querySelectorAll(selector)) {
      if (!isVisible(element)) {
        continue;
      }
      const text = collapse(
        element.innerText ||
          element.textContent ||
          element.getAttribute("title") ||
          element.getAttribute("aria-label") ||
          ""
      ).toLowerCase();
      if (
        text.length < 8 ||
        !keywords.some((keyword) => text.includes(keyword))
      ) {
        continue;
      }
      const target = clickableAncestor(element);
      target.scrollIntoView({ block: "center", inline: "center" });
      if (target instanceof HTMLElement) {
        target.click();
        return { status: "clicked", text };
      }
    }
  }
}
return { status: "missing" };
"#;

const CURRENT_CONTEXT_TEXT_SCRIPT: &str = r#"
return (document.body?.innerText || "").replace(/\s+/g, " ").trim();
"#;
const CURRENT_URL_SCRIPT: &str = r#"
return window.location.href || "";
"#;

const FIND_FIRST_ELEMENT_SCRIPT: &str = r#"
const [selectors] = arguments;
for (const selector of selectors) {
  const element = document.querySelector(selector);
  if (element) {
    return element;
  }
}
return null;
"#;

const FILL_VISIBLE_INPUT_IN_CURRENT_CONTEXT_SCRIPT: &str = r#"
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

if (!(input instanceof HTMLInputElement || input instanceof HTMLTextAreaElement)) {
  return { status: "missing" };
}

input.scrollIntoView({ block: "center", inline: "center" });
input.focus();
const proto =
  input instanceof HTMLInputElement
    ? window.HTMLInputElement?.prototype
    : window.HTMLTextAreaElement?.prototype;
const setter = proto && Object.getOwnPropertyDescriptor(proto, "value")?.set;
if (setter) {
  setter.call(input, text);
} else {
  input.value = text;
}
input.dispatchEvent(new Event("input", { bubbles: true, composed: true }));
input.dispatchEvent(new Event("change", { bubbles: true }));
if (typeof input.blur === "function") {
  input.blur();
}
return { status: "filled", value: input.value || null };
"#;

const CLICK_VISIBLE_ELEMENT_IN_CURRENT_CONTEXT_SCRIPT: &str = r#"
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
    if (!isVisible(candidate)) {
      continue;
    }
    candidate.scrollIntoView({ block: "center", inline: "center" });
    if (candidate instanceof HTMLElement) {
      candidate.click();
      return { status: "clicked" };
    }
  }
}

return { status: "missing" };
"#;

impl DashboardWebDriver {
    pub(super) async fn complete_163_email_verification_if_needed(
        &self,
        session_id: &str,
        email: &str,
        mailbox_password: &str,
    ) -> io::Result<()> {
        if !email.ends_with("@163.com") {
            return Ok(());
        }
        if !self
            .wait_for_visible_input(
                session_id,
                &[
                    "input[name='code']",
                    "input[id*='code']",
                    "input[placeholder*='验证码']",
                ],
                EMAIL_VERIFICATION_TIMEOUT,
            )
            .await?
        {
            return Ok(());
        }

        let original_handle = self.current_window_handle(session_id).await?;
        let mail_handle = self.open_new_tab(session_id).await?;
        self.switch_to_window(session_id, &mail_handle).await?;
        let mut attempted_codes = HashSet::new();
        let result = async {
            for attempt in 0..3 {
                let code = match self
                    .fetch_openai_code_from_163_mailbox(
                        session_id,
                        email,
                        mailbox_password,
                        &attempted_codes,
                    )
                    .await
                {
                    Ok(code) => code,
                    Err(err) if is_missing_openai_verification_email_error(&err) => {
                        self.switch_back_to_original_window(
                            session_id,
                            &original_handle,
                            &mail_handle,
                        )
                        .await?;
                        if attempt == 2 {
                            return Err(err);
                        }
                        self.click_visible_text(
                            session_id,
                            &["button", "a", "[role='button']"],
                            &["重新发送电子邮件", "Resend email"],
                        )
                        .await?;
                        self.switch_to_window(session_id, &mail_handle).await?;
                        continue;
                    }
                    Err(err) => return Err(err),
                };
                self.switch_back_to_original_window(session_id, &original_handle, &mail_handle)
                    .await?;
                self.fill_visible_input_and_submit_once(
                    session_id,
                    &[
                        "input[name='code']",
                        "input[id*='code']",
                        "input[placeholder*='验证码']",
                        "input[type='text']",
                    ],
                    &["button[type='submit']", "input[type='submit']", "button"],
                    &code,
                    "email verification code",
                    AUTH_PAGE_WAIT_TIMEOUT,
                )
                .await?;
                match self
                    .wait_for_email_verification_submission_outcome(session_id)
                    .await?
                {
                    EmailVerificationSubmissionOutcome::Accepted => return Ok(()),
                    EmailVerificationSubmissionOutcome::Rejected => {}
                    EmailVerificationSubmissionOutcome::AttemptsExhausted => {
                        return Err(self
                            .inspect_attempts_exhausted_verification_failure(
                                session_id,
                                &mail_handle,
                                email,
                                mailbox_password,
                                &code,
                            )
                            .await);
                    }
                }
                attempted_codes.insert(code);
                if attempt == 2 {
                    break;
                }
                self.click_visible_text(
                    session_id,
                    &["button", "a", "[role='button']"],
                    &["重新发送电子邮件", "Resend email"],
                )
                .await?;
                self.switch_to_window(session_id, &mail_handle).await?;
            }
            Err(io::Error::other(
                "OpenAI rejected every verification code fetched from the 163 mailbox",
            ))
        }
        .await;

        let restore_result = self
            .switch_back_to_original_window(session_id, &original_handle, &mail_handle)
            .await;
        result?;
        restore_result
    }

    async fn inspect_attempts_exhausted_verification_failure(
        &self,
        session_id: &str,
        mail_handle: &str,
        email: &str,
        mailbox_password: &str,
        submitted_code: &str,
    ) -> io::Error {
        let latest_mailbox_code = async {
            self.switch_to_window(session_id, mail_handle).await?;
            self.fetch_openai_code_from_163_mailbox(
                session_id,
                email,
                mailbox_password,
                &HashSet::new(),
            )
            .await
        }
        .await;

        match latest_mailbox_code {
            Ok(latest_code) if latest_code == submitted_code => io::Error::other(
                "OpenAI rejected email verification with max_check_attempts after submitting the latest 163 mailbox code; retry this account later",
            ),
            Ok(_) => io::Error::other(
                "OpenAI rejected email verification with max_check_attempts after submitting a stale or incorrect 163 mailbox code; re-check the latest 163 email and retry",
            ),
            Err(err) => io::Error::other(format!(
                "OpenAI rejected email verification with max_check_attempts after the submitted code; failed to re-check the latest 163 mailbox code: {err}"
            )),
        }
    }

    async fn fetch_openai_code_from_163_mailbox(
        &self,
        session_id: &str,
        email: &str,
        mailbox_password: &str,
        rejected_codes: &HashSet<String>,
    ) -> io::Result<String> {
        self.fetch_openai_code_from_163_mailbox_via_webdriver(
            session_id,
            email,
            mailbox_password,
            rejected_codes,
        )
        .await
    }

    async fn fetch_openai_code_from_163_mailbox_via_webdriver(
        &self,
        session_id: &str,
        email: &str,
        mailbox_password: &str,
        rejected_codes: &HashSet<String>,
    ) -> io::Result<String> {
        let mailbox_account = email.split('@').next().unwrap_or(email);
        let mailbox_password = strip_wrapping_quotes(mailbox_password);
        self.goto_url(session_id, MAIL_163_URL).await?;
        self.switch_to_default_content(session_id).await?;
        let _ = self.ensure_163_unread_inbox_view(session_id).await;
        if let Some(code) = self
            .try_collect_code_from_mailbox_frames(session_id, rejected_codes)
            .await?
        {
            return Ok(code);
        }
        if self.is_logged_into_163_mailbox(session_id).await? {
            return self
                .wait_for_openai_code_in_logged_in_163_mailbox(session_id, rejected_codes)
                .await;
        }
        let _ = self
            .click_visible_text_in_frames(
                session_id,
                &[
                    "#switchAccountLogin",
                    "#lbNormal",
                    "button",
                    "a",
                    "[role='button']",
                    "div",
                    "span",
                    "li",
                ],
                &["账号登录", "密码登录"],
                Duration::from_secs(5),
            )
            .await;
        let login_frame = self
            .wait_for_element(
                session_id,
                &[
                    "#loginDiv iframe",
                    "iframe[id^='x-URS-iframe']",
                    "iframe[src*='dl.reg.163.com']",
                    "iframe[src*='passport']",
                ],
                MAILBOX_LOGIN_TIMEOUT,
                "163 login iframe",
            )
            .await?;
        self.switch_to_frame(session_id, login_frame).await?;
        let _ = self
            .click_visible_text_in_frames(
                session_id,
                &[
                    "#switchAccountLogin",
                    "#lbNormal",
                    "button",
                    "a",
                    "[role='button']",
                    "div",
                    "span",
                    "li",
                ],
                &["账号登录", "密码登录"],
                Duration::from_secs(5),
            )
            .await;
        self.fill_visible_input_in_current_context(
            session_id,
            &[
                "input[name='email']",
                ".dlemail",
                "input[type='email']",
                "input[name*='email']",
                "input[name*='account']",
                "input[name*='user']",
                "input[placeholder*='邮箱']",
                "input[placeholder*='账号']",
                "input[placeholder*='帐号']",
                "input[type='text']",
            ],
            mailbox_account,
            "163 mailbox email",
            MAILBOX_LOGIN_TIMEOUT,
        )
        .await?;
        self.fill_visible_input_in_current_context(
            session_id,
            &[
                "input[name='password']",
                ".dlpwd",
                "input[type='password']",
                "input[name*='password']",
                "input[placeholder*='密码']",
            ],
            mailbox_password,
            "163 mailbox password",
            MAILBOX_LOGIN_TIMEOUT,
        )
        .await?;
        self.click_visible_element_in_current_context(
            session_id,
            &[
                "#dologin",
                "button[type='submit']",
                "input[type='submit']",
                "button",
                "a",
                "[role='button']",
                "div",
                "span",
                "li",
            ],
            "163 mailbox submit button",
            MAILBOX_LOGIN_TIMEOUT,
        )
        .await?;
        self.raise_mailbox_login_error_if_present(session_id)
            .await?;
        self.switch_to_default_content(session_id).await?;
        self.ensure_163_unread_inbox_view(session_id).await?;
        self.wait_for_openai_code_in_logged_in_163_mailbox(session_id, rejected_codes)
            .await
    }

    async fn wait_for_openai_code_in_logged_in_163_mailbox(
        &self,
        session_id: &str,
        rejected_codes: &HashSet<String>,
    ) -> io::Result<String> {
        let deadline = Instant::now() + MAILBOX_CODE_TIMEOUT;
        while Instant::now() < deadline {
            let _ = self.ensure_163_unread_inbox_view(session_id).await;
            self.raise_mailbox_login_error_from_page_if_present(session_id)
                .await?;
            if let Some(code) = self
                .pick_fresh_code_candidate_from_163_unread_list(session_id, rejected_codes)
                .await?
            {
                return Ok(code);
            }
            if let Some(code) = self
                .try_collect_code_from_mailbox_frames(session_id, rejected_codes)
                .await?
            {
                return Ok(code);
            }
            sleep(POLL_INTERVAL).await;
        }

        self.raise_mailbox_login_error_from_page_if_present(session_id)
            .await?;

        Err(io::Error::new(
            io::ErrorKind::TimedOut,
            format!(
                "timed out waiting for OpenAI verification email in 163 mailbox ({})",
                self.page_debug_summary(session_id).await
            ),
        ))
    }

    async fn is_logged_into_163_mailbox(&self, session_id: &str) -> io::Result<bool> {
        let current_url = self
            .execute_script(session_id, CURRENT_URL_SCRIPT, Value::Array(Vec::new()))
            .await?
            .as_str()
            .map(str::to_string)
            .ok_or_else(|| io::Error::other("webdriver returned a non-string current url"))?;
        Ok(current_url.contains("mail.163.com/js6/main.jsp?sid="))
    }

    async fn pick_fresh_code_candidate_from_163_unread_list(
        &self,
        session_id: &str,
        rejected_codes: &HashSet<String>,
    ) -> io::Result<Option<String>> {
        self.switch_to_default_content(session_id).await?;
        let current_url = self
            .execute_script(session_id, CURRENT_URL_SCRIPT, Value::Array(Vec::new()))
            .await?
            .as_str()
            .map(str::to_string)
            .ok_or_else(|| io::Error::other("webdriver returned a non-string current url"))?;
        if !current_url.contains("#module=mbox.ListModule") {
            return Ok(None);
        }
        if !self
            .open_latest_openai_mail_from_163_unread_list(session_id)
            .await?
        {
            return Ok(None);
        }
        self.wait_for_fresh_code_candidate(session_id, rejected_codes, Duration::from_secs(5))
            .await
    }

    async fn open_latest_openai_mail_from_163_unread_list(
        &self,
        session_id: &str,
    ) -> io::Result<bool> {
        Ok(self
            .execute_script(
                session_id,
                OPEN_LATEST_OPENAI_MAIL_FROM_UNREAD_LIST_SCRIPT,
                Value::Array(Vec::new()),
            )
            .await?
            .get("status")
            .and_then(Value::as_str)
            == Some("clicked"))
    }

    async fn fill_visible_input_in_current_context(
        &self,
        session_id: &str,
        selectors: &[&str],
        text: &str,
        field_name: &str,
        wait_timeout: Duration,
    ) -> io::Result<()> {
        let deadline = Instant::now() + wait_timeout;
        while Instant::now() < deadline {
            let outcome = self
                .execute_script(
                    session_id,
                    FILL_VISIBLE_INPUT_IN_CURRENT_CONTEXT_SCRIPT,
                    json!([selectors, text]),
                )
                .await?;
            if outcome.get("status").and_then(Value::as_str) == Some("filled") {
                return Ok(());
            }
            sleep(POLL_INTERVAL).await;
        }

        Err(io::Error::new(
            io::ErrorKind::TimedOut,
            format!(
                "timed out waiting for visible {field_name} field ({})",
                self.page_debug_summary(session_id).await
            ),
        ))
    }

    async fn click_visible_element_in_current_context(
        &self,
        session_id: &str,
        selectors: &[&str],
        element_name: &str,
        wait_timeout: Duration,
    ) -> io::Result<()> {
        let deadline = Instant::now() + wait_timeout;
        while Instant::now() < deadline {
            let outcome = self
                .execute_script(
                    session_id,
                    CLICK_VISIBLE_ELEMENT_IN_CURRENT_CONTEXT_SCRIPT,
                    json!([selectors]),
                )
                .await?;
            if outcome.get("status").and_then(Value::as_str) == Some("clicked") {
                return Ok(());
            }
            sleep(POLL_INTERVAL).await;
        }

        Err(io::Error::new(
            io::ErrorKind::TimedOut,
            format!(
                "timed out waiting for {element_name} ({})",
                self.page_debug_summary(session_id).await
            ),
        ))
    }

    async fn click_visible_text_in_frames(
        &self,
        session_id: &str,
        selectors: &[&str],
        texts: &[&str],
        wait_timeout: Duration,
    ) -> io::Result<()> {
        let deadline = Instant::now() + wait_timeout;
        while Instant::now() < deadline {
            let outcome = self
                .execute_script(
                    session_id,
                    FRAME_AWARE_CLICK_TEXT_SCRIPT,
                    json!([selectors, texts]),
                )
                .await?;
            if outcome.get("status").and_then(Value::as_str) == Some("clicked") {
                return Ok(());
            }
            sleep(POLL_INTERVAL).await;
        }

        Err(io::Error::new(
            io::ErrorKind::TimedOut,
            format!(
                "timed out waiting to click {:?} ({})",
                texts,
                self.page_debug_summary(session_id).await
            ),
        ))
    }

    async fn extract_code_candidates_from_frames(
        &self,
        session_id: &str,
    ) -> io::Result<Vec<String>> {
        self.execute_script(
            session_id,
            FRAME_AWARE_EXTRACT_CODES_SCRIPT,
            Value::Array(Vec::new()),
        )
        .await?
        .as_array()
        .map(|items| {
            items
                .iter()
                .filter_map(Value::as_str)
                .map(str::to_string)
                .collect::<Vec<_>>()
        })
        .ok_or_else(|| io::Error::other("webdriver returned non-array code candidates"))
    }

    async fn pick_fresh_code_candidate(
        &self,
        session_id: &str,
        rejected_codes: &HashSet<String>,
    ) -> io::Result<Option<String>> {
        Ok(self
            .extract_code_candidates_from_frames(session_id)
            .await?
            .into_iter()
            .find(|code| !rejected_codes.contains(code)))
    }

    async fn wait_for_fresh_code_candidate(
        &self,
        session_id: &str,
        rejected_codes: &HashSet<String>,
        wait_timeout: Duration,
    ) -> io::Result<Option<String>> {
        let deadline = Instant::now() + wait_timeout;
        while Instant::now() < deadline {
            if let Some(code) = self
                .pick_fresh_code_candidate(session_id, rejected_codes)
                .await?
            {
                return Ok(Some(code));
            }
            sleep(POLL_INTERVAL).await;
        }
        Ok(None)
    }

    async fn wait_for_email_verification_submission_outcome(
        &self,
        session_id: &str,
    ) -> io::Result<EmailVerificationSubmissionOutcome> {
        let deadline = Instant::now() + Duration::from_secs(12);
        while Instant::now() < deadline {
            let text = self
                .current_context_text(session_id)
                .await?
                .to_ascii_lowercase();
            if is_email_verification_attempts_exhausted_text(&text) {
                return Ok(EmailVerificationSubmissionOutcome::AttemptsExhausted);
            }
            if self.is_email_verification_success_page(session_id).await? {
                return Ok(EmailVerificationSubmissionOutcome::Accepted);
            }
            if text.contains("代码不正确") || text.contains("code is incorrect") {
                return Ok(EmailVerificationSubmissionOutcome::Rejected);
            }
            if !self
                .has_visible_input(
                    session_id,
                    &[
                        "input[name='code']",
                        "input[id*='code']",
                        "input[placeholder*='验证码']",
                    ],
                )
                .await?
            {
                return Ok(EmailVerificationSubmissionOutcome::Accepted);
            }
            sleep(POLL_INTERVAL).await;
        }
        Ok(EmailVerificationSubmissionOutcome::Accepted)
    }

    async fn current_context_text(&self, session_id: &str) -> io::Result<String> {
        self.execute_script(
            session_id,
            CURRENT_CONTEXT_TEXT_SCRIPT,
            Value::Array(Vec::new()),
        )
        .await?
        .as_str()
        .map(str::to_string)
        .ok_or_else(|| io::Error::other("current context text returned a non-string payload"))
    }

    async fn ensure_163_unread_inbox_view(&self, session_id: &str) -> io::Result<()> {
        let current_url = self
            .execute_script(session_id, CURRENT_URL_SCRIPT, Value::Array(Vec::new()))
            .await?
            .as_str()
            .map(str::to_string)
            .ok_or_else(|| io::Error::other("webdriver returned a non-string current url"))?;
        if !current_url.contains("mail.163.com/js6/main.jsp?sid=") {
            return Ok(());
        }
        let base_url = current_url
            .split('#')
            .next()
            .unwrap_or(current_url.as_str());
        let unread_url = format!(
            "{base_url}#module=mbox.ListModule%7C%7B%22filter%22%3A%7B%22flags%22%3A%7B%22read%22%3Afalse%7D%7D%2C%22order%22%3A%22date%22%2C%22desc%22%3Atrue%2C%22fids%22%3A%5B1%2C3%5D%7D"
        );
        self.goto_url(session_id, &unread_url).await
    }

    async fn goto_url(&self, session_id: &str, url: &str) -> io::Result<()> {
        self.request_json(
            Method::POST,
            &format!("/session/{session_id}/url"),
            Some(json!({ "url": url })),
        )
        .await?;
        Ok(())
    }

    async fn current_window_handle(&self, session_id: &str) -> io::Result<String> {
        self.request_json(
            Method::GET,
            &format!("/session/{session_id}/window"),
            Option::<Value>::None,
        )
        .await?
        .as_str()
        .map(str::to_string)
        .ok_or_else(|| io::Error::other("webdriver did not return a window handle"))
    }

    async fn wait_for_element(
        &self,
        session_id: &str,
        selectors: &[&str],
        wait_timeout: Duration,
        element_name: &str,
    ) -> io::Result<Value> {
        let deadline = Instant::now() + wait_timeout;
        while Instant::now() < deadline {
            let element = self
                .execute_script(session_id, FIND_FIRST_ELEMENT_SCRIPT, json!([selectors]))
                .await?;
            if !element.is_null() {
                return Ok(element);
            }
            sleep(POLL_INTERVAL).await;
        }

        Err(io::Error::new(
            io::ErrorKind::TimedOut,
            format!(
                "timed out waiting for {element_name} ({})",
                self.page_debug_summary(session_id).await
            ),
        ))
    }

    async fn find_element_if_present(
        &self,
        session_id: &str,
        selectors: &[&str],
    ) -> io::Result<Option<Value>> {
        let element = self
            .execute_script(session_id, FIND_FIRST_ELEMENT_SCRIPT, json!([selectors]))
            .await?;
        if element.is_null() {
            Ok(None)
        } else {
            Ok(Some(element))
        }
    }

    async fn switch_to_frame(&self, session_id: &str, frame: Value) -> io::Result<()> {
        self.request_json(
            Method::POST,
            &format!("/session/{session_id}/frame"),
            Some(json!({ "id": frame })),
        )
        .await?;
        Ok(())
    }

    async fn switch_to_default_content(&self, session_id: &str) -> io::Result<()> {
        self.request_json(
            Method::POST,
            &format!("/session/{session_id}/frame"),
            Some(json!({ "id": Value::Null })),
        )
        .await?;
        Ok(())
    }

    async fn try_collect_code_from_mailbox_frames(
        &self,
        session_id: &str,
        rejected_codes: &HashSet<String>,
    ) -> io::Result<Option<String>> {
        self.switch_to_default_content(session_id).await?;
        let frame = self
            .find_element_if_present(
                session_id,
                &[
                    "#frameforlogin",
                    "#frameJS6",
                    "iframe[name='frameforlogin']",
                    "iframe[src*='mail.163.com']",
                    "iframe[src*='preload6']",
                ],
            )
            .await?;
        let Some(frame) = frame else {
            return Ok(None);
        };

        self.switch_to_frame(session_id, frame).await?;
        let _ = self
            .click_visible_text_in_frames(
                session_id,
                &[
                    "a",
                    "button",
                    "[role='button']",
                    "div",
                    "span",
                    "li",
                    "tr",
                    "td",
                ],
                &[
                    "未读",
                    "收件箱",
                    "OpenAI",
                    "ChatGPT",
                    "验证码",
                    "verification code",
                    "写信",
                ],
                Duration::from_secs(2),
            )
            .await;
        let code = self
            .pick_fresh_code_candidate(session_id, rejected_codes)
            .await?;
        self.switch_to_default_content(session_id).await?;
        Ok(code)
    }

    async fn raise_mailbox_login_error_if_present(&self, session_id: &str) -> io::Result<()> {
        let deadline = Instant::now() + MAILBOX_LOGIN_ERROR_TIMEOUT;
        while Instant::now() < deadline {
            let text = self.current_context_text(session_id).await?;
            if text.contains("请输入账号") {
                return Err(io::Error::other(
                    "163 mailbox did not accept the account name input",
                ));
            }
            if text.contains("账号或密码错误") {
                return Err(io::Error::other(
                    "163 mailbox rejected the credentials; set MAILBOX_PASSWORD in .env if the mailbox password differs from PASSWORD",
                ));
            }
            if text.contains("验证码错误") {
                return Err(io::Error::other(
                    "163 mailbox requested an extra verification step; the current automation only supports direct password login",
                ));
            }
            sleep(POLL_INTERVAL).await;
        }
        Ok(())
    }

    async fn raise_mailbox_login_error_from_page_if_present(
        &self,
        session_id: &str,
    ) -> io::Result<()> {
        self.switch_to_default_content(session_id).await?;
        let login_frame = self
            .find_element_if_present(
                session_id,
                &[
                    "#loginDiv iframe",
                    "iframe[id^='x-URS-iframe']",
                    "iframe[src*='dl.reg.163.com']",
                    "iframe[src*='passport']",
                ],
            )
            .await?;
        let Some(login_frame) = login_frame else {
            return Ok(());
        };
        self.switch_to_frame(session_id, login_frame).await?;
        let result = self.raise_mailbox_login_error_if_present(session_id).await;
        self.switch_to_default_content(session_id).await?;
        result
    }

    async fn open_new_tab(&self, session_id: &str) -> io::Result<String> {
        let response = self
            .request_json(
                Method::POST,
                &format!("/session/{session_id}/window/new"),
                Some(json!({ "type": "tab" })),
            )
            .await?;
        response
            .get("handle")
            .and_then(Value::as_str)
            .map(str::to_string)
            .ok_or_else(|| io::Error::other("webdriver did not return a new tab handle"))
    }

    async fn switch_to_window(&self, session_id: &str, handle: &str) -> io::Result<()> {
        self.request_json(
            Method::POST,
            &format!("/session/{session_id}/window"),
            Some(json!({ "handle": handle })),
        )
        .await?;
        Ok(())
    }

    async fn window_handles(&self, session_id: &str) -> io::Result<Vec<String>> {
        self.request_json(
            Method::GET,
            &format!("/session/{session_id}/window/handles"),
            Option::<Value>::None,
        )
        .await?
        .as_array()
        .map(|handles| {
            handles
                .iter()
                .filter_map(Value::as_str)
                .map(str::to_string)
                .collect::<Vec<_>>()
        })
        .ok_or_else(|| io::Error::other("webdriver did not return window handles"))
    }

    async fn switch_back_to_original_window(
        &self,
        session_id: &str,
        original_handle: &str,
        mail_handle: &str,
    ) -> io::Result<()> {
        let handles = self.window_handles(session_id).await?;
        let return_handle = handles
            .iter()
            .find(|handle| handle.as_str() == original_handle)
            .cloned()
            .or_else(|| {
                handles
                    .iter()
                    .find(|handle| handle.as_str() != mail_handle)
                    .cloned()
            })
            .ok_or_else(|| io::Error::other("webdriver lost the original ChatGPT tab"))?;
        let _ = self.switch_to_default_content(session_id).await;
        self.switch_to_window(session_id, &return_handle).await
    }
}

fn is_missing_openai_verification_email_error(err: &io::Error) -> bool {
    err.to_string()
        .contains("timed out waiting for OpenAI verification email in 163 mailbox")
}

fn is_email_verification_attempts_exhausted_text(text: &str) -> bool {
    text.contains("max_check_attempts")
        || text.contains("验证过程中出错")
        || (text.contains("please retry") && text.contains("verification"))
}
