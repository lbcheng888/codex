use serde_json::Value;
use serde_json::json;
use std::env;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::process::Stdio;
use std::time::Duration;
use tokio::process::Command;

const SAFARI_WEBDRIVER_PORT: u16 = 7055;
const CHROME_WEBDRIVER_PORT: u16 = 9517;
const CHROME_READY_TIMEOUT: Duration = Duration::from_secs(45);
const SAFARI_READY_TIMEOUT: Duration = Duration::from_secs(8);
const DEFAULT_CHROME_BINARY_PATH: &str =
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const DASHBOARD_BROWSER_ENV: &str = "CODEX_RATE_LIMIT_DASHBOARD_BROWSER";
const WEBDRIVER_BROWSER_ENV: &str = "CODEX_RATE_LIMIT_DASHBOARD_WEBDRIVER_BROWSER";
const WEBDRIVER_URL_ENV: &str = "CODEX_RATE_LIMIT_DASHBOARD_WEBDRIVER_URL";
const CHROME_BINARY_ENV: &str = "CODEX_RATE_LIMIT_DASHBOARD_CHROME_BINARY";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum WebDriverBrowser {
    Chrome,
    Safari,
}

impl WebDriverBrowser {
    pub(super) fn session_capabilities(self) -> Value {
        match self {
            Self::Chrome => {
                let mut chrome_options = serde_json::Map::from_iter([
                    (
                        "args".to_string(),
                        json!(["--disable-blink-features=AutomationControlled"]),
                    ),
                    ("excludeSwitches".to_string(), json!(["enable-automation"])),
                ]);
                if let Some(binary) = local_chrome_binary_path() {
                    chrome_options.insert(
                        "binary".to_string(),
                        Value::String(binary.display().to_string()),
                    );
                }
                json!({
                    "browserName": "chrome",
                    "acceptInsecureCerts": true,
                    "pageLoadStrategy": "eager",
                    "goog:chromeOptions": chrome_options,
                })
            }
            Self::Safari => json!({
                "browserName": "Safari",
                "acceptInsecureCerts": true,
                "pageLoadStrategy": "eager",
            }),
        }
    }

    pub(super) fn label(self) -> &'static str {
        match self {
            Self::Chrome => "Chrome",
            Self::Safari => "Safari",
        }
    }

    fn default_base_url(self) -> String {
        match self {
            Self::Chrome => format!("http://127.0.0.1:{CHROME_WEBDRIVER_PORT}"),
            Self::Safari => format!("http://127.0.0.1:{SAFARI_WEBDRIVER_PORT}"),
        }
    }

    fn ready_timeout(self) -> Duration {
        match self {
            Self::Chrome => CHROME_READY_TIMEOUT,
            Self::Safari => SAFARI_READY_TIMEOUT,
        }
    }

    fn unavailable_message(self) -> &'static str {
        match self {
            Self::Chrome => {
                "Chrome WebDriver is unavailable; install Google Chrome and ensure `npx` is on PATH, or set CODEX_RATE_LIMIT_DASHBOARD_WEBDRIVER_URL with CODEX_RATE_LIMIT_DASHBOARD_WEBDRIVER_BROWSER=chrome"
            }
            Self::Safari => {
                "Safari WebDriver is unavailable; on macOS enable Safari Settings > Developer > Allow Remote Automation, or set CODEX_RATE_LIMIT_DASHBOARD_WEBDRIVER_URL with CODEX_RATE_LIMIT_DASHBOARD_WEBDRIVER_BROWSER=safari"
            }
        }
    }
}

pub(super) struct WebDriverLaunchSpec {
    pub(super) base_url: String,
    pub(super) browser: WebDriverBrowser,
    pub(super) ready_timeout: Duration,
}

impl WebDriverLaunchSpec {
    pub(super) fn unavailable_message(&self) -> &'static str {
        self.browser.unavailable_message()
    }
}

pub(super) fn uses_external_webdriver() -> bool {
    env::var(WEBDRIVER_URL_ENV).is_ok()
}

pub(super) fn resolve_webdriver_launch_spec() -> io::Result<WebDriverLaunchSpec> {
    let browser = selected_browser()?;
    let base_url = env::var(WEBDRIVER_URL_ENV).unwrap_or_else(|_| browser.default_base_url());
    Ok(WebDriverLaunchSpec {
        base_url,
        browser,
        ready_timeout: browser.ready_timeout(),
    })
}

pub(super) async fn spawn_local_driver(browser: WebDriverBrowser) -> io::Result<()> {
    if uses_external_webdriver() {
        return Ok(());
    }

    match browser {
        WebDriverBrowser::Chrome => spawn_chromedriver().await,
        WebDriverBrowser::Safari => spawn_safaridriver().await,
    }
}

fn selected_browser() -> io::Result<WebDriverBrowser> {
    if let Some(browser) = browser_from_env(WEBDRIVER_BROWSER_ENV)? {
        return Ok(browser);
    }
    if let Some(browser) = browser_from_env(DASHBOARD_BROWSER_ENV)? {
        return Ok(browser);
    }

    if cfg!(target_os = "macos") && local_chrome_binary_path().is_some() && command_in_path("npx") {
        return Ok(WebDriverBrowser::Chrome);
    }
    if cfg!(target_os = "macos") {
        return Ok(WebDriverBrowser::Safari);
    }

    Err(io::Error::other(
        "webdriver automation requires a configured CODEX_RATE_LIMIT_DASHBOARD_WEBDRIVER_URL on this platform",
    ))
}

fn browser_from_env(key: &str) -> io::Result<Option<WebDriverBrowser>> {
    let Ok(value) = env::var(key) else {
        return Ok(None);
    };
    match value.trim().to_ascii_lowercase().as_str() {
        "chrome" => Ok(Some(WebDriverBrowser::Chrome)),
        "safari" => Ok(Some(WebDriverBrowser::Safari)),
        other => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("{key} must be `chrome` or `safari`, got `{other}`"),
        )),
    }
}

fn command_in_path(command: &str) -> bool {
    env::var_os("PATH").is_some_and(|paths| {
        env::split_paths(&paths).any(|path| {
            let full_path = path.join(command);
            full_path.is_file()
        })
    })
}

fn local_chrome_binary_path() -> Option<PathBuf> {
    if let Ok(path) = env::var(CHROME_BINARY_ENV) {
        let path = PathBuf::from(path);
        if path.is_file() {
            return Some(path);
        }
    }

    let path = PathBuf::from(DEFAULT_CHROME_BINARY_PATH);
    path.is_file().then_some(path)
}

async fn spawn_chromedriver() -> io::Result<()> {
    let Some(chrome_binary_path) = local_chrome_binary_path() else {
        return Err(io::Error::other(
            "Google Chrome is not installed; set CODEX_RATE_LIMIT_DASHBOARD_BROWSER=safari to keep using Safari automation",
        ));
    };
    if !command_in_path("npx") {
        return Err(io::Error::other(
            "Chrome automation requires `npx` on PATH to launch a matching chromedriver",
        ));
    }

    let chrome_major = installed_chrome_major_version(&chrome_binary_path).await?;
    Command::new("npx")
        .arg("--yes")
        .arg(format!("chromedriver@{chrome_major}"))
        .arg(format!("--port={CHROME_WEBDRIVER_PORT}"))
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|err| io::Error::other(format!("failed to start chromedriver: {err}")))?;
    Ok(())
}

async fn spawn_safaridriver() -> io::Result<()> {
    if !cfg!(target_os = "macos") {
        return Err(io::Error::other(
            "Safari WebDriver is only supported on macOS",
        ));
    }

    Command::new("safaridriver")
        .arg("-p")
        .arg(SAFARI_WEBDRIVER_PORT.to_string())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|err| io::Error::other(format!("failed to start safaridriver: {err}")))?;
    Ok(())
}

async fn installed_chrome_major_version(chrome_binary_path: &Path) -> io::Result<u32> {
    let output = Command::new(chrome_binary_path)
        .arg("--version")
        .output()
        .await
        .map_err(|err| io::Error::other(format!("failed to inspect Chrome version: {err}")))?;
    if !output.status.success() {
        return Err(io::Error::other(format!(
            "failed to inspect Chrome version from {}",
            chrome_binary_path.display()
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    parse_chrome_major_version(&stdout)
}

fn parse_chrome_major_version(raw_version: &str) -> io::Result<u32> {
    let version_token = raw_version
        .split_whitespace()
        .last()
        .ok_or_else(|| io::Error::other("Chrome --version did not return a version number"))?;
    let major_token = version_token
        .split('.')
        .next()
        .ok_or_else(|| io::Error::other("Chrome version was missing a major component"))?;
    major_token.parse::<u32>().map_err(|err| {
        io::Error::other(format!(
            "failed to parse Chrome major version from `{raw_version}`: {err}"
        ))
    })
}

#[cfg(test)]
mod tests {
    use super::parse_chrome_major_version;
    use pretty_assertions::assert_eq;

    #[test]
    fn parse_chrome_major_version_reads_terminal_version_token() {
        assert_eq!(
            146,
            parse_chrome_major_version("Google Chrome 146.0.7680.178")
                .expect("Chrome major version should parse"),
        );
        assert_eq!(
            146,
            parse_chrome_major_version("Chromium 146.0.7680.178")
                .expect("Chromium major version should parse"),
        );
    }
}
