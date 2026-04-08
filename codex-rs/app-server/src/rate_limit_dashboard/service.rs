use async_trait::async_trait;
use chrono::Utc;
use codex_app_server_protocol::RateLimitSnapshot;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;
use std::collections::HashSet;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::sync::watch;
use tracing::warn;

#[async_trait]
pub(crate) trait RateLimitDashboardFetcher: Send + Sync {
    async fn fetch_account_rate_limits(
        &self,
        email: &str,
        password: &str,
        mailbox_password: Option<&str>,
    ) -> io::Result<Vec<RateLimitSnapshot>>;

    async fn start_manual_login(&self, email: &str) -> io::Result<ManualLoginStart>;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ManualLoginStart {
    pub verification_url: String,
    pub user_code: String,
    pub html: String,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub(crate) enum DashboardDataSource {
    Cache,
    Fresh,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub(crate) enum ManagedRateLimitStatus {
    Idle,
    Loading,
    Success,
    Failed,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub(crate) struct ManagedRateLimitRecord {
    pub email: String,
    pub status: ManagedRateLimitStatus,
    pub fetched_at: Option<i64>,
    #[serde(default)]
    pub rate_limits: Vec<RateLimitSnapshot>,
    pub error: Option<String>,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub(crate) struct RateLimitDashboardResponse {
    pub accounts: Vec<ManagedRateLimitRecord>,
    pub last_updated_at: Option<i64>,
    pub source: DashboardDataSource,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
struct DashboardCacheFile {
    accounts: Vec<ManagedRateLimitRecord>,
    last_updated_at: Option<i64>,
}

#[derive(Debug)]
struct AccountsConfig {
    accounts: Vec<String>,
    password: String,
    mailbox_password: Option<String>,
}

#[derive(Clone, Debug)]
struct DashboardState {
    accounts: Vec<ManagedRateLimitRecord>,
    last_updated_at: Option<i64>,
    source: DashboardDataSource,
}

impl DashboardState {
    fn response(&self) -> RateLimitDashboardResponse {
        RateLimitDashboardResponse {
            accounts: self.accounts.clone(),
            last_updated_at: self.last_updated_at,
            source: self.source.clone(),
        }
    }

    fn cache_file(&self) -> DashboardCacheFile {
        DashboardCacheFile {
            accounts: self.accounts.clone(),
            last_updated_at: self.last_updated_at,
        }
    }
}

pub(crate) struct RateLimitDashboardService {
    env_path: PathBuf,
    cache_path: PathBuf,
    fetcher: Arc<dyn RateLimitDashboardFetcher>,
    refresh_lock: Mutex<()>,
    state: Mutex<DashboardState>,
    updates_tx: watch::Sender<RateLimitDashboardResponse>,
}

impl RateLimitDashboardService {
    pub(crate) fn new(
        env_path: PathBuf,
        cache_path: PathBuf,
        fetcher: Arc<dyn RateLimitDashboardFetcher>,
    ) -> Self {
        let cached_state = load_cached_state(&cache_path);
        let configured_accounts = load_accounts_config(&env_path)
            .map(|config| config.accounts)
            .unwrap_or_default();
        let mut state = cached_state.unwrap_or(DashboardState {
            accounts: Vec::new(),
            last_updated_at: None,
            source: DashboardDataSource::Fresh,
        });
        reconcile_accounts(&configured_accounts, &mut state.accounts);
        let (updates_tx, _updates_rx) = watch::channel(state.response());

        Self {
            env_path,
            cache_path,
            fetcher,
            refresh_lock: Mutex::new(()),
            state: Mutex::new(state),
            updates_tx,
        }
    }

    pub(crate) fn subscribe(&self) -> watch::Receiver<RateLimitDashboardResponse> {
        self.updates_tx.subscribe()
    }

    pub(crate) async fn snapshot(&self) -> io::Result<RateLimitDashboardResponse> {
        let accounts_config = load_accounts_config(&self.env_path)?;
        let response = {
            let mut state = self.state.lock().await;
            reconcile_accounts(&accounts_config.accounts, &mut state.accounts);
            state.response()
        };
        self.publish(response.clone());
        Ok(response)
    }

    pub(crate) async fn refresh_all(&self) -> io::Result<RateLimitDashboardResponse> {
        let _refresh_guard = self.refresh_lock.lock().await;
        let accounts_config = load_accounts_config(&self.env_path)?;

        {
            let mut state = self.state.lock().await;
            reconcile_accounts(&accounts_config.accounts, &mut state.accounts);
            for record in &mut state.accounts {
                record.status = ManagedRateLimitStatus::Loading;
                record.error = None;
            }
            self.publish(state.response());
        }

        for email in &accounts_config.accounts {
            let result = self
                .fetcher
                .fetch_account_rate_limits(
                    email,
                    &accounts_config.password,
                    accounts_config.mailbox_password.as_deref(),
                )
                .await;
            self.finish_refresh(email, result).await?;
        }

        self.finalize_refresh().await
    }

    pub(crate) async fn refresh_account(
        &self,
        target_email: &str,
    ) -> io::Result<RateLimitDashboardResponse> {
        let _refresh_guard = self.refresh_lock.lock().await;
        let accounts_config = load_accounts_config(&self.env_path)?;
        if !accounts_config
            .accounts
            .iter()
            .any(|email| email == target_email)
        {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("account `{target_email}` is not configured in .env"),
            ));
        }

        {
            let mut state = self.state.lock().await;
            reconcile_accounts(&accounts_config.accounts, &mut state.accounts);
            if let Some(record) = state
                .accounts
                .iter_mut()
                .find(|record| record.email == target_email)
            {
                record.status = ManagedRateLimitStatus::Loading;
                record.error = None;
            }
            self.publish(state.response());
        }

        let result = self
            .fetcher
            .fetch_account_rate_limits(
                target_email,
                &accounts_config.password,
                accounts_config.mailbox_password.as_deref(),
            )
            .await;
        self.finish_refresh(target_email, result).await?;
        self.finalize_refresh().await
    }

    pub(crate) async fn start_manual_login(
        &self,
        target_email: &str,
    ) -> io::Result<ManualLoginStart> {
        let accounts_config = load_accounts_config(&self.env_path)?;
        if !accounts_config
            .accounts
            .iter()
            .any(|email| email == target_email)
        {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("account `{target_email}` is not configured in .env"),
            ));
        }

        self.fetcher.start_manual_login(target_email).await
    }

    async fn finish_refresh(
        &self,
        email: &str,
        result: io::Result<Vec<RateLimitSnapshot>>,
    ) -> io::Result<()> {
        let response = {
            let mut state = self.state.lock().await;
            if let Some(record) = state
                .accounts
                .iter_mut()
                .find(|record| record.email == email)
            {
                match result {
                    Ok(mut rate_limits) => {
                        sort_snapshots(&mut rate_limits);
                        record.status = ManagedRateLimitStatus::Success;
                        record.fetched_at = Some(Utc::now().timestamp());
                        record.rate_limits = rate_limits;
                        record.error = None;
                    }
                    Err(err) => {
                        record.status = ManagedRateLimitStatus::Failed;
                        record.error = Some(err.to_string());
                    }
                }
            }
            persist_cache(&self.cache_path, &state)?;
            state.response()
        };
        self.publish(response);
        Ok(())
    }

    async fn finalize_refresh(&self) -> io::Result<RateLimitDashboardResponse> {
        let response = {
            let mut state = self.state.lock().await;
            state.last_updated_at = Some(Utc::now().timestamp());
            state.source = DashboardDataSource::Fresh;
            persist_cache(&self.cache_path, &state)?;
            state.response()
        };
        self.publish(response.clone());
        Ok(response)
    }

    fn publish(&self, response: RateLimitDashboardResponse) {
        let _ = self.updates_tx.send(response);
    }
}

fn load_cached_state(cache_path: &Path) -> Option<DashboardState> {
    let contents = match std::fs::read_to_string(cache_path) {
        Ok(contents) => contents,
        Err(err) if err.kind() == io::ErrorKind::NotFound => return None,
        Err(err) => {
            warn!(
                "failed to read rate limit cache from {}: {err}",
                cache_path.display()
            );
            return None;
        }
    };

    match serde_json::from_str::<DashboardCacheFile>(&contents) {
        Ok(cache_file) => Some(DashboardState {
            accounts: cache_file.accounts,
            last_updated_at: cache_file.last_updated_at,
            source: DashboardDataSource::Cache,
        }),
        Err(err) => {
            warn!(
                "failed to parse rate limit cache from {}: {err}",
                cache_path.display()
            );
            None
        }
    }
}

fn persist_cache(cache_path: &Path, state: &DashboardState) -> io::Result<()> {
    if let Some(parent) = cache_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let payload = serde_json::to_vec_pretty(&state.cache_file())
        .map_err(|err| io::Error::other(format!("failed to serialize cache: {err}")))?;
    std::fs::write(cache_path, payload)
}

fn reconcile_accounts(configured_accounts: &[String], records: &mut Vec<ManagedRateLimitRecord>) {
    let mut existing_by_email: HashMap<String, ManagedRateLimitRecord> = records
        .drain(..)
        .map(|record| (record.email.clone(), record))
        .collect();

    let mut next_records = Vec::with_capacity(configured_accounts.len());
    for email in configured_accounts {
        if let Some(record) = existing_by_email.remove(email) {
            next_records.push(record);
        } else {
            next_records.push(ManagedRateLimitRecord {
                email: email.clone(),
                status: ManagedRateLimitStatus::Idle,
                fetched_at: None,
                rate_limits: Vec::new(),
                error: None,
            });
        }
    }
    *records = next_records;
}

fn sort_snapshots(rate_limits: &mut [RateLimitSnapshot]) {
    rate_limits.sort_by(|left, right| {
        left.limit_id
            .cmp(&right.limit_id)
            .then(left.limit_name.cmp(&right.limit_name))
    });
}

fn load_accounts_config(env_path: &Path) -> io::Result<AccountsConfig> {
    let contents = std::fs::read_to_string(env_path).map_err(|err| {
        io::Error::new(
            err.kind(),
            format!("failed to read env file {}: {err}", env_path.display()),
        )
    })?;

    let mut accounts_value = None;
    let mut password = None;
    let mut mailbox_password = None;
    for raw_line in contents.lines() {
        let trimmed = raw_line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        let normalized = trimmed.strip_prefix("export ").unwrap_or(trimmed);
        let Some((key, raw_value)) = normalized.split_once('=') else {
            continue;
        };
        let value = strip_wrapping_quotes(raw_value.trim());
        match key.trim() {
            "ACCOUNTS" => accounts_value = Some(value.to_string()),
            "PASSWORD" => password = Some(value.to_string()),
            "MAILBOX_PASSWORD" => mailbox_password = Some(value.to_string()),
            _ => {}
        }
    }

    let accounts_value = accounts_value.ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("ACCOUNTS is missing from {}", env_path.display()),
        )
    })?;
    let password = password.ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("PASSWORD is missing from {}", env_path.display()),
        )
    })?;

    let mut seen = HashSet::new();
    let accounts = accounts_value
        .split_whitespace()
        .filter(|value| !value.is_empty())
        .filter_map(|value| {
            let email = value.trim().to_string();
            if seen.insert(email.clone()) {
                Some(email)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    if accounts.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("ACCOUNTS in {} is empty", env_path.display()),
        ));
    }
    if password.trim().is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("PASSWORD in {} is empty", env_path.display()),
        ));
    }

    if let Some(mailbox_password) = &mailbox_password
        && mailbox_password.trim().is_empty()
    {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!("MAILBOX_PASSWORD in {} is empty", env_path.display()),
        ));
    }

    Ok(AccountsConfig {
        accounts,
        password,
        mailbox_password,
    })
}

fn strip_wrapping_quotes(value: &str) -> &str {
    if value.len() >= 2
        && let Some(quote) = value.chars().next()
        && (quote == '"' || quote == '\'')
        && value.ends_with(quote)
    {
        return &value[1..value.len() - 1];
    }
    value
}

#[cfg(test)]
mod tests {
    use super::DashboardDataSource;
    use super::ManagedRateLimitRecord;
    use super::ManagedRateLimitStatus;
    use super::RateLimitDashboardFetcher;
    use super::RateLimitDashboardService;
    use async_trait::async_trait;
    use codex_app_server_protocol::CreditsSnapshot;
    use codex_app_server_protocol::RateLimitSnapshot;
    use codex_app_server_protocol::RateLimitWindow;
    use codex_protocol::account::PlanType;
    use pretty_assertions::assert_eq;
    use std::collections::HashMap;
    use std::io;
    use std::sync::Arc;
    use tempfile::TempDir;

    struct FakeFetcher {
        responses: HashMap<String, io::Result<Vec<RateLimitSnapshot>>>,
    }

    #[async_trait]
    impl RateLimitDashboardFetcher for FakeFetcher {
        async fn fetch_account_rate_limits(
            &self,
            email: &str,
            _password: &str,
            _mailbox_password: Option<&str>,
        ) -> io::Result<Vec<RateLimitSnapshot>> {
            match self.responses.get(email) {
                Some(Ok(value)) => Ok(value.clone()),
                Some(Err(err)) => Err(io::Error::new(err.kind(), err.to_string())),
                None => Err(io::Error::new(
                    io::ErrorKind::NotFound,
                    format!("missing fake response for {email}"),
                )),
            }
        }

        async fn start_manual_login(&self, email: &str) -> io::Result<super::ManualLoginStart> {
            Ok(super::ManualLoginStart {
                verification_url: "https://example.com/device".to_string(),
                user_code: "ABCD-1234".to_string(),
                html: format!("<html><body>{email}</body></html>"),
            })
        }
    }

    #[test]
    fn load_accounts_config_supports_quoted_values() {
        let temp_dir = TempDir::new().expect("temp dir should exist");
        let env_path = temp_dir.path().join(".env");
        std::fs::write(
            &env_path,
            "ACCOUNTS=\"alpha@example.com beta@example.com alpha@example.com\"\nPASSWORD='secret'\n",
        )
        .expect("env file should write");

        let config = super::load_accounts_config(&env_path).expect("env config should parse");
        assert_eq!(
            vec![
                "alpha@example.com".to_string(),
                "beta@example.com".to_string()
            ],
            config.accounts
        );
        assert_eq!("secret", config.password);
        assert_eq!(None, config.mailbox_password);
    }

    #[test]
    fn load_accounts_config_reads_optional_mailbox_password() {
        let temp_dir = TempDir::new().expect("temp dir should exist");
        let env_path = temp_dir.path().join(".env");
        std::fs::write(
            &env_path,
            "ACCOUNTS=alpha@example.com\nPASSWORD=secret\nMAILBOX_PASSWORD=mail-secret\n",
        )
        .expect("env file should write");

        let config = super::load_accounts_config(&env_path).expect("env config should parse");
        assert_eq!(Some("mail-secret".to_string()), config.mailbox_password);
    }

    #[tokio::test]
    async fn refresh_all_persists_cache_and_reloads_from_disk() {
        let temp_dir = TempDir::new().expect("temp dir should exist");
        let env_path = temp_dir.path().join(".env");
        let cache_path = temp_dir.path().join("dashboard-cache.json");
        std::fs::write(
            &env_path,
            "ACCOUNTS=\"alpha@example.com beta@example.com\"\nPASSWORD=secret\n",
        )
        .expect("env file should write");

        let mut responses = HashMap::new();
        responses.insert(
            "alpha@example.com".to_string(),
            Ok(vec![sample_snapshot(18)]),
        );
        responses.insert(
            "beta@example.com".to_string(),
            Err(io::Error::other("webdriver disabled")),
        );
        let service = RateLimitDashboardService::new(
            env_path.clone(),
            cache_path.clone(),
            Arc::new(FakeFetcher { responses }),
        );

        let initial = service.snapshot().await.expect("snapshot should load");
        assert_eq!(DashboardDataSource::Fresh, initial.source);
        assert_eq!(
            vec![
                ManagedRateLimitRecord {
                    email: "alpha@example.com".to_string(),
                    status: ManagedRateLimitStatus::Idle,
                    fetched_at: None,
                    rate_limits: Vec::new(),
                    error: None,
                },
                ManagedRateLimitRecord {
                    email: "beta@example.com".to_string(),
                    status: ManagedRateLimitStatus::Idle,
                    fetched_at: None,
                    rate_limits: Vec::new(),
                    error: None,
                },
            ],
            initial.accounts
        );

        let refreshed = service.refresh_all().await.expect("refresh should succeed");
        assert_eq!(DashboardDataSource::Fresh, refreshed.source);
        assert_eq!(
            ManagedRateLimitStatus::Success,
            refreshed.accounts[0].status
        );
        assert_eq!(ManagedRateLimitStatus::Failed, refreshed.accounts[1].status);
        assert_eq!(
            Some("webdriver disabled".to_string()),
            refreshed.accounts[1].error
        );
        assert_eq!(vec![sample_snapshot(18)], refreshed.accounts[0].rate_limits);

        let reloaded = RateLimitDashboardService::new(
            env_path,
            cache_path,
            Arc::new(FakeFetcher {
                responses: HashMap::new(),
            }),
        )
        .snapshot()
        .await
        .expect("cached snapshot should load");
        assert_eq!(DashboardDataSource::Cache, reloaded.source);
        assert_eq!(refreshed.accounts, reloaded.accounts);
    }

    #[tokio::test]
    async fn refresh_all_publishes_latest_snapshot_to_subscribers() {
        let temp_dir = TempDir::new().expect("temp dir should exist");
        let env_path = temp_dir.path().join(".env");
        let cache_path = temp_dir.path().join("dashboard-cache.json");
        std::fs::write(&env_path, "ACCOUNTS=alpha@example.com\nPASSWORD=secret\n")
            .expect("env file should write");

        let mut responses = HashMap::new();
        responses.insert(
            "alpha@example.com".to_string(),
            Ok(vec![sample_snapshot(25)]),
        );
        let service = RateLimitDashboardService::new(
            env_path,
            cache_path,
            Arc::new(FakeFetcher { responses }),
        );
        let mut updates = service.subscribe();

        let refreshed = service.refresh_all().await.expect("refresh should succeed");

        updates
            .changed()
            .await
            .expect("subscriber should observe a refresh update");
        assert_eq!(refreshed, updates.borrow().clone());
    }

    #[tokio::test]
    async fn start_manual_login_rejects_unconfigured_account() {
        let temp_dir = TempDir::new().expect("temp dir should exist");
        let env_path = temp_dir.path().join(".env");
        let cache_path = temp_dir.path().join("dashboard-cache.json");
        std::fs::write(&env_path, "ACCOUNTS=alpha@example.com\nPASSWORD=secret\n")
            .expect("env file should write");

        let service = RateLimitDashboardService::new(
            env_path,
            cache_path,
            Arc::new(FakeFetcher {
                responses: HashMap::new(),
            }),
        );

        let err = service
            .start_manual_login("beta@example.com")
            .await
            .expect_err("unconfigured account should fail");
        assert_eq!(io::ErrorKind::NotFound, err.kind());
    }

    fn sample_snapshot(used_percent: u8) -> RateLimitSnapshot {
        RateLimitSnapshot {
            limit_id: Some("codex".to_string()),
            limit_name: Some("Codex".to_string()),
            primary: Some(RateLimitWindow {
                used_percent: used_percent.into(),
                window_duration_mins: Some(300),
                resets_at: Some(1234),
            }),
            secondary: None,
            credits: Some(CreditsSnapshot {
                has_credits: true,
                unlimited: false,
                balance: Some("42".to_string()),
            }),
            plan_type: Some(PlanType::Plus),
        }
    }
}
