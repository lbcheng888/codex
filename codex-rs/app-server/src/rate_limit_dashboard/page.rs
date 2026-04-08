pub(crate) const DASHBOARD_PAGE: &str = r#"<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>Rate Limit Dashboard</title>
  <style>
    :root {
      --bg: #ffffff;
      --fg: #111111;
      --muted: #666666;
      --line: #dddddd;
      --danger: #b42318;
      --success: #117a37;
      --loading: #9a6700;
    }

    * {
      box-sizing: border-box;
    }

    body {
      margin: 0;
      background: var(--bg);
      color: var(--fg);
      font: 14px/1.4 -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
    }

    .shell {
      max-width: 1280px;
      margin: 0 auto;
      padding: 16px;
    }

    .toolbar {
      display: grid;
      grid-template-columns: minmax(0, 1fr) 180px auto auto;
      gap: 8px;
      margin-bottom: 12px;
    }

    input,
    select,
    button {
      height: 36px;
      padding: 0 10px;
      border: 1px solid var(--line);
      background: #fff;
      color: var(--fg);
      font: inherit;
    }

    button {
      cursor: pointer;
    }

    button:disabled {
      cursor: progress;
      color: var(--muted);
    }

    .status-bar {
      display: flex;
      justify-content: space-between;
      gap: 8px;
      margin-bottom: 12px;
      color: var(--muted);
    }

    .banner {
      display: none;
      margin-bottom: 12px;
      color: var(--danger);
    }

    table {
      width: 100%;
      border-collapse: collapse;
      table-layout: fixed;
    }

    th,
    td {
      padding: 8px 10px;
      border: 1px solid var(--line);
      text-align: left;
      vertical-align: middle;
      word-break: break-word;
    }

    th {
      background: #f7f7f7;
      font-weight: 600;
    }

    .status-idle {
      color: var(--muted);
    }

    .status-loading {
      color: var(--loading);
    }

    .status-success {
      color: var(--success);
    }

    .status-failed {
      color: var(--danger);
    }

    .mono {
      font-family: ui-monospace, SFMono-Regular, Menlo, monospace;
    }

    .error-summary {
      color: var(--danger);
    }

    .error-details {
      color: var(--danger);
    }

    .error-details summary {
      cursor: pointer;
      list-style: none;
    }

    .error-details summary::-webkit-details-marker {
      display: none;
    }

    .error-full {
      margin-top: 6px;
      max-height: 120px;
      overflow: auto;
      white-space: pre-wrap;
      word-break: break-word;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.5;
    }

    .action-group {
      display: flex;
      gap: 6px;
      flex-wrap: wrap;
    }

    .action-group button {
      flex: 1 1 72px;
      min-width: 0;
    }

    .empty {
      padding: 24px 0;
      color: var(--muted);
    }

    @media (max-width: 960px) {
      .toolbar {
        grid-template-columns: 1fr;
      }

      .status-bar {
        flex-direction: column;
      }

      table {
        font-size: 13px;
      }
    }
  </style>
</head>
<body>
  <div class="shell">
    <div class="toolbar">
      <input id="search" type="search" placeholder="Search accounts by email" />
      <select id="status-filter">
        <option value="all">All statuses</option>
        <option value="success">Success</option>
        <option value="failed">Failed</option>
        <option value="loading">Loading</option>
        <option value="idle">Idle</option>
      </select>
      <button id="refresh-all" type="button">Refresh all accounts</button>
      <button id="reload" type="button">Reload cache</button>
    </div>

    <div id="banner" class="banner"></div>

    <div class="status-bar">
      <div id="status-copy">Waiting for data.</div>
      <div id="last-updated">Last updated: never</div>
    </div>

    <table>
      <thead>
        <tr>
          <th style="width: 20%">Account</th>
          <th style="width: 8%">Status</th>
          <th style="width: 8%">Plan</th>
          <th style="width: 12%">5 hours left</th>
          <th style="width: 12%">7 days left</th>
          <th style="width: 14%">Updated</th>
          <th style="width: 18%">Error</th>
          <th style="width: 8%">Action</th>
        </tr>
      </thead>
      <tbody id="account-rows"></tbody>
    </table>

    <div id="empty-state" class="empty" hidden>No accounts match the current filters.</div>
  </div>

  <script>
    const state = {
      connected: false,
      loading: false,
      reconnectTimer: null,
      response: null,
      search: "",
      socket: null,
      statusFilter: "all",
    };

    const dom = {
      accountRows: document.getElementById("account-rows"),
      banner: document.getElementById("banner"),
      emptyState: document.getElementById("empty-state"),
      lastUpdated: document.getElementById("last-updated"),
      reload: document.getElementById("reload"),
      refreshAll: document.getElementById("refresh-all"),
      search: document.getElementById("search"),
      statusCopy: document.getElementById("status-copy"),
      statusFilter: document.getElementById("status-filter"),
    };

    function formatTimestamp(value) {
      if (!value) {
        return "never";
      }
      return new Intl.DateTimeFormat(undefined, {
        dateStyle: "medium",
        timeStyle: "short",
      }).format(new Date(value * 1000));
    }

    function escapeHtml(text) {
      return String(text)
        .replaceAll("&", "&amp;")
        .replaceAll("<", "&lt;")
        .replaceAll(">", "&gt;")
        .replaceAll('"', "&quot;")
        .replaceAll("'", "&#39;");
    }

    function websocketUrl() {
      const protocol = window.location.protocol === "https:" ? "wss" : "ws";
      return `${protocol}://${window.location.host}/rate-limit-dashboard/ws`;
    }

    function inferLoading(response) {
      return (response?.accounts || []).some((account) => account.status === "loading");
    }

    function codexSnapshot(account) {
      return (account.rateLimits || []).find((snapshot) => snapshot.limitId === "codex") || null;
    }

    function remaining(windowData) {
      if (!windowData || typeof windowData.usedPercent !== "number") {
        return "-";
      }
      return `${Math.max(0, 100 - windowData.usedPercent)}%`;
    }

    function plan(snapshot) {
      return snapshot?.planType || "-";
    }

    function statusClass(status) {
      return `status-${status}`;
    }

    function summarizeError(error) {
      if (!error) {
        return "";
      }
      const normalized = error.trim();
      const detailStart = normalized.indexOf(" (");
      if (detailStart > 0) {
        return normalized.slice(0, detailStart);
      }
      return normalized;
    }

    function renderErrorCell(error) {
      if (!error) {
        return "";
      }
      const summary = summarizeError(error);
      if (summary === error) {
        return `<span class="error-summary" title="${escapeHtml(error)}">${escapeHtml(summary)}</span>`;
      }
      return `
        <details class="error-details">
          <summary title="${escapeHtml(error)}">${escapeHtml(summary)}</summary>
          <div class="error-full mono">${escapeHtml(error)}</div>
        </details>
      `;
    }

    function supportsManualLogin(account) {
      return account.status === "failed";
    }

    function matchesFilter(account) {
      if (state.statusFilter !== "all" && account.status !== state.statusFilter) {
        return false;
      }
      if (!state.search) {
        return true;
      }
      return account.email.toLowerCase().includes(state.search);
    }

    function renderAccounts() {
      if (!state.response) {
        dom.accountRows.innerHTML = "";
        dom.emptyState.hidden = true;
        return;
      }

      const accounts = (state.response?.accounts || []).filter(matchesFilter);
      dom.accountRows.innerHTML = "";

      if (!accounts.length) {
        dom.emptyState.hidden = false;
        return;
      }

      dom.emptyState.hidden = true;
      const refreshDisabled = !state.connected || state.loading ? " disabled" : "";
      for (const account of accounts) {
        const snapshot = codexSnapshot(account);
        const row = document.createElement("tr");
        const buttons = [];
        if (supportsManualLogin(account)) {
          buttons.push(
            `<button type="button" data-login="${encodeURIComponent(account.email)}">Login</button>`,
          );
        }
        buttons.push(
          `<button type="button" data-account="${encodeURIComponent(account.email)}"${refreshDisabled}>Refresh</button>`,
        );
        row.innerHTML = `
          <td class="mono">${escapeHtml(account.email)}</td>
          <td class="${statusClass(account.status)}">${escapeHtml(account.status)}</td>
          <td>${escapeHtml(plan(snapshot))}</td>
          <td>${escapeHtml(remaining(snapshot?.primary))}</td>
          <td>${escapeHtml(remaining(snapshot?.secondary))}</td>
          <td>${escapeHtml(formatTimestamp(account.fetchedAt))}</td>
          <td>${renderErrorCell(account.error)}</td>
          <td><div class="action-group">${buttons.join("")}</div></td>
        `;
        row
          .querySelector("button[data-account]")
          .addEventListener("click", () => refreshAccount(account.email));
        const loginButton = row.querySelector("button[data-login]");
        if (loginButton) {
          loginButton.addEventListener("click", () => startManualLogin(account.email));
        }
        dom.accountRows.appendChild(row);
      }
    }

    function renderMeta() {
      const source = state.response?.source || "fresh";
      dom.lastUpdated.textContent = `Last updated: ${formatTimestamp(state.response?.lastUpdatedAt)}`;
      if (!state.connected) {
        dom.statusCopy.textContent = "Dashboard websocket disconnected. Reconnecting.";
      } else if (state.loading) {
        dom.statusCopy.textContent = "Refreshing rate limit data.";
      } else if (!state.response) {
        dom.statusCopy.textContent = "Connected. Waiting for dashboard data.";
      } else if (source === "cache") {
        dom.statusCopy.textContent = "Showing cached data.";
      } else {
        dom.statusCopy.textContent = "Showing latest data.";
      }
      dom.refreshAll.disabled = !state.connected || state.loading;
      dom.reload.disabled = !state.connected || state.loading;
    }

    function renderError(message) {
      if (!message) {
        dom.banner.style.display = "none";
        dom.banner.textContent = "";
        return;
      }
      dom.banner.style.display = "block";
      dom.banner.textContent = message;
    }

    function render() {
      renderMeta();
      renderAccounts();
    }

    function handleSnapshot(payload) {
      state.response = payload;
      state.loading = inferLoading(payload);
      render();
    }

    function sendSocketMessage(message) {
      if (!state.socket || state.socket.readyState !== WebSocket.OPEN) {
        renderError("Dashboard websocket not connected.");
        render();
        return false;
      }
      state.socket.send(JSON.stringify(message));
      return true;
    }

    function scheduleReconnect() {
      if (state.reconnectTimer) {
        return;
      }
      state.reconnectTimer = window.setTimeout(() => {
        state.reconnectTimer = null;
        connectSocket();
      }, 1000);
    }

    function connectSocket() {
      if (state.socket && state.socket.readyState <= WebSocket.OPEN) {
        return;
      }

      const socket = new WebSocket(websocketUrl());
      state.socket = socket;
      state.connected = false;
      render();

      socket.addEventListener("open", () => {
        if (state.socket !== socket) {
          return;
        }
        state.connected = true;
        renderError("");
        render();
      });

      socket.addEventListener("message", (event) => {
        if (state.socket !== socket) {
          return;
        }
        try {
          const payload = JSON.parse(event.data);
          if (payload.type === "snapshot") {
            handleSnapshot(payload.data);
            return;
          }
          if (payload.type === "error") {
            state.loading = inferLoading(state.response);
            renderError(payload.message || "Dashboard websocket command failed.");
            render();
            return;
          }
          renderError(`Unsupported dashboard websocket message: ${payload.type || "unknown"}`);
        } catch (error) {
          renderError(`Invalid dashboard websocket payload: ${error.message}`);
        }
      });

      socket.addEventListener("close", () => {
        if (state.socket !== socket) {
          return;
        }
        state.connected = false;
        state.socket = null;
        state.loading = inferLoading(state.response);
        render();
        scheduleReconnect();
      });

      socket.addEventListener("error", () => {
        if (state.socket !== socket) {
          return;
        }
        renderError("Dashboard websocket error.");
      });
    }

    function requestSnapshot() {
      renderError("");
      sendSocketMessage({ type: "snapshot" });
    }

    function refreshAll() {
      renderError("");
      if (sendSocketMessage({ type: "refreshAll" })) {
        state.loading = true;
        render();
      }
    }

    function refreshAccount(email) {
      renderError("");
      if (sendSocketMessage({ type: "refreshAccount", email })) {
        state.loading = true;
        render();
      }
    }

    function startManualLogin(email) {
      renderError(`OpenAI device-code login for ${email} opened in a new tab. Finish it there, then click Refresh.`);
      window.open(`/api/rate-limits/login/${encodeURIComponent(email)}`, "_blank", "noopener");
    }

    dom.search.addEventListener("input", (event) => {
      state.search = event.target.value.trim().toLowerCase();
      renderAccounts();
    });

    dom.statusFilter.addEventListener("change", (event) => {
      state.statusFilter = event.target.value;
      renderAccounts();
    });

    dom.refreshAll.addEventListener("click", refreshAll);
    dom.reload.addEventListener("click", requestSnapshot);

    render();
    connectSocket();
  </script>
</body>
</html>
"#;
