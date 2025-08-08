# 配置参考与常见问题

## [graphiti] 持久化上下文存储（默认启用）

- 作用：为会话状态、文件快照等提供本地持久化（Graphiti/Cozo 驱动），供 Agent 引擎与工具后续使用。
- 默认：开启。数据库文件位于 ${CODEX_HOME}/graphiti.db，命名空间为 codex_default。

配置示例（~/.codex/config.toml 或 ~/.codex/codex.toml）：

```toml
[graphiti]
# 可使用绝对路径，或相对 CODEX_HOME 的路径
# db_path = "/absolute/path/graphiti.db"
db_path = "state/graphiti.db"
namespace = "my_workspace"
```

环境变量覆盖：
- GRAPHITI_DB_PATH：覆盖数据库路径（相对路径仍以 CODEX_HOME 为基准解析）
- GRAPHITI_NAMESPACE：覆盖命名空间

常见问题：
- Q: 首次启动是否需要手动建表？
  - A: 不需要，系统会在首次初始化时自动创建所需表（ctx_kv、file_snapshots 等）。
- Q: 数据库存放在哪里？
  - A: 默认在 ${HOME}/.codex/graphiti.db，可通过配置覆盖。
- Q: 失败时是否影响主流程？
  - A: 初始化失败会告警并回退到内存实现，不会阻断主流程。

本文汇总 Serena 常用配置入口与关键字段，并解答集成中的常见问题。

## 配置文件一览
- serena.toml（Tools/CLI 路径）：定义项目、工具、日志等。
- serena-agent.toml（Agent 路径）：定义代理、计划、工具授权、上下文策略。
- serena-lsp.json（LSP 路径）：定义 LSP 端口、工作目录、语言清单与特性开关。

## serena.toml 字段（示例）
```toml
[project]
name = "demo"
root = "."

[logging]
level = "info"          # trace|debug|info|warn|error
file = "serena.log"      # 可选，本地日志文件

[tools.ECHO]
kind = "process"
cmd = "/bin/echo"
args = ["hello"]
timeout_ms = 2000

[tools.HTTPBIN]
kind = "http"
method = "GET"
url = "https://httpbin.org/get"
headers.Content-Type = "application/json"
```

## serena-agent.toml 字段（示例）
```toml
[agent]
name = "demo-agent"
policy = "sequential"    # sequential|reactive|custom

[tools]
allow = ["ECHO", "HTTPBIN"]

[context]
max_tokens = 8192
retain_history = true
```

## serena-lsp.json 字段（示例）
```json
{
  "listen": { "host": "127.0.0.1", "port": 9333 },
  "workspace": ".",
  "languages": ["rust", "typescript", "python"],
  "features": { "completion": true, "hover": true, "diagnostics": true },
  "logLevel": "info"
}
```

## 环境变量约定（示例）
- SERENA_CONFIG：指定默认加载的配置文件路径。
- SERENA_LOG_LEVEL：覆盖日志级别。
- SERENA_CACHE_DIR：缓存/DB 目录。

## FAQ
- Q: 如何定位启动失败？
  - A: 打开日志级别为 debug/trace；查看 serena.log 与 stderr，同步参考 docs/perf_tuning_and_troubleshooting.md。
- Q: 如何在 CI 中运行？
  - A: 使用 serena-cli 并固定配置文件；日志与缓存落在工作目录以便归档。
- Q: LSP 连接 IDE 失败？
  - A: 检查端口与权限，IDE 侧指向 127.0.0.1:9333；必要时关闭冲突插件。
- Q: 工具执行超时？
  - A: 提高 timeout_ms，或在工具端加缓存/降载；确认并发限制与资源配额。

