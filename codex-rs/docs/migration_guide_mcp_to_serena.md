# 迁移指南：从 MCP 与旧管理器到 Serena 原生路径（已退役）

注意：本仓库已删除 MCP 与旧管理器相关实现、依赖与配置。本文档仅保留作为历史记录，内容不再适用于当前代码库。
请参考 docs/serena_native_integration.md 与 serena-rust 子项目文档获取最新指南。

本文指导你将现有 MCP 或旧管理器集成迁移到 Serena 原生路径，分阶段降低风险并保留既有能力。

## 概念映射
- 工具/资源注册 → Serena Tools 注册与配置（serena-tools + serena-config）。
- 会话与调用编排 → Serena Core/Agent 策略与计划。
- LSP 能力 → Serena LSP（serena-lsp）端到端接入 IDE。
- 配置/凭据 → Serena Config 统一装载，支持多文件与环境变量。

## 迁移路径建议
1. 最小可用：优先在 CLI 下以 Serena 原生配置跑通一个关键用例（见 examples/tools-minimal）。
2. 并行对照：同一用例保留 MCP 路径，新增 Serena 原生路径，做结果对照与性能对比。
3. 扩面替换：将更多工具、任务与 LSP 能力切到 Serena；MCP 层仅保留必要兼容。
4. 清理收敛：去除不再使用的旧配置与脚手架，统一到 Serena Config。

## 配置示例：从 MCP 到 Serena
- MCP/旧：通常分散在多处 JSON/YAML/代码中，按工具或通道各自维护。
- Serena：集中到 serena.toml / serena-agent.toml / serena-lsp.json 等入口文件，由 Serena Config 统一解析。

示意（Serena 工具最小示例）
```toml
# examples/tools-minimal/serena.toml
[project]
name = "demo-tools"

[tools.echo]
kind = "process"          # 也可以是 http/custom
cmd = "/bin/echo"
args = ["hello-from-serena"]
timeout_ms = 2000

[logging]
level = "info"
```

运行：
```bash
cargo run -p serena-cli -- --config examples/tools-minimal/serena.toml run tool echo
```

## LSP 迁移要点
- 使用 examples/lsp-minimal/serena-lsp.json 中的最小配置启动 Serena LSP。
- 在 IDE 中指向本地 LSP 端口或套接字，逐步替换原先的 MCP/旧管理器 LSP 代理。

## 兼容策略
- 过渡期可通过 serena-mcp 适配复用既有 MCP 工具。
- 对性能敏感路径，优先改造为 Serena 原生 Tools 调用（减少中间层开销）。

## 验收清单
- [ ] 用例在 CLI 下稳定运行（多次验证、一致结果）
- [ ] IDE 下 LSP 功能可用（补全/跳转/诊断至少一项）
- [ ] 同步日志与度量接入（错误能快速定位）
- [ ] 回滚预案明确（可切回 MCP/旧路径）

## 常见问题
- Q: 旧脚本大量依赖 MCP 客户端 SDK？
  - A: 使用 serena-mcp 先保持兼容；逐步将工具端改造成原生 process/http 适配，再切换调用方。
- Q: 配置项分散？
  - A: 将入口集中到 serena.toml / serena-agent.toml / serena-lsp.json，并在 docs/config_reference_and_faq.md 建立对照表。

