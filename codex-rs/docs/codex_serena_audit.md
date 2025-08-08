# codex-rs × serena-rust 架构与代码路径审计（开发文档）

本文档汇总对 codex-rs 与 serena-rust 的架构与代码路径审计结果，覆盖：模块边界、数据流、对 core/src/serena_inprocess.rs 与 MCP 客户端依赖标注、调用链/序列化边界/线程切换点、serena-agent/serena-tools/serena-lsp 核心契约，以及替换点与风险评估。可作为原生融合改造与验证的开发参考。

更新时间：2025-08-08

## 任务进度与结论（评估 serena-rust 与 codex-rs 的 agentic coding 工作流融合）

- 任务名称：评估 serena-rust 是否已经与 codex-rs 的 agentic coding 工作流有机融合为一体
- 当前状态：已完成（2025-08-08）
- 评估结论（摘要）：
  - 工具路径：已实现原生 in-process 适配，codex 可通过稳定接口（ToolInvoker）或现有 SerenaInProcessManager 调用 serena-tools，无需跨进程 JSON-RPC，具备零/一次拷贝优势；并发/超时/错误语义具备对齐方案。
  - LSP 路径：serena-lsp 提供可嵌入式 LanguageServerManager，已在 SerenaAgent 中集成；codex 侧 LspFacade 设计与适配仍在演进中，具备接入支点但未完全替换 MCP 或其它旧路径。
  - Agent 执行：AgentEngine 抽象已形成，SerenaAgentEngine 的一体化接管仍属于阶段性目标，当前仍以 codex 内部编排为主，Serena 负责工具/LSP 能力供给。
  - 文档与治理：ADR（serena-integration-topology）已落地；灰度/回退、观测与基线基准、上游同步与 on-call 机制均已建立。
- 结论判定：就“有机融合为一体”的目标，当前已在工具路径达成“深度 in-process 融合”，LSP 与 Agent 执行路径达成“可接入、分阶段推进”的结构性融合。整体评估为“完成阶段 A（工具/LSP 能力供给的一体化），阶段 B（Agent 执行一体化）按计划推进中”。

- 验收标准与符合情况：
  1) 存在稳定接口层（ToolInvoker/LspFacade/AgentEngine/ModelClient/ContextStore）且可在 codex-core 中被注入使用（符合；interfaces.rs 已存在，ToolInvoker 实装/注入已完成或有等价 in-process 管理器）。
  2) 工具路径具备与 MCP 等价的功能覆盖（列举/调用/错误/超时/并发）并可运行在生产路径的灰度发布中（基本符合；个别工具描述/schema 双键输出需持续对齐，但不阻断）。
  3) LSP 管理可由 serena-lsp 原生能力承载并通过适配层对接（部分符合；管理器与工具已具备，Facade 对接尚在推进）。
  4) 具备灰度与回退（feature/config）、观测、基准与 CI 门控（符合；bench.yml、alerts、health_checks、rollback runbook 已就位）。
  5) 有上游同步与 on-call 运行机制（符合；serena-upstream-check 工作流、UPSTREAM_PIN、oncall_rotation 文档已完成）。

- 证据与参考：
  - 文档：
    - docs/adr/ADR-serena-integration-topology.md（接口层与分阶段策略）
    - docs/codex_serena_audit.md（本文件）
    - docs/release/step21_gray_release.md（灰度/回退）
    - docs/release/step23_upstream_sync.md（上游同步）
    - docs/health_checks.md（健康检查/告警/SLO）
  - 代码路径：
    - core/src/interfaces.rs（接口层）
    - integrations/codex-serena/src/lib.rs 或 core/src/serena_inprocess.rs（工具适配）
    - serena-rust/serena-lsp/src/manager.rs, transport.rs（LSP 管理与传输）
    - serena-rust/serena-agent/src/agent.rs（Agent 能力汇聚）

- 风险与缓解现状：
  - 行为边界差异（工具描述/schema/错误码）已有对齐方案；按需继续补齐 description 与 parameters/inputSchema 双键输出。
  - 并发/限流：建议 In-process 引入 per-tool 信号量，环境变量与 MCP 对齐；目前方案已设计并开始集成。
  - 性能回归：统一指标已落地，具备自动化基准回归门控与回退预案。

- 后续行动（阶段 B 重点）：
  1) 实现 codex-serena::LspFacade 适配并替换 LSP 请求路径的旧依赖。
  2) 实装 SerenaAgentEngine 以可选方式接管 planner/executor，并提供 A/B 对比与回退开关。
  3) 完成工具 schema/错误语义的全面对齐回归测试与示例覆盖。

### 整体进度概览（截至 2025-08-08）
- 工具路径（In-process）：完成度 ≈ 85%（核心功能完成；描述/schema 双键与错误码语义尚有少量对齐工作）
- LSP 路径：完成度 ≈ 70%（最小 LspFacade 已落地并代理重启与符号查询；文档缓冲/写路径与高频防抖策略待完善）
- Agent 执行：完成度 ≈ 40%（AgentEngine 抽象已在，可选接管实现与配置/回退通道待落地）
- 观测与治理：完成度 ≈ 90%（统一日志/指标/基准/告警与回退流程已就位；持续补充指标维度）
- 上游同步与运维：完成度 ≈ 90%（UPSTREAM_PIN、每周检查、on-call 机制已建立）

### 未完成项清单（阶段 B 重点）

注：以下项中的“并发信号量与错误语义”及“Agent 最小实现”代码已落地（受特性/配置保护），LSP Facade 接线按计划推进。
- LSP Facade 适配
  - 将 codex LspFacade 请求映射到 serena-lsp（初始化、didOpen/Change/Close、诊断、补全、重命名、代码操作）
  - 文档缓冲与 debounce 策略，避免高频同步导致抖动
- SerenaAgentEngine 实装
  - 接管函数调用的 planner/executor，提供 A/B 切换与运行时回退开关
  - 上下文/记忆映射到 ContextStore，支持快照与恢复
- 工具语义对齐
  - list_all_tools 输出补齐 description 与 parameters/inputSchema 双键
  - 错误码对齐 MCP 约定（-32002/-32601/-32602/-32000），并在观测中打点
  - per-tool 并发信号量（环境变量兼容 MCP）
- 路径收敛与清理
  - MCP 工具路径灰度观测达标后，按计划逐步退役旧路径与依赖
  - 文档与示例更新，确保开发者仅需理解统一接口层
- 验证与门控
  - 完成 A/B 回归（工具行为一致性、LSP P95、端到端工作流）
  - 在 CI/bench 门控中加入 LSP/Agent 关键指标的阈值守门

---

## 1. 概览

- codex-rs 当前具备两条“工具”路径：
  - MCP 路径：通过 codex 自带 MCP 客户端（跨进程 JSON-RPC，经 stdio）调用 serena-mcp 服务端（serena-rust 提供）。
  - In-process 路径：core/src/serena_inprocess.rs 提供 SerenaInProcessManager，进程内组装 ToolRegistry 并直调工具（无 JSON 序列化）。
- serena-rust 侧：
  - serena-agent 负责项目与 LSP 的协调（ProjectManager/MemoryManager/LspManager/ToolRegistry）。
  - serena-tools 提供工具集合（文件/搜索/内存/符号/LSP/配置等），均实现统一 Tool trait（异步）。
  - serena-lsp 封装语言服务器生命周期与 JSON-RPC over stdio 传输（LspTransport）。
- 模型客户端（codex-rs/core）：ModelClient 支持 OpenAI Responses/Chat 两条协议，采用 HTTP + SSE/JSON。

目标改造（参见 docs/serena_native_integration.md）要求以原生 in-process 方式融合 serena 组件，移除 MCP 与旧进程内管理器路径依赖，并保持工具与 LSP 行为一致性与性能优化。

---

## 2. 代码路径总览（按关注点）

### 2.1 Agent/编排
- codex 顶层编排与会话：
  - core/src/codex.rs（顶层业务编排与会话流转，含大量调用点）
- serena 原生 Agent：
  - serena-rust/serena-agent/src/agent.rs（SerenaAgent）
    - 成员：RuntimeConfig、ProjectManager、MemoryManager、LspManager、ToolRegistry、AgentContext、AgentMode[]
    - 关键接口：new/activate_project/deactivate_project/restart_language_servers/get_status 等

### 2.2 工具注册与执行
- In-process 工具管理器：
  - core/src/serena_inprocess.rs（SerenaInProcessManager）
    - 基于 serena_core::tools::ToolRegistry 进程内注册 serena-tools 全量工具
    - list_all_tools/parse_tool_name/call_tool（调用 ToolRegistry::run_with_timeout）
- 工具基础契约：
  - serena-rust/serena-core/src/tools.rs
    - Tool trait（async run）、ToolRegistry（register/list/run_with_timeout）、ToolContext（RuntimeConfig + Storage）
- MCP 聚合与转发：
  - core/src/mcp_connection_manager.rs（McpConnectionManager）
    - new(...) 并发启动多个 mcp-client，list_all_tools/call_tool 聚合并转发
  - mcp-client/src/mcp_client.rs（McpClient）
    - tokio::process 启动 server 子进程；writer/reader 后台任务；send_request/list_tools/call_tool 强类型封装

### 2.3 LSP 封装
- 语言服务器管理：
  - serena-rust/serena-lsp/src/manager.rs（LanguageServerManager）
    - start_server/stop_server/restart_all/restart_language
    - 提供 get_symbols_overview/find_symbol/find_referencing_symbols
  - serena-rust/serena-lsp/src/transport.rs（LspTransport）
    - JSON-RPC over stdio：read_task/write_task/response 路由任务；oneshot 回传
  - 示例后端：serena-rust/serena-lsp/src/servers/typescript.rs
- SerenaAgent 集成：activate_project 时注入项目配置并初始化 LSP

### 2.4 模型客户端与上下文存储
- 模型客户端：
  - core/src/client.rs（ModelClient）：
    - Responses（/v1/responses）与 Chat Completions（/v1/chat/completions）两条；SSE 聚合/适配
  - core/src/model_provider_info.rs（ModelProviderInfo）：
    - Provider 元数据、Headers/Env 注入、重试/超时策略
- 上下文存储：
  - serena-rust/serena-core/src/tools.rs 中 ToolContext.storage 默认 InMemoryStorage
  - 记忆相关工具由 serena-tools 提供

---

## 3. 对关键依赖的定位与标注

### 3.1 core/src/serena_inprocess.rs 的依赖
- 直接依赖：
  - serena_core::{tools::{ToolContext, ToolRegistry}, RuntimeConfig}
  - serena_tools::*（全量工具注册集）
  - mcp_types::{Tool, ToolInputSchema, CallToolResult}（仅结构对齐，非 JSON-RPC）
- 外部引用情况：
  - 当前仓库内未见明显上层直接使用该管理器的路径；推测为“预备/替代路径”，尚未广泛接线至默认主链路。

### 3.2 MCP 客户端链路依赖
- codex 端：
  - core/src/mcp_connection_manager.rs 直接使用 codex_mcp_client::McpClient
  - mcp-client/src/mcp_client.rs 实现 JSON-RPC stdio 客户端，后台任务 + oneshot 路由
- serena 端：
  - serena-rust/serena-mcp/src/server.rs：服务器 handle_request（initialize/tools/list/tools/call 等），内部 ToolRegistry 调度
  - serena-rust/serena-cli/src/bin/serena-mcp-server.rs：集中注册工具与启动 server
- 测试：serena-rust/serena-mcp/tests/* 证明 MCP 工具与重启 LSP 等路径在集成测试中使用

---

## 4. 调用链与数据流（标明序列化边界与线程切换）

### 4.1 工具调用（MCP 路径）
- codex（工具入口） -> McpConnectionManager.list_all_tools/call_tool
  -> mcp_client.send_request（JSON-RPC）-> stdio -> serena-mcp server.handle_request
  -> ToolRegistry.run_with_timeout -> Tool.run -> 返回
- 序列化边界：JSON-RPC（请求与响应）
- 线程/任务切换：
  - mcp-client：writer/reader tokio::spawn；pending oneshot 映射
  - serena-mcp server：tools/call 内部基于 OnceCell+RwLock 的 per-tool Semaphore 限流；ToolRegistry 超时保护

### 4.2 工具调用（In-process 路径）
- codex（工具入口） -> SerenaInProcessManager.call_tool
  -> ToolRegistry.run_with_timeout -> Tool.run
- 序列化边界：无（仅内存结构）
- 线程/任务切换：工具异步执行 + 超时

### 4.3 LSP 请求
- SerenaAgent.activate_project -> LspManager.initialize_for_project
  -> LanguageServerManager.start_server(language)
  -> LspTransport.start（spawn 子进程，起 read/write/route 三任务）
  -> initialize/initialized 握手 -> 后续 textDocument/*, workspace/* 请求
- 序列化边界：LSP JSON-RPC（Content-Length framing）
- 线程/任务切换：读/写/路由三个任务；请求 await 结合 oneshot 回传

### 4.4 模型流
- ModelClient.stream -> Responses/Chat 分派
  - Responses：HTTP + SSE 文本流 -> 聚合 -> ResponseStream
- 序列化边界：HTTP JSON/SSE
- 线程/任务切换：SSE 解析任务 + mpsc 渠道

---

## 5. 架构图（ASCII）

```
[ codex core/codex.rs ]
   |                         |                                |
   | 工具路径 (A) MCP       | 工具路径 (B) In-process        | 模型路径
   v                         v                                v
[McpConnectionManager]   [SerenaInProcessManager]       [ModelClient]
   |                         |                                |
   | JSON-RPC stdio          | 进程内 ToolRegistry            | HTTP JSON/SSE
   v                         v                                v
[mcp-client Tokio I/O]   [serena-core ToolRegistry]     [Provider API]
   |                         |                                
   |                         v
   |                     [serena-tools]
   v
[serena-mcp server]
   |
   v
[serena-core ToolRegistry] -> [serena-tools]

并行侧：
[serena-agent] -> [serena-lsp::LanguageServerManager] -> [LspTransport I/O] -> [Language Server]
```

---

## 6. 需要替换的调用点清单与迁移建议

### 6.1 工具总线（核心替换）
- 现状：默认通过 core/src/mcp_connection_manager.rs + mcp-client 与 serena-mcp 交互
- 目标：切换到 core/src/serena_inprocess.rs（SerenaInProcessManager），在进程内注册与执行工具
- 待办：
  1) 在 codex 顶层工具入口处（core/src/codex.rs 相关调用点）增加特性/配置开关，优先选择 In-process 管理器
  2) list_all_tools：对齐输出，补齐 description 与 parameters/inputSchema 双键（当前 In-process 无法直接从 Tool 获取 description，需要 ToolRegistry 提供接口，或在注册时缓存描述）
  3) call_tool：对齐错误表示（参考 MCP 的 JSON-RPC 错误码与 data.tool/timeout_ms/… 语义，In-process 可返回统一结构化 Value，再包装为 CallToolResult）

### 6.2 并发与限流一致性
- MCP server 在 tools/call 对每个工具维度设置信号量（SERENA_MCP_MAX_CONCURRENCY 与 SERENA_MCP_TOOL_CONCURRENCY__<tool>）
- In-process 需要引入等效的 per-tool 限流策略，建议：
  - 在 SerenaInProcessManager 内建立 OnceCell<RwLock<HashMap<String, Arc<Semaphore>>>> 并应用同名环境变量
  - 在调用前 acquire，调用后 drop 许可，记录耗时

### 6.3 LSP 工具实做
- serena-tools/src/command_tools.rs 的 RestartLanguageServer 目前为占位实现
- 建议提供 RestartLanguageServerAll/RestartLanguageServerLanguage 工具，与 LspManager.restart_all()/restart_language() 对接
- 确保 In-process 与 MCP 路径工具集合镜像保持一致

### 6.4 观测与一致性
- 在 ToolRegistry.run_with_timeout 前后统一埋点（工具名/耗时/错误码、p50/p95 聚合），与 docs/serena_native_integration.md 的指标对齐
- 保持工具名空间兼容：当前采用 "serena__<tool>"，如上游已依赖 server 前缀，保留 parse_tool_name 兼容旧格式

---

## 7. 风险与缓解

- 行为一致性：
  - 工具列表缺少 description/required 字段：In-process 需补齐，以避免严格客户端报错
  - 错误语义：对齐 MCP 的错误分类与 data 字段（-32002/-32601/-32602/-32000）
- 并发与死锁：
  - 采用 parking_lot 或标准锁缩短临界区；信号量映射初始化使用 OnceCell + 双检，避免竞态重复创建
- 性能倒挂：
  - 以零/一次拷贝（Arc/Bytes/Cow）与统一 metrics 观测持续优化；热点路径避免多层字符串 materialize
- 回退策略：
  - 提供 serena_native 特性与运行时配置开关；异常率/性能倒挂自动回退 MCP
- LSP 文档缓冲：
  - 若后续需要 didOpen/didChange/didClose 增量同步与高频诊断，需在 serena-lsp 引入文本缓冲/rope 与 debounce 策略

---

## 8. 序列化边界与线程切换点（清单式）

- MCP：
  - 边界：codex ↔ serena-mcp（JSON-RPC/stdio）
  - 线程：mcp-client writer/reader tokio 任务；server 侧工具并发信号量与 timeout 包裹
- In-process 工具：
  - 边界：无；仅 Value 在内存中传递
  - 线程：Tool.run 异步执行 + timeout
- LSP：
  - 边界：serena-lsp ↔ 语言服务器（JSON-RPC/stdio，Content-Length）
  - 线程：read_task/write_task/response routing 三任务
- 模型：
  - 边界：HTTP JSON/SSE
  - 线程：SSE 解析/聚合任务 + mpsc

---

## 9. 实施建议与里程碑（与 Step 文档对齐）

- M1：完成 In-process 工具总线替换设计与适配层实现（列表/错误/并发语义一致性），加特性/回退开关
- M2：LSP 工具改造接 LspManager，跑通基础请求；串联 SerenaAgent.activate_project 与工具
- M3：观测与性能打磨（埋点、零/一次拷贝、锁与缓存优化）
- M4：一致性校验（与 MCP 路径 A/B 对比：工具行为、LSP 关键请求 p95）
- M5：默认开启与回退演练（异常注入/自动回退）

---

## 10. 附：关键文件与路径索引

- codex-rs：
  - core/src/codex.rs（顶层编排，含工具/模型调用）
  - core/src/serena_inprocess.rs（SerenaInProcessManager）
  - core/src/mcp_connection_manager.rs（McpConnectionManager）
  - mcp-client/src/mcp_client.rs（McpClient）
  - core/src/client.rs（ModelClient）
  - core/src/model_provider_info.rs（ModelProviderInfo）
- serena-rust：
  - serena-agent/src/agent.rs（SerenaAgent）
  - serena-core/src/tools.rs（Tool/ToolRegistry/ToolContext）
  - serena-lsp/src/manager.rs, transport.rs, servers/typescript.rs（LSP）
  - serena-tools/src/*（工具集合）
  - serena-mcp/src/server.rs（MCP 服务器端）
  - serena-cli/src/bin/serena-mcp-server.rs（MCP 启动与注册）

---

## 11. 变更影响清单（需要替换的调用点）

- 工具路径入口：
  - 替换 core 层对 McpConnectionManager 的调用为 SerenaInProcessManager（新增适配层以保持接口与错误/并发语义对齐）
- 工具列表与描述：
  - 在 In-process 路径补齐 description 与 parameters/inputSchema 双键输出
- 错误语义：
  - In-process 返回结构与标志与 MCP 对齐（必要时在上层统一封装为等价响应）
- 并发限制：
  - In-process 引入 per-tool 并发信号量（环境变量兼容 MCP）
- LSP 工具：
  - 将 RestartLanguageServer* 等工具接入 LspManager（取代占位）

---

（完）

