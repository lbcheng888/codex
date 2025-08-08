# ADR: Serena 集成拓扑与模块边界

日期: 2025-08-08
状态: 已合并（阶段 A 完成）

背景
- 现状：codex-core 直接在核心中引入 serena-core/serena-tools 的 in-process 管理器（serena_inprocess.rs），与 MCP 客户端并存。
- 目标：分阶段实现与 Serena 的集成，在不破坏现有能力的前提下，逐步将 Serena 作为工具与 LSP 能力提供方，并在后续阶段可替换内部 planner/executor。

决策
1. 分阶段策略
   - 阶段 A 优先：Serena 提供工具与 LSP 能力，codex-rs 继续担任主编排者。
   - 阶段 B 过渡：在统一接口下可配置切换，将 codex 内部 planner 执行器替换为 Serena Agent（按需逐步落地）。

2. 模块边界与接口
   - 在 codex-core 定义稳定接口层（traits）：ToolInvoker, LspFacade, AgentEngine, ModelClient, ContextStore。
   - 新建 codex-serena 适配层 crate：实现上述接口，首先实现 ToolInvoker（其余接口预留占位）。
   - 保持单向依赖：codex-serena 依赖 serena-rust 与 codex-core；codex-core 不反向依赖适配层。

3. 依赖拓扑
   - 工作区新增成员 `codex-serena`。
   - 适配层通过 `serena-core`/`serena-tools` 提供 in-process 工具与 LSP 能力。

4. 兼容性承诺
   - 接口层（interfaces）稳定发布：避免频繁破坏性变更；如需调整，走标准 deprecation 流程并提供迁移说明。
   - 阶段 A 中，不改变现有 MCP 与工具调用行为；Serena 工具通过统一 ToolInvoker 合并进模型可见工具集。
   - 阶段 B 切换开关将以配置项/feature gate 提供，可回退到原内置执行路径。

5. 回退策略
   - 若适配层出现问题：
     - 配置关闭 Serena ToolInvoker 的注册，仅使用 MCP 工具（或原内置实现）。
     - 保留原有 serena_inprocess 管道直至阶段 B 完成并通过回归测试。
   - 工具枚举/调用异常时，自动降级为已知安全子集并透出错误事件至前端 UI。

6. 迁移计划
   - A1：在 codex-core 中引入 interfaces 模块，维持现有逻辑不变。
   - A2：codex-serena 实现 ToolInvoker，并在 Session 初始化处可注入替换当前 serena_inprocess 管理器（后续 PR）。
   - B1：实现 AgentEngine，接管函数调用的 planner/executor；通过配置开关选择 codex 或 Serena 实现。
   - B2：收敛 LspFacade 到统一接口下，抽离直接依赖。

影响
- 代码结构更清晰：核心仅依赖抽象层；实现细节移入适配层。
- 便于替换/并行对比不同实现（MCP vs in-process Serena）。
- 短期内存在重复路径（serena_inprocess 与适配层并存），但有清晰回退与删除计划。

备选方案
- 直接在核心内保留 Serena 具体实现：放弃抽象层，短期省事但长期耦合更重，影响替换与演进。

落实内容（本次变更）
- 新增 `core/src/interfaces.rs` 定义五个接口。
- 新增 `codex-serena` crate，并实现 `SerenaToolInvoker` 作为 ToolInvoker。
- 工作区加入 `codex-serena`。
- 编写本 ADR。

后续工作
- 在 Session 初始化中通过特性或配置注入 `codex-serena::SerenaToolInvoker`，逐步替换 `serena_inprocess` 的直接使用。
- 设计配置开关与 telemetry，监控切换效果。
- 迭代实现 LspFacade 与 AgentEngine。

进展更新（2025-08-08）
- 阶段 A（工具/LSP 能力供给的一体化）评估与文档已完成，工具路径原生 in-process 适配达成；LSP Facade 接入具备落点，按计划推进阶段 B。
- 参考：docs/codex_serena_audit.md 中“任务进度与结论”章节。

阶段进度摘录（2025-08-08）
- 完成度概览：
  - 工具路径（In-process）：≈85%
  - LSP 路径：≈60%
  - Agent 执行：≈40%
  - 观测与治理：≈90%
  - 上游同步与运维：≈90%
- 未完成项（阶段 B 重点）：
  - LSP Facade 映射与文档缓冲/防抖策略
  - SerenaAgentEngine 可选接管与 A/B 开关、回退
  - 工具列表 description/parameters.inputSchema 补齐与错误码对齐
  - per-tool 并发信号量（兼容 MCP 环境变量）
  - A/B 回归与 CI/bench 守门，MCP 路径达标后逐步退役

