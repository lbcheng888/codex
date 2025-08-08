# Serena × 本地大模型（Candle+Metal）× Graphiti 一体化开发计划

目标
- 将 SerenaAgentEngine 作为“外部决策器/执行器”，与 Codex 主模型协作，实现从需求确认到产品交付的一体化闭环。
- 用本地大模型（HuggingFace: openai/gpt-oss-20b）基于 Candle+Metal 加速，承担规划/仲裁/审批与部分生成任务；
- 以 Graphiti 维护长时上下文/知识图，统一会话、工具结果、代码仓与任务状态的图谱化存储与检索；
- 保持可灰度、可回退、可观测，分阶段安全落地。

范围与不在范围
- 在范围：本地模型接入、Serena 决策回路、Graphiti 上下文、联调与观测、CI/bench 门控。
- 暂不包含：大规模分布式推理、跨机器共享 KV Cache、端到端 TTS/多模态流水线。

一、总体架构
- 模型层
  - 本地模型客户端 LocalHfCandleModelClient（Candle + Metal）：实现 codex-core::interfaces::ModelClient
    - 支持流式输出（sink.on_token）、CancelToken、deadline、重试预算（stream_max_retries）
    - Tokenizer/提示适配：将 Codex 的 prompt 格式映射到本地模型约定（role/stop/系统提示）
    - KV Cache/量化：优先 int8/awq；必要时降到 q4；Metal 后端启用；提供超时与OOM降级
  - Graphiti-LLM 复用：如需可在知识抽取或摘要阶段复用 graphiti-llm 提供的 OpenAIClient/OllamaClient/QwenCandleClient 能力
- 决策层
  - SerenaAgentEngine：作为“外部决策器”，在以下场景介入：
    - 函数调用路由：对 serena__ 前缀函数调用优先处理（已接线）；
    - 规划/仲裁：当主模型需要工具决策或多步计划时，调用本地模型或基于规则/图谱做决策（plan/step/observe/resume）；
- 上下文层
  - Graphiti 适配（SerenaContextStoreGraphiti）：实现 ContextStore（save/load、file快照、索引引用）
  - 存储：优先采用 graphiti-cozo 的 CozoDriver（本地 sqlite/rocksdb 模式），后续可切换 engine
  - 检索：文本检索使用 graphiti-search（Tantivy），向量检索使用 graphiti-search::vector（可配合 graphiti-llm 的 Embedder）
  - 知识/上下文组织：实体（File/Commit/ToolRun/Decision/Task/User/Memory），边（依赖、修改、引用、归属）；
  - RAG on graph：按任务与文件图谱检索，摘要后拼接系统提示（带预算控制与优先级）
- 主模型协作
  - 初期：云端模型（OpenAI Responses/Chat）为主对话生成，本地 20B 为“副驾/决策器”；
  - 共享上下文：Graphiti 统一写入/读取，按策略注入系统提示与工具上下文；
  - 后期：探索“本地主稿 + 云端润色/审核”或反向协作。

二、分阶段路线图
- P0 原型（1-2周）
  1) Candle+Metal 启动 gpt-oss-20b（或临时用 7B/8B 验证）
     - 下载权重/Tokenizer；若需量化，离线转换；验证 stream、cancel、deadline 行为
  2) LocalHfCandleModelClient 实现：满足 ModelClient::stream_chat
     - 简单 prompt 映射、错误/超时语义、日志指标埋点
3) Graphiti 适配：ContextStore 最小实现（save/load/put_index_ref/get_index_ref）
     - 直接依赖 graphiti-core（Graphiti、GraphStorage trait）与 graphiti-cozo（CozoDriver）
     - 默认使用 CozoConfig { engine="sqlite", path="./data/graphiti.db" }
     - 定义基础实体/命名空间（session_id、project_id）
  4) 接线不生效（默认关闭 feature/config），仅单元测试与本地试验

- P1 决策回路（2-3周）
  1) SerenaAgentEngine 使用本地模型替换 dummy model（已对接接口）
  2) 决策/仲裁：对主模型给出的函数调用进行审核、参数重写、拒绝或分解；
  3) 小步规划：根据 Graphiti 中状态与代码图生成下一步工具序列，控制并发与限流；
  4) 观测与回退：
     - 指标：工具错误率、决策时延、成功率、回退次数；
     - 配置：serena.enabled / serena.agent 开关 + kill switch；

- P2 上下文融合（1-2周）
  1) Graphiti 检索增强与摘要注入：
     - 文件/符号/变更摘要；
     - 上下文预算器：按权重裁剪与摘要分层；
  2) 自动验证工具：工具规划结果静态/动态预检（路径安全、并发、权限等）；
  3) LspFacade 与工具一体化：在规划内可发起 LSP 查询（符号/引用）与工作区编辑建议

- P3 质量与扩展（持续）
  - A/B 开关与放量：Alpha→Beta→默认；
  - 评测：工具选择准确率、端到端工期、失败率、回滚次数；
  - 性能：量化/缓存/分段推理优化；
  - 文档/示例：最小项目模板、运行脚本、问题排障指南。

三、详细实现计划
1) LocalHfCandleModelClient
- 接口：实现 codex-core::interfaces::ModelClient
  - stream_max_retries：由配置控制（默认 0-1）
  - stream_chat：
    - 输入：Value（已包含消息/工具/系统），适配为本地模型可消费的 prompt
    - 输出：将 token 增量推送到 sink.on_token，完成后 sink.on_close
    - 取消/超时：cancel/deadline，必要时中断解码并返回 Err/CodexErr
- 性能/内存：
  - KV Cache：按会话缓存，配置清理策略；
  - 量化：优先 int8/awq；预算不足时降级 q4；
  - Metal：启用 MPS/Metal 后端，验证 kernel 覆盖度
- 观测：
  - tracing 事件：创建/解码延迟、token 速率、取消/超时次数；
  - 指标：latency p50/p95/p99、错误率、OOM/降级次数

2) SerenaAgentEngine（增强）
- handle_function_call：
  - serena__ 前缀优先路由（已接线），在引擎内部：
    - 若需要规划/仲裁，使用 LocalHfCandleModelClient 在 plan/step 内做本地推理或规则推断；
    - 若需要工具调用：发起 serena-tools/LSP 调用；
  - 返回：ResponseInputItem（FunctionCallOutput/McpToolCallOutput）按现有统一格式
- plan/step/observe/resume：
  - plan：读取 Graphiti 任务节点，生成“下一步计划”
  - step：执行一步（工具/编辑/LSP查询），写回 Graphiti
  - observe：接收外部反馈（用户/测试/构建结果），更新图谱
  - resume：从 Graphiti 快照恢复
- 限流与并发：沿用 per-tool 信号量与全局队列，避免资源争用

3) Graphiti 适配
- ContextStoreGraphiti：
  - 以 Graphiti<CozoDriver, LLMClient, EmbeddingClient> 为核心编排；存取通过 GraphStorage 实现
  - save/load：会话/步骤/工具结果的结构化存储；落到 Cozo 表（episode_nodes/entity_nodes/edges 等）
  - save_file_snapshot/load_file_snapshot：必要时存储关键文件快照（可选）
  - put_index_ref/get_index_ref：用于 RAG 检索索引/引用；
    - 文本检索：构建/更新 graphiti-search::TextSearchIndex（tantivy），索引实体 name/content/labels
    - 向量检索：graphiti-search::vector VectorIndex，向量由 graphiti-llm 的 EmbedderClient 生成
- 数据模型（建议）：
  - 实体：Project, Session, Task, Decision, ToolRun, File, Commit, Symbol, Memory（映射到 entity/episode 节点）
  - 边：belongs_to, depends_on, modifies, references, derived_from
- 检索策略：
  - 基于 Task 与最近 ToolRun 进行文件/符号检索、变更摘要获取
  - 预算器：阈值（token/字节）+ 优先级队列（任务描述>失败记录>最新变更>相关文件片段）

4) 配置与特性
- 配置：[serena]
  - enabled=true|false
  - tools / lsp / agent = true|false
  - concurrency, timeout_ms, log_level
- Graphiti 配置：
  - cozo.engine = "sqlite"|"rocksdb"|"mem"；cozo.path = "./data/graphiti.db"
  - search.text.index_dir = "./data/graphiti_index"；search.text.limit/min_score/fuzzy 等
  - embedding.provider = openai|huggingface|qwen_local；向量维度与度量在 graphiti-search::vector 配置
- 特性：
  - core: serena_native（已加），集成本地 LSP/Agent 可注入
  - 本地模型：local_candle（可在适配层或 integrations 添加）
  - graphiti_rust 集成：默认启用 graphiti-core / graphiti-cozo / graphiti-search / graphiti-llm

5) CI/测试/基准
- 单测：
  - LocalHfCandleModelClient：取消/超时/错误映射
  - SerenaAgentEngine：serena__ 路由、plan/step 状态写入 Graphiti
  - Graphiti 适配：存取一致性、检索结果稳定性
- 集成（trycmd/端到端）：
  - CLI 工具调用 vs 本地决策器仲裁对比
  - LSP 重启/查询与规划联动
- 基准（criterion/bench.yml）：
  - 本地模型推理延迟与吞吐（限定 prompt 长度/生成长度）
  - 规划/工具流水线端到端延迟
  - 回归阈值：P99 不超过基线×1.2；错误率不超过基线+3σ

四、风险与回退
- 资源/性能：
  - 20B 在 Metal 上性能有限；先以“副驾/决策器”角色落地，降低主路径压力
  - 回退：本地模型失败/超时→仅云端；serena.agent=false
- 质量：
  - 本地模型代码/工具理解不足：将其限制在“仲裁/规划”而非长文生成；
  - 引入自动验证工具，减少错误外放
- 工程：
  - Candle/Metal 算子覆盖/稳定性：准备替代路径（llama.cpp/ggml 或远端 vLLM）
- 安全/合规：
  - 权限与沙箱：沿用 exec/file 工具的最小授权与审计；
  - 日志脱敏：已在 observability 中支持；确保文档与Graphiti存储避免泄露

五、里程碑与验收
- P0 完成标准：
  - LocalHfCandleModelClient 能跑通最小流；Graphiti 适配能存取基本实体；
  - 单测通过；bench 跑通（不设严格阈值）
- P1 完成标准：
  - SerenaAgentEngine 使用本地模型实现仲裁/规划最小闭环；
  - 函数调用 serena__ 路由稳定；灰度开关与回退有效
- P2 完成标准：
  - Graphiti 检索/摘要注入稳定；端到端质量与性能未显著回归；
  - LSP/工具/规划联动场景通过 E2E 测试
- P3 进入常态化优化：
  - CI/bench 门槛生效；错误率/延迟符合 SLO；
  - 文档与示例完善；按灰度策略放量

六、实施清单（任务粒度）
- 模型客户端
  - [ ] 选择/转换权重与量化；Metal 后端验证
  - [ ] 实现 LocalHfCandleModelClient（stream_chat/重试/取消）
  - [ ] prompt/stop/role 适配与单测
  - [ ] 指标与日志埋点
- 决策引擎
  - [ ] SerenaAgentEngine 接入本地模型（替换 dummy）
  - [ ] plan/step/observe/resume 的最小实现与 Graphiti 写入
  - [ ] 函数调用路由/回退测试
- Graphiti 适配
  - [ ] ContextStoreGraphiti 接口（save/load/snapshot/index_ref）
  - [ ] 实体/边 schema 与读写测试
  - [ ] 检索与摘要注入 MVP
- 配置/特性/观测
  - [ ] [serena] 配置说明与默认值
  - [ ] feature gates（serena_native/local_candle）
  - [ ] Prometheus 规则与日志字段
- 测试与 CI
  - [ ] 单测/集成/E2E 覆盖
  - [ ] Graphiti 端到端：CozoDriver schema 初始化、写读一致性；Text/Vector 检索结果与分值稳定性
  - [ ] bench.yml 扩展：本地模型延迟/吞吐基准；回归阈值

七、运维与灰度
- 灰度策略：Alpha（内部）→ Beta（小流量/CI）→ 默认开启（保留回退2版本）
- 回退路径：关闭 serena.agent 或降级为 dummy/云端；保留 kill switch
- 监控与告警：错误率、P95/P99、仲裁拒绝率、工具失败率、回退触发次数

附录
- 仓库内组件位置
  - graphiti-core：graphiti-rust/crates/graphiti-core（Graph/Storage/Orchestrator/LLM接入traits）
  - graphiti-cozo：graphiti-rust/crates/graphiti-cozo（CozoDB存储驱动：sqlite/rocksdb/mem）
  - graphiti-search：graphiti-rust/crates/graphiti-search（Tantivy 文本检索、向量检索模块）
  - graphiti-llm：graphiti-rust/crates/graphiti-llm（OpenAI/Ollama/QwenCandle 等 LLM/Embedder 实现）
- 参考资源
  - 模型：https://huggingface.co/openai/gpt-oss-20b
  - Candle：https://github.com/huggingface/candle
  - Graphiti（上游）：https://github.com/getzep/graphiti
- 运行环境建议
  - macOS + Apple Silicon（M2/M3 Pro/Max），内存≥64GB（建议96/128GB）
  - 最新 Xcode/Metal 驱动；磁盘充足；网络良好（首次下载/同步）

