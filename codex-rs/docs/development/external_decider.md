# 外部决策器（本地大模型）集成指南

本指南说明如何启用本地外部决策器（gpt-oss-20b）并在 macOS 上使用 Candle/Metal（占位）或 Mock 模式，同时开启可回溯、可审计的 JSONL 日志。

## 配置
在 `~/.codex/codex.toml` 新增：

```toml
[external_decider]
enabled = true
model_id = "gpt-oss-20b"
# 设备/精度仅作为提示参数，真实推理接入后生效
device = "metal"
precision = "bf16"
max_tokens = 512
temperature = 0.2
top_p = 0.9
# 审计 JSONL 输出路径（支持 ~ 展开）
audit_log = "~/.codex/audit/external_decisions.jsonl"
# 本地权重与分词器路径（存在则进入 metal 后端占位流程）
model_path = "/Users/lbcheng/gpt-oss-20b/model.safetensors"
tokenizer_path = "/Users/lbcheng/gpt-oss-20b/tokenizer.json"
# 并发度：每次最多允许的并发决策数（默认 1）
concurrency = 1
```

也可使用环境变量覆盖：
- EXTERNAL_DECIDER_ENABLED, EXTERNAL_DECIDER_MODEL_ID, EXTERNAL_DECIDER_DEVICE, EXTERNAL_DECIDER_PRECISION
- EXTERNAL_DECIDER_MAX_TOKENS, EXTERNAL_DECIDER_TEMPERATURE, EXTERNAL_DECIDER_TOP_P
- EXTERNAL_DECIDER_AUDIT_LOG, EXTERNAL_DECIDER_MODEL_PATH, EXTERNAL_DECIDER_TOKENIZER_PATH
- EXTERNAL_DECIDER_CONCURRENCY

## 特性开关
- 若希望使用 Metal tokenizer 占位路径（非真实推理），编译时启用：
  - `--features codex-core/external_decider_metal`
- 未启用该特性或路径无效时，将自动降级到 Mock 占位流输出。

## 运行与日志
- 启用后，Serena 的 AgentEngine 会在“外部决策”阶段调用本地外部决策器，并将每次决策写入 JSONL 审计文件（含 decision_id、timestamp、latency_ms、success 等）。
- 并发满载时，外部决策器会立即返回 `[external-decider] busy, please retry` 提示，不阻塞主流程。

## 后续（真实推理）
- 后续将接入 Candle 真实推理（Metal 优先，失败回退 CPU），实现流式采样/取消、warmup、并发限流与 p50/p95 指标。
