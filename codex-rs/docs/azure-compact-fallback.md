# Azure Responses `/compact` 行为兼容说明

本文档汇总了 2025-10-24 之前针对 Azure OpenAI `gpt-5-codex` 部署出现 “`Re-connecting…` 重试后失败” 的修复。主要变更位于 `codex-rs/core/src/model_provider_info.rs` 与 `codex-rs/core/src/client.rs`，核心目标是在 Azure Responses 流缺失 `response.completed` 事件时，依旧让自动摘要与手动 `/compact` 顺利收尾。

## 修复摘要

- **请求构造统一化**  
  新增 `ModelProviderInfo::create_request_builder_with_method`，在既有鉴权、Header 逻辑基础上支持 `GET` 请求（参见 `codex-rs/core/src/model_provider_info.rs:97-169`）。Azure fallback 会复用该接口生成 `GET /responses/{id}` 请求，确保携带 `Bearer` / 其他自定义 Header。

- **SSE 结束逻辑容错**  
  `process_sse` 现在在流提前结束时按顺序尝试：
  1. 如果已经收到 `response.completed`，沿用原先路径。
  2. 否则若识别为 Azure Responses 且拿到了 `response.created` 的 ID，则发起一次补抓；成功时把返回的 usage 填入 `ResponseEvent::Completed`。
  3. 若补抓失败，则记录 warning，并在保留 `response_id` 的前提下返回一个 `token_usage = None` 的 `ResponseEvent::Completed`，避免 `/compact` 直接报错。（代码位置：`codex-rs/core/src/client.rs:759-845`。）

- **观测信号**  
  使用 `tracing::warn` 记录 fallback 抓取失败的场景，方便后续排查；成功路径仍会让 OTEL 事件携带 token usage 指标。

- **回归测试**  
  `azure_missing_completed_fetches_usage_via_fallback`（`codex-rs/core/src/client.rs:1330-1414`）通过 Wiremock 模拟 Azure SSE，再验证：  
  - 第一次 `response.created` + `response.output_item.done` 后立即关流  
  - fallback 补抓 JSON 中的 usage 被正确灌入 `ResponseEvent::Completed`

## 使用注意事项

1. **配置要求**  
   - `model_providers.<id>.wire_api = "responses"`  
   - `query_params` 中包含 Azure 要求的 `api-version`  
   - 建议继续使用 `model = "gpt-5-codex"`，从而沿用默认的自动摘要阈值（也可以手动设置 `model_auto_compact_token_limit`）。

2. **日志期望**  
   - 常规成功：SSE 流中出现 `EventMsg::TaskComplete`，命令行不再显示持续的 “Re-connecting…”。  
   - fallback 未取到 usage：日志中会出现 `failed to fetch response completion…` 的 warning，但 `/compact` 仍会返回完成事件。

3. **验证方式**  
   - 手动触发 `/compact`；或制造超限上下文观察自动摘要是否完成。  
   - 运行 `CODEX_SANDBOX_NETWORK_DISABLED=1 cargo test -p codex-core` 可复现本地回归测试。仓库也默认执行 `just fix -p codex-core` 和 `just fmt` 以保持风格与 Clippy 一致。

## 相关文件索引

| 位置 | 说明 |
| --- | --- |
| `codex-rs/core/src/model_provider_info.rs` | 新增的通用 RequestBuilder 构造函数 |
| `codex-rs/core/src/client.rs` | SSE 处理与 Azure fallback 主逻辑、单元测试 |
| `docs/config.md` | Azure provider 配置示例与自动摘要相关选项 |

如需进一步排查 Azure Responses 的兼容性，可留意日志中的 `codex.sse_event` / `codex.api_request` 指标，确认 fallback 是否被触发及请求状态。
