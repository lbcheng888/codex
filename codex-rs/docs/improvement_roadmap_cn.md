# codex-rs 改进路线图（与 Claude Code UX 对齐）

> 本文档总结了对当前 **codex‑rs** 代码库的快速审查结果，列出需要关注的潜在问题、可优化点，以及如何在用户体验（UX）层面向 *Claude Code* 对齐的建议。

## 目录

1. 代码质量与性能改进
   1. 统一错误处理
   2. 代码复用与解耦
   3. 异步 / 并发
   4. 性能微调
   5. 测试覆盖率
2. UX 对齐 Claude Code 的方向
   1. 单一入口命令与子命令体系
   2. 交互式体验（TUI / CLI 自动切换）
   3. 流式响应输出
   4. 上下文缓存与多会话管理
   5. 命令行提示与文档
   6. 自动补丁生成工作流
3. CI / 发布建议
4. 总结

---

## 1. 代码质量与性能改进

### 1.1 统一错误处理

* 当前多处仍存在 `unwrap/expect`，出错即 `panic`。
* 推荐做法：
  * 所有内部函数返回 `anyhow::Result<T>`；
  * 使用 `thiserror` 定义领域错误；
  * 在 CLI/TUI 入口捕捉错误并渲染友好信息。

### 1.2 代码复用与解耦

* `mcp-*`、`chatgpt`、`file-search` 等子 crate 出现重复的 I/O & JSON 逻辑。
* 建议在 `common` 或新建 `utils` crate 中抽象：
  * 文件读取 / 写入；
  * 配置加载；
  * 日志初始化。

### 1.3 异步 / 并发

* 大量实现仍基于阻塞 I/O。
* 如果要提升流畅度，可迁移到 `tokio`：
  * `reqwest::Client` async 版；
  * `tokio::fs`；
  * 结合 `indicatif` / `console` 的异步进度条。

### 1.4 性能微调

* 大仓库遍历：用 `ignore::WalkBuilder`（支持 `.gitignore`）+ `rayon` 并行。
* JSON 解析：`serde_json` → `simd_json` 可带来 30–50% 提升（在 CPU 受限场景）。

### 1.5 测试覆盖率

* 目前 unit tests 主要集中在 `core`，其他子 crate 稀缺。
* 引入 `cargo-nextest`；对 CLI/TUI 进行端到端测试：`assert_cmd` + `predicates`。

---

## 2. UX 对齐 Claude Code 的方向

### 2.1 单一入口命令与子命令体系

* 现状：`chatgpt`, `mcp-*`, `exec` 等可执行文件混杂。
* 目标：提供单一顶层命令 `codex`，内部用 `clap` v4 `Subcommand`：
  * `codex chat`
  * `codex search`
  * `codex run` …

### 2.2 交互式体验（TUI / CLI 自动切换）

* 使用 `ratatui` + `crossterm` 新 API。
* 当 `isatty` 为真：进入全屏 TUI；否则输出纯文本，方便脚本化。

### 2.3 流式响应输出

* Claude 支持逐 token 流。
* 在 `chatgpt` 模块中启用 SSE / `stream=true`：边收包边写 stdout。
* 利用 `console::style` 高亮代码块（或选 `syntect` 做语法着色）。

### 2.4 上下文缓存与多会话管理

* 建议目录结构：`~/.codex/sessions/{timestamp}-{title}.md`。
* 新增命令：
  * `codex chat --resume last`
  * `codex chat --session <id>`

### 2.5 命令行提示与文档

* 借助 `clap` 的 `value_parser`、动态补全。
* `--help` 内嵌示例；在 `docs/USAGE.md` 维护常见用例。

### 2.6 自动补丁生成工作流

* `codex fix`：
  1. 调用 AI 生成 unified diff；
  2. 提示用户；
  3. 确认后 `git apply`。

---

## 3. CI / 发布建议

1. 集成 `cargo deny`, `cargo udeps`, `cargo audit`。
2. 发布双通道：稳定版 + nightly（启用 `--features experimental`）。

---

## 4. 总结

* **代码层面**：
  * 统一错误处理，抽象公共逻辑，全面 async 化，性能微调，补齐测试。
* **UX 层面**：
  * 单一命令入口、自动 TUI 切换、流式输出、完善会话管理、自动 patch 工作流、完备文档。

循序渐进实施以上改动，可逐步提升 codex‑rs 的稳定性、性能与开发者体验，并与 Claude Code 的交互风格保持一致。

