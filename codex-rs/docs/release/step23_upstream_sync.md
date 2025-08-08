# Step 23: 维护与上游同步机制（serena-rust）

目标
- 固定 serena-rust 依赖提交或版本，确保可重复构建与可控升级节奏。
- 每周检查上游变更，主动评估升级窗口。
- 升级流程包含兼容性评估与回滚预案，降低业务风险。

一、固定策略（Pinning）
- 记录文件：serena-rust/UPSTREAM_PIN
  - 格式：`<git_sha> <YYYY-MM-DD> <note>`
  - 例：`8f4a1d2 2025-07-20 sync: up to v0.5.1, perf +3%`
- 变更入口：仅通过 PR 更新 UPSTREAM_PIN，PR 必须包含：
  - 变更说明（上游 release notes/commit highlights）。
  - 影响评估（API 兼容性、配置/行为差异）。
  - 验证报告（编译+测试+基准）与结果摘要。
  - 回滚预案确认（参见 ops/runbooks/rollback_playbook.md）。

二、每周上游检查
- 自动化：.github/workflows/serena-upstream-check.yml
  - 读取 .github/serena-upstream.yaml（repo/branch/subdir）。
  - 比较 UPSTREAM_PIN 与上游 HEAD，如有新提交则创建 Issue 提醒。
- 人工频率：每周一次例会评审 Issue，确定是否进入升级评估。

三、升级评估流程（Checklist）
1) 编译与单测
- 受影响 crate：serena-core、serena-config、serena-db、serena-tools、serena-lsp 及依赖它们的 codex-* crates。
- 要求：构建无 warnings 失败（CI 已设置 -Dwarnings）。
2) 行为与兼容性
- API/类型签名变动：是否影响本仓接口层（core/src/interfaces.rs 等）。
- 配置项变化：默认值、字段新增/移除、迁移脚本需求。
- 错误码/重试/超时策略：是否调整了失败语义或退避策略。
3) 性能与资源
- 运行 .github/workflows/bench.yml（或本地 cargo bench -p serena-bench）。
- 比较 Criterion 结果与基线：阈值默认 1.10（可按模块细化）。
- 资源变化（CPU、内存、IO）：是否超出当前预算或容器限额。
4) 安全与可观测性
- 日志/指标/追踪字段兼容性：是否破坏现有告警规则与仪表盘。
- 新依赖审查：许可证、供应链、CVE 风险。

四、上线与回滚预案
- 上线节奏：先灰度到内部/小流量，再逐步放量（参考 step21 灰度策略）。
- 回滚手册：ops/runbooks/rollback_playbook.md
  - 必备：可快速生效的 Kill-Switch/特性开关。
  - 降级路径：恢复至上一个稳定 UPSTREAM_PIN；必要时回滚二方配置。

五、变更记录模板（PR 描述）
- 上游来源与范围：repo/branch，起止 SHA。
- 兼容性影响：API、配置、语义变更。
- 验证结果：编译、测试、基准数据截图或摘要。
- 风险点与缓解：
- 回滚方案已演练：是/否（说明证据）。

六、健康检查与监控
- 指标分层：
  - 业务：成功率、请求量、尾延迟（P95/P99）。
  - 系统：工具调用失败率、重试率、超时率、CPU/内存水位。
- 现有参考：docs/perf_tuning_and_troubleshooting.md，bench 工作流的回归门槛。
- 告警建议：
  - 高错误率：CodexSerenaHighErrorRate（阈值按环境分级）。
  - 高延迟：CodexSerenaHighLatencyP99。
  - 基准回退：Bench Regression（PR Gate）。

七、负责人与轮值机制
- 角色
  - On-call（周值班）：响应上游破坏性更新、告警处置、变更冻结判断。
  - Owner（模块负责人）：签署兼容性评估与回滚预案。
- 节奏
  - 周期：按周轮换（周一 00:00 UTC 切换）。
  - 编排：在 .github/CODEOWNERS 或 docs/oncall_rotation.md 维护排班。
- 响应
  - TRIAGE ≤ 15 分钟；出具初判 ≤ 2 小时；是否回滚决策 ≤ 4 小时。

附：常用命令
- 本地评估：
  - cargo build -p serena-core -p serena-config -p serena-db -p serena-tools -p serena-lsp
  - cargo test  -p serena-core -p serena-config -p serena-db -p serena-tools -p serena-lsp -- --nocapture
  - cargo bench  -p serena-bench --no-default-features

