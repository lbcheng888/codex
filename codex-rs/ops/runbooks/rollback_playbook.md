# 回退操作手册（Rollback Playbook）

目标
- 在灰度放量期间，当指标回归或用户反馈异常时，快速、安全地回退到上一个稳定状态。
- 支持分级回退（缩量 → 关闭 Beta → 全量关闭），并记录修复窗口承诺。

一、触发条件
- 收到 Critical 级别告警（如 CodexSerenaHighErrorRate / CodexSerenaHighLatencyP99）。
- 值班工程师或负责人（On-call）人工确认重大问题。

二、回退层级与命令
1) 缩量（优先）
- 编辑 configs/feature_flags/*feature*.yaml，按 min_safe_percentage 缩量（例如从 25% → 10% → 5%）。

2) 关闭 Beta（仅保留 Alpha）
- 将 stage: beta 调整为 alpha 或直接对该特性置 enabled_for: internal_only。

3) Kill-Switch（紧急关闭）
- 将 kill_switch: true 并保存，流水线同步生效。

建议命令（示例脚本，可在 CI 中封装）：
- scripts/rollback.sh scale_down example_feature 10
- scripts/rollback.sh disable_beta example_feature
- scripts/rollback.sh kill_switch example_feature

三、验证与观察
- 验证：
  - 确认 CI/CD 输出中回退动作已成功下发；
  - Prometheus 指标回到安全区间（5~10 分钟内观察）。
- 观察：
  - 错误率、P99 延迟、业务成功率三类核心指标；
  - 失败请求与慢请求的采样日志（Alpha 阶段 100% 采样，Beta 阶段按采样率）。

四、沟通与记录
- 告警路由：@On-call 频道、负责人、相关项目群。
- 创建 Issue/工单：包含以下内容（见模板）。

五、修复窗口承诺模板
- 影响范围：
- 根因初判：
- 临时缓解：已回退至（缩量/关闭Beta/Kill-Switch）。
- 修复方案：
- 修复负责人：
- 预计修复窗口：起始时间 ~ 截止时间（如 24~48 小时）
- 回归验证：计划与触发条件
- 风险与回退路径：

六、恢复放量步骤
- 条件：指标稳定于基线±阈值内，相关问题关闭；
- 节奏：5% → 10% → 25% → 50% → 100%（每档观察 ≥ 1 发布周期）；
- 注意：每次放量前检查未关闭告警；放量后 10 分钟重点观察高风险指标。

