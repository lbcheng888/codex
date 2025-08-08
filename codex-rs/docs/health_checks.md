# 健康检查与指标看护（Serena 集成）

目的
- 持续跟踪性能指标与错误趋势，提前发现回归，支撑升级/回滚决策。

指标分层
- 系统层：
  - http_requests_total 按 route 分类的 5xx 比例
  - http_request_duration_seconds P95/P99 直方图
  - serena_tools_errors_total、serena_tools_timeout_total
  - 资源：进程 CPU、RSS、IO 等
- 业务层：
  - codex_serena_business_total / codex_serena_business_success_total

数据源与规则
- Prometheus 规则参考：ops/alerts/prometheus_rules_example.yaml
- Benchmark 回归：.github/workflows/bench.yml 在 PR 中 gate（阈值默认 1.10）

SLO 与阈值（建议）
- 错误率：较基线+3σ 触发 Critical（5 分钟）
- P99 延迟：高于基线 1.2x 触发 Critical（10 分钟）
- 成功率：低于基线 2% 触发 Warning（15 分钟）

处置流程
1) On-call 在 15 分钟内响应，收集最近 1 小时的指标与日志摘要。
2) 若触发 Critical，优先执行缩量回退（参见 ops/runbooks/rollback_playbook.md）。
3) 形成 Issue 更新：根因初判、临时缓解、下一步动作与负责人。
4) 24~48 小时内验证修复并逐步恢复流量。

责任与轮值
- On-call 与 Owner 角色说明见 docs/oncall_rotation.md。

