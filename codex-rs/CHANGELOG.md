# Changelog

All notable changes to this repository will be documented in this file.

The format is based on Keep a Changelog, and this project (for now) does not strictly follow Semantic Versioning.

## 2025-08-08

### Added
- Documentation updates marking the completion of Serena integration evaluation Phase A (native in-process tools integration) and capturing Phase B roadmap.
  - docs/codex_serena_audit.md: Added “任务进度与结论”, “整体进度概览”, and “未完成项清单（阶段 B 重点）”.
  - docs/adr/ADR-serena-integration-topology.md: Status updated to “已合并（阶段 A 完成）” with progress excerpt.
  - docs/release/step21_gray_release.md: Added milestone note and progress excerpt for gray release planning.
  - README.md: Added “Serena integration status” section with quick links.

### Governance/Operations
- Added health checks documentation (docs/health_checks.md) and references to alerts and rollback playbooks.

### Notes
- Phase B focus: LSP Facade adapter, SerenaAgentEngine optional takeover and A/B switch, tool schema/error semantics alignment, per-tool concurrency parity, CI/bench gates for LSP/Agent KPIs.

