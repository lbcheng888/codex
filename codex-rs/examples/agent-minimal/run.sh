#!/usr/bin/env bash
set -euo pipefail

CONFIG=examples/agent-minimal/serena-agent.toml

# 优先以工具调用验证能跑通
cargo run -p serena-agent -- --config "$CONFIG" run tool echo

