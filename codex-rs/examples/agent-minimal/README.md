# Agent 最小示例

本示例以 Serena Agent 为统一入口，演示如何串联代理与工具。

## 运行
```bash
cargo run -p serena-agent -- --config examples/agent-minimal/serena-agent.toml run task say-hello
```

注：如未内置 task 定义，可直接调用工具路径：
```bash
cargo run -p serena-agent -- --config examples/agent-minimal/serena-agent.toml run tool echo
```

预期输出：
- 标准输出包含 "hello-from-agent" 或 "hello-from-serena"。

## 扩展
- 在 [tools] 下追加 HTTP 等工具，结合 policy=reactive 做多步规划。

