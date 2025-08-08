# Tools 最小示例

本示例演示如何通过 Serena CLI 调用一个最简单的进程型工具。

## 运行
```bash
cargo run -p serena-cli -- --config examples/tools-minimal/serena.toml run tool echo
```

预期输出：
- 标准输出包含 "hello-from-serena"。
- 退出码为 0。

## 修改
- 若在非 Unix 系统，将 cmd 替换为本地可用命令。
- 可添加更多 [tools.*] 配置测试并发与超时。

