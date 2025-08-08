# LSP 最小示例

本示例演示以最小配置启动 Serena LSP，并由 IDE 连接。

## 步骤
1. 启动 LSP：
   ```bash
   cargo run -p serena-lsp -- --config examples/lsp-minimal/serena-lsp.json
   ```
2. 在 IDE 中配置 LSP 客户端，指向 127.0.0.1:9333。
3. 打开本仓库任意 Rust 文件，验证补全/跳转/诊断是否生效。

> 可将 serena-lsp.json 中的 workspace 替换为你的工作目录或留空以默认当前目录。

