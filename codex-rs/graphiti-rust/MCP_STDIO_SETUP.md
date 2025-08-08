# Graphiti MCP JSON stdio 配置指南

## 🎯 概述

本指南介绍如何配置Graphiti MCP服务器使用JSON stdio模式，以便与Claude Desktop或其他MCP客户端集成。

## 📁 配置文件

### 1. 通用MCP配置 (`mcp-config-stdio.json`)

```json
{
  "mcpServers": {
    "graphiti": {
      "command": "/Users/lbcheng/codex/graphiti-rust/target/release/graphiti-mcp-server",
      "args": [
        "--stdio",
        "--project",
        "/Users/lbcheng/codex",
        "--log-level",
        "warn"
      ],
      "env": {
        "RUST_LOG": "warn",
        "PATH": "/usr/local/bin:/usr/bin:/bin"
      }
    }
  }
}
```

### 2. Claude Desktop配置

**位置**: `~/Library/Application Support/Claude/claude_desktop_config.json`

```json
{
  "mcpServers": {
    "graphiti": {
      "command": "/Users/lbcheng/codex/graphiti-rust/target/release/graphiti-mcp-server",
      "args": [
        "--stdio",
        "--project",
        "/Users/lbcheng/codex",
        "--log-level",
        "warn"
      ],
      "env": {
        "RUST_LOG": "warn"
      }
    }
  }
}
```

## 🔧 参数说明

- `--stdio`: 启用JSON stdio模式（必需）
- `--project`: 指定项目目录路径
- `--log-level`: 设置日志级别（debug, info, warn, error）
- `RUST_LOG`: 环境变量控制Rust日志输出

## 🧪 测试配置

```bash
# 测试stdio模式
echo '{"jsonrpc": "2.0", "id": 1, "method": "tools/list", "params": {}}' | \
  ./target/release/graphiti-mcp-server --stdio --project /Users/lbcheng/codex --log-level error
```

## 🛠️ 可用工具

Graphiti MCP提供10个工具：

1. **add_memory** - 添加记忆
2. **search_memory** - 搜索记忆
3. **add_code_entity** - 添加代码实体
4. **search_code** - 搜索代码实体
5. **record_activity** - 记录开发活动
6. **get_context_suggestions** - 获取智能建议
7. **batch_add_code_entities** - 批量添加代码实体
8. **batch_record_activities** - 批量记录活动
9. **scan_project** - 扫描项目结构
10. **get_related_memories** - 获取相关记忆

## 🚀 启动步骤

1. 确保可执行文件存在并有执行权限
2. 复制配置到Claude Desktop配置目录
3. 重启Claude Desktop
4. 在Claude中测试MCP工具

## ⚠️ 注意事项

- 确保路径正确（绝对路径）
- 确保可执行文件有执行权限
- 日志级别建议设为warn以减少输出
- 项目路径应指向你的工作目录
