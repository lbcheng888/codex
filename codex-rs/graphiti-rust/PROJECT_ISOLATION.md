# Graphiti MCP 项目隔离配置指南

## 概述

Graphiti MCP 服务器现在支持项目级别的数据隔离，确保不同项目的知识图谱数据不会混合，避免数据冲突和污染。

## 功能特性

### ✅ 项目隔离功能

- **项目专用数据库** - 每个项目使用独立的 SQLite 数据库
- **自动配置管理** - 自动创建项目专用的配置文件
- **目录结构管理** - 自动创建 `.graphiti/` 目录结构
- **环境变量支持** - 支持通过环境变量覆盖配置
- **命令行参数** - 支持通过命令行指定项目路径

### 📁 项目目录结构

```
project-root/
├── .graphiti/
│   ├── config.toml      # 项目专用配置
│   ├── data/           
│   │   └── graphiti.db  # 项目专用数据库
│   └── logs/           # 日志文件
├── src/
├── README.md
└── ...
```

## 使用方法

### 1. 自动项目隔离（推荐）

最简单的方式，直接设置 `cwd` 为项目目录：

```json
{
  "mcpServers": {
    "graphiti": {
      "type": "stdio",
      "command": "cargo",
      "args": [
        "run", "-p", "graphiti-mcp", "--bin", "graphiti-mcp-server"
      ],
      "cwd": "/path/to/your/project",
      "env": {
        "RUST_LOG": "info"
      },
      "alwaysAllow": [
        "add_memory", "search_memories", "get_stats",
        "export_knowledge", "search_code_entities"
      ]
    }
  }
}
```

### 2. 使用 --project 参数（类似 Serena）

如果需要从固定位置运行但指定不同的项目目录：

```json
{
  "mcpServers": {
    "graphiti": {
      "type": "stdio",
      "command": "cargo",
      "args": [
        "run", "-p", "graphiti-mcp", "--bin", "graphiti-mcp-server",
        "--project", "/Users/lbcheng/codex"
      ],
      "cwd": "/Users/lbcheng/codex/codex-rs/graphiti-rust",
      "env": {
        "RUST_LOG": "info",
        "GRAPHITI_PROJECT": "/Users/lbcheng/codex"
      },
      "alwaysAllow": [
        "add_memory", "search_memories", "get_stats",
        "export_knowledge", "search_code_entities"
      ]
    }
  }
}
```

### 3. 使用预编译二进制（自动项目隔离）

```json
{
  "mcpServers": {
    "graphiti": {
      "type": "stdio",
      "command": "/Users/lbcheng/codex/codex-rs/graphiti-rust/target/debug/graphiti-mcp-server",
      "args": [],
      "cwd": "/path/to/your/project",
      "env": {
        "RUST_LOG": "info"
      },
      "alwaysAllow": [
        "add_memory", "search_memories", "get_stats",
        "export_knowledge", "search_code_entities"
      ]
    }
  }
}
```

### 4. 自定义数据库路径

```json
{
  "mcpServers": {
    "graphiti": {
      "type": "stdio",
      "command": "cargo",
      "args": [
        "run", "-p", "graphiti-mcp", "--bin", "graphiti-mcp-server",
        "--db-path", "/custom/path/to/database.db"
      ],
      "cwd": "/Users/lbcheng/codex/codex-rs/graphiti-rust",
      "env": {
        "GRAPHITI_DB_PATH": "/custom/path/to/database.db"
      },
      "alwaysAllow": [
        "add_memory", "search_memories", "get_stats",
        "export_knowledge", "search_code_entities"
      ]
    }
  }
}
```

## 命令行参数

### 新增参数

- `--project <PATH>` - 指定项目目录（类似 Serena 的 --project）
- `--db-path <PATH>` - 指定数据库文件路径
- `--data-dir <PATH>` - 指定数据目录路径

### 环境变量

- `GRAPHITI_PROJECT` - 项目目录
- `GRAPHITI_DB_PATH` - 数据库路径
- `GRAPHITI_DATA_DIR` - 数据目录路径

## 自动化行为

### 1. 项目目录确定

- **自动模式**：使用当前工作目录（MCP 配置中的 `cwd`）
- **显式模式**：使用 `--project` 参数指定的目录（类似 Serena）

### 2. 配置文件管理

无论哪种模式，都会：
1. 检查 `{project-directory}/.graphiti/config.toml` 是否存在
2. 如果不存在，自动创建 `.graphiti/` 目录
3. 复制模板配置文件到项目目录
4. 自动设置项目专用的数据库路径

### 3. 数据库路径

- **默认**: `./data/graphiti.db`（相对于 graphiti-rust 目录）
- **项目模式**: `{project-directory}/.graphiti/data/graphiti.db`
- **自定义**: 通过 `--db-path` 或环境变量指定

### 3. 目录创建

服务器启动时会自动创建必要的目录：
- `.graphiti/` - 主配置目录
- `.graphiti/data/` - 数据库目录
- `.graphiti/logs/` - 日志目录（如果配置了）

## 最佳实践

### 1. 项目级配置

为每个项目创建专用的 MCP 配置：

```bash
# 项目 A
export GRAPHITI_PROJECT_ROOT="/path/to/project-a"

# 项目 B  
export GRAPHITI_PROJECT_ROOT="/path/to/project-b"
```

### 2. 配置模板

使用 `project-config-template.toml` 作为新项目的配置模板，可以根据需要自定义：

```toml
[project]
name = "My Awesome Project"
description = "Project description"
root_path = "."

[graphiti]
learning_enabled = true
auto_scan_enabled = true
scan_interval_minutes = 30
```

### 3. 备份和迁移

项目数据完全隔离，便于：
- 单独备份项目知识图谱
- 迁移项目到其他环境
- 删除不需要的项目数据

## 故障排除

### 常见问题

1. **权限错误** - 确保对项目目录有写权限
2. **路径不存在** - 确保指定的项目路径存在
3. **配置冲突** - 检查环境变量和命令行参数的优先级

### 调试

启用详细日志：
```bash
export RUST_LOG=debug
```

检查生成的配置：
```bash
cat .graphiti/config.toml
```

## 升级说明

从旧版本升级时：
1. 现有的全局数据库不会受影响
2. 新项目将自动使用项目隔离
3. 可以手动迁移现有数据到项目专用数据库
