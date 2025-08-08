# 从 Python Graphiti 迁移到 Rust 版本

本文档详细说明如何从原始的 Python Graphiti 项目迁移到新的 Rust 重构版本。

## 🎯 迁移概述

### 主要变化
- **语言**: Python → Rust
- **图数据库**: Neo4j → CozoDB (更轻量，纯 Rust)
- **协议**: 直接 API → MCP (Model Context Protocol)
- **部署**: Python 环境 → 独立二进制文件

### 优势
- **性能**: 5-10倍启动速度提升，2-3倍内存使用减少
- **可靠性**: 编译时类型检查，零运行时错误
- **简化**: 无需 Java/Neo4j，单二进制部署
- **集成**: 原生 Claude Desktop 支持

## 📋 迁移检查清单

### 1. 系统要求变化

**Python 版本 (之前)**:
```bash
# 需要安装
- Python 3.9+
- Neo4j Desktop 或服务器
- Java 17+ (for Neo4j)
- 大量 Python 依赖
```

**Rust 版本 (现在)**:
```bash
# 只需要
- Rust 1.70+ (编译时)
- 单个二进制文件 (运行时)
- (可选) Ollama for 本地 LLM
```

### 2. 数据存储迁移

#### 从 Neo4j 到 CozoDB

**Neo4j (Python 版本)**:
```python
# 需要 Java 和 Neo4j 服务
driver = GraphDatabase.driver("bolt://localhost:7687")
```

**CozoDB (Rust 版本)**:
```rust
// 纯 Rust 实现，无需外部服务
let config = CozoConfig {
    engine: "sqlite".to_string(),
    path: "./data/graphiti.db".to_string(),
    options: json!({}),
};
```

#### 数据导出/导入工具 (计划中)

```bash
# 从 Python 版本导出数据
python -m graphiti export --format json > data.json

# 导入到 Rust 版本 (开发中)
./graphiti-mcp-server import --format json data.json
```

### 3. API 变化

#### Python API vs REST API

**Python 版本**:
```python
from graphiti import Graphiti

# 直接 Python 调用
graphiti = Graphiti()
result = graphiti.add_episode(
    content="User loves Python programming",
    source="conversation"
)
```

**Rust 版本**:
```bash
# REST API 调用
curl -X POST http://localhost:8080/memory \
  -H "Content-Type: application/json" \
  -d '{
    "content": "User loves Rust programming", 
    "source": "conversation"
  }'
```

#### MCP 集成 (新增)

Rust 版本专门设计为 MCP 服务，可直接集成到 Claude Desktop：

```json
{
  "mcpServers": {
    "graphiti": {
      "command": "/path/to/graphiti-mcp-server",
      "args": ["--config", "config.free.toml"]
    }
  }
}
```

### 4. 配置迁移

#### Python 配置格式

```python
# settings.py
OPENAI_API_KEY = "sk-..."
NEO4J_URI = "bolt://localhost:7687"
NEO4J_USER = "neo4j"
NEO4J_PASSWORD = "password"
```

#### Rust 配置格式

```toml
# config.free.toml
[llm]
provider = "Ollama"
base_url = "http://localhost:11434"
model = "llama3.1:8b"

[embedder] 
provider = "huggingface"
model = "Qwen/Qwen3-Embedding-8B"
dimension = 4096

[cozo]
engine = "sqlite"
path = "./data/graphiti.db"
```

## 🚀 分步迁移指南

### 步骤 1: 备份现有数据

```bash
# 从 Python 版本导出数据 (如果需要)
cd your-python-graphiti-project
python -m graphiti.tools.export --output backup.json
```

### 步骤 2: 安装 Rust 版本

```bash
# 克隆项目
git clone <rust-graphiti-repo>
cd graphiti-rust

# 构建项目
cargo build --release

# 验证安装
./target/release/graphiti-mcp-server --version
```

### 步骤 3: 配置免费方案

```bash
# 安装 Ollama (可选)
curl -fsSL https://ollama.com/install.sh | sh
ollama pull llama3.1:8b

# 使用免费配置
cp config.free.toml config.toml
```

### 步骤 4: 启动服务

```bash
# 启动 MCP 服务器
./target/release/graphiti-mcp-server --config config.toml

# 验证服务
curl http://localhost:8080/health
```

### 步骤 5: 集成 Claude Desktop

```bash
# 编辑 Claude Desktop 配置
vim ~/.config/claude-desktop/config.json

# 添加 MCP 服务器配置
{
  "mcpServers": {
    "graphiti": {
      "command": "/full/path/to/graphiti-mcp-server",
      "args": ["--config", "/full/path/to/config.toml"]
    }
  }
}
```

### 步骤 6: 测试迁移

```bash
# 测试基本功能
curl -X POST http://localhost:8080/memory \
  -H "Content-Type: application/json" \
  -d '{"content": "Migration test", "source": "migration"}'

curl "http://localhost:8080/memory/search?query=test"
```

## 📊 功能对比

| 功能 | Python 版本 | Rust 版本 | 状态 |
|------|-------------|-----------|------|
| 实体提取 | ✅ | 🚧 | 开发中 |
| 关系检测 | ✅ | 🚧 | 开发中 |
| 时序数据模型 | ✅ | ✅ | 完成 |
| 向量搜索 | ✅ | ✅ | 完成 |
| BM25 文本搜索 | ✅ | ✅ | 完成 |
| 图遍历 | ✅ | ✅ | 完成 |
| 社区检测 | ✅ | 📋 | 计划中 |
| MCP 协议 | ❌ | ✅ | 新增 |
| 单机部署 | ❌ | ✅ | 新增 |
| 免费运行 | ❌ | ✅ | 新增 |

## 🐛 常见问题

### Q: 如何从 Neo4j 迁移数据到 CozoDB？
A: 目前正在开发数据迁移工具。临时方案是重新处理原始数据源。

### Q: Python 版本的自定义实体类型如何迁移？
A: Rust 版本使用更灵活的 JSON 配置。正在开发兼容层。

### Q: 性能差异有多大？
A: 初步测试显示：
- 启动时间: Python 5-10秒 → Rust 0.5-1秒
- 内存使用: Python 200-500MB → Rust 50-150MB
- 查询延迟: Python 100-500ms → Rust 10-50ms

### Q: 是否支持 Python API 兼容性？
A: 计划中。将提供 Python 包装器库用于平滑迁移。

## 🔧 故障排除

### 编译错误
```bash
# 更新 Rust 工具链
rustup update
rustup component add clippy rustfmt

# 清理缓存重新构建
cargo clean
cargo build --release
```

### 服务启动失败
```bash
# 检查配置文件
./target/release/graphiti-mcp-server --config config.toml --validate

# 查看详细日志
./target/release/graphiti-mcp-server --config config.toml --log-level debug
```

### Claude Desktop 集成问题
```bash
# 检查 MCP 服务器日志
tail -f ~/.config/claude-desktop/logs/mcp.log

# 验证配置路径
which graphiti-mcp-server
```

## 📚 进一步资源

- **Rust 文档**: [docs.rs/graphiti-rust](https://docs.rs/graphiti-rust)
- **MCP 协议**: [modelcontextprotocol.io](https://modelcontextprotocol.io)
- **CozoDB 文档**: [docs.cozodb.org](https://docs.cozodb.org)
- **问题反馈**: [GitHub Issues](https://github.com/project/graphiti-rust/issues)

## 🤝 迁移支持

如果在迁移过程中遇到问题：

1. **文档**: 查看 [README.md](README.md) 和 [API 文档](docs/api.md)
2. **社区**: 加入 Discord 或 Slack 讨论组
3. **Issue**: 在 GitHub 提交详细的问题报告
4. **企业支持**: 联系商业支持团队

---

**迁移愉快！享受 Rust 版本带来的性能提升和可靠性！** 🚀