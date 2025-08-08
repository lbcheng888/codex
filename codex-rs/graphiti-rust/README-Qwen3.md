# Graphiti Rust MCP Server - Qwen3-Embedding-8B 集成

## 概述

这个版本的 Graphiti MCP 服务器使用了 **Qwen3-Embedding-8B** 作为嵌入模型，这是阿里巴巴开发的多语言嵌入模型，在 MTEB 多语言排行榜上排名第一（截至2025年6月5日，得分70.58）。

## 特性

- 🚀 **高性能嵌入**: Qwen3-Embedding-8B，4096维度向量
- 🌍 **多语言支持**: 支持100+种语言，中英文效果优异
- 💾 **本地运行**: 完全本地化，无需外部API
- 🔍 **混合搜索**: 结合语义嵌入、BM25文本搜索和图遍历
- 📊 **图数据库**: 使用CozoDB作为纯Rust图存储后端
- 🔌 **MCP协议**: 与Claude Code完美集成

## 配置文件

### config.qwen3-embedding.toml
使用本地Ollama运行的Qwen3-Embedding-8B模型：

```toml
[embedder]
provider = "ollama"
model = "dengcao/Qwen3-Embedding-8B:Q5_K_M"
dimension = 4096
batch_size = 8
timeout = 60
```

## 系统要求

- **内存**: 至少8GB RAM（推荐16GB+）
- **存储**: ~6GB用于Qwen3-Embedding-8B:Q5_K_M模型
- **CPU**: 现代多核CPU（推荐8核+）
- **Ollama**: 需要安装并运行Ollama服务

## 安装和运行

### 1. 安装Qwen3-Embedding模型

```bash
# 推荐的量化版本（平衡性能和内存）
ollama pull dengcao/Qwen3-Embedding-8B:Q5_K_M

# 或者其他版本
ollama pull dengcao/Qwen3-Embedding-8B:Q4_K_M  # 更少内存
ollama pull dengcao/Qwen3-Embedding-8B:Q8_0    # 更高质量
```

### 2. 编译项目

```bash
cargo build --release -p graphiti-mcp
```

### 3. 启动服务器

```bash
./target/release/graphiti-mcp-server -c config.qwen3-embedding.toml -p 8091
```

### 4. 配置Claude Code

```bash
claude mcp add -t http graphiti http://localhost:8091/mcp
```

### 5. 测试集成

```bash
./test-qwen-embedding.sh
```

## 性能对比

| 嵌入模型 | 维度 | 多语言支持 | MTEB排名 | 内存需求 |
|---------|------|-----------|----------|----------|
| nomic-embed-text | 768 | 英语为主 | - | ~1GB |
| **Qwen3-Embedding-8B** | **4096** | **100+语言** | **#1** | **~6GB** |
| HuggingFace API | 4096 | 多语言 | - | 需要API密钥 |

## 使用示例

### 添加中文知识
```json
{
  "method": "tools/call",
  "params": {
    "name": "add_memory",
    "arguments": {
      "content": "Qwen3-Embedding-8B是阿里巴巴开发的多语言嵌入模型",
      "source": "技术文档"
    }
  }
}
```

### 多语言搜索
```json
{
  "method": "tools/call",
  "params": {
    "name": "search_memory",
    "arguments": {
      "query": "阿里巴巴 embedding model",
      "limit": 10
    }
  }
}
```

## 架构说明

```
Claude Code
    ↓ HTTP MCP Protocol
Graphiti MCP Server
    ↓ Rust async
┌─────────────────┐  ┌──────────────────┐  ┌─────────────────┐
│   Qwen3-8B      │  │ Qwen3-Embedding  │  │    CozoDB       │
│   (LLM)         │  │ (4096d vectors)  │  │ (Graph Store)   │
│   实体提取       │  │   语义搜索        │  │   关系存储      │
└─────────────────┘  └──────────────────┘  └─────────────────┘
```

## 故障排除

### 常见问题

1. **模型未找到**
   ```bash
   ollama list | grep Qwen3-Embedding
   ```

2. **内存不足**
   - 使用Q4版本：`dengcao/Qwen3-Embedding-8B:Q4_K_M`
   - 或降低batch_size

3. **连接失败**
   ```bash
   curl -X POST http://localhost:8091/mcp -d '{"method":"initialize"}' 
   ```

4. **查看日志**
   ```bash
   tail -f server-qwen.log
   ```

## 开发说明

这个配置特别适合：
- 需要高质量中文嵌入的应用
- 多语言知识图谱
- 本地部署的企业环境
- 对数据隐私有要求的场景

Qwen3-Embedding-8B的4096维度向量提供了比小模型更丰富的语义表示，特别是在处理复杂的中英混合文本时表现优异。