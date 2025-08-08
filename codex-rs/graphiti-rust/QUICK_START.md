# 🚀 Graphiti 免费版快速开始指南

## 📋 准备工作清单

### ✅ 必需软件
- [x] Rust (已安装)
- [x] Ollama (已安装)  
- [x] Docker (已安装)

### 📥 下载模型 (后台进行中)
模型正在后台下载中，预计需要 10-15 分钟：
```bash
# 这些命令正在运行中，请耐心等待
ollama pull llama3.1:8b              # 约 4.9GB (主要 LLM)
ollama pull gemma2:2b                # 约 1.6GB (备用 LLM)  
# Qwen3-Embedding-8B 通过 Hugging Face Inference API 使用，无需本地下载
```

## 🔧 Claude Desktop 配置

### 步骤 1: 找到 Claude Desktop 配置文件
- **macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
- **Windows**: `%APPDATA%\Claude\claude_desktop_config.json`

### 步骤 2: 添加 Graphiti MCP 服务器配置

在配置文件中添加以下内容：

```json
{
  "mcpServers": {
    "graphiti": {
      "command": "cargo",
      "args": [
        "run", 
        "--bin", 
        "graphiti-mcp-server",
        "--",
        "--config",
        "/Users/lbcheng/graphiti/graphiti-rust/config.free.toml",
        "--log-level",
        "info"
      ],
      "cwd": "/Users/lbcheng/graphiti/graphiti-rust",
      "env": {
        "RUST_LOG": "info"
      }
    }
  }
}
```

### 步骤 3: 验证配置

1. 重启 Claude Desktop
2. 在对话中应该看到 "🔧" 图标，表示 MCP 工具可用
3. 可用工具包括：
   - `add_memory` - 添加记忆
   - `search_memory` - 搜索记忆  
   - `get_memory` - 获取记忆详情
   - `clear` - 清除记忆

## 🎯 快速测试

### 模型下载完成后，运行：

```bash
# 1. 检查设置状态
./test-setup.sh

# 2. 启动所有服务
./start-free.sh
```

### 在 Claude Desktop 中测试：

```markdown
请帮我添加一个记忆：Alice 今天和 Bob 讨论了新的项目计划，决定下个月完成 MVP。

然后搜索关于 Alice 的记忆。
```

## 🔧 配置文件说明

当前使用的免费配置 (`config.free.toml`):

```toml
[server]
host = "0.0.0.0"
port = 8080

# 完全免费的 LLM 配置
[llm]
provider = "Ollama"                    # 本地免费 LLM
base_url = "http://localhost:11434"
model = "llama3.1:8b"                  # 或 "gemma2:2b" 更快
temperature = 0.3
max_retries = 3

# 免费嵌入配置 (使用 Qwen3-Embedding-8B via Hugging Face)
[embedder]
provider = "huggingface"               # 使用 Hugging Face Inference API
api_key = ""                           # HF 免费层不需要 API key (可选)
model = "Qwen/Qwen3-Embedding-8B"      # Hugging Face 上的 Qwen3 嵌入模型  
dimension = 4096                       # Qwen3-Embedding-8B 的维度
batch_size = 8                         # HF API 使用更小的批次
timeout = 180                          # HF API 可能需要更长时间（模型冷启动）

# Neo4j 数据库配置
[neo4j]
uri = "bolt://localhost:7687"
username = "neo4j"
password = "password"
database = "neo4j"
max_connections = 10

[graphiti]
group_id = "default"
```

## 🎉 成功标志

当一切就绪时，你会看到：

1. **终端输出**：
   ```
   🎯 启动 MCP 服务器...
   服务地址: http://localhost:8080
   [INFO] Graphiti MCP server listening on 0.0.0.0:8080
   ```

2. **Claude Desktop**：
   - 工具栏出现 🔧 图标
   - 可以使用 "添加记忆" 和 "搜索记忆" 功能

3. **浏览器访问** `http://localhost:8080/health` 返回 OK

## 💰 成本对比

| 功能 | OpenAI 版本 | 免费版本 | 节省 |
|------|-------------|----------|------|
| 月使用成本 | ~$50+ | **$0** | 100% |
| 数据隐私 | ⭐⭐ | ⭐⭐⭐⭐⭐ | 完全本地 |
| 启动时间 | 即时 | 2-3分钟 | 一次性设置 |
| 响应速度 | 0.5-2秒 | 2-5秒 | 可接受 |

## 🛠️ 故障排除

### 常见问题：

1. **模型下载慢**
   ```bash
   # 使用更小的模型
   ollama pull gemma2:2b
   # 然后修改 config.free.toml 中的 model = "gemma2:2b"
   ```

2. **Neo4j 连接失败**
   ```bash
   docker ps | grep neo4j  # 检查是否运行
   docker logs neo4j-graphiti  # 查看日志
   ```

3. **Claude Desktop 无法连接**
   - 检查配置文件路径是否正确
   - 重启 Claude Desktop
   - 查看 Console.app 中的错误日志

### 获取帮助：
- 检查终端输出的错误信息
- 运行 `./test-setup.sh` 诊断问题
- 查看 `cargo run` 的详细日志

## 🎊 享受免费的知识图谱服务！

现在你拥有了一个完全免费、高性能、保护隐私的知识图谱服务，可以在 Claude Desktop 中无限制使用！