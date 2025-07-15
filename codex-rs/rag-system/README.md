# UniConnectNative RAG System

高性能的代码检索增强生成 (RAG) 系统，专为 UniConnectNative 代码库设计。

## 功能特性

- **多语言支持**: Rust, Kotlin, Java 代码解析
- **高性能嵌入**: 基于 Candle 的 CodeBERT 模型
- **向量存储**: Qdrant 向量数据库集成
- **并行处理**: Rayon + Tokio 异步优化
- **RESTful API**: 完整的 HTTP 接口
- **代码智能**: 语义搜索、代码生成、补全建议

## 快速开始

### 1. 安装依赖

```bash
# 安装 Qdrant (向量数据库)
docker run -d -p 6334:6334 qdrant/qdrant

# 安装 Rust 工具链
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### 2. 配置系统

创建 `rag_config.json`:

```json
{
  "embedding": {
    "model_name": "microsoft/codebert-base",
    "embedding_size": 768,
    "max_sequence_length": 512,
    "batch_size": 8
  },
  "vector_store": {
    "url": "http://localhost:6334",
    "collection_name": "uniconnect_code",
    "distance_metric": "Cosine"
  },
  "indexing": {
    "include_patterns": ["**/*.rs", "**/*.kt", "**/*.java"],
    "exclude_patterns": ["**/target/**", "**/build/**"],
    "max_file_size": 1048576,
    "chunk_size": 1000,
    "chunk_overlap": 100
  },
  "query": {
    "max_results": 10,
    "similarity_threshold": 0.7,
    "context_window": 2000
  }
}
```

### 3. 索引代码库

```bash
# 编译项目
cd rust-modules/rag-system
cargo build --release

# 索引 UniConnectNative 代码库
./target/release/rag-indexer --path ../../ --verbose

# 或只索引特定目录
./target/release/rag-indexer --path ../../android/app/src/main/java
```

### 4. 启动 API 服务器

```bash
# 启动 REST API 服务器
./target/release/rag-server

# 服务器将在 http://localhost:3000 启动
```

## API 使用示例

### 搜索代码示例

```bash
curl -X POST http://localhost:3000/examples \
  -H "Content-Type: application/json" \
  -d '{
    "query": "P2P connection management",
    "language": "Rust",
    "max_results": 5
  }'
```

### 代码生成

```bash
curl -X POST http://localhost:3000/generate \
  -H "Content-Type: application/json" \
  -d '{
    "description": "Create a P2P network discovery service",
    "language": "Rust",
    "include_tests": true
  }'
```

### 代码补全

```bash
curl -X POST http://localhost:3000/complete \
  -H "Content-Type: application/json" \
  -d '{
    "prefix": "pub async fn connect_to_peer",
    "language": "Rust",
    "max_suggestions": 3
  }'
```

## 高级用法

### 性能优化

```rust
use rag_system::{
    PerformanceOptimizer,
    CodeEmbedder,
    VectorStore,
    RagConfig,
};

#[tokio::main]
async fn main() -> Result<()> {
    let config = RagConfig::default();
    let optimizer = PerformanceOptimizer::new(8, 50);
    
    // 高性能批量处理
    let results = optimizer.batch_embed_and_store(
        embedder,
        vector_store,
        code_units,
    ).await?;
    
    Ok(())
}
```

### 自定义查询

```rust
use rag_system::QueryInterface;

let query = QueryInterface::new(embedder, vector_store, config).await?;

// 查找代码模式
let patterns = query.find_patterns(
    "async P2P networking",
    Some("Rust".to_string()),
    Some(10)
).await?;

// 解释代码功能
let explanation = query.explain_code(
    "your_code_snippet",
    Some(5)
).await?;
```

## 性能基准

| 操作 | 平均时间 | 并发度 |
|------|----------|--------|
| 代码解析 | ~50ms/文件 | 8线程 |
| 嵌入生成 | ~100ms/文件 | GPU加速 |
| 向量搜索 | ~10ms/查询 | 并行 |
| 批量索引 | ~500文件/分钟 | 4并发 |

## 架构设计

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Code Sources  │───▶│   Code Parser   │───▶│   Embedding     │
│   - Rust        │    │   - AST分析     │    │   - CodeBERT    │
│   - Kotlin      │    │   - 语义提取    │    │   - 向量化      │
│   - Java        │    │   - 依赖关系    │    │   - 并行处理    │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                                        │
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Query API     │◀───│   Vector Store  │◀───│   Performance   │
│   - REST API    │    │   - Qdrant      │    │   - 缓存优化    │
│   - 代码生成    │    │   - 索引优化    │    │   - 并发控制    │
│   - 智能补全    │    │   - 相似性搜索  │    │   - 批处理      │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## 故障排除

### 常见问题

1. **Qdrant 连接失败**
   ```bash
   # 检查容器状态
   docker ps | grep qdrant
   
   # 重启容器
   docker restart qdrant
   ```

2. **内存不足**
   ```bash
   # 减少批处理大小
   export RAG_BATCH_SIZE=10
   
   # 使用CPU模式
   export RAG_DEVICE=cpu
   ```

3. **模型下载失败**
   ```bash
   # 手动下载模型
   python -c "from transformers import AutoTokenizer, AutoModel; AutoTokenizer.from_pretrained('microsoft/codebert-base')"
   ```

### 调试模式

```bash
# 启用详细日志
RUST_LOG=debug ./target/release/rag-server

# 性能分析
RUST_LOG=perf cargo run --release
```

## 贡献指南

1. 添加新的语言支持：实现 `Parser` trait
2. 优化嵌入模型：扩展 `CodeEmbedder`
3. 改进查询算法：增强 `QueryEngine`
4. 性能优化：扩展 `PerformanceOptimizer`

## 许可证

MIT License - 与 UniConnectNative 项目保持一致