# Claude Code + RAG 集成方案

## 概述
将UniConnectNative的RAG系统与Claude Code深度集成，提供智能代码查找和生成功能。

## 集成架构

### 1. Claude Code MCP 服务器扩展

创建 `mcp-rag-server` 作为Claude Code的扩展：

```rust
// mcp-rag-server/src/main.rs
use rag_system::RagSystem;
use serde_json::json;
use std::sync::Arc;
use tokio::sync::RwLock;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 初始化RAG系统
    let config = rag_system::config::RagConfig::default();
    let rag_system = Arc::new(RagSystem::new(config).await?);
    
    // 启动MCP服务器
    mcp_server::run(rag_system).await
}
```

### 2. Claude Code 配置文件

在 `~/.claude/config.json` 中添加RAG集成：

```json
{
  "mcp_servers": {
    "uniconnect_rag": {
      "command": "cargo",
      "args": ["run", "--bin", "mcp-rag-server"],
      "cwd": "/Users/lbcheng/DevEcoStudioProjects/UniConnectNative/rust-modules/rag-system"
    }
  }
}
```

### 3. 代码查找增强

#### 3.1 智能上下文感知
当Claude Code需要理解代码时，自动查询RAG系统：

```rust
// 在claude-code的扩展中
async fn get_relevant_context(
    query: &str,
    current_file: &str,
    rag_system: &RagSystem
) -> Vec<String> {
    let results = rag_system.query(query, 5).await.unwrap();
    results.iter()
        .map(|r| format!("文件: {}\n内容: {}\n", 
            r.file_path.display(), 
            r.content.lines().take(10).collect::<Vec<_>>().join("\n")))
        .collect()
}
```

#### 3.2 实时代码建议
在编写代码时提供上下文相关的建议：

```rust
// 集成到Claude Code的代码补全
async fn get_code_suggestions(
    prefix: &str,
    language: &str,
    rag_system: &RagSystem
) -> Vec<String> {
    let query = format!("{} {} implementation", language, prefix);
    let results = rag_system.query(&query, 3).await.unwrap();
    
    results.iter()
        .map(|r| extract_function_signature(&r.content))
        .collect()
}
```

### 4. 工作流集成

#### 4.1 查找相关实现
```bash
# Claude Code 命令集成
claude-code: "/find_similar rust libp2p bootstrap"
→ 查询RAG系统返回相关Rust实现
→ 提供代码片段和文件位置
```

#### 4.2 生成代码模板
```bash
# 基于现有代码生成
claude-code: "/generate_from_context kotlin p2p connection"
→ 搜索相关Kotlin P2P代码
→ 生成符合项目风格的模板
```

#### 4.3 验证最佳实践
```bash
# 检查代码是否符合项目规范
claude-code: "/check_best_practice android jni bridge"
→ 查询项目中的JNI实现
→ 对比当前代码与最佳实践
```

## 使用示例

### 1. 启动RAG服务
```bash
cd /Users/lbcheng/DevEcoStudioProjects/UniConnectNative/rust-modules/rag-system
cargo build --release
./target/release/rag-server
```

### 2. 索引代码库
```bash
./target/release/rag-indexer --path /Users/lbcheng/DevEcoStudioProjects/UniConnectNative
```

### 3. Claude Code 集成使用

在Claude Code中：
```
# 查找相关代码
/find_code "libp2p bootstrap connection"

# 获取代码示例
/get_examples "android p2p implementation"

# 验证实现
/check_implementation "rust jni android"
```

## 配置文件

创建 `~/.claude/rag-config.json`:

```json
{
  "rag_server": {
    "url": "http://localhost:3001",
    "timeout": 5000
  },
  "indexing": {
    "include_patterns": ["**/*.rs", "**/*.kt", "**/*.java"],
    "exclude_patterns": ["**/target/**", "**/build/**"]
  }
}
```

## 高级功能

### 1. 代码模式识别
自动识别代码中的重复模式并提供重构建议。

### 2. 跨语言参考
例如：查找Rust中的libp2p实现，为Android Kotlin代码提供参考。

### 3. 实时错误检测
通过RAG系统检测代码中的潜在问题，基于历史错误模式。

### 4. 文档生成
根据代码上下文自动生成符合项目风格的文档。

## 性能优化

- **增量索引**: 只索引修改的文件
- **缓存机制**: 缓存常用查询结果
- **并发处理**: 支持并行查询和索引

## 监控和调试

```bash
# 查看索引状态
curl http://localhost:3001/collection_info

# 手动触发查询
curl "http://localhost:3001/search?q=libp2p%20bootstrap&limit=5"
```

这个集成方案让Claude Code能够智能地理解和利用UniConnectNative项目中的代码知识，提供更准确的代码建议和生成。