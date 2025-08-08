# Graphiti Rust MCP Server - 正确的架构说明

## 🔧 数据库架构修正

经过代码分析，发现Rust版本的Graphiti **并不使用Neo4j**，而是使用了以下图数据库：

### 🗄️ 支持的图数据库

#### 1. **CozoDB** (主要使用，默认推荐)
- **纯Rust实现**的多模型数据库
- 支持**关系型、图、向量混合查询**
- 引擎选项：
  - `mem` - 内存模式（快速，非持久化）
  - `sqlite` - SQLite文件存储（持久化，推荐生产环境）
  - `rocksdb` - RocksDB存储（高性能，持久化）

#### 2. **IndraDB** (可选)
- 纯Rust图数据库
- 支持多种后端：Memory、RocksDB、PostgreSQL、Sled
- 高性能图查询

#### 3. **Neo4j** (可选，通过neo4rs驱动)
- 与Python版本兼容
- 需要单独部署Neo4j服务器

## 🏗️ 当前实际架构

### 生产环境 (默认)
```
┌─────────────────────────────────┐    ┌─────────────────┐
│      Graphiti MCP Server        │    │     Ollama      │
│         (Port 8080)             │    │   (Port 11434)  │
│                                 │    │                 │
│  ┌─────────────────────────┐    │    │  - LLM Models   │
│  │       CozoDB            │    │    │  - Embeddings   │
│  │    (SQLite Engine)      │    │    │                 │
│  │   - Graph Storage       │    │    └─────────────────┘
│  │   - Vector Search       │    │
│  │   - Relational Queries  │    │
│  └─────────────────────────┘    │
│                                 │
│  ┌─────────────────────────┐    │
│  │      Tantivy            │    │
│  │   (Text Search)         │    │
│  └─────────────────────────┘    │
└─────────────────────────────────┘
```

### 免费模式
```
┌─────────────────────────────────┐    ┌─────────────────┐
│      Graphiti MCP Server        │    │     Ollama      │
│         (Port 8091)             │    │   (Port 11434)  │
│                                 │    │                 │
│  ┌─────────────────────────┐    │    │  - LLM Models   │
│  │       CozoDB            │    │    │  - Embeddings   │
│  │    (Memory Engine)      │    │    │                 │
│  │   - Graph Storage       │    │    └─────────────────┘
│  │   - Vector Search       │    │
│  │   - Relational Queries  │    │
│  └─────────────────────────┘    │
│                                 │
│  ┌─────────────────────────┐    │
│  │      Tantivy            │    │
│  │   (Text Search)         │    │
│  └─────────────────────────┘    │
└─────────────────────────────────┘
```

## 🔄 修正的Docker配置

### 主要变更

1. **移除了Neo4j服务** - 不再需要单独的Neo4j容器
2. **CozoDB嵌入式** - 数据库直接嵌入在MCP服务器中
3. **简化的架构** - 只需要Ollama + MCP服务器
4. **数据持久化** - 通过Docker volumes保存CozoDB数据

### 配置文件更新

#### `config.toml` (生产环境)
```toml
[cozo]
engine = "sqlite"
path = "/var/lib/graphiti/cozo/graphiti.db"
options = {}
```

#### `config.free.toml` (免费模式)
```toml
[cozo]
engine = "mem"
path = "/var/lib/graphiti/cozo/graphiti-free.db"
options = {}
```

## 🚀 部署选项

### 1. 完整设置 (推荐生产环境)
```bash
docker-compose up -d
```
- **端口**: 8080
- **数据库**: CozoDB SQLite (持久化)
- **LLM**: Ollama
- **存储**: 文件系统持久化

### 2. 免费设置 (开发/测试)
```bash
docker-compose --profile free up -d
```
- **端口**: 8091
- **数据库**: CozoDB Memory (非持久化)
- **LLM**: Ollama
- **存储**: 内存模式

### 3. 开发设置
```bash
docker-compose -f docker-compose.yml -f docker-compose.dev.yml up -d
```
- **特性**: 热重载 + 调试日志
- **数据库**: CozoDB SQLite
- **开发工具**: cargo-watch

### 4. 监控设置
```bash
docker-compose --profile monitoring up -d
```
- **额外服务**: Prometheus + Grafana
- **监控端口**: 9090 (Prometheus), 3000 (Grafana)

## 🔍 CozoDB 优势

### 为什么选择CozoDB？

1. **纯Rust实现** - 与项目技术栈完美匹配
2. **多模型支持** - 图、关系、向量查询统一
3. **嵌入式部署** - 无需单独数据库服务器
4. **高性能** - 针对现代硬件优化
5. **灵活存储** - 支持内存、SQLite、RocksDB
6. **零配置** - 开箱即用

### CozoDB vs Neo4j

| 特性 | CozoDB | Neo4j |
|------|--------|-------|
| 部署复杂度 | 嵌入式，零配置 | 需要单独服务器 |
| 技术栈 | 纯Rust | Java |
| 查询语言 | CozoScript | Cypher |
| 多模型支持 | ✅ 图+关系+向量 | ❌ 主要是图 |
| 内存模式 | ✅ | ❌ |
| 许可证 | MPL-2.0 | 社区版免费 |

## 📊 性能特点

### CozoDB性能优势
- **内存模式**: 极快的读写速度
- **SQLite模式**: 平衡性能和持久化
- **RocksDB模式**: 大数据量高性能
- **向量搜索**: 内置向量相似度搜索
- **事务支持**: ACID事务保证

### 存储选择建议
- **开发/测试**: `mem` 引擎 (最快)
- **小型生产**: `sqlite` 引擎 (平衡)
- **大型生产**: `rocksdb` 引擎 (高性能)

## 🔧 迁移指南

如果你之前期望使用Neo4j，现在需要了解：

1. **无需Neo4j安装** - CozoDB是嵌入式的
2. **查询语法不同** - 使用CozoScript而不是Cypher
3. **更简单的部署** - 只需要一个容器
4. **更好的集成** - 与Rust代码原生集成

## 🎯 总结

Rust版本的Graphiti使用**CozoDB作为主要图数据库**，这是一个更现代、更适合Rust生态系统的选择。Docker配置已经相应更新，提供了更简单、更高效的部署方案。
