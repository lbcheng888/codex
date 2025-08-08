#!/bin/bash

# Graphiti 免费版启动脚本
echo "🚀 启动 Graphiti 免费版..."

# 检查必要的工具
check_tool() {
    if ! command -v $1 &> /dev/null; then
        echo "❌ $1 未安装，请先安装"
        exit 1
    fi
}

echo "🔍 检查依赖..."
check_tool cargo
check_tool ollama
check_tool docker

# 启动 Ollama 服务 (如果未运行)
echo "🦙 启动 Ollama 服务..."
if ! pgrep -f "ollama serve" > /dev/null; then
    ollama serve &
    sleep 3
fi

# 检查并下载必要的模型
echo "📥 检查 LLM 模型..."
if ! ollama list | grep -q "llama3.1:8b"; then
    echo "下载 llama3.1:8b 模型 (约 4.9GB, 需要几分钟)..."
    ollama pull llama3.1:8b
fi

# Qwen3-Embedding-8B 现在通过 Hugging Face Inference API 使用，无需本地下载
echo "✅ 使用 Hugging Face Inference API 运行 Qwen3-Embedding-8B 嵌入模型"

# 启动 Neo4j (如果未运行)
echo "🗄️ 启动 Neo4j 数据库..."
if ! docker ps | grep -q neo4j; then
    docker run -d \
        --name neo4j \
        -p 7474:7474 -p 7687:7687 \
        -e NEO4J_AUTH=neo4j/password \
        -v neo4j_data:/data \
        neo4j:5.26
    
    echo "等待 Neo4j 启动..."
    sleep 10
fi

# 构建 Rust 项目
echo "🔨 构建 Graphiti..."
cargo build --release

# 启动 MCP 服务器
echo "🎯 启动 MCP 服务器..."
echo "配置文件: config.free.toml"
echo "服务地址: http://localhost:8080"
echo ""
echo "在 Claude Desktop 中使用以下配置:"
echo "配置文件路径: $(pwd)/claude_desktop_config.json"
echo ""

cargo run --bin graphiti-mcp-server -- --config config.free.toml