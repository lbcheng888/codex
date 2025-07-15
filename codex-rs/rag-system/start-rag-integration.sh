#!/bin/bash

# UniConnectNative RAG + Claude Code 集成启动脚本

set -e

echo "🚀 启动UniConnectNative RAG系统与Claude Code集成..."

# 获取当前目录
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# 检查是否已构建
if [ ! -f "$SCRIPT_DIR/target/release/mcp-server" ]; then
    echo "📦 首次构建RAG系统..."
    cd "$SCRIPT_DIR"
    cargo build --release --bin mcp-server
fi

# 创建配置文件（如果不存在）
CONFIG_FILE="$SCRIPT_DIR/rag_config.json"
if [ ! -f "$CONFIG_FILE" ]; then
    echo "⚙️  创建默认配置文件..."
    cat > "$CONFIG_FILE" << EOF
{
  "embedding": {
    "model_name": "mock-embedding",
    "embedding_size": 768,
    "max_sequence_length": 512
  },
  "vector_store": {
    "url": "http://localhost:6334",
    "collection_name": "uniconnect_code",
    "embedding_size": 768,
    "distance_metric": "Cosine"
  },
  "indexing": {
    "include_patterns": ["**/*.rs", "**/*.kt", "**/*.java"],
    "exclude_patterns": ["**/target/**", "**/build/**", "**/.git/**"],
    "max_file_size": 1048576,
    "chunk_size": 1000,
    "chunk_overlap": 100
  },
  "query": {
    "max_results": 10,
    "similarity_threshold": 0.7
  }
}
EOF
fi

# 启动RAG HTTP服务器（可选）
if [[ "$1" == "--with-server" ]]; then
    echo "🌐 启动RAG HTTP服务器..."
    nohup "$SCRIPT_DIR/target/release/rag-server" > rag-server.log 2>&1 &
    SERVER_PID=$!
    echo "✅ RAG服务器已启动，PID: $SERVER_PID"
    echo "📝 日志文件: $SCRIPT_DIR/rag-server.log"
fi

# 索引当前项目代码库
echo "📊 索引当前项目代码..."
cd "$SCRIPT_DIR"
"$SCRIPT_DIR/target/release/rag-indexer" --path "../.." --verbose

echo "✅ 集成完成！"
echo ""
echo "🎯 使用方式："
echo "1. 确保Claude Code已配置MCP服务器"
echo "2. 在Claude Code中使用："
echo "   - 查找代码: /find_code 'libp2p bootstrap'"
echo "   - 获取示例: /get_examples 'android p2p'"
echo "   - 检查实现: /check_implementation 'rust jni'"
echo ""
echo "📁 配置文件位置:"
echo "   - RAG配置: $CONFIG_FILE"
echo "   - Claude配置: ~/.claude/mcp-rag-config.json"
echo ""
echo "🔧 管理命令："
echo "   - 重新索引: ./start-rag-integration.sh"
echo "   - 启动HTTP: ./start-rag-integration.sh --with-server"
echo "   - 停止服务: pkill -f rag-server"