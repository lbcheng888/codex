#!/bin/bash

# Graphiti Status - 快速查看知识图谱状态
# 使用: ./graphiti-status.sh [--watch]

PORT=${GRAPHITI_PORT:-8092}
HOST=${GRAPHITI_HOST:-localhost}
WATCH_MODE=false

# 解析参数
if [[ "$1" == "--watch" ]]; then
    WATCH_MODE=true
fi

# 颜色定义
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[0;37m'
NC='\033[0m' # No Color

# MCP请求函数
mcp_request() {
    local method=$1
    local params=$2
    local request_body=$(cat <<EOF
{
    "jsonrpc": "2.0",
    "id": $(date +%s),
    "method": "tools/call",
    "params": {
        "name": "$method",
        "arguments": $params
    }
}
EOF
)
    
    curl -s -X POST "http://$HOST:$PORT/mcp" \
        -H "Content-Type: application/json" \
        -d "$request_body" 2>/dev/null
}

# 显示标题
show_header() {
    clear
    echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${CYAN}║${NC}              ${PURPLE}🧠 Graphiti Knowledge Status${NC}                     ${CYAN}║${NC}"
    echo -e "${CYAN}╠════════════════════════════════════════════════════════════════╣${NC}"
}

# 获取最近的记忆
get_recent_memories() {
    local response=$(mcp_request "search_memory" '{"query": "*", "limit": 5}')
    echo "$response" | jq -r '.result.content[0].text // "无数据"' 2>/dev/null || echo "连接失败"
}

# 获取最近的代码实体
get_recent_entities() {
    local response=$(mcp_request "search_code" '{"query": "*", "limit": 5}')
    echo "$response" | jq -r '.result.content[0].text // "无数据"' 2>/dev/null || echo "连接失败"
}

# 获取今日活动统计
get_today_stats() {
    local today=$(date +%Y-%m-%d)
    local response=$(mcp_request "search_memory" "{\"query\": \"*\", \"start_time\": \"${today}T00:00:00Z\", \"limit\": 100}")
    local count=$(echo "$response" | jq -r '.result.total // 0' 2>/dev/null || echo "0")
    echo "$count"
}

# 显示快速状态
show_quick_status() {
    show_header
    
    echo -e "${CYAN}║${NC} ${GREEN}📊 快速概览${NC}                    $(date '+%Y-%m-%d %H:%M:%S')"
    echo -e "${CYAN}╟────────────────────────────────────────────────────────────────╢${NC}"
    
    # 检查服务器状态
    if curl -s "http://$HOST:$PORT/mcp" > /dev/null 2>&1; then
        echo -e "${CYAN}║${NC} ${GREEN}✓${NC} 服务器状态: ${GREEN}在线${NC} (${HOST}:${PORT})"
    else
        echo -e "${CYAN}║${NC} ${RED}✗${NC} 服务器状态: ${RED}离线${NC}"
        echo -e "${CYAN}╚════════════════════════════════════════════════════════════════╝${NC}"
        exit 1
    fi
    
    # 今日统计
    local today_count=$(get_today_stats)
    echo -e "${CYAN}║${NC} 📅 今日新增: ${YELLOW}${today_count}${NC} 条记录"
    echo -e "${CYAN}╟────────────────────────────────────────────────────────────────╢${NC}"
    
    # 最近的记忆
    echo -e "${CYAN}║${NC} ${BLUE}📝 最近记忆：${NC}"
    local memories=$(get_recent_memories)
    if [[ "$memories" != "无数据" && "$memories" != "连接失败" ]]; then
        echo "$memories" | while IFS= read -r line; do
            if [[ -n "$line" ]]; then
                echo -e "${CYAN}║${NC}   • ${line:0:58}"
            fi
        done | head -5
    else
        echo -e "${CYAN}║${NC}   ${YELLOW}暂无记录${NC}"
    fi
    
    echo -e "${CYAN}╟────────────────────────────────────────────────────────────────╢${NC}"
    
    # 最近的代码实体
    echo -e "${CYAN}║${NC} ${PURPLE}🔧 最近代码实体：${NC}"
    local entities=$(get_recent_entities)
    if [[ "$entities" != "无数据" && "$entities" != "连接失败" ]]; then
        echo "$entities" | while IFS= read -r line; do
            if [[ -n "$line" ]]; then
                echo -e "${CYAN}║${NC}   • ${line:0:58}"
            fi
        done | head -5
    else
        echo -e "${CYAN}║${NC}   ${YELLOW}暂无记录${NC}"
    fi
    
    echo -e "${CYAN}╚════════════════════════════════════════════════════════════════╝${NC}"
}

# 实时监控模式
watch_mode() {
    while true; do
        show_quick_status
        echo -e "\n${YELLOW}[实时监控模式] 每5秒刷新 - 按 Ctrl+C 退出${NC}"
        sleep 5
    done
}

# 主程序
main() {
    if [[ "$WATCH_MODE" == true ]]; then
        watch_mode
    else
        show_quick_status
        echo -e "\n${WHITE}提示: 使用 ${CYAN}--watch${WHITE} 参数进入实时监控模式${NC}"
    fi
}

# 处理退出信号
trap 'echo -e "\n${GREEN}👋 再见！${NC}"; exit 0' INT

# 运行主程序
main