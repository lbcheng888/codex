#!/usr/bin/env node

/**
 * Graphiti Knowledge Monitor - 实时查看知识图谱学习内容
 * 
 * 使用方法：
 * node knowledge-monitor.js [--port 8092] [--interval 5000]
 */

const http = require('http');
const readline = require('readline');

// 配置
const args = process.argv.slice(2);
const config = {
    port: args.includes('--port') ? args[args.indexOf('--port') + 1] : '8092',
    interval: args.includes('--interval') ? parseInt(args[args.indexOf('--interval') + 1]) : 5000,
    host: 'localhost'
};

// 终端界面
const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

// MCP请求辅助函数
async function mcpRequest(method, params = {}) {
    const data = JSON.stringify({
        jsonrpc: "2.0",
        id: Date.now(),
        method: "tools/call",
        params: {
            name: method,
            arguments: params
        }
    });

    const options = {
        hostname: config.host,
        port: config.port,
        path: '/mcp',
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            'Content-Length': data.length
        }
    };

    return new Promise((resolve, reject) => {
        const req = http.request(options, (res) => {
            let body = '';
            res.on('data', (chunk) => body += chunk);
            res.on('end', () => {
                try {
                    const result = JSON.parse(body);
                    resolve(result);
                } catch (e) {
                    reject(e);
                }
            });
        });

        req.on('error', reject);
        req.write(data);
        req.end();
    });
}

// 知识统计
let stats = {
    memories: { total: 0, recent: [] },
    entities: { total: 0, recent: [] },
    activities: { total: 0, recent: [] },
    lastUpdate: new Date()
};

// 获取最新数据
async function fetchLatestData() {
    try {
        // 获取最近的记忆
        const memoriesRes = await mcpRequest('search_memory', {
            query: '*',
            limit: 5
        });

        // 获取最近的代码实体
        const entitiesRes = await mcpRequest('search_code', {
            query: '*',
            limit: 5
        });

        // 更新统计
        if (memoriesRes.result) {
            const memories = memoriesRes.result.content?.[0]?.text || '';
            stats.memories.recent = parseMemories(memories);
            stats.memories.total = extractTotal(memories);
        }

        if (entitiesRes.result) {
            const entities = entitiesRes.result.content?.[0]?.text || '';
            stats.entities.recent = parseEntities(entities);
            stats.entities.total = extractTotal(entities);
        }

        stats.lastUpdate = new Date();
    } catch (error) {
        console.error('获取数据失败:', error.message);
    }
}

// 解析返回的文本数据
function parseMemories(text) {
    // 简单解析，实际应根据返回格式调整
    return text.split('\n')
        .filter(line => line.trim())
        .slice(0, 5)
        .map(line => line.substring(0, 80));
}

function parseEntities(text) {
    return text.split('\n')
        .filter(line => line.trim())
        .slice(0, 5)
        .map(line => line.substring(0, 80));
}

function extractTotal(text) {
    const match = text.match(/total[:\s]+(\d+)/i);
    return match ? parseInt(match[1]) : 0;
}

// 显示仪表板
function displayDashboard() {
    console.clear();
    console.log('╔════════════════════════════════════════════════════════════════╗');
    console.log('║              🧠 Graphiti Knowledge Monitor                     ║');
    console.log('╠════════════════════════════════════════════════════════════════╣');
    
    console.log(`║ 📊 统计概览                  更新时间: ${stats.lastUpdate.toLocaleTimeString()}`);
    console.log('╟────────────────────────────────────────────────────────────────╢');
    console.log(`║ 📝 总记忆数: ${stats.memories.total.toString().padEnd(10)} 🔧 总实体数: ${stats.entities.total.toString().padEnd(10)}`);
    console.log('╟────────────────────────────────────────────────────────────────╢');
    
    console.log('║ 📝 最近记忆：');
    stats.memories.recent.forEach(memory => {
        console.log(`║   • ${memory.substring(0, 58).padEnd(58)}`);
    });
    
    console.log('╟────────────────────────────────────────────────────────────────╢');
    console.log('║ 🔧 最近代码实体：');
    stats.entities.recent.forEach(entity => {
        console.log(`║   • ${entity.substring(0, 58).padEnd(58)}`);
    });
    
    console.log('╟────────────────────────────────────────────────────────────────╢');
    console.log('║ 🎮 命令: [R]刷新 [S]搜索 [A]添加记忆 [E]导出 [Q]退出         ║');
    console.log('╚════════════════════════════════════════════════════════════════╝');
}

// 搜索功能
async function searchKnowledge() {
    rl.question('🔍 搜索关键词: ', async (query) => {
        try {
            const result = await mcpRequest('search_memory', {
                query: query,
                limit: 10
            });
            
            console.log('\n📋 搜索结果：');
            console.log('─'.repeat(60));
            
            const text = result.result?.content?.[0]?.text || '无结果';
            console.log(text);
            
            console.log('\n按任意键返回...');
            rl.once('line', () => displayDashboard());
        } catch (error) {
            console.error('搜索失败:', error.message);
            setTimeout(displayDashboard, 2000);
        }
    });
}

// 添加记忆
async function addMemory() {
    rl.question('📝 输入要记录的内容: ', async (content) => {
        rl.question('📂 来源 (可选): ', async (source) => {
            try {
                await mcpRequest('add_memory', {
                    content: content,
                    source: source || '手动记录'
                });
                
                console.log('✅ 记忆添加成功！');
                setTimeout(() => {
                    fetchLatestData().then(displayDashboard);
                }, 1000);
            } catch (error) {
                console.error('添加失败:', error.message);
                setTimeout(displayDashboard, 2000);
            }
        });
    });
}

// 实时监控会话信息
class SessionMonitor {
    constructor() {
        this.sessionStart = new Date();
        this.actions = [];
        this.activeFiles = new Set();
    }

    trackAction(action) {
        this.actions.push({
            type: action.type,
            timestamp: new Date(),
            details: action.details
        });
    }

    getSessionSummary() {
        const duration = Math.floor((new Date() - this.sessionStart) / 1000 / 60);
        return {
            duration: `${duration} 分钟`,
            totalActions: this.actions.length,
            filesEdited: this.activeFiles.size,
            recentActions: this.actions.slice(-5)
        };
    }
}

const sessionMonitor = new SessionMonitor();

// 处理键盘输入
function handleKeyPress() {
    rl.on('line', async (input) => {
        switch (input.toLowerCase()) {
            case 'r':
                await fetchLatestData();
                displayDashboard();
                break;
            case 's':
                await searchKnowledge();
                break;
            case 'a':
                await addMemory();
                break;
            case 'e':
                console.log('📤 导出功能开发中...');
                setTimeout(displayDashboard, 2000);
                break;
            case 'q':
                console.log('👋 再见！');
                process.exit(0);
            default:
                displayDashboard();
        }
    });
}

// 启动监控
async function startMonitoring() {
    console.log('🚀 启动 Graphiti Knowledge Monitor...');
    console.log(`📡 连接到 http://${config.host}:${config.port}`);
    
    // 初始获取数据
    await fetchLatestData();
    displayDashboard();
    
    // 定期刷新
    setInterval(async () => {
        await fetchLatestData();
        displayDashboard();
    }, config.interval);
    
    // 处理键盘输入
    handleKeyPress();
}

// WebSocket 监听实时更新（如果支持）
function connectWebSocket() {
    // 未来可以添加 WebSocket 支持以获得真正的实时更新
    console.log('💡 提示: WebSocket 实时更新功能即将推出');
}

// 主程序
async function main() {
    try {
        // 测试连接
        await mcpRequest('search_memory', { query: 'test', limit: 1 });
        
        // 启动监控
        await startMonitoring();
    } catch (error) {
        console.error('❌ 无法连接到 Graphiti 服务器');
        console.error(`   请确保服务器运行在 http://${config.host}:${config.port}`);
        console.error(`   错误: ${error.message}`);
        process.exit(1);
    }
}

// 优雅退出
process.on('SIGINT', () => {
    console.log('\n\n📊 会话总结：');
    const summary = sessionMonitor.getSessionSummary();
    console.log(`• 会话时长: ${summary.duration}`);
    console.log(`• 总操作数: ${summary.totalActions}`);
    console.log(`• 编辑文件: ${summary.filesEdited}`);
    console.log('\n👋 感谢使用 Graphiti Knowledge Monitor！');
    process.exit(0);
});

// 启动
main().catch(console.error);