# 从编程任务到知识图谱：完整工作流指南

## 🎯 任务示例：实现用户认证功能

让我们通过一个真实的编程任务，展示Graphiti如何捕获和学习您的开发过程。

## 📋 任务开始前的准备

### 1. 启动Graphiti MCP服务器

```bash
# 启动服务器
./target/release/graphiti-mcp-server -c config.test.toml -p 8092

# 服务器启动后会显示：
# [INFO] Starting MCP server on 0.0.0.0:8092
```

### 2. 检查连接状态

```bash
# 在Claude Code中
claude mcp list

# 应该看到：
# graphiti: http://localhost:8092/mcp (HTTP) - ✓ Connected
```

## 🚀 开始编程任务

### 步骤1：搜索相关经验

```javascript
// 在开始前，先搜索历史经验
search_memory({
  "query": "用户认证 JWT authentication 登录",
  "limit": 5
})

// Graphiti会返回：
// - 之前的JWT实现方案
// - 相关的安全考虑
// - 类似功能的代码结构
```

### 步骤2：记录设计决策

```javascript
// 记录您的技术选择
add_memory({
  "content": "决定使用JWT + Redis实现用户认证，因为需要支持分布式会话和移动端。选择bcrypt进行密码哈希，成本因子设为12。",
  "source": "架构决策"
})
```

### 步骤3：开始编码

当您创建新文件时：

```javascript
// 创建 auth/AuthService.js
class AuthService {
  async login(email, password) {
    // 实现登录逻辑
  }
  
  async generateToken(userId) {
    // JWT token生成
  }
}
```

**🤖 Graphiti自动捕获：**
- ✅ 检测到新类：`AuthService`
- ✅ 检测到新方法：`login`, `generateToken`
- ✅ 识别活动类型：功能开发
- ✅ 关联到"用户认证"上下文

### 步骤4：遇到问题时

```javascript
// 搜索错误
grep "TokenExpiredError" -r src/

// 修复token过期处理
if (error.name === 'TokenExpiredError') {
  // 添加自动刷新逻辑
  const newToken = await this.refreshToken(oldToken);
}
```

**🤖 Graphiti自动学习：**
- ✅ 检测到调试活动
- ✅ 识别问题：Token过期处理
- ✅ 记录解决方案：自动刷新机制

### 步骤5：添加测试

```javascript
// 创建 auth/AuthService.test.js
describe('AuthService', () => {
  test('should authenticate valid user', async () => {
    const token = await authService.login('test@example.com', 'password');
    expect(token).toBeDefined();
  });
});
```

**🤖 Graphiti识别：**
- ✅ 测试文件创建
- ✅ 关联测试与被测代码
- ✅ 记录测试覆盖情况

## 📊 实时查看学习内容

### 方法1：查询最近的活动

```javascript
// 查看最近记录的内容
search_memory({
  "query": "AuthService login JWT",
  "limit": 10
})

// 返回：
// 1. [新增] AuthService类 - 处理用户认证
// 2. [决策] 使用JWT + Redis的认证方案
// 3. [修复] Token过期自动刷新机制
// 4. [测试] AuthService单元测试
```

### 方法2：查看代码实体

```javascript
// 查看捕获的代码实体
search_code({
  "query": "auth",
  "entity_type": "Class",
  "limit": 5
})

// 返回：
// 1. AuthService - 用户认证服务类
// 2. AuthController - 认证API控制器
// 3. AuthMiddleware - JWT验证中间件
```

### 方法3：查看开发活动

```javascript
// 查看今天的开发活动
get_context_suggestions({
  "context": "Summary",
  "description": "今天的开发活动总结"
})

// 返回：
// - 功能开发：用户认证系统 (2小时)
// - Bug修复：Token过期处理 (30分钟)
// - 测试编写：认证功能测试 (45分钟)
// - 相关文件：8个
// - 代码实体：12个新增
```

## 🔍 知识图谱可视化

### 实时仪表板

创建一个简单的监控脚本：

```javascript
// monitor.js - 实时监控知识图谱
async function monitorKnowledge() {
  console.log('🧠 知识图谱实时监控');
  
  // 每分钟更新
  setInterval(async () => {
    const recentMemories = await search_memory({
      query: "*",
      limit: 5
    });
    
    const recentEntities = await search_code({
      query: "*",
      limit: 5
    });
    
    console.clear();
    console.log('📊 最新知识更新：');
    console.log('─'.repeat(50));
    
    console.log('📝 最近记忆：');
    recentMemories.results.forEach(m => {
      console.log(`  • ${m.content.substring(0, 60)}...`);
    });
    
    console.log('\n🔧 最近代码实体：');
    recentEntities.results.forEach(e => {
      console.log(`  • [${e.entity_type}] ${e.name}`);
    });
    
    console.log('\n📈 统计：');
    console.log(`  • 总记忆数：${recentMemories.total}`);
    console.log(`  • 总实体数：${recentEntities.total}`);
    console.log('─'.repeat(50));
  }, 60000);
}
```

### 知识关系图

```javascript
// 查看实体之间的关系
async function visualizeRelations(entityName) {
  // 搜索相关实体
  const entity = await search_code({
    query: entityName,
    limit: 1
  });
  
  if (entity.results.length > 0) {
    const id = entity.results[0].id;
    
    // 获取相关联的实体
    const related = await get_related(id, 2); // 深度为2
    
    console.log(`\n🔗 ${entityName} 的关系图：`);
    related.forEach(r => {
      console.log(`  ${r.source} --[${r.relation_type}]--> ${r.target}`);
    });
  }
}

// 使用示例
visualizeRelations("AuthService");
// 输出：
// 🔗 AuthService 的关系图：
//   AuthService --[USES]--> JwtService
//   AuthService --[DEPENDS_ON]--> UserRepository
//   AuthService --[TESTED_BY]--> AuthService.test.js
//   AuthController --[CALLS]--> AuthService
```

## 📱 实时通知

### 设置开发活动通知

```toml
# ~/.graphiti/integration.toml
[notifications]
show_captures = true      # 显示捕获通知
show_suggestions = true   # 显示建议
level = "info"

# 自定义通知规则
[[notification_rules]]
trigger = "bug_fix"
message = "🐛 Bug修复已记录：{title}"

[[notification_rules]]
trigger = "new_pattern"
message = "💡 发现新的代码模式：{pattern}"
```

### 命令行实时日志

```bash
# 查看实时日志
tail -f ~/.graphiti/logs/capture.log

# 输出示例：
[INFO] 检测到新类：AuthService
[INFO] 记录架构决策：JWT认证方案
[INFO] 检测到Bug修复：Token过期处理
[INFO] 关联测试文件：AuthService.test.js
[INFO] 智能建议：考虑添加密码重置功能
```

## 🎯 任务完成后的总结

### 自动生成任务报告

```javascript
// 生成任务总结
async function generateTaskSummary(taskName) {
  const summary = await get_context_suggestions({
    context: "TaskSummary",
    description: `总结任务：${taskName}`
  });
  
  console.log(`\n📋 任务总结：${taskName}`);
  console.log('═'.repeat(50));
  
  summary.forEach(item => {
    console.log(`• ${item}`);
  });
  
  // 获取具体指标
  const activities = await search_memory({
    query: taskName,
    memory_type: "activity"
  });
  
  console.log(`\n📊 开发指标：`);
  console.log(`• 总时长：${calculateTotalTime(activities)} 小时`);
  console.log(`• 文件数：${countUniqueFiles(activities)}`);
  console.log(`• 代码行：${estimateCodeLines(activities)}`);
  console.log(`• 问题解决：${countBugFixes(activities)}`);
}

// 使用
generateTaskSummary("用户认证功能");
```

## 💡 最佳实践

### 1. 任务开始时
- 🔍 先搜索相关历史知识
- 📝 记录关键技术决策
- 🎯 明确任务目标和范围

### 2. 开发过程中
- 💭 遇到问题时先搜索
- 🐛 解决问题后立即记录
- 💡 采纳智能建议

### 3. 任务结束后
- 📊 查看学习到的内容
- 📋 生成任务总结
- 🔄 回顾和优化

## 🚨 实时监控命令

### Claude Code中的快捷命令

```javascript
// 查看今日学习
"显示今天Graphiti学到了什么"

// 查看当前上下文
"当前的开发上下文是什么"

// 查看相关知识
"显示与当前文件相关的知识"

// 生成报告
"生成本次编程session的报告"
```

### 终端监控命令

```bash
# 实时统计
watch -n 5 'curl -s http://localhost:8092/stats | jq'

# 查看最新活动
curl http://localhost:8092/mcp \
  -H "Content-Type: application/json" \
  -d '{"method":"get_recent_activities","params":{"limit":10}}'

# 导出知识图谱
curl http://localhost:8092/export > knowledge_graph.json
```

## 🎉 效果展示

完成一个典型的编程任务后，Graphiti会学到：

### 知识类型
- 🏗️ **架构决策**：5-10条
- 🔧 **代码实体**：20-50个
- 🐛 **问题解决**：3-8个
- 📝 **最佳实践**：2-5条
- ⏱️ **时间分配**：完整记录

### 价值体现
- ⚡ **下次类似任务**：节省30%时间
- 🎯 **避免重复错误**：减少80%相同问题
- 📚 **知识传承**：新人上手时间减半
- 🔍 **快速定位**：秒级找到历史方案

通过这个完整的工作流，您可以清楚地看到Graphiti如何在您编程的每个阶段捕获知识，并且能够实时查看和利用这些知识！