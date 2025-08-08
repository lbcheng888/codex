# Graphiti MCP 使用指南

## 🎯 快速开始

### 1. 启动服务器

```bash
# 使用测试配置（跳过embedding，快速响应）
./target/release/graphiti-mcp-server -c config.test.toml -p 8092

# 或使用完整免费配置（包含Qwen3 embedding）
./target/release/graphiti-mcp-server -c config.free.toml -p 8091
```

### 2. 在Claude Code中配置

MCP服务器已经配置为HTTP transport，连接到：`http://localhost:8092/mcp`

### 3. 可用工具列表

Claude Code中可以直接使用以下MCP工具：

## 📝 基础知识管理

### add_memory - 添加记忆
```javascript
// 记录技术知识
add_memory({
  "content": "Spring Boot 3.0要求最低Java 17版本，推荐使用Java 21 LTS",
  "source": "技术文档"
})

// 记录项目决策
add_memory({
  "content": "决定使用Redis作为缓存层，因为需要支持分布式Session共享",
  "source": "架构决策"
})
```

### search_memory - 搜索记忆
```javascript
// 搜索技术相关内容
search_memory({
  "query": "Spring Boot 版本",
  "limit": 5
})

// 搜索架构决策
search_memory({
  "query": "缓存 Redis",
  "limit": 10
})
```

## 🔧 代码实体管理

### add_code_entity - 添加代码实体
```javascript
// 记录一个重要的类
add_code_entity({
  "entity_type": "Class",
  "name": "UserAuthenticationService",
  "description": "处理用户认证的核心服务类，包含JWT token生成和验证",
  "file_path": "src/main/java/com/example/auth/UserAuthenticationService.java",
  "language": "Java",
  "framework": "Spring Boot",
  "complexity": 7,
  "importance": 9
})

// 记录一个API接口
add_code_entity({
  "entity_type": "Api",
  "name": "POST /api/auth/login",
  "description": "用户登录接口，接收用户名密码，返回JWT token",
  "framework": "REST API",
  "importance": 10
})

// 记录一个函数
add_code_entity({
  "entity_type": "Function",
  "name": "validateJwtToken",
  "description": "验证JWT token的有效性，检查签名和过期时间",
  "file_path": "src/utils/jwt.js",
  "line_range": [45, 78],
  "language": "JavaScript",
  "complexity": 5
})
```

### search_code - 搜索代码实体
```javascript
// 搜索所有认证相关的代码
search_code({
  "query": "authentication OR auth OR 认证",
  "limit": 10
})

// 搜索特定类型的实体
search_code({
  "query": "token",
  "entity_type": "Function",
  "language": "Java"
})

// 搜索特定框架的代码
search_code({
  "query": "controller",
  "framework": "Spring Boot",
  "limit": 5
})
```

## 📊 开发活动记录

### record_activity - 记录开发活动
```javascript
// 记录功能实现
record_activity({
  "activity_type": "Implementation",
  "title": "实现用户角色管理功能",
  "description": "完成了角色CRUD、权限分配、用户角色绑定等功能",
  "developer": "张三",
  "project": "用户管理系统",
  "duration_minutes": 480,
  "difficulty": 7,
  "quality": 9
})

// 记录Bug修复
record_activity({
  "activity_type": "BugFix",
  "title": "修复登录超时问题",
  "description": "发现是数据库连接池配置不当，调整了HikariCP参数",
  "developer": "李四",
  "project": "认证服务",
  "duration_minutes": 120,
  "difficulty": 5
})

// 记录代码审查
record_activity({
  "activity_type": "CodeReview",
  "title": "审查用户服务代码",
  "description": "发现SQL注入风险，建议使用参数化查询",
  "developer": "王五",
  "project": "用户管理系统",
  "quality": 7
})
```

## ⚡ 批量操作

### batch_add_code_entities - 批量添加代码实体
```javascript
batch_add_code_entities({
  "entities": [
    {
      "entity_type": "Class",
      "name": "UserController",
      "description": "用户管理REST控制器"
    },
    {
      "entity_type": "Class", 
      "name": "UserService",
      "description": "用户业务逻辑服务"
    },
    {
      "entity_type": "Class",
      "name": "UserRepository",
      "description": "用户数据访问层"
    }
  ]
})
```

### batch_record_activities - 批量记录活动
```javascript
batch_record_activities({
  "activities": [
    {
      "activity_type": "Implementation",
      "title": "实现用户注册功能",
      "description": "包含邮箱验证",
      "developer": "张三",
      "project": "用户系统"
    },
    {
      "activity_type": "UnitTesting",
      "title": "编写用户服务测试",
      "description": "覆盖率达到85%",
      "developer": "张三",
      "project": "用户系统"
    }
  ]
})
```

## 🤖 智能建议

### get_context_suggestions - 获取上下文建议
```javascript
// 获取Bug修复建议
get_context_suggestions({
  "context": "BugFix",
  "description": "用户登录时偶尔失败，报连接超时错误"
})

// 获取性能优化建议
get_context_suggestions({
  "context": "Performance",
  "description": "API响应时间过长，数据库查询慢"
})

// 获取安全建议
get_context_suggestions({
  "context": "Security",
  "description": "处理用户输入和认证"
})

// 获取API设计建议
get_context_suggestions({
  "context": "ApiDesign",
  "description": "设计用户管理的RESTful接口"
})

// 获取重构建议
get_context_suggestions({
  "context": "Refactoring",
  "description": "代码重复太多，类职责不清"
})
```

## 🌟 实际使用场景

### 场景1：开始新功能开发
```javascript
// 1. 先搜索相似功能
search_memory({"query": "用户认证 登录", "limit": 5})
search_code({"query": "authentication", "entity_type": "Class"})

// 2. 记录设计决策
add_memory({
  "content": "决定使用JWT而非Session，因为需要支持移动端和微服务架构",
  "source": "架构决策"
})

// 3. 添加代码实体
add_code_entity({
  "entity_type": "Class",
  "name": "JwtAuthenticationFilter",
  "description": "JWT认证过滤器，拦截所有需要认证的请求",
  "framework": "Spring Security"
})

// 4. 记录开发活动
record_activity({
  "activity_type": "Implementation",
  "title": "实现JWT认证机制",
  "description": "完成token生成、验证、刷新功能",
  "developer": "您的名字",
  "project": "认证服务"
})
```

### 场景2：调试问题
```javascript
// 1. 搜索历史问题
search_memory({"query": "连接超时 timeout", "limit": 10})

// 2. 获取智能建议
get_context_suggestions({
  "context": "BugFix",
  "description": "数据库连接池在高并发时耗尽"
})

// 3. 记录解决方案
add_memory({
  "content": "解决连接池耗尽：maxPoolSize从10增加到20，connectionTimeout设为20秒",
  "source": "问题解决"
})

// 4. 记录修复活动
record_activity({
  "activity_type": "BugFix",
  "title": "修复高并发连接池耗尽问题",
  "description": "调整HikariCP配置参数",
  "developer": "您的名字",
  "project": "数据库层",
  "difficulty": 6
})
```

### 场景3：代码审查
```javascript
// 1. 获取审查清单
search_memory({"query": "代码审查 checklist", "limit": 5})

// 2. 记录审查发现
batch_add_code_entities({
  "entities": [
    {
      "entity_type": "SecurityFix",
      "name": "修复SQL注入漏洞",
      "description": "UserDao中使用了字符串拼接SQL"
    },
    {
      "entity_type": "PerformanceOptimization", 
      "name": "优化N+1查询",
      "description": "UserService.getAllWithRoles存在N+1问题"
    }
  ]
})

// 3. 记录审查活动
record_activity({
  "activity_type": "CodeReview",
  "title": "审查用户管理模块",
  "description": "发现2个安全问题，1个性能问题",
  "developer": "审查者",
  "project": "用户管理",
  "quality": 7
})
```

## 💡 最佳实践

1. **持续记录** - 养成记录重要决策和发现的习惯
2. **搜索先行** - 在解决问题前先搜索历史经验
3. **结构化信息** - 使用合适的entity_type和activity_type
4. **团队共享** - 记录的知识对整个团队都有价值
5. **定期总结** - 使用search功能回顾和总结经验

## 🔍 故障排查

如果工具调用失败：
1. 检查服务器是否运行：`curl http://localhost:8092/mcp`
2. 查看服务器日志了解错误信息
3. 确保使用正确的参数格式
4. 对于中文内容，确保使用UTF-8编码

现在您可以开始使用这个强大的知识管理系统，让每次开发都成为知识积累的过程！