# 新功能开发工作流

此工作流展示如何在开发新功能时有效利用Graphiti知识图谱，实现知识驱动的开发模式。

## 阶段1: 需求分析与知识发现

### 1.1 搜索相似功能实现
```bash
# 搜索相似的功能实现经验
search_memory({
  "query": "用户认证功能 OR 登录系统 OR 权限管理",
  "limit": 10
})
```

### 1.2 查找设计模式
```bash
# 查找相关的设计模式和架构决策
search_memory({
  "query": "MVC架构 OR 数据库设计 OR API设计",
  "limit": 5
})
```

### 1.3 记录需求分析
```bash
add_memory({
  "content": "新功能需求：实现用户角色管理系统，包含角色定义、权限分配、用户角色绑定功能。技术要求：RESTful API + JWT认证 + 数据库持久化",
  "source": "需求分析",
  "memory_type": "requirement"
})
```

## 阶段2: 架构设计

### 2.1 记录技术选型
```bash
add_memory({
  "content": "技术选型决策：使用Spring Boot + Spring Security + PostgreSQL。选择原因：团队熟悉度高，安全性好，社区支持完善。备选方案：Node.js + Passport.js",
  "source": "架构设计",
  "memory_type": "decision"
})
```

### 2.2 保存数据库设计
```bash
add_memory({
  "content": "数据库设计：users表(id,username,email)，roles表(id,name,description)，user_roles关联表(user_id,role_id)，permissions表(id,name,resource)，role_permissions关联表",
  "source": "数据库设计",
  "memory_type": "design"
})
```

### 2.3 记录API接口设计
```bash
add_memory({
  "content": "API接口设计：POST /api/roles (创建角色)，GET /api/roles (角色列表)，PUT /api/users/{id}/roles (分配角色)，GET /api/users/{id}/permissions (获取用户权限)",
  "source": "API设计",
  "memory_type": "design"
})
```

## 阶段3: 实现过程

### 3.1 搜索实现参考
```bash
# 在开始编码前，搜索相关实现模式
search_memory({
  "query": "JWT Token生成 OR Spring Security配置",
  "limit": 3
})
```

### 3.2 记录关键实现决策
```bash
add_memory({
  "content": "实现决策：JWT Token有效期设置为2小时，使用RS256算法签名，包含用户ID和角色信息。刷新Token机制：接近过期时自动刷新",
  "source": "实现细节",
  "memory_type": "implementation"
})
```

### 3.3 记录遇到的问题和解决方案
```bash
add_memory({
  "content": "问题解决：Spring Security配置冲突导致认证失败。解决方案：调整filterChain顺序，确保JWT过滤器在UsernamePasswordAuthenticationFilter之前",
  "source": "问题解决",
  "memory_type": "troubleshooting"
})
```

## 阶段4: 测试与优化

### 4.1 记录测试策略
```bash
add_memory({
  "content": "测试策略：单元测试覆盖Service层，集成测试验证API接口，安全测试包含权限边界检查、Token伪造防护、SQL注入防护",
  "source": "测试设计",
  "memory_type": "testing"
})
```

### 4.2 性能优化记录
```bash
add_memory({
  "content": "性能优化：角色权限查询添加Redis缓存，TTL设置为30分钟。数据库查询优化：user_roles表添加复合索引(user_id, role_id)",
  "source": "性能优化",
  "memory_type": "optimization"
})
```

## 阶段5: 完成总结

### 5.1 记录功能总结
```bash
add_memory({
  "content": "功能完成：用户角色管理系统开发完成，包含8个API接口，3个数据库表，完整的权限控制机制。开发耗时：5天，代码行数：约800行",
  "source": "项目总结",
  "memory_type": "completion"
})
```

### 5.2 经验教训记录
```bash
add_memory({
  "content": "经验教训：1. Spring Security配置需要注意过滤器顺序 2. JWT设计要考虑刷新机制 3. 权限缓存显著提升性能 4. 单元测试对Service层很重要",
  "source": "经验教训",
  "memory_type": "lesson"
})
```

### 5.3 后续改进建议
```bash
add_memory({
  "content": "改进建议：1. 增加角色继承功能 2. 实现细粒度权限控制 3. 添加权限变更审计日志 4. 考虑引入RBAC模型的扩展",
  "source": "改进建议",
  "memory_type": "improvement"
})
```

## 知识复用模式

### 查找可复用组件
```bash
search_memory({
  "query": "JWT工具类 OR 权限注解 OR 安全配置",
  "limit": 5
})
```

### 查找类似问题解决方案
```bash
search_memory({
  "query": "认证失败 OR 权限拒绝 OR Token过期",
  "limit": 3
})
```

## 团队协作

### 知识分享
```bash
add_memory({
  "content": "团队分享：在技术分享会上介绍了JWT最佳实践，团队决定统一使用这套权限管理模式。相关文档：confluence/jwt-best-practices",
  "source": "团队协作",
  "memory_type": "sharing"
})
```

### 代码审查记录
```bash
add_memory({
  "content": "代码审查反馈：建议将权限检查逻辑抽取为AOP切面，提高代码复用性。已采纳并实现@RequirePermission注解",
  "source": "代码审查",
  "memory_type": "review"
})
```

这个工作流确保了开发过程中产生的所有有价值的知识都被系统化地保存和组织，为后续的开发工作提供智能支持。