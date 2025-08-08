# 代码审查工作流

此工作流展示如何利用Graphiti知识图谱提升代码审查质量，积累审查经验和编码标准。

## 阶段1: 审查准备

### 1.1 获取审查清单
```bash
# 搜索代码审查最佳实践和检查要点
search_memory({
  "query": "代码审查 OR code review OR 审查清单 OR checklist",
  "limit": 10
})
```

### 1.2 查找相关编码规范
```bash
# 根据代码类型查找相关规范
search_memory({
  "query": "Java编码规范 OR Spring Boot最佳实践 OR API设计规范",
  "limit": 5
})
```

### 1.3 了解业务上下文
```bash
# 搜索相关功能的设计背景
search_memory({
  "query": "用户认证 OR 权限管理 OR 安全设计",
  "limit": 8
})
```

## 阶段2: 代码审查执行

### 2.1 架构层面审查
```bash
add_memory({
  "content": "架构审查发现：新增的UserService直接依赖UserRepository，符合分层架构原则。但缺少缓存层，建议在高频查询方法上添加@Cacheable注解",
  "source": "代码审查",
  "memory_type": "architecture_review"
})
```

### 2.2 代码质量审查
```bash
add_memory({
  "content": "代码质量问题：1. getUserById方法缺少空值检查 2. 异常处理使用了catch-all模式，建议具体化异常类型 3. 日志级别使用不当，debug信息用了info级别",
  "source": "代码审查",
  "memory_type": "quality_issue"
})
```

### 2.3 安全性审查
```bash
add_memory({
  "content": "安全审查：发现SQL拼接存在注入风险，updateUserStatus方法应使用参数化查询。密码相关字段缺少脱敏处理，建议在日志输出时masking敏感信息",
  "source": "代码审查",
  "memory_type": "security_review"
})
```

### 2.4 性能审查
```bash
add_memory({
  "content": "性能问题：getAllUsers方法存在N+1查询问题，应使用JOIN或批量查询优化。循环中调用外部服务，建议改为批量调用或异步处理",
  "source": "代码审查",
  "memory_type": "performance_review"
})
```

## 阶段3: 最佳实践识别

### 3.1 记录好的实践
```bash
add_memory({
  "content": "最佳实践发现：开发者正确使用了@Transactional注解的rollbackFor属性，异常事务回滚处理得当。Service方法的参数校验使用了@Valid注解，符合Spring规范",
  "source": "最佳实践",
  "memory_type": "good_practice"
})
```

### 3.2 记录改进建议
```bash
add_memory({
  "content": "改进建议：1. 建议使用Builder模式构建复杂对象 2. 常量应定义在常量类中避免魔法数字 3. 单元测试覆盖率需提升，特别是边界条件测试",
  "source": "改进建议",
  "memory_type": "improvement_suggestion"
})
```

## 阶段4: 知识传播

### 4.1 记录团队讨论
```bash
add_memory({
  "content": "团队讨论：关于异常处理策略达成共识 - 业务异常使用自定义异常类，系统异常统一捕获并记录。更新了异常处理规范文档",
  "source": "团队讨论",
  "memory_type": "team_decision"
})
```

### 4.2 编码规范更新
```bash
add_memory({
  "content": "规范更新：基于本次审查发现的问题，更新了Java编码规范：1. 强制要求public方法参数非空检查 2. 数据库操作必须使用参数化查询 3. 敏感信息日志脱敏",
  "source": "编码规范",
  "memory_type": "coding_standard"
})
```

## 阶段5: 审查总结

### 5.1 记录审查结果
```bash
add_memory({
  "content": "审查总结：本次审查覆盖8个文件，发现15个问题(安全:3个，性能:2个，质量:10个)。总体代码质量良好，主要问题集中在异常处理和边界检查",
  "source": "审查总结",
  "memory_type": "review_summary"
})
```

### 5.2 记录学习收获
```bash
add_memory({
  "content": "审查收获：学习了Spring Security的最新用法，了解了JWT Token的刷新机制设计。发现了新的代码质量检查工具SonarQube的规则配置技巧",
  "source": "学习收获",
  "memory_type": "learning"
})
```

## 智能审查辅助

### 模式匹配检查
```bash
# 搜索常见的代码问题模式
search_memory({
  "query": "常见错误 OR 代码坏味道 OR anti-pattern",
  "limit": 10
})
```

### 历史问题查询
```bash
# 查找特定类型的历史问题
search_memory({
  "query": "SQL注入 OR XSS漏洞 OR 性能瓶颈",
  "limit": 5
})
```

### 最佳实践推荐
```bash
# 根据代码类型推荐最佳实践
search_memory({
  "query": "Spring Boot OR RESTful API OR 数据访问层",
  "limit": 8
})
```

## 专项审查模板

### 安全审查清单
```bash
search_memory({
  "query": "安全检查清单 OR 安全审查要点 OR 漏洞防护",
  "limit": 5
})
```

### 性能审查清单
```bash
search_memory({
  "query": "性能检查 OR 性能优化 OR 数据库优化",
  "limit": 5
})
```

### API设计审查
```bash
search_memory({
  "query": "API设计规范 OR RESTful最佳实践 OR 接口设计",
  "limit": 5
})
```

## 新人培养

### 审查技能传授
```bash
add_memory({
  "content": "新人培养：为Junior开发者整理了代码审查技能清单，包含常见问题识别、审查工具使用、沟通技巧。建立了mentor-mentee审查配对制度",
  "source": "人才培养",
  "memory_type": "training"
})
```

### 审查经验分享
```bash
add_memory({
  "content": "经验分享：在技术沙龙分享了'高效代码审查的艺术'，重点介绍了如何平衡代码质量和开发效率，如何给出建设性的审查意见",
  "source": "知识分享",
  "memory_type": "knowledge_sharing"
})
```

## 质量度量

### 审查效果跟踪
```bash
add_memory({
  "content": "质量度量：通过代码审查，本月发现并修复了25个潜在bug，代码质量评分从B+提升至A-。审查覆盖率达到95%，平均审查时间控制在30分钟内",
  "source": "质量度量",
  "memory_type": "metrics"
})
```

### 持续改进
```bash
add_memory({
  "content": "持续改进：基于审查数据分析，识别出团队在异常处理方面的共性问题，计划组织专项培训。引入静态代码分析工具减少低级错误",
  "source": "持续改进",
  "memory_type": "improvement"
})
```

这个工作流将代码审查从单纯的质量检查转化为知识创造和传播的过程，每次审查都为团队的技术标准和最佳实践库增添价值。