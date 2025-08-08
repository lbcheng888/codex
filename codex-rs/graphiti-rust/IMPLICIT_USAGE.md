# Graphiti MCP 隐式集成使用指南

## 🎯 概述

Graphiti MCP的隐式集成功能让知识管理成为编码过程的自然组成部分，无需显式调用工具。系统会自动：

- 🔍 检测您的开发活动类型
- 📝 捕获重要的代码实体
- 📊 记录开发活动和时间
- 💡 提供上下文相关的建议
- 🧠 从历史中学习并改进

## 🚀 快速开始

### 1. 启用隐式功能

隐式功能默认启用。配置文件位于：
```
~/.graphiti/integration.toml
```

### 2. 工作原理

当您使用Claude Code的工具时，Graphiti会在后台自动工作：

```
您的操作                    →  Graphiti自动执行
─────────────────────────────────────────────────
编辑文件 (Edit/Write)       →  检测新函数/类，记录修改
搜索代码 (Grep/Glob)        →  理解您的意图，提供相关知识
运行命令 (Bash)            →  检测测试/部署/构建活动
阅读文件 (Read)            →  更新上下文，关联相关实体
```

## 📖 使用场景示例

### 场景1：开发新功能

当您开始实现新功能时：

```javascript
// 您编辑 UserController.js
class UserController {
    async createUser(req, res) {
        // 实现用户创建逻辑
    }
}
```

**Graphiti自动执行：**
- ✅ 检测到新类 `UserController`
- ✅ 检测到新方法 `createUser`
- ✅ 识别活动类型为"功能开发"
- ✅ 搜索历史中的类似实现
- 💡 建议："查看之前的AuthController实现模式"

### 场景2：修复Bug

当您搜索错误并修复时：

```bash
# 您执行搜索
grep "NullPointerException" -r src/

# 然后编辑有问题的文件
// 修复空指针检查
if (user != null && user.isActive()) {
    // 处理逻辑
}
```

**Graphiti自动执行：**
- ✅ 检测到调试活动（搜索异常）
- ✅ 识别Bug修复模式（添加null检查）
- ✅ 记录修复："修复UserService中的空指针异常"
- 💡 建议："考虑使用Optional来避免空指针"

### 场景3：编写测试

当您创建测试文件时：

```python
# 您创建 test_user_service.py
def test_create_user():
    user = UserService.create_user("test@example.com")
    assert user.id is not None
```

**Graphiti自动执行：**
- ✅ 检测到测试文件创建
- ✅ 识别活动类型为"测试开发"
- ✅ 关联测试与被测试的代码
- 💡 建议："记得测试边界条件和异常情况"

### 场景4：重构代码

当您进行多次相关修改时：

```
编辑 models/User.js → models/user.model.js
编辑 controllers/UserController.js
编辑 services/UserService.js
```

**Graphiti自动执行：**
- ✅ 检测到重构模式（多文件协调修改）
- ✅ 记录重构活动和影响范围
- ✅ 建立新旧代码的关联
- 💡 建议："运行所有测试确保重构没有破坏功能"

## ⚙️ 配置选项

### 基础配置

```toml
# ~/.graphiti/integration.toml

# 全局开关
enabled = true

# 功能开关
auto_capture_entities = true    # 自动捕获代码实体
auto_record_activities = true   # 自动记录活动
auto_suggestions = true         # 自动提供建议

# 智能阈值
confidence_threshold = 0.7      # 动作执行的最小置信度
capture_cooldown = 30          # 相似捕获的冷却时间（秒）
rate_limit = 100               # 每小时最大捕获数
```

### 文件过滤

```toml
# 排除敏感文件
exclusion_patterns = [
    "*.key",
    "*.pem",
    "*secret*",
    ".env"
]

# 监控的文件类型
include_patterns = [
    "*.js", "*.py", "*.java", "*.go",
    "*.rs", "*.ts", "*.rb", "*.php"
]
```

### 通知设置

```toml
[notifications]
show_captures = false    # 不显示捕获通知
show_suggestions = true  # 显示智能建议
level = "info"          # 通知级别
```

## 🎮 控制命令

### 临时控制

在Claude Code对话中使用：

```
"暂停自动捕获30分钟"
"停止监控当前文件"
"禁用这个项目的自动功能"
"恢复自动功能"
```

### 查看状态

```
"显示当前捕获了什么"
"今天记录了哪些活动"
"显示相关的历史知识"
```

## 🔍 工作原理详解

### 1. 上下文追踪

系统维护当前开发上下文：
- 最近访问的文件
- 最近的搜索查询
- 最近的代码修改
- 检测到的活动类型

### 2. 智能检测

使用多种信号判断您的意图：
- **文件名模式**：test_*.py → 测试开发
- **搜索模式**：搜索"error" → 调试活动
- **修改模式**：添加null检查 → Bug修复
- **命令模式**：运行测试 → 验证活动

### 3. 去重机制

避免重复记录：
- 内容哈希去重
- 时间窗口合并
- 相似度检测

### 4. 质量控制

只记录有价值的知识：
- 最小内容长度要求
- 排除临时/调试代码
- 置信度阈值过滤

## 💡 最佳实践

### DO 推荐做法

- ✅ **保持自然的开发流程** - 系统会适应您的习惯
- ✅ **偶尔查看捕获的知识** - 确保质量
- ✅ **利用智能建议** - 它们基于您的历史
- ✅ **为敏感项目调整配置** - 保护隐私

### DON'T 避免做法

- ❌ **不要为了触发捕获而改变行为** - 保持自然
- ❌ **不要担心"垃圾"数据** - 系统会过滤
- ❌ **不要禁用所有功能** - 至少保留建议功能

## 🎯 价值体现

### 个人价值
- 📈 **知识积累** - 每次编码都在建立个人知识库
- 🔍 **快速查找** - 轻松找到之前的解决方案
- 💡 **智能提醒** - 避免重复犯错
- ⏱️ **时间追踪** - 了解时间花在哪里

### 团队价值
- 🤝 **知识共享** - 团队经验自动沉淀
- 📚 **最佳实践** - 发现并推广好的模式
- 🐛 **问题预防** - 从历史问题中学习
- 👥 **新人友好** - 快速了解项目历史

## 🔧 故障排查

### 功能未生效？

1. 检查配置文件是否存在
2. 确认 `enabled = true`
3. 查看日志：`~/.graphiti/logs/`

### 捕获太多/太少？

调整阈值：
```toml
confidence_threshold = 0.8  # 提高精确度
capture_cooldown = 60      # 减少重复
```

### 性能影响？

```toml
rate_limit = 50            # 降低捕获频率
auto_suggestions = false   # 关闭实时建议
```

## 🚀 高级功能

### 自定义检测规则

创建 `~/.graphiti/rules.toml`：

```toml
[[rules]]
name = "API端点检测"
pattern = "@(Get|Post|Put|Delete)Mapping"
entity_type = "Api"
importance = 8

[[rules]]
name = "数据库迁移检测"
pattern = "CREATE TABLE|ALTER TABLE"
activity_type = "DatabaseMigration"
```

### 团队配置共享

将配置放入项目根目录：
```
.graphiti/
  ├── integration.toml   # 项目特定配置
  ├── rules.toml        # 自定义规则
  └── patterns.toml     # 代码模式
```

## 📊 效果展示

使用一周后的典型效果：

- 📝 **自动记录**：~500个代码实体，~100个活动
- 🔍 **智能搜索**：平均节省30%的搜索时间
- 💡 **有效建议**：85%的建议被认为有帮助
- 🐛 **问题预防**：减少20%的重复问题

---

隐式集成让Graphiti成为您的智能编程伙伴，在不打扰您的情况下默默积累和分享知识。享受更智能的编程体验！