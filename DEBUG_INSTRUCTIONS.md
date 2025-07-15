# Grok-4-0709 TUI调试指南

## 问题描述
Grok-4-0709在TUI交互模式下无法调用工具的function calling，执行ls等命令时既没有返回结果也没有报错。

## 已实施的调试增强

我已经在代码的关键路径上添加了详细的调试日志，可以追踪从用户输入到函数调用执行的每一步。

### 调试日志覆盖范围：

1. **TUI层** (chatwidget.rs)
   - 用户消息提交
   - Op::UserInput发送

2. **核心层** (codex.rs)
   - Op处理和任务创建
   - 任务执行流程
   - API调用前后
   - 响应流接收
   - 函数调用识别和路由
   - 参数解析

3. **客户端层** (client.rs)
   - API调用详情
   - 使用的模型和提供商信息

### 启用调试模式

设置环境变量 `CODEX_DEBUG` 来启用详细日志：

```bash
export CODEX_DEBUG=1
codex-tui --model grok-4-0709
```

### 调试日志示例

当你输入"执行ls命令"时，你会看到类似这样的日志：

```
[DEBUG TUI] User message submitted: text='执行ls命令', images=0
[DEBUG TUI] Sending Op::UserInput with 1 items
[DEBUG CORE] Processing Op::UserInput with 1 items
[DEBUG CORE] Item 0: Text = '执行ls命令'
[DEBUG CORE] No current task, spawning new AgentTask for 1 items
[DEBUG CORE] run_task started with 1 input items
[DEBUG CORE] Input 0: Text = '执行ls命令'
[DEBUG CORE] Calling run_turn with 1 items
[DEBUG CLIENT] stream() called with 1 input items
[DEBUG CLIENT] Using provider: Chat, model: grok-4-0709
[DEBUG CORE] Creating stream for model API call...
[DEBUG CORE] Stream event: Created
[DEBUG CORE] Stream received FunctionCall: 'shell' (call_id: call_123)
[DEBUG CORE] Stream event: Completed
[DEBUG CORE] Stream ended with 3 events
[DEBUG CORE] run_turn completed with 1 output items
[DEBUG CORE] handle_response_item: FunctionCall 'shell' (call_id: call_123)
[DEBUG CORE] Function call received - name: 'shell', call_id: 'call_123'
[DEBUG CORE] Function arguments: {"command": "ls"}
[DEBUG CORE] Routing to built-in shell handler (tool: 'shell')
[DEBUG CORE] Parsing shell arguments: {"command": "ls"}
[DEBUG CORE] Successfully parsed standard shell tool call params
```

### 关键诊断点

1. **检查是否收到函数调用**
   - 查找 `Stream received FunctionCall` 日志
   - 如果没有，说明Grok没有返回函数调用

2. **检查工具名称识别**
   - 查找 `Function call received - name:` 日志
   - 确认工具名称是否被正确识别

3. **检查参数解析**
   - 查找 `Parsing shell arguments` 日志
   - 确认参数格式是否正确

4. **检查执行结果**
   - 查找后续的执行日志和输出

### 支持的工具名称

增强版现在支持多种工具名称别名：
- container.exec
- shell
- bash
- run_shell
- execute
- exec

### 支持的参数格式

1. 标准格式：`{"command": ["ls", "-la"]}`
2. 字符串格式：`{"command": "ls -la"}`

## 下一步

1. 使用调试模式运行 codex-tui
2. 尝试让Grok执行一个shell命令
3. 收集完整的调试日志
4. 根据日志确定问题发生的具体位置

调试日志将帮助我们确定是：
- Grok没有返回函数调用？
- 函数调用格式不兼容？
- 工具名称不被识别？
- 还是其他问题？