# 并行文件读取工具演示

这个文档展示了新添加的并行文件读取MCP工具的功能。

## 工具概述

`parallel_read` 工具允许同时读取多个文件，提供了以下特性：

- **并行处理**: 使用Tokio异步运行时并行读取多个文件
- **错误隔离**: 单个文件失败不会影响其他文件的读取
- **大小限制**: 可配置每个文件的最大读取字节数
- **超时控制**: 可设置整个操作的超时时间
- **性能限制**: 最多同时处理50个文件，防止资源滥用

## 参数说明

```json
{
  "file_paths": ["file1.txt", "file2.txt", "file3.txt"],  // 必需：要读取的文件路径数组
  "max_bytes_per_file": 2097152,                          // 可选：每个文件最大字节数 (默认2MB)
  "timeout_ms": 30000,                                    // 可选：超时时间毫秒 (默认30秒)
  "base_dir": "/path/to/base"                            // 可选：相对路径的基础目录
}
```

## 返回结果

```json
{
  "files": {
    "file1.txt": {
      "path": "file1.txt",
      "content": "文件内容...",
      "error": null,
      "size_bytes": 1024,
      "truncated": false
    },
    "file2.txt": {
      "path": "file2.txt", 
      "content": null,
      "error": "Failed to access file: No such file or directory",
      "size_bytes": null,
      "truncated": false
    }
  },
  "success_count": 1,
  "error_count": 1,
  "duration_ms": 245
}
```

## 使用场景

1. **代码审查**: 同时读取多个源文件进行分析
2. **配置检查**: 批量检查配置文件的内容
3. **文档生成**: 并行读取多个文档文件进行处理
4. **日志分析**: 同时读取多个日志文件

## 性能优势

相比串行读取，并行读取提供了显著的性能提升：

- 对于网络文件系统特别有效
- 减少了I/O阻塞时间
- 充分利用现代多核处理器
- 可配置的资源限制确保系统稳定性

## 安全特性

- 文件路径验证
- 大小限制防止内存耗尽
- 超时机制防止长时间阻塞
- 错误隔离确保部分失败不影响整体操作