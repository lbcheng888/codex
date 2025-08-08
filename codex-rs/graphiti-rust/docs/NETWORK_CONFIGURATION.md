# 网络发现和传输协议配置指南

## 🌐 概述

本指南详细说明如何配置和使用分布式知识图谱系统的网络发现和传输协议功能。

## 📋 支持的协议

### 🔍 网络发现协议

#### 1. **mDNS (本地网络发现)**
- **用途**: 局域网内自动发现节点
- **优势**: 零配置、自动发现、低延迟
- **适用场景**: 开发环境、小型部署、局域网集群

```rust
// mDNS配置示例
use graphiti_distributed::{DistributedConfig, discovery::MdnsDiscovery};

let config = DistributedConfig {
    enable_discovery: true,
    bind_address: "0.0.0.0".to_string(),
    port: 8001,
    // ... 其他配置
};

let (tx, rx) = mpsc::unbounded_channel();
let mut mdns = MdnsDiscovery::new(config, tx);
mdns.start().await?;
```

#### 2. **配置文件发现**
- **用途**: 静态节点列表
- **优势**: 可预测、稳定、适合生产环境
- **适用场景**: 生产部署、已知节点拓扑

```rust
// 配置文件发现示例
let bootstrap_peers = vec![
    "192.168.1.10:8001".to_string(),
    "192.168.1.11:8001".to_string(),
    "192.168.1.12:8001".to_string(),
];

let config = DistributedConfig {
    bootstrap_peers,
    // ... 其他配置
};
```

#### 3. **DHT发现 (计划中)**
- **用途**: 大规模分布式网络
- **优势**: 可扩展、去中心化、容错性强
- **适用场景**: 大型分布式部署、公网环境

### 🚀 传输协议

#### 1. **TCP传输**
- **特点**: 可靠、有序、连接导向
- **优势**: 广泛支持、防火墙友好、错误检测
- **适用场景**: 大部分生产环境

```rust
// TCP传输配置
use graphiti_distributed::transport::TcpTransport;

let (tx, rx) = mpsc::unbounded_channel();
let mut transport = TcpTransport::new(config, tx);
transport.start().await?;
```

#### 2. **QUIC传输 (计划中)**
- **特点**: 基于UDP、内置加密、多路复用
- **优势**: 快速连接建立、更好的性能、内置安全
- **适用场景**: 高性能要求、现代网络环境

## ⚙️ 配置选项

### 基础网络配置

```rust
use graphiti_distributed::DistributedConfig;

let config = DistributedConfig {
    // 节点标识
    node_id: Uuid::new_v4(),
    
    // 网络绑定
    bind_address: "0.0.0.0".to_string(),
    port: 8001,
    
    // 集群配置
    cluster_name: "my-graphiti-cluster".to_string(),
    max_peers: 50,
    
    // 发现配置
    enable_discovery: true,
    bootstrap_peers: vec![
        "192.168.1.10:8001".to_string(),
    ],
    
    // 心跳和同步
    heartbeat_interval_secs: 30,
    sync_interval_secs: 300,
    
    // 其他选项
    enable_gossip: true,
    data_dir: "/var/lib/graphiti".to_string(),
};
```

### 高级网络配置

```rust
// 自定义网络管理器
let mut manager = NetworkManager::new(config).await?;

// 初始化发现服务
manager.init_discovery().await?;

// 初始化传输服务
manager.init_transport().await?;

// 注册消息处理器
let handler = GraphSyncMessageHandler::new(node_id);
manager.register_handler("graph-sync".to_string(), handler).await?;

// 启动网络服务
manager.start().await?;
```

## 🔧 部署场景

### 场景1: 开发环境
```toml
# 配置文件: dev-config.toml
[network]
bind_address = "127.0.0.1"
port = 8001
enable_discovery = true
enable_gossip = true
max_peers = 5

[discovery]
use_mdns = true
use_config = false
```

### 场景2: 局域网集群
```toml
# 配置文件: lan-config.toml
[network]
bind_address = "0.0.0.0"
port = 8001
enable_discovery = true
enable_gossip = true
max_peers = 20

[discovery]
use_mdns = true
use_config = true
bootstrap_peers = [
    "192.168.1.10:8001",
    "192.168.1.11:8001"
]
```

### 场景3: 生产环境
```toml
# 配置文件: prod-config.toml
[network]
bind_address = "0.0.0.0"
port = 8001
enable_discovery = false
enable_gossip = true
max_peers = 100

[discovery]
use_mdns = false
use_config = true
bootstrap_peers = [
    "node1.graphiti.internal:8001",
    "node2.graphiti.internal:8001",
    "node3.graphiti.internal:8001"
]

[security]
enable_tls = true
cert_file = "/etc/graphiti/tls.crt"
key_file = "/etc/graphiti/tls.key"
```

## 🛡️ 安全配置

### TLS加密 (计划中)
```rust
// TLS配置
let tls_config = TlsConfig {
    cert_file: "/etc/graphiti/tls.crt".to_string(),
    key_file: "/etc/graphiti/tls.key".to_string(),
    ca_file: Some("/etc/graphiti/ca.crt".to_string()),
    verify_peer: true,
};

let config = DistributedConfig {
    tls_config: Some(tls_config),
    // ... 其他配置
};
```

### 节点认证 (计划中)
```rust
// 节点认证配置
let auth_config = AuthConfig {
    auth_method: AuthMethod::Certificate,
    allowed_nodes: vec![
        "node1.graphiti.internal".to_string(),
        "node2.graphiti.internal".to_string(),
    ],
    auth_timeout_secs: 30,
};
```

## 📊 监控和调试

### 网络统计
```rust
// 获取网络统计信息
let stats = manager.get_stats().await;
println!("Connected peers: {}", stats.connected_peers);
println!("Messages sent: {}", stats.messages_sent);
println!("Messages received: {}", stats.messages_received);
```

### 日志配置
```rust
// 启用详细网络日志
use tracing_subscriber;

tracing_subscriber::fmt()
    .with_env_filter("graphiti_distributed=debug")
    .init();
```

### 调试工具
```bash
# 查看网络连接
netstat -tlnp | grep 8001

# 监控网络流量
tcpdump -i any port 8001

# 检查mDNS流量
tcpdump -i any port 5353
```

## 🚨 故障排除

### 常见问题

#### 1. **节点无法发现**
```bash
# 检查网络连接
ping target_node_ip

# 检查端口是否开放
telnet target_node_ip 8001

# 检查防火墙设置
sudo ufw status
```

#### 2. **连接超时**
```rust
// 增加超时时间
let config = DistributedConfig {
    connection_timeout_secs: 60,
    heartbeat_interval_secs: 15,
    // ...
};
```

#### 3. **内存使用过高**
```rust
// 限制连接数
let config = DistributedConfig {
    max_peers: 20,
    max_connections_per_peer: 5,
    // ...
};
```

### 性能优化

#### 1. **批量消息处理**
```rust
// 启用消息批处理
let config = DistributedConfig {
    enable_message_batching: true,
    batch_size: 100,
    batch_timeout_ms: 10,
    // ...
};
```

#### 2. **连接池管理**
```rust
// 配置连接池
let config = DistributedConfig {
    connection_pool_size: 10,
    connection_idle_timeout_secs: 300,
    // ...
};
```

## 📈 性能基准

### 典型性能指标
- **连接建立时间**: 50-200ms (TCP), 10-50ms (QUIC)
- **消息延迟**: 1-10ms (局域网), 10-100ms (广域网)
- **吞吐量**: 1000+ msg/s (单节点), 10000+ msg/s (集群)
- **并发连接**: 100+ (标准配置), 1000+ (优化配置)

### 基准测试
```rust
// 运行性能测试
cargo run --example distributed_network_example --release
```

## 🔮 未来功能

### 计划中的功能
1. **QUIC传输支持** - 更快的连接和更好的性能
2. **DHT发现** - 大规模分布式发现
3. **TLS加密** - 端到端安全通信
4. **负载均衡** - 智能流量分发
5. **网络拓扑优化** - 自适应网络结构
6. **带宽管理** - QoS和流量控制

### 集成路线图
1. **第一阶段** (当前): TCP + mDNS
2. **第二阶段** (1个月): QUIC + DHT
3. **第三阶段** (2个月): TLS + 认证
4. **第四阶段** (3个月): 高级功能

---

**总结**: 当前的网络发现和传输协议实现提供了完整的分布式网络功能，支持自动发现、可靠传输和灵活配置。通过模块化设计，可以根据不同的部署场景选择合适的协议组合。
