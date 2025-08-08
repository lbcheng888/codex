# Graphiti Rust MCP Server - Docker Deployment Guide

This document provides a comprehensive guide for deploying the Graphiti Rust MCP Server using Docker, based on the [Graphiti Python MCP Server](https://github.com/getzep/graphiti/blob/main/mcp_server/README.md) reference implementation.

## 🚀 Quick Start

### Automated Setup (Recommended)

```bash
# Clone the repository
git clone https://github.com/getzep/graphiti.git
cd graphiti/graphiti-rust

# Run the interactive setup script
./scripts/setup-docker.sh
```

The setup script will guide you through choosing the right deployment option for your needs.

### Manual Setup

```bash
# Copy environment configuration
cp .env.docker .env
# Edit .env with your API keys (optional for free setup)

# Choose your deployment:

# 1. Full setup (Neo4j + Ollama + MCP Server)
docker-compose up -d

# 2. Free setup (CozoDB + Ollama + MCP Server)
docker-compose --profile free up -d

# 3. Development setup (with hot reload)
docker-compose -f docker-compose.yml -f docker-compose.dev.yml up -d

# 4. Full setup with monitoring
docker-compose --profile monitoring up -d
```

## 📋 Deployment Options

### 1. Full Setup (Recommended for Production)

**Services:** CozoDB (SQLite) + Ollama + MCP Server
**Port:** 8080

```bash
docker-compose up -d
```

**Endpoints:**
- MCP Server: http://localhost:8080
- Ollama API: http://localhost:11434
- CozoDB: Embedded SQLite database

### 2. Free Setup (No External Dependencies)

**Services:** CozoDB (Memory) + Ollama + MCP Server
**Port:** 8091

```bash
docker-compose --profile free up -d
```

**Endpoints:**
- MCP Server: http://localhost:8091
- Ollama API: http://localhost:11434

### 3. Development Setup

**Services:** CozoDB (SQLite) + Ollama + MCP Server + Hot reload
**Port:** 8080

```bash
docker-compose -f docker-compose.yml -f docker-compose.dev.yml up -d
```

**Features:**
- Source code mounted for hot reload
- Debug logging enabled
- Development-optimized configuration

### 4. Monitoring Setup

**Services:** CozoDB (SQLite) + Ollama + MCP Server + Prometheus + Grafana
**Ports:** 8080, 9090, 3000

```bash
docker-compose --profile monitoring up -d
```

**Additional Endpoints:**
- Prometheus: http://localhost:9090
- Grafana: http://localhost:3000 (admin/admin)

## 🔧 Configuration

### Environment Variables

Key environment variables (see `.env.docker` for full list):

```env
# LLM Provider API Keys (optional for Ollama)
OPENAI_API_KEY=your-openai-api-key
GROQ_API_KEY=your-groq-api-key
ANTHROPIC_API_KEY=your-anthropic-api-key

# Embedding Providers (optional)
VOYAGE_API_KEY=your-voyage-api-key
COHERE_API_KEY=your-cohere-api-key

# Neo4j Configuration
NEO4J_URI=bolt://neo4j:7687
NEO4J_USERNAME=neo4j
NEO4J_PASSWORD=password

# Server Configuration
RUST_LOG=info,graphiti=debug
```

### Configuration Files

- `config.toml` - Full configuration with Neo4j
- `config.free.toml` - Free configuration with CozoDB + Ollama

## 🧪 Testing Your Deployment

### Health Check Script

```bash
./scripts/health-check.sh
```

### Manual Testing

```bash
# Health check
curl http://localhost:8080/health

# Add an episode
curl -X POST http://localhost:8080/episodes \
  -H "Content-Type: application/json" \
  -d '{"content": "Alice met Bob at the conference", "source": "text"}'

# Search memories
curl "http://localhost:8080/memory/search?query=Alice&limit=10"
```

## 🔗 MCP Client Integration

### Claude Desktop (stdio)

```json
{
  "mcpServers": {
    "graphiti-memory": {
      "command": "docker",
      "args": [
        "exec", "-i", "graphiti-mcp-server",
        "/usr/local/bin/graphiti-mcp-server",
        "--config", "/etc/graphiti/config.toml",
        "--stdio"
      ]
    }
  }
}
```

### Claude Desktop (SSE with mcp-remote)

```json
{
  "mcpServers": {
    "graphiti-memory": {
      "command": "npx",
      "args": ["mcp-remote", "http://localhost:8080/sse"]
    }
  }
}
```

### Cursor IDE

```json
{
  "mcpServers": {
    "graphiti-memory": {
      "url": "http://localhost:8080/sse"
    }
  }
}
```

## 🛠️ Development Commands

```bash
# Build images
make docker-build

# Start services
make docker-full      # Full setup
make docker-free      # Free setup
make docker-monitoring # With monitoring

# Management
make docker-status    # Check service status
make docker-health    # Run health checks
make docker-logs      # View logs
make docker-clean     # Clean up resources
make docker-reset     # Reset everything

# Ollama management
make ollama-setup     # Pull required models
make ollama-models    # List available models

# Development
make docker-shell     # Access container shell
make docker-test      # Run tests in container
```

## 📊 Monitoring and Observability

When using the monitoring profile:

- **Prometheus**: Metrics collection at http://localhost:9090
- **Grafana**: Visualization at http://localhost:3000 (admin/admin)
- **Health checks**: Built-in health endpoints for all services
- **Logging**: Structured logging with configurable levels

## 🔍 Troubleshooting

### Common Issues

1. **Port conflicts**: Check if ports 8080, 7474, 7687, 11434 are available
2. **Ollama models**: Run `make ollama-setup` to pull required models
3. **Neo4j connection**: Check logs with `docker-compose logs neo4j`
4. **Memory issues**: Adjust Neo4j memory settings in docker-compose.yml

### Debugging Commands

```bash
# Check service status
docker-compose ps

# View logs
docker-compose logs -f graphiti
docker-compose logs -f neo4j
docker-compose logs -f ollama

# Access container shell
docker-compose exec graphiti bash

# Test Neo4j connection
docker-compose exec neo4j cypher-shell -u neo4j -p password "RETURN 1"

# Check Ollama models
docker-compose exec ollama ollama list
```

## 📚 Additional Resources

- [Docker README](docker/README.md) - Detailed Docker documentation
- [Main README](README.md) - Project overview and native installation
- [Graphiti Python MCP Server](https://github.com/getzep/graphiti/blob/main/mcp_server/README.md) - Reference implementation

## 🎯 Next Steps

1. **Test the deployment** using the health check script
2. **Configure your MCP client** (Claude Desktop, Cursor, etc.)
3. **Add your API keys** to the `.env` file for cloud LLM providers
4. **Explore the API endpoints** and start building with Graphiti!

For production deployments, consider:
- Using external Neo4j instances
- Setting up proper TLS certificates
- Configuring backup strategies
- Monitoring and alerting setup
