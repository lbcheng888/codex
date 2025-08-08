# LLM and Embedding Service Integration

This document describes how to use the LLM and embedding service integration in Graphiti Rust.

## Overview

The Rust implementation now supports:
- **LLM Services**: OpenAI, Ollama (local)
- **Embedding Services**: Qwen local models via HTTP server
- **Service Factory**: Easy configuration and creation of services
- **Health Monitoring**: Built-in health checks for all services

## Quick Start

### 1. Set up Qwen Embedding Server

First, install the required Python dependencies:

```bash
pip install transformers torch fastapi uvicorn numpy
```

Start the Qwen embedding server:

```bash
python scripts/qwen_embedding_server.py --model Qwen/Qwen3-0.6B-Base --port 8001
```

The server will automatically download the model on first run.

### 2. Set up Ollama (Optional)

For local LLM inference, install and start Ollama:

```bash
# Install Ollama
curl -fsSL https://ollama.ai/install.sh | sh

# Start Ollama
ollama serve

# Pull a model
ollama pull llama3.2:3b
```

### 3. Run the Integration Test

```bash
# Test local services (Ollama + Qwen)
cargo run --example llm_integration_test

# Test with OpenAI (requires API key)
OPENAI_API_KEY=your_key_here cargo run --example llm_integration_test
```

## Service Configurations

### Local Development Setup

```rust
use graphiti_llm::{ServiceFactory, ServiceConfig, LLMServiceConfig, EmbeddingServiceConfig};

// Create local services
let (llm_client, embedding_client) = ServiceFactory::create_local_services().await?;
```

### Hybrid Setup (OpenAI + Local Embeddings)

```rust
// Create hybrid services
let (llm_client, embedding_client) = ServiceFactory::create_hybrid_services(
    "your-openai-api-key".to_string()
).await?;
```

### Custom Configuration

```rust
let config = ServiceConfig {
    llm: LLMServiceConfig::Ollama(OllamaConfig {
        model: "llama3.2:3b".to_string(),
        base_url: "http://localhost:11434".to_string(),
        ..Default::default()
    }),
    embedding: EmbeddingServiceConfig::QwenLocal(QwenLocalConfig {
        model_name: "Qwen/Qwen3-0.6B-Base".to_string(),
        server_url: Some("http://localhost:8001".to_string()),
        batch_size: 8,
        ..Default::default()
    }),
};

let (llm_client, embedding_client) = ServiceFactory::create_services(&config).await?;
```