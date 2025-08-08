# Graphiti Rust: Free LLM & Embedding Guide

This guide shows you how to use Graphiti Rust with completely free LLM and embedding providers, eliminating any API costs.

## Quick Start with Free Providers

### 1. Ollama (Recommended - 100% Free & Local)

[Ollama](https://ollama.ai) runs LLMs locally on your machine with no API costs.

#### Installation

```bash
# Install Ollama
curl -fsSL https://ollama.ai/install.sh | sh

# Pull a free model (choose one)
ollama pull llama3.2:latest     # 3B parameters - fast, good quality
ollama pull mistral:latest      # 7B parameters - higher quality
ollama pull codellama:latest    # Code-specialized model

# Pull an embedding model
ollama pull nomic-embed-text:latest
```

#### Configuration

Update `config.toml`:

```toml
[llm]
provider = "ollama"

[llm.ollama]
base_url = "http://localhost:11434"
model = "llama3.2:latest"
timeout_secs = 120
max_retries = 3
rate_limit = 120
keep_alive_secs = 600

[embedder]
provider = "ollama"

[embedder.ollama]
base_url = "http://localhost:11434"
model = "nomic-embed-text:latest"
dimension = 768
batch_size = 16
timeout_secs = 60
```

### 2. Local SentenceTransformers Server

For embeddings, you can run a local server using Python and SentenceTransformers.

#### Setup Local Embedding Server

Create `embedding_server.py`:

```python
from sentence_transformers import SentenceTransformers
from flask import Flask, request, jsonify
import logging

app = Flask(__name__)
logging.basicConfig(level=logging.INFO)

# Load model (downloads automatically first time)
model = SentenceTransformers('sentence-transformers/all-MiniLM-L6-v2')

@app.route('/embed', methods=['POST'])
def embed():
    data = request.json
    texts = data.get('texts', [])
    
    if not texts:
        return jsonify({'error': 'No texts provided'}), 400
    
    try:
        embeddings = model.encode(texts).tolist()
        return jsonify({'embeddings': embeddings})
    except Exception as e:
        return jsonify({'error': str(e)}), 500

@app.route('/health', methods=['GET'])
def health():
    return jsonify({'status': 'healthy'})

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8000)
```

```bash
# Install dependencies
pip install sentence-transformers flask

# Run server
python embedding_server.py
```

#### Configuration for Local Embeddings

```toml
[embedder]
provider = "local"

[embedder.local]
base_url = "http://localhost:8000"
model = "sentence-transformers/all-MiniLM-L6-v2"
dimension = 384
batch_size = 32
timeout_secs = 60
```

## Alternative Free Providers

### 3. Groq (Free Tier)

Groq offers a generous free tier with fast inference.

#### Setup

1. Sign up at [console.groq.com](https://console.groq.com)
2. Get your free API key
3. Set environment variable:

```bash
export GROQ_API_KEY="your-groq-api-key"
```

#### Configuration

```toml
[llm]
provider = "groq"

[llm.groq]
base_url = "https://api.groq.com/openai/v1"
model = "llama-3.1-8b-instant"
timeout_secs = 30
max_retries = 3
rate_limit = 30  # Respect rate limits
```

### 4. Hugging Face Inference API (Free Tier)

Hugging Face offers free inference with rate limits.

#### Setup

1. Sign up at [huggingface.co](https://huggingface.co)
2. Get your free access token (optional for basic usage)
3. Set environment variable (optional):

```bash
export HUGGINGFACE_API_KEY="your-hf-token"
```

#### Configuration

```toml
[llm]
provider = "huggingface"

[llm.huggingface]
base_url = "https://api-inference.huggingface.co"
model = "microsoft/DialoGPT-medium"
timeout_secs = 60
max_retries = 3
rate_limit = 60
```

## Fallback Configuration (Recommended)

Use multiple providers with automatic fallback for maximum reliability:

```rust
use graphiti_llm::{FallbackConfig, FallbackLLMClient, FallbackEmbeddingClient, create_fallback_clients};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create fallback configuration with free providers
    let config = FallbackConfig::default(); // Uses Ollama -> Groq -> HuggingFace
    
    let (llm_client, embedding_client) = create_fallback_clients(config).await?;
    
    // Use the clients - they'll automatically try providers in order
    let response = llm_client.complete(&messages, &params).await?;
    let embeddings = embedding_client.embed_batch(&texts).await?;
    
    Ok(())
}
```

## Usage Examples

### Basic LLM Usage

```rust
use graphiti_llm::{
    MultiLLMClient, LLMConfig, LLMProvider, LLMClient,
    Message, CompletionParams
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Configure for Ollama
    let mut config = LLMConfig::default();
    config.provider = LLMProvider::Ollama;
    config.from_env(); // Load any environment overrides
    
    let client = MultiLLMClient::new(&config).await?;
    
    let messages = vec![
        Message::system("You are a helpful assistant."),
        Message::user("What is the capital of France?"),
    ];
    
    let params = CompletionParams::default();
    let response = client.complete(&messages, &params).await?;
    
    println!("Response: {}", response);
    Ok(())
}
```

### Entity Extraction

```rust
use graphiti_llm::{MultiLLMClient, LLMConfig, LLMProvider};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut config = LLMConfig::default();
    config.provider = LLMProvider::Ollama;
    config.from_env();
    
    let client = MultiLLMClient::new(&config).await?;
    
    let text = "Alice works at OpenAI in San Francisco. She collaborates with Bob on machine learning projects.";
    let result = client.extract(text, None).await?;
    
    println!("Entities: {:?}", result.entities);
    println!("Relationships: {:?}", result.relationships);
    println!("Summary: {:?}", result.summary);
    
    Ok(())
}
```

### Embeddings

```rust
use graphiti_llm::{EmbedderClient, EmbedderConfig, EmbeddingProvider, EmbeddingClient};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut config = EmbedderConfig::default();
    config.provider = EmbeddingProvider::Local;
    
    let client = EmbedderClient::new(config)?;
    
    let texts = vec![
        "This is a sample text.".to_string(),
        "Another example sentence.".to_string(),
    ];
    
    let embeddings = client.embed_batch(&texts).await?;
    println!("Generated {} embeddings with {} dimensions", 
             embeddings.len(), embeddings[0].len());
    
    Ok(())
}
```

## Environment Variables

Control configuration via environment variables:

```bash
# LLM Configuration
export LLM_PROVIDER=ollama           # ollama, groq, huggingface, openai
export LLM_MODEL=llama3.2:latest     # Model name for the provider

# API Keys (when needed)
export GROQ_API_KEY=your-groq-key
export HUGGINGFACE_API_KEY=your-hf-key
export OPENAI_API_KEY=your-openai-key

# Embedding Configuration  
export EMBEDDING_PROVIDER=local      # local, ollama, openai, voyage, cohere
export EMBEDDING_MODEL=sentence-transformers/all-MiniLM-L6-v2
```

## Docker Setup

Use Docker Compose for easy setup:

```yaml
# docker-compose.yml
version: '3.8'

services:
  ollama:
    image: ollama/ollama:latest
    ports:
      - "11434:11434"
    volumes:
      - ollama_data:/root/.ollama
    restart: unless-stopped
    
  embedding-server:
    build:
      context: .
      dockerfile: Dockerfile.embeddings
    ports:
      - "8000:8000"
    restart: unless-stopped
    
  graphiti:
    build: .
    ports:
      - "8080:8080"
    depends_on:
      - ollama
      - embedding-server
    environment:
      - LLM_PROVIDER=ollama
      - EMBEDDING_PROVIDER=local
    volumes:
      - ./config.toml:/app/config.toml

volumes:
  ollama_data:
```

Create `Dockerfile.embeddings`:

```dockerfile
FROM python:3.9-slim

RUN pip install sentence-transformers flask

COPY embedding_server.py /app/
WORKDIR /app

EXPOSE 8000
CMD ["python", "embedding_server.py"]
```

## Performance Optimization

### Ollama Performance Tips

1. **GPU Support**: Install CUDA/ROCm drivers for GPU acceleration
2. **Model Selection**: 
   - `llama3.2:1b` - Very fast, basic quality
   - `llama3.2:3b` - Good balance of speed and quality
   - `llama3.1:8b` - Higher quality, slower
3. **Memory**: Ensure sufficient RAM (8GB+ recommended)
4. **Keep-Alive**: Set `keep_alive_secs` to keep models loaded

### Local Embeddings Performance

1. **Batch Processing**: Use larger batch sizes for better throughput
2. **Model Selection**:
   - `all-MiniLM-L6-v2` - Fast, 384 dimensions
   - `all-mpnet-base-v2` - Higher quality, 768 dimensions
3. **GPU**: Use `device='cuda'` in SentenceTransformers for GPU acceleration

## Troubleshooting

### Common Issues

1. **Ollama not responding**:
   ```bash
   # Check if Ollama is running
   curl http://localhost:11434/api/tags
   
   # Restart Ollama
   ollama serve
   ```

2. **Model not found**:
   ```bash
   # List available models
   ollama list
   
   # Pull missing model
   ollama pull llama3.2:latest
   ```

3. **Local embedding server down**:
   ```bash
   # Check health
   curl http://localhost:8000/health
   
   # Restart server
   python embedding_server.py
   ```

4. **Rate limits with free APIs**:
   - Reduce `rate_limit` in config
   - Use fallback configuration
   - Add delays between requests

### Resource Requirements

| Provider | RAM | Storage | Network |
|----------|-----|---------|---------|
| Ollama (3B) | 4GB+ | 2GB | None |
| Ollama (7B) | 8GB+ | 4GB | None |
| Local Embeddings | 2GB+ | 1GB | None |
| Groq | Minimal | None | Required |
| HuggingFace | Minimal | None | Required |

## Cost Comparison

| Provider | Cost | Rate Limit | Quality | Local |
|----------|------|------------|---------|-------|
| Ollama | $0 | None | High | ✅ |
| Local Embeddings | $0 | None | High | ✅ |
| Groq | $0* | 30 RPM | High | ❌ |
| HuggingFace | $0* | Variable | Medium | ❌ |
| OpenAI | $$ | High | Highest | ❌ |

*Free tier with limitations

## Best Practices

1. **Start Local**: Begin with Ollama for development
2. **Use Fallbacks**: Configure multiple providers for production
3. **Monitor Usage**: Track API usage for free tiers
4. **Cache Results**: Enable caching to reduce redundant calls
5. **Optimize Models**: Choose the smallest model that meets your quality needs
6. **Test Locally**: Validate functionality before deploying

## Getting Help

- **Ollama Issues**: [github.com/ollama/ollama](https://github.com/ollama/ollama)
- **Groq Support**: [console.groq.com](https://console.groq.com)
- **HuggingFace**: [discuss.huggingface.co](https://discuss.huggingface.co)
- **Graphiti**: [github.com/getzep/graphiti](https://github.com/getzep/graphiti)

---

This guide enables you to run Graphiti Rust with zero ongoing costs using local models, while providing cloud-based fallbacks for when you need them. Start local, scale as needed!