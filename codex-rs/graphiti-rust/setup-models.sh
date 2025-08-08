#!/bin/bash

# Setup script for Ollama models required by Graphiti MCP Server

echo "Setting up Ollama models for Graphiti..."
echo "========================================"

# Check if Ollama is running
if ! curl -s http://localhost:11434/api/tags > /dev/null 2>&1; then
    echo "Error: Ollama is not running. Please start Ollama first."
    echo "Run: ollama serve"
    exit 1
fi

# Install embedding model
echo "1. Installing nomic-embed-text for embeddings..."
ollama pull nomic-embed-text

# Install LLM models
echo -e "\n2. Installing llama3.1:8b for entity extraction..."
ollama pull llama3.1:8b

# Optional: Install better model for structured output
echo -e "\n3. (Optional) Installing qwen2.5:7b for better structured output..."
echo "This model provides better JSON generation. Install? (y/N)"
read -r response
if [[ "$response" =~ ^[Yy]$ ]]; then
    ollama pull qwen2.5:7b
fi

echo -e "\nModel setup complete!"
echo "========================================"

# List installed models
echo -e "\nInstalled models:"
ollama list

echo -e "\nRecommendations:"
echo "- For basic usage: Use config.free.toml with llama3.1:8b"
echo "- For better results: Use config.qwen.toml with qwen2.5:7b"
echo ""
echo "Start the server with:"
echo "  ./target/release/graphiti-mcp-server -c config.free.toml -p 8091"
echo "or"
echo "  ./target/release/graphiti-mcp-server -c config.qwen.toml -p 8091"