#!/bin/bash
# Graphiti Rust MCP Server Docker Setup Script

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

log_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

log_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

log_error() {
    echo -e "${RED}❌ $1${NC}"
}

# Check if Docker is installed and running
check_docker() {
    log_info "Checking Docker installation..."
    
    if ! command -v docker &> /dev/null; then
        log_error "Docker is not installed. Please install Docker first."
        exit 1
    fi
    
    if ! docker info &> /dev/null; then
        log_error "Docker is not running. Please start Docker first."
        exit 1
    fi
    
    log_success "Docker is installed and running"
}

# Check if Docker Compose is available
check_docker_compose() {
    log_info "Checking Docker Compose..."
    
    if docker compose version &> /dev/null; then
        COMPOSE_CMD="docker compose"
    elif docker-compose --version &> /dev/null; then
        COMPOSE_CMD="docker-compose"
    else
        log_error "Docker Compose is not available. Please install Docker Compose."
        exit 1
    fi
    
    log_success "Docker Compose is available: $COMPOSE_CMD"
}

# Setup environment file
setup_env() {
    log_info "Setting up environment configuration..."
    
    if [ ! -f .env ]; then
        if [ -f .env.docker ]; then
            cp .env.docker .env
            log_success "Created .env from .env.docker template"
        elif [ -f .env.example ]; then
            cp .env.example .env
            log_success "Created .env from .env.example template"
        else
            log_warning "No environment template found. You may need to create .env manually."
        fi
    else
        log_info ".env file already exists"
    fi
}

# Build Docker images
build_images() {
    log_info "Building Docker images..."
    $COMPOSE_CMD build
    log_success "Docker images built successfully"
}

# Setup based on user choice
setup_deployment() {
    echo ""
    echo "🚀 Choose your deployment option:"
    echo "1) Full setup (Neo4j + Ollama + MCP Server) - Recommended"
    echo "2) Free setup (CozoDB + Ollama + MCP Server) - No external dependencies"
    echo "3) Development setup (with hot reload)"
    echo "4) Full setup with monitoring (includes Prometheus + Grafana)"
    echo ""
    read -p "Enter your choice (1-4): " choice
    
    case $choice in
        1)
            log_info "Starting full setup..."
            $COMPOSE_CMD up -d
            show_full_endpoints
            ;;
        2)
            log_info "Starting free setup..."
            $COMPOSE_CMD --profile free up -d
            show_free_endpoints
            ;;
        3)
            log_info "Starting development setup..."
            $COMPOSE_CMD -f docker-compose.yml -f docker-compose.dev.yml up -d
            show_dev_endpoints
            ;;
        4)
            log_info "Starting full setup with monitoring..."
            $COMPOSE_CMD --profile monitoring up -d
            show_monitoring_endpoints
            ;;
        *)
            log_error "Invalid choice. Please run the script again."
            exit 1
            ;;
    esac
}

# Show endpoints for full setup
show_full_endpoints() {
    log_success "Full setup completed! Available endpoints:"
    echo ""
    echo "🔗 MCP Server: http://localhost:8080"
    echo "🔗 Neo4j Browser: http://localhost:7474 (neo4j/password)"
    echo "🔗 Ollama API: http://localhost:11434"
    echo ""
    echo "📋 Health checks:"
    echo "   curl http://localhost:8080/health"
    echo "   curl http://localhost:11434/api/tags"
}

show_free_endpoints() {
    log_success "Free setup completed! Available endpoints:"
    echo ""
    echo "🔗 MCP Server: http://localhost:8091"
    echo "🔗 Ollama API: http://localhost:11434"
    echo ""
    echo "📋 Health checks:"
    echo "   curl http://localhost:8091/health"
    echo "   curl http://localhost:11434/api/tags"
}

show_dev_endpoints() {
    log_success "Development setup completed! Available endpoints:"
    echo ""
    echo "🔗 MCP Server: http://localhost:8080 (with hot reload)"
    echo "🔗 Neo4j Browser: http://localhost:7474 (neo4j/password)"
    echo "🔗 Ollama API: http://localhost:11434"
    echo ""
    echo "🔧 Development features:"
    echo "   - Hot reload enabled"
    echo "   - Debug logging"
    echo "   - Source code mounted"
}

show_monitoring_endpoints() {
    log_success "Full setup with monitoring completed! Available endpoints:"
    echo ""
    echo "🔗 MCP Server: http://localhost:8080"
    echo "🔗 Neo4j Browser: http://localhost:7474 (neo4j/password)"
    echo "🔗 Ollama API: http://localhost:11434"
    echo "🔗 Prometheus: http://localhost:9090"
    echo "🔗 Grafana: http://localhost:3000 (admin/admin)"
}

# Setup Ollama models
setup_ollama() {
    log_info "Setting up Ollama models..."
    
    # Wait for Ollama to be ready
    log_info "Waiting for Ollama service to be ready..."
    timeout=60
    while [ $timeout -gt 0 ]; do
        if $COMPOSE_CMD exec ollama curl -f http://localhost:11434/api/tags >/dev/null 2>&1; then
            break
        fi
        sleep 2
        timeout=$((timeout - 2))
    done
    
    if [ $timeout -le 0 ]; then
        log_warning "Ollama service not ready. You may need to pull models manually later."
        return
    fi
    
    log_info "Pulling required models..."
    $COMPOSE_CMD exec ollama ollama pull llama3.1:8b
    $COMPOSE_CMD exec ollama ollama pull nomic-embed-text
    
    log_success "Ollama models ready"
}

# Show usage instructions
show_usage() {
    echo ""
    log_info "Next steps:"
    echo ""
    echo "📖 View logs:"
    echo "   $COMPOSE_CMD logs -f graphiti"
    echo ""
    echo "🔍 Check service status:"
    echo "   $COMPOSE_CMD ps"
    echo ""
    echo "🛑 Stop services:"
    echo "   $COMPOSE_CMD down"
    echo ""
    echo "📚 For more information, see docker/README.md"
}

# Main execution
main() {
    echo "🚀 Graphiti Rust MCP Server Docker Setup"
    echo "========================================"
    
    check_docker
    check_docker_compose
    setup_env
    build_images
    setup_deployment
    
    # Setup Ollama models for non-development setups
    if [[ $choice != "3" ]]; then
        setup_ollama
    fi
    
    show_usage
    
    log_success "Setup completed! 🎉"
}

# Run main function
main "$@"
