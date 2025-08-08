#!/bin/bash
# Health check script for Graphiti Docker deployment

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

# Check if a service is responding
check_service() {
    local name=$1
    local url=$2
    local expected_status=${3:-200}
    
    log_info "Checking $name at $url..."
    
    if response=$(curl -s -o /dev/null -w "%{http_code}" "$url" 2>/dev/null); then
        if [ "$response" = "$expected_status" ]; then
            log_success "$name is healthy (HTTP $response)"
            return 0
        else
            log_warning "$name returned HTTP $response (expected $expected_status)"
            return 1
        fi
    else
        log_error "$name is not responding"
        return 1
    fi
}

# Check Docker services
check_docker_services() {
    log_info "Checking Docker services status..."
    
    if command -v docker-compose &> /dev/null; then
        COMPOSE_CMD="docker-compose"
    elif docker compose version &> /dev/null; then
        COMPOSE_CMD="docker compose"
    else
        log_error "Docker Compose not found"
        return 1
    fi
    
    echo ""
    echo "📋 Docker Services Status:"
    $COMPOSE_CMD ps
    echo ""
}

# Main health check
main() {
    echo "🏥 Graphiti Docker Health Check"
    echo "==============================="
    
    check_docker_services
    
    # Check main services
    log_info "Checking service endpoints..."
    echo ""
    
    # Check Graphiti MCP Server (full setup)
    if check_service "Graphiti MCP Server" "http://localhost:8080/health"; then
        GRAPHITI_FULL=true
    else
        GRAPHITI_FULL=false
    fi
    
    # Check Graphiti MCP Server (free setup)
    if check_service "Graphiti MCP Server (Free)" "http://localhost:8091/health"; then
        GRAPHITI_FREE=true
    else
        GRAPHITI_FREE=false
    fi
    
    # Check Ollama
    if check_service "Ollama API" "http://localhost:11434/api/tags"; then
        OLLAMA=true
    else
        OLLAMA=false
    fi
    
    # CozoDB is embedded in the Graphiti server, no separate check needed
    COZO=true  # Always true if Graphiti is running
    
    # Check Prometheus (if running)
    if check_service "Prometheus" "http://localhost:9090/-/healthy"; then
        PROMETHEUS=true
    else
        PROMETHEUS=false
    fi
    
    # Check Grafana (if running)
    if check_service "Grafana" "http://localhost:3000/api/health"; then
        GRAFANA=true
    else
        GRAFANA=false
    fi
    
    echo ""
    echo "📊 Health Check Summary:"
    echo "========================"
    
    if [ "$GRAPHITI_FULL" = true ]; then
        log_success "Graphiti MCP Server (Full): http://localhost:8080"
    fi
    
    if [ "$GRAPHITI_FREE" = true ]; then
        log_success "Graphiti MCP Server (Free): http://localhost:8091"
    fi
    
    if [ "$OLLAMA" = true ]; then
        log_success "Ollama API: http://localhost:11434"
    fi
    
    if [ "$COZO" = true ]; then
        log_success "CozoDB: Embedded in MCP Server"
    fi
    
    if [ "$PROMETHEUS" = true ]; then
        log_success "Prometheus: http://localhost:9090"
    fi
    
    if [ "$GRAFANA" = true ]; then
        log_success "Grafana: http://localhost:3000"
    fi
    
    echo ""
    
    # Overall status
    if [ "$GRAPHITI_FULL" = true ] || [ "$GRAPHITI_FREE" = true ]; then
        log_success "At least one Graphiti MCP Server is running! 🎉"
        
        echo ""
        echo "🧪 Test the API:"
        if [ "$GRAPHITI_FULL" = true ]; then
            echo "   curl http://localhost:8080/health"
            echo "   curl -X POST http://localhost:8080/episodes -H 'Content-Type: application/json' -d '{\"content\":\"Test episode\"}'"
        fi
        if [ "$GRAPHITI_FREE" = true ]; then
            echo "   curl http://localhost:8091/health"
            echo "   curl -X POST http://localhost:8091/episodes -H 'Content-Type: application/json' -d '{\"content\":\"Test episode\"}'"
        fi
        
        echo ""
        echo "📚 For more information:"
        echo "   docker-compose logs -f graphiti"
        echo "   See docker/README.md for detailed documentation"
        
        exit 0
    else
        log_error "No Graphiti MCP Server is running!"
        echo ""
        echo "🔧 Troubleshooting:"
        echo "   docker-compose ps"
        echo "   docker-compose logs graphiti"
        echo "   ./scripts/setup-docker.sh"
        
        exit 1
    fi
}

# Run main function
main "$@"
