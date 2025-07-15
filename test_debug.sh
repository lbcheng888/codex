#!/bin/bash

echo "Testing Grok Function Calling Debug System"
echo "=========================================="

# Test 1: Simple debug message test
echo -e "\n1. Testing debug message output:"
CODEX_DEBUG=1 echo "Debug test message" 2>&1 | head -1

# Test 2: Test TUI with debug enabled and capture stderr
echo -e "\n2. Testing codex-tui debug output:"
echo "Will run: CODEX_DEBUG=1 codex-tui --profile xai --model grok-4-0709"
echo "Please try entering: '执行ls命令' and watch for debug output"
echo "Check the following for logs:"
echo "- Terminal stderr output"
echo "- ~/.codex/debug.log file"

echo -e "\nPress Enter to start TUI test..."
read -r

# Clear previous logs
> ~/.codex/debug.log 2>/dev/null || true

# Run TUI with debug
CODEX_DEBUG=1 codex-tui --profile xai --model grok-4-0709 2>&1 | tee grok_debug.log