#!/usr/bin/env bash
set -euo pipefail

export SERENA_CONFIG=examples/lsp-minimal/serena-lsp.json

cargo run -p serena-lsp -- --config "$SERENA_CONFIG"

