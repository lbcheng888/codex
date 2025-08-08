#!/usr/bin/env bash
set -euo pipefail

cargo run -p serena-cli -- --config examples/tools-minimal/serena.toml run tool echo

