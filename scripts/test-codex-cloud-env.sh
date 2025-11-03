#!/usr/bin/env bash
set -euxo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IMAGE="${CODEX_TEST_IMAGE:-regexle-codex-env:latest}"

docker build --platform=linux/amd64 -f "${ROOT_DIR}/Dockerfile.codex-cloud" -t "$IMAGE" "$ROOT_DIR"

docker run --rm \
  --platform linux/amd64 \
  --entrypoint /bin/bash \
  "$IMAGE" \
  -lc 'bd version && cabal --version'
