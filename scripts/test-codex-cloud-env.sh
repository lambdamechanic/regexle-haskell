#!/usr/bin/env bash
set -euxo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IMAGE="${CODEX_TEST_IMAGE:-regexle-codex-env:latest}"

docker build --platform=linux/amd64 -f "${ROOT_DIR}/Dockerfile.codex-cloud" -t "$IMAGE" "$ROOT_DIR"

if [ ! -f "${ROOT_DIR}/z3-415.3/z3.cabal" ]; then
  echo "error: expected submodule z3-415.3 to be present. Run 'git submodule update --init --recursive' first." >&2
  exit 1
fi

docker run --rm \
  --platform linux/amd64 \
  -v "${ROOT_DIR}:/workspace" \
  -w /workspace \
  --entrypoint /bin/bash \
  "$IMAGE" \
  -lc 'cabal test --enable-tests'
