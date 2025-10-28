#!/usr/bin/env bash
# Capture a fresh hot-solver dump and immediately rerun the crashing command so
# we know the same day-range still segfaults (exit code 139).

set -euo pipefail

SIDE=${SIDE:-3}
DAYS=${DAYS:-0..409}
CHUNK=${CHUNK:-0}
DUMP_ROOT=${1:-stats}
STAMP=$(date +%Y%m%d-%H%M%S)
DUMP_DIR="$DUMP_ROOT/crash-$SIDE-$DAYS-$STAMP"

run() {
  echo "➤ $*"
  "$@"
}

mkdir -p "$DUMP_ROOT"
rm -rf "$DUMP_DIR"

echo "[1/2] Dumping constraints to $DUMP_DIR"
if ! run cabal run regexle-haskell -- \
  repro-hot \
  --side "$SIDE" \
  --days "$DAYS" \
  --chunk-size "$CHUNK" \
  --dump-dir "$DUMP_DIR" \
  --dump-only; then
  echo "Dump failed; aborting." >&2
  rm -rf "$DUMP_DIR"
  exit 1
fi

echo "[2/2] Running hot solver without dumping (expecting segfault)"
set +e
cabal run regexle-haskell -- \
  repro-hot \
  --side "$SIDE" \
  --days "$DAYS" \
  --chunk-size "$CHUNK"
status=$?
set -e
if [[ $status -ne 139 ]]; then
  echo "Run exited with $status (expected 139); discarding dump." >&2
  rm -rf "$DUMP_DIR"
  exit $status
fi

echo "Segfault reproduced (exit 139). Dump saved under $DUMP_DIR"
