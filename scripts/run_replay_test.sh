#!/usr/bin/env bash
set -euo pipefail
BIN="$(pwd)/dist-newstyle/build/x86_64-linux/ghc-9.12.2/regexle-haskell-0.1.0.0/x/replay-dump/build/replay-dump/replay-dump"
python3 scripts/test_dump_dir.py --binary "$BIN" "$@"
