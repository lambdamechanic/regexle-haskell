#!/usr/bin/env bash
set -euo pipefail

Z3_LIB_DIR="$(pwd)/z3-415.3/vendor/z3-4.15.3/bin"
if [ -d "$Z3_LIB_DIR" ]; then
  if [ -n "${LD_LIBRARY_PATH:-}" ]; then
    export LD_LIBRARY_PATH="${Z3_LIB_DIR}:${LD_LIBRARY_PATH}"
  else
    export LD_LIBRARY_PATH="${Z3_LIB_DIR}"
  fi
fi

BIN="$(pwd)/dist-newstyle/build/x86_64-linux/ghc-9.12.2/regexle-haskell-0.1.0.0/x/replay-dump/build/replay-dump/replay-dump"
TIMEOUT=${SHRINKRAY_TEST_TIMEOUT:-10}
python3 scripts/test_dump_dir.py --binary "$BIN" --timeout "$TIMEOUT" "$@"
