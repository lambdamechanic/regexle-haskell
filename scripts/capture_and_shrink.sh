#!/usr/bin/env bash
# Capture a fresh segfaulting dump and immediately shrink it in-place using shrinkray.
# Usage: scripts/capture_and_shrink.sh [dump-root]
# Optional env vars forwarded to capture script: SIDE, DAYS, CHUNK.
# Optional env var SHRINKRAY_SEED (default 7).

set -euo pipefail

DUMP_ROOT=${1:-stats}
CAPTURE_SCRIPT=$(dirname "$0")/capture_segfault_dump.sh
SHRINK_SCRIPT=$(dirname "$0")/run_replay_test.sh
SHRINKRAY_SEED=${SHRINKRAY_SEED:-7}

if ! command -v shrinkray >/dev/null 2>&1; then
  echo "ERROR: shrinkray not found on PATH" >&2
  exit 1
fi

TMP_OUT=$(mktemp -t capture-and-shrink.XXXXXX.log)
KEEP_TRANSCRIPT=0
cleanup() {
  if [[ $KEEP_TRANSCRIPT -eq 0 ]]; then
    rm -f "$TMP_OUT"
  else
    echo "Capture transcript retained at $TMP_OUT" >&2
  fi
}
trap cleanup EXIT

# Run the capture script, teeing combined stdout/stderr for the user and parsing.
if ! "$CAPTURE_SCRIPT" "$DUMP_ROOT" 2>&1 | tee "$TMP_OUT"; then
  KEEP_TRANSCRIPT=1
  echo "Capture step failed. Transcript saved to $TMP_OUT" >&2
  exit 1
fi

DUMP_DIR=$(python - <<'PY' "$TMP_OUT"
import re
import sys
from pathlib import Path

path = Path(sys.argv[1])
dump_dir = ""
for line in path.read_text(errors="ignore").splitlines():
    match = re.search(r"dump saved under\s+(\S+)", line, re.IGNORECASE)
    if match:
        dump_dir = match.group(1)
print(dump_dir, end="")
PY
)

if [[ -z "$DUMP_DIR" || ! -d "$DUMP_DIR" ]]; then
  KEEP_TRANSCRIPT=1
  echo "ERROR: could not determine dump directory from capture output" >&2
  echo "----- capture transcript begin -----" >&2
  cat "$TMP_OUT" >&2 || true
  echo "----- capture transcript end -----" >&2
  exit 1
fi

WORK_DIR="${DUMP_DIR}-shrink"
rm -rf "$WORK_DIR"
cp -R "$DUMP_DIR" "$WORK_DIR"

echo "[3/3] Shrinking dump in $WORK_DIR"
shrinkray --ui=basic --formatter=none --volume=quiet \
  --in-place --input-type=arg --parallelism=1 --timeout=5 --seed "$SHRINKRAY_SEED" \
  "$SHRINK_SCRIPT" "$WORK_DIR"

echo "Shrink complete. Reduced corpus lives at $WORK_DIR"
