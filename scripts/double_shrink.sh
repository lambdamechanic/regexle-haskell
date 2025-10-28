#!/usr/bin/env bash
# Shrink the parameter set first (JSON), then capture & shrink the resulting dump.
# Usage: scripts/double_shrink.sh [param-root] [dump-root]

set -euo pipefail

set -x

PARAM_ROOT=${1:-stats}
DUMP_ROOT=${2:-stats}
PARAM_TEMPLATE="$PARAM_ROOT/hot-params.json"
WORK_PARAMS="$PARAM_ROOT/hot-params.work.json"
MIN_PARAMS="$PARAM_ROOT/hot-params-min.json"
DEFAULT_HIGH_DAY=${DEFAULT_HIGH_DAY:-49}
SHRINKRAY_SEED=${SHRINKRAY_SEED:-7}
SHRINKRAY_TIMEOUT=${SHRINKRAY_TIMEOUT:-30}
DEFAULT_SEEDS="7 13 23 37 53"

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
TEST_PARAMS_SCRIPT="$SCRIPT_DIR/test_hot_params.py"
CAPTURE_AND_SHRINK="$SCRIPT_DIR/capture_and_shrink.sh"

if ! command -v shrinkray >/dev/null 2>&1; then
  echo "ERROR: shrinkray not available on PATH" >&2
  exit 1
fi

mkdir -p "$PARAM_ROOT"

if [[ ! -f "$PARAM_TEMPLATE" ]]; then
  python - <<'PY' "$PARAM_TEMPLATE"
import json, sys
from pathlib import Path
path = Path(sys.argv[1])
path.parent.mkdir(parents=True, exist_ok=True)
data = {"side": 3, "chunk": 0, "days": list(range(410))}
path.write_text(json.dumps(data, indent=2))
PY
fi

if ! python "$TEST_PARAMS_SCRIPT" "$PARAM_TEMPLATE" >/dev/null; then
  echo "Initial parameter set does not produce a segfault; aborting." >&2
  exit 1
fi

SEED_LIST=${SHRINKRAY_SEEDS:-$DEFAULT_SEEDS}
SUCCESS=0
for seed in $SEED_LIST; do
  cp "$PARAM_TEMPLATE" "$WORK_PARAMS"

  shrinkray --formatter=none --volume=quiet --in-place \
    --timeout="$SHRINKRAY_TIMEOUT" --parallelism=1 --seed "$seed" \
    "$TEST_PARAMS_SCRIPT" "$WORK_PARAMS" || continue

  cp "$WORK_PARAMS" "$MIN_PARAMS"

  if python "$TEST_PARAMS_SCRIPT" "$MIN_PARAMS" >/dev/null; then
    SUCCESS=1
    SHRUNK_SEED=$seed
    break
  fi
done

if [[ $SUCCESS -ne 1 ]]; then
  echo "Unable to find a shrinking that still segfaults; try adjusting seeds." >&2
  exit 1
fi

echo "Parameter shrink succeeded with seed $SHRUNK_SEED"

readarray -t PARAM_VALUES < <(python - <<'PY' "$MIN_PARAMS"
import json, sys
from pathlib import Path
path = Path(sys.argv[1])
data = json.loads(path.read_text())
side = int(data.get("side", 3))
chunk = int(data.get("chunk", 0))
days = data.get("days", [])
if isinstance(days, int):
    days_list = [days]
elif isinstance(days, list):
    days_list = sorted({int(d) for d in days})
elif isinstance(days, str):
    days_list = [int(x.strip()) for x in days.split(",") if x.strip()]
else:
    days_list = []
if not days_list:
    print("", file=sys.stderr)
    sys.exit(1)
print(side)
print(chunk)
print(",".join(str(d) for d in days_list))
PY) || { echo "Failed to parse shrunk params" >&2; exit 1; }

SIDE=${PARAM_VALUES[0]}
CHUNK=${PARAM_VALUES[1]}
DAYS=${PARAM_VALUES[2]}

if [[ -z "$DAYS" ]]; then
  echo "Shrunk parameter set removed all days; aborting." >&2
  exit 1
fi

SIDE="$SIDE" CHUNK="$CHUNK" DAYS="$DAYS" "$CAPTURE_AND_SHRINK" "$DUMP_ROOT"
