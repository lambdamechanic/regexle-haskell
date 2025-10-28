# Stats + Benchmark Workflow

This guide captures the exact steps we use to reproduce the profiling numbers that feed `stats/combined-benchmark-table.jsonl`.

## 1. Collect per-strategy profiles

Use the built-in `profile` subcommand to sweep a contiguous day range. The example below reproduces the direct Z3 lambda baseline on days 400–409 with side 3:

```bash
cabal run regexle-haskell -- \
  profile \
  --side 3 \
  --days 400..409 \
  --strategy z3 \
  --output stats/profile-z3-lambda-400-409.json
```

Toggles to remember:

- `--strategy` accepts `lookup`, `lambda`, `enum`, `z3`, and `z3-legacy`. When comparing SBV variants, run one command per encoding.
- `--hot-solver` keeps a single solver alive across the sweep (useful for experiments, not the baseline numbers above).
- `--strategy-config` flags from the Python tool can be mirrored by extending the CLI or the JSON output if needed.

Each profile writes a columnar JSON blob with build/solve timings per puzzle plus `_meta.wall_time_seconds` for the whole sweep.

## 2. Summarize into the combined table

Every line in `stats/combined-benchmark-table.jsonl` is a hand-curated JSON object. The helper snippet below derives the numeric columns directly from a profile artifact and prints a ready-to-append row:

```bash
python3 - <<'PY' stats/profile-z3-lambda-400-409.json z3_lambda "Haskell direct Z3 (lambda)" haskell
import json, statistics, sys
from pathlib import Path

path = Path(sys.argv[1])
key = sys.argv[2]
label = sys.argv[3]
platform = sys.argv[4]

data = json.loads(path.read_text())
rows = len(data["day"])

def clean(column):
    return [v for v in column.values() if isinstance(v, (int, float))]

build = clean(data["build_time"])
solve = clean(data["solve_time"])
total = clean(data["total_time"])

payload = {
    "key": key,
    "label": label,
    "platform": platform,
    "avg_build": statistics.fmean(build),
    "avg_solve": statistics.fmean(solve),
    "avg_total": statistics.fmean(total),
    "wall": data["_meta"]["wall_time_seconds"],
    "solved": f"{len(build)}/{rows}",
    "source": str(path),
}

print(json.dumps(payload, sort_keys=True))
PY >> stats/combined-benchmark-table.jsonl
```

Run the snippet once per strategy/profile pair, adjusting the `key`, human-readable `label`, and `platform` string (usually `haskell` or `python`).

## 3. Render the comparison table

After appending new rows, pretty-print the table to sanity-check ordering and spot regressions:

```bash
python3 scripts/render_stats_table.py --sort total stats/combined-benchmark-table.jsonl
```

Flags worth flipping:

- `--sort build|solve|total|wall` to focus on a specific metric.
- `--reverse` to highlight slowest entries first.

## 4. Checklist before saving results

- Confirm every profile JSON lists the intended strategy in `strategy`/`strategy_config`.
- Verify `solved` counts read `10/10` (side 3 baseline) before trusting the averages.
- Commit both the raw profile artifacts and the updated JSONL so future agents can re-render the same table verbatim.
