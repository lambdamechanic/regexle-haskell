# PyClone vs Python enum_func λ (side 3, days 400–449)

Date: November 3, 2025

## Inputs

- Haskell profile: `benchmarks/profiles/side3-days400-449-pyclone.json`
- Python profile: `benchmarks/profiles/side3-days400-449-python-enum-func.json`
- Comparison helper: `scripts/compare_profiles.py`

## Summary

| Metric (ms) | PyClone mean ± σ | Python mean ± σ | Δ (PyClone − Python) |
|-------------|------------------|-----------------|-----------------------|
| Build       | 13.08 ± 4.28     | 114.04 ± 11.24  | −100.96               |
| Solve       | 16.21 ± 4.19     | 7.58 ± 0.63     | +8.63                 |
| Total       | 29.28 ± 5.47     | 121.62 ± 11.37  | −92.33                |

- Worst remaining solve delta: day 402 at +20.49 ms (PyClone solve 27.83 ms, Python 7.33 ms).
- PyClone consistently eliminates redundant `:added-eqs` during the build phase; remaining spikes stem from transition encoding gaps tracked under regex-34/35/36/37.

## Reproduce

```bash
# Re-run Haskell profile
cabal run regexle-haskell -- profile \
  --side 3 --days 400..449 --strategy pyclone \
  --output benchmarks/profiles/side3-days400-449-pyclone.json

# Re-run Python enum_func λ profile
PYTHONPATH=third_party/nelhage-sandbox/regexle/src \
python -m regexle.main profile \
  --side 3 --days 400..449,449 --strategy enum_func \
  --out benchmarks/profiles/side3-days400-449-python-enum-func.json

# Compare summaries
scripts/compare_profiles.py \
  benchmarks/profiles/side3-days400-449-pyclone.json \
  benchmarks/profiles/side3-days400-449-python-enum-func.json
```

## Next Steps

1. regex-34: replace `distinct` bans with `mkEq`/`mkNot` to match Python inequality shape.
2. regex-35: rebuild transition lambda as an ITE ladder per column class.
3. regex-36: stop round-tripping constraints through `Z3.Goal`.
4. regex-37: ensure DFA minimisation matches `greenery.reduce()` results.

## PyClone transition lambda guard vs baseline (side 3, days 400–401)

- Baseline commit `a7ba800` (unguarded fallback) averaged ~75 ms to build and ~20 ms to solve each clue.
- Guarded commit `62e1fd5` keeps the fallback branch conditional but adds about 5 ms to the build and 1–2 ms to the solve per clue, nudging total runtime up by roughly 7 %.
- The guard prevents PyClone from asserting an unconditional transition, eliminating unsound states at the expense of a small runtime regression.
- Profiles are captured in `benchmarks/profiles/side3-days400-401-pyclone-transition-comparison.json` with the exact command recorded in the artifact metadata.
