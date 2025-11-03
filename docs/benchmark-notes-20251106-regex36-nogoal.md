# PyClone without Z3.Goal staging (side 3, days 400–449)

Date: November 6, 2025

## Inputs

- Post-change profile: `benchmarks/profiles/side3-days400-449-pyclone-postgoal.json`
- Pre-change profile: `benchmarks/profiles/side3-days400-449-pyclone-pregoal.json`
- Python baseline: `benchmarks/profiles/side3-days400-449-python-enum-func.json`
- Comparison helper: `scripts/compare_profiles.py`

## Summary

| Metric (ms) | PyClone (post-change) | PyClone (pre-change) | Python enum_func λ | Δ post−python | Δ post−pre |
|-------------|-----------------------|----------------------|--------------------|--------------:|-----------:|
| Build       | 21.22 ± 19.45         | 24.68 ± 20.77        | 114.04 ± 11.24     | −92.82        | −3.46      |
| Solve       | 19.35 ± 4.74          | 18.69 ± 4.32         | 7.58 ± 0.63        | +11.77        | +0.67      |
| Total       | 40.57 ± 21.26         | 43.36 ± 21.03        | 121.62 ± 11.37     | −81.04        | −2.79      |

- The removal of `Z3.Goal` staging trims **3.46 ms** from PyClone’s mean build time (≈14% faster) and **2.79 ms** from total runtime across the 50-puzzle sweep.
- Solve time regresses slightly (+0.67 ms) but stays within the existing 4.7 ms standard deviation.
- Compared to the Python `enum_func` baseline, PyClone now leads by **81 ms** on total time while still trailing by **11.8 ms** in solve time.

## Reproduce

```bash
# Post-change profile (current worktree)
cabal run regexle-haskell -- profile \
  --side 3 --days 400..449 --strategy pyclone \
  --output benchmarks/profiles/side3-days400-449-pyclone-postgoal.json

# Pre-change profile (HEAD^ via worktree)
git worktree add ../regexle-haskell-prev HEAD^
(cd ../regexle-haskell-prev && \
  git submodule update --init --recursive && \
  cabal run regexle-haskell -- profile \
    --side 3 --days 400..449 --strategy pyclone \
    --output ../regexle-haskell/benchmarks/profiles/side3-days400-449-pyclone-pregoal.json)

# Python baseline comparison
python3 scripts/compare_profiles.py \
  benchmarks/profiles/side3-days400-449-pyclone-postgoal.json \
  benchmarks/profiles/side3-days400-449-python-enum-func.json
```
