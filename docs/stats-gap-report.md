# Solver Performance Snapshot — October 27, 2025

This note summarizes the most recent benchmark sweep (side 3, days 400–409) as recorded in `stats/combined-benchmark-table.jsonl`. All numbers are averages across the 10 puzzles enumerated by the profiling harness.

| Key            | Strategy Label                     | Platform | Build (s) | Solve (s) | Total (s) | Wall (s) | Source File                                   |
|----------------|------------------------------------|----------|-----------|-----------|-----------|----------|-----------------------------------------------|
| z3_enum_hot    | Haskell direct Z3 enum hot         | haskell  | 0.0062    | 0.0063    | 0.0125    | 0.1759   | stats/profile-z3-enum-hot-400-409.json        |
| z3_enum        | Haskell direct Z3 enum             | haskell  | 0.0178    | 0.0117    | 0.0296    | 0.2968   | stats/profile-z3-enum-400-409.json            |
| z3_lambda      | Haskell direct Z3 (lambda)         | haskell  | 0.0185    | 0.0148    | 0.0333    | 0.3332   | stats/profile-z3-lambda-4153-400-409.json     |
| z3_lambda_hot  | Haskell direct Z3 hot (lambda)     | haskell  | 0.0061    | 0.0169    | 0.0230    | 0.2875   | stats/profile-z3-lambda-hot-400-409.json      |
| z3_legacy      | Haskell direct Z3 (legacy)         | haskell  | 0.0268    | 0.0293    | 0.0561    | 0.5637   | stats/profile-z3-legacy-400-409.json          |
| py_enum_lambda | Python enum_func λ-transition      | python   | 0.1084    | 0.0075    | 0.1160    | 1.1599   | stats/profile-python-enum-func-lambda-400-409.json |
| py_int_lambda  | Python int_func λ-transition       | python   | 0.1386    | 0.0238    | 0.1624    | 1.6238   | stats/profile-python-int-func-lambda-400-409.json  |
| sbv_lookup     | Haskell SBV lookup                 | haskell  | 1.1216    | 0.0839    | 1.2054    | 13.0002  | stats/profile-sbv-lookup-400-409.json         |
| sbv_lambda     | Haskell SBV lambda                 | haskell  | 1.1483    | 0.0869    | 1.2353    | 13.3780  | stats/profile-sbv-lambda-400-409.json         |

## Key findings

- **Solve-stage parity achieved**: The new enum-based hot backend averages **6.3 ms solve / 6.2 ms build** per puzzle, edging out Python `enum_func` lambda on both axes while keeping total time to ~12.5 ms. (See `z3_enum_hot` vs `py_enum_lambda` above.)
- **Cold enum baseline**: Even without solver reuse, the enum encoding closes most of the remaining gap (11.7 ms solve) while staying under 18 ms build, so we can fall back to `z3_enum` if the hot solver is disabled.
- **Legacy lambda fallback**: The substitute-style direct backend (`z3_legacy`) is still ~1.5× slower than the tuple/enum encodings, so it remains a debugging-only path.
- **SBV baseline**: SBV variants continue to spend ≈1.2 s per puzzle on build, reaffirming that the direct Z3 backend is the only viable option for competitive runs.
- **Version parity**: All measurements above are with Z3 4.15.3 on both Python and Haskell, so we can attribute differences to encoding/implementation rather than solver vintage.

## Next measurement targets

1. **Stabilize hot solver** (regex-23): Hot mode still crashes after ~25 push/pop cycles; capturing a minimal SMT repro is the top priority.
2. **Repeatable profiling workflow** (regex-17): Capture the profile/matrix command invocations plus the `scripts/render_stats_table.py` helper so future sweeps are identical and auditable.

The numbers above will serve as the baseline for the next round of optimization experiments.
