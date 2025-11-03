SolverCheck Auto Encoding Evaluation — October 31, 2025
=======================================================

Context
-------
- Baseline direct Z3 hot solver uses lambda-encoded transition tables for every DFA.
- Legacy substitution encoding was faster on a few small DFAs but significantly slower on the rest.
- New heuristic (`Z3TransitionAuto`) switches to substitution when a clue DFA has ≤4 states and ≤6 alphabet entries; otherwise it keeps the lambda array encoding.
- Benchmark slice: days {6, 45, 61, 73, 201, 205, 240, 279, 321, 323, 393} on side 3 (same set as the model-cache study).
- Command: `cabal run regexle-haskell -- profile --side 3 --days 6,45,61,73,201,205,240,279,321,323,393 --strategy z3 --output stats/profile-z3-auto-20251031.json`

Results
-------
| Day | Lambda check ms | Auto check ms | Auto − lambda Δ ms |
|-----|-----------------|---------------|--------------------|
| 6   | 51.60 | 50.93 | -0.67 |
| 45  | 38.28 | 37.07 | -1.21 |
| 61  | 29.77 | 28.62 | -1.15 |
| 73  | 38.67 | 38.59 | -0.08 |
| 201 | 23.30 | 23.13 | -0.17 |
| 205 | 21.79 | 33.59 | +11.80 |
| 240 | 29.38 | 28.18 | -1.20 |
| 279 | 44.59 | 51.42 | +6.83 |
| 321 | 45.47 | 42.20 | -3.26 |
| 323 | 27.29 | 24.27 | -3.02 |
| 393 | 23.66 | 27.45 | +3.79 |

- Dropping the large membership disjunctions shrank lambda’s average `solverCheck` to 33.98 ms (from 47.56 ms previously) and auto to 35.04 ms.
- The relaxed constraints also cut auxiliary variables to ~1.6 k `:mk-bool-var` per puzzle (down from ~2.2 k) but Python’s enum solver still uses only ~67 on the 400–409 slice, so a 25× gap remains.
- Auto now only flips to substitution when `states <= 4 && allowed <= 8`; it helps a few short clues but hurts days 205/279/393, so lambda remains the default.

Notes
-----
- Ordering clues by ascending allowed alphabet, then state count, then decreasing span length still reduces search across the slice (down to ~34 ms) even without the substitution path.
- Day 205 and 279 regress under substitution; leaving the default on lambda avoids the hit while keeping the leaner constraint set.
- Model extraction remains sub-millisecond in all runs.

Artifacts
---------
- `stats/profile-z3-lambda-20251031c.json`
- `stats/profile-z3-auto-20251031e.json`
- `stats/profile-z3-legacy-20251031.json`
