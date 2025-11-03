Model Cache Evaluation — October 31, 2025
=========================================

Context
-------
- Goal: reduce hot solver “solve” time by caching `modelEval` calls in the direct Z3 backend.
- Implementation: added `ModelCache` (AST string → Word8) in `Regexle.Solver`. Each grid cell logs the AST string once, looks it up in the map, and only hits `modelEval` on a cache miss.
- Test slice: days {6, 45, 61, 73, 201, 205, 240, 279, 321, 323, 393} — the puzzles where Haskell hot solve time lagged Python by ≥20 ms.

Results
-------
- Solve times with cache vs. prior implementation (ms, hot mode, Haskell):

  | Day | Before | After cache | Δ |
  |-----|--------|-------------|----|
  | 6   | 39.9   | 40.4        | +0.5 |
  | 45  | 29.9   | 32.8        | +2.9 |
  | 61  | 30.7   | 60.2        | +29.5 (outlier rerun) |
  | 73  | 40.3   | 23.4        | -16.9 (due to earlier constraint batching) |
  | 201 | 29.7   | 35.0        | +5.3 |
  | 205 | 31.9   | 19.4        | -12.5 (constraint batching) |
  | 240 | 47.5   | 25.4        | -22.1 (constraint batching) |
  | 279 | 32.0   | 32.2        | +0.2 |
  | 321 | 58.7   | 42.9        | -15.8 (constraint batching) |
  | 323 | 43.7   | 33.0        | -10.7 |
  | 393 | 39.2   | 49.3        | +10.1 |

  (Numbers include the earlier constraint batching change; cache alone does not materially change solve time.)

- Aggregate comparison against Python enum_func λ over those 11 days:

 - Python average solve: ~8.3 ms.
 - Haskell hot solve (with cache): ~36.7 ms.
 - Gap remains ~28 ms on average; cache does not narrow it.
- Per-run timing instrumentation (added `check_seconds`/`model_seconds` in `srStats`) shows that almost the entire budget is spent in `solverCheck`:

  | Day | check_seconds | model_seconds |
  |-----|---------------|---------------|
  | 6   | 0.0402        | 0.0007        |
  | 61  | 0.0594        | 0.0007        |
  | 240 | 0.0242        | 0.0007        |
  | 321 | 0.0411        | 0.0008        |
  | 393 | 0.0476        | 0.0023        |

  Model extraction is sub-millisecond; the extra latency stems from solver search/simplification.

Analysis
--------
- Each grid variable is evaluated exactly once per puzzle, so the cache does not eliminate any work.
- The dominant runtime still comes from the solver search (including the large lambda-based transition assertions) rather than model extraction.
- For day 61 and day 393 the reruns even spiked higher, indicating solver nondeterminism rather than cache overhead.
- Timing breakdown reinforces that future optimisations must target the `solverCheck` stage (encoding size, parameter choices, lambda vs. array transitions), not model evaluation.

Next Steps
----------
- Instrument solve time to break out `solverCheck` vs. `modelEval` to pinpoint where the remaining 20–50 ms is spent.
- Explore alternate encodings (arrays/substitution) for the transition function on pathological puzzles.
- Consider reducing the size of the lambdas (e.g., split clues or precompute smaller tables) to ease Z3’s simplifier.
