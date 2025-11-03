Hot Solver via SBV Query Mode
=============================

> **Status (November 3, 2025):** The SBV backend has been removed from the main build because it remained an order of magnitude slower than our direct Z3 encoders even after extensive tuning. This document is retained for historical context only.

Motivation
----------
- Profiling (`regexle-haskell.prof`, Oct 25 2025) shows ~28% of CPU time in `Data.SBV.SMT.SMTLib2.cvt` and ~20% in the `Data.Data.gfoldl` traversals SBV uses while constructing constraints. The actual Z3 solve time is only ~0.09s per puzzle.
- Keeping a single Z3 session alive across puzzles would let us avoid reinitializing the solver and potentially reuse expensive symbolic definitions, narrowing the gap with Nelson’s Python solver (0.17s total per puzzle over days 400–410).

Current Status
--------------
- Branch `feature/hot-solver-query-mode` now contains a query-mode implementation that allocates grids/states with `freshVar` and rebuilds constraints inside the solver loop. Main keeps the pre-query baseline for easier bisects.
- 10-day sweep (side 3, days 400–409) shows that the hot solver is still slower despite staying inside one Z3 session:
  - Hot: avg build 1.30 s, avg solve 0.50 s, wall 19.5 s (`cabal run … --hot-solver --output /tmp/profile-hot-400-409.json`).
  - Cold: avg build 1.13 s, avg solve 0.084 s, wall 13.1 s (no `--hot-solver`).
- The slowdown appears to come from rebuilding every constraint inside the query and from `resetAssertions` leaving all declarations in place, so the solver context only grows. We still avoid SBV's SMT-LIB emission cost, but the retained declarations/arrays outweigh that win.
- Given the limited upside, we are pausing SBV hot-solver work here and pivoting to regex-12 (direct Z3). If we revive this path, we likely need push/pop discipline plus partial clue reuse.

What’s Required
---------------
1. **Rewrite grid allocation using query primitives.**
   - Replace `mkGrid`/`mkStateVars` to allocate each cell/state via `freshVar` instead of `sWord8`/`sWord16`.
   - Re-implement `applyClue` so it works over those query-created SVs.
   - Ensure dead-state pruning, transitions, and acceptance constraints can be asserted incrementally (i.e., via `addConstraint` or by building SBV expressions directly).
2. **Manage solver reset between puzzles.**
   - After each puzzle, call `resetAssertions` (already available) or use push/pop to roll back the grid constraints.
   - Reuse per-clue data (DFA tables, dead vocab) without re-running the heavy `gfoldl` traversals if possible.
3. **Profiling checkpoint after rewrite.**
   - Re-run the 10-day profile to verify that the build phase shrinks and that the solver session is truly persistent.

Open Questions
--------------
- Can we share the same symbolic grid structure across puzzles (i.e., treat puzzle clues as data parameters) to avoid rebuilding anything more than necessary?
- Does SBV expose enough hooks to define the grid once and only swap the puzzle-specific regex data (e.g., via `namedLambda`)? If not, we may need deeper changes to SBV itself.
