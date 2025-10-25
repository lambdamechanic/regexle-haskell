Direct Z3 Backend Plan
======================

Motivation
----------
- SBV’s SMT-LIB emission dominates build time (~28% `cvt`, ~20% `gfoldl`). Even with a persistent solver, we still need to rebuild the entire symbolic representation for each puzzle.
- Nelson’s Python solver leans heavily on custom DFA encodings and direct Z3 APIs (via `z3-solver`), which keeps per-puzzle overhead low (0.17s total over days 400–410).
- A bespoke Z3 backend would let us:
  * Keep a solver context warm across puzzles.
  * Use native arrays / algebraic datatypes for DFA transitions instead of huge `ite` ladders.
  * Avoid SMT-LIB pretty-printing entirely by calling Z3’s C API (via Haskell FFI).

High-Level Tasks
----------------
1. **Z3 binding layer**
   - For the first cut, depend on the Hackage `z3` package (bindings that ship with GHCJS/Stack) so we can move quickly without writing FFI. If we run into missing API or perf issues, fall back to our own shim.
   - Wrap the minimal subset of the API we need (context creation, AST building, arrays, solver push/pop, model extraction, statistics).
2. **DFA encoding**
   - Reuse `Regexle.DFA` outputs (transition tables, dead states) to build Z3 arrays/functions once per clue.
   - Investigate representing transitions as either:
     * `Array (state × alphabet) → state`, updated via `store`.
     * Immutable functions built with `Z3_mk_lambda` (if available).
3. **Grid construction**
   - Allocate per-cell AST nodes (e.g., `Z3_mk_const` of sort `BitVec 5`) and keep them alive across puzzles if side dimensions match.
   - Apply each clue by iterating over coordinates and adding transition constraints using the prebuilt DFA arrays.
4. **Solver lifecycle**
   - Keep a single solver `Z3_solver` and use `Z3_solver_reset` or push/pop to clear between puzzles.
   - Cache per-clue AST structures so reloading a puzzle only means asserting new per-clue equalities.
5. **Integration**
   - Provide a Haskell API mirroring `solvePuzzle`, so the rest of the code (CLI, JSON output) stays the same.
   - Add configuration flag to pick `sbv` vs `z3-direct`.

Open Questions
--------------
- How much live-state sharing can we get? (e.g., can we reuse the entire grid AST across puzzles, only swapping clue constraints?)
- Model extraction: we’ll need to walk the Z3 model to read each cell; ensure we don’t regress on correctness.
- Packaging: do we vendor Z3 bindings or depend on a maintained Haskell package?

Next Steps
----------
- Prototype a tiny Z3 binding (context + solver + BitVec constants) and solve a single puzzle to prove out the approach.
- Document performance vs. SBV on the same slice (days 400–410) to justify the migration.
- If the `z3` package isn’t already a dependency, add it to `regexle-haskell.cabal` and ensure the system Z3 shared library is available in CI/dev shells.
