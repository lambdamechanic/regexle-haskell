SBV Regexle Solver Outline
==========================

Objectives
----------
- Re-implement the regexle solver in Haskell using the SBV library for SMT solving.
- Leverage the `kleene` package for regex→DFA compilation, providing minimized DFAs and dead-state insights.
- Preserve Nelson Elhage's performance optimizations: explicit DFA transition encoding, dead-state pruning, and benchmark comparability.

DFA Preparation Layer
---------------------
1. **Regex parsing & compilation**
   - Use `kleene`'s `compile` pipeline to turn puzzle regex strings into derivative-based DFAs.
   - Maintain a shared alphabet `[A..Z]`, mapping to `Word8` indices for SBV integration.
2. **Transition extraction**
   - Expose transition function `δ : state × alphabet → state` via `kleene`'s deterministic automaton record.
   - Generate dense transition tables (array-of-arrays) for fast SBV encoding.
3. **State minimization & dead-state analysis**
   - Rely on `kleene`'s minimized DFA output; compute `dfaBlackholes` to identify dead states.
   - Derive per-state dead vocab and initial-state exclusions mirroring Nelson's pruning.
4. **Caching**
   - Mirror Python cache strategy (`~/.cache/regexle`) by serializing DFA tables (e.g., CBOR/JSON) to avoid recompilation during benchmarking runs.

SBV Encoding Strategy
---------------------
1. **Symbolic grid**
   - Represent each board cell as `SWord8` constrained to `[0, 25]`.
   - Provide mapping helpers `indexToChar`/`charToIndex` for round-tripping.
2. **Row/column clues**
   - For each clue, allocate symbolic state variables `[s_0 … s_n]` using `SWord8`/enumeration sorts.
   - Assert `s_0 == startState` and `s_{i+1} == δ(s_i, cell_i)`.
   - Enforce acceptance by constraining `s_n` to lie within the DFA's accepting set.
3. **Dead-state pruning**
   - For each clue, forbid dead states in the `s_i` sequence.
   - Constrain first-letter domain using initial dead-vocab; apply per-step dead-vocab exclusions.
4. **Transition encoding**
   - Prefer explicit nested `ite` or table lookup implemented with SBV's `select`/`foldl` to avoid uninterpreted functions.
   - Optionally evaluate both `ite` ladder and `SArray` encodings to compare performance (mirroring Nelson's strategies).
5. **Solver configuration**
   - Support thread count, macro-finder, and logging parity with Python tool to enable fair benchmarking.

Benchmark Integration
---------------------
1. **Puzzle ingestion**
   - Reuse shared cache (via task `regex-7`) so both Python and Haskell pipelines operate on identical inputs.
2. **Strategy harness**
   - Recreate `profile`/`matrix` commands in Haskell (task `regex-9`) to sweep puzzle sets and strategy variants.
   - Record per-run build/solve times and solver statistics to JSON (CBOR) for downstream comparison.
3. **Cross-language comparisons**
   - Define command-line interface enabling toggles between SBV encodings (ite vs array) and potential alternative Haskell FSM libraries (HaLeX fallback).

Open Questions / Follow-up Tasks
--------------------------------
- `regex-7`: Port HTTP fetch + caching pipeline into shared Haskell module.
- `regex-8`: Implement DFA-based pruning logic using `kleene` outputs.
- `regex-9`: Build benchmarking harness comparable to Python's profile/matrix commands.
- Evaluate performance of SBV encodings (ite ladder vs `SArray`); create benchmarks if gap discovered.

