# Z3 Hot Solver Crash Repro (October 27)

## Minimal failing command

The smallest range that still crashes the enum-based hot backend is ten puzzles:

```bash
cabal run regexle-haskell -- \
  repro-hot \
  --side 3 \
  --days 0..9 \
  --chunk-size 0
```

The process segfaults inside `Z3_solver_get_model` after the tenth puzzle when chunking is disabled. Keeping `--chunk-size 5` (the production default) continues to avoid the crash.

## Capturing SMT dumps

Pass `--dump-dir <dir>` to mirror the incremental run and emit replayable SMT-LIB files. Example:

```bash
cabal run regexle-haskell -- \
  repro-hot \
  --side 3 \
  --days 0..49 \
  --chunk-size 0 \
  --dump-dir stats/crash-live-0-49
```

Each directory now contains:

- `base.smt2` with the enum datatype declarations plus all shared grid variables.
- `puzzle-XXXX.smt2` files, each wrapping a `(push) … (check-sat) (get-model)` block along with per-cell `(get-value …)` calls mirroring the solver’s `modelEval` sequence.
- `driver.smt2`, which includes the base file followed by each puzzle include.

Replaying the dump is as simple as:

```bash
cd stats/crash-live-0-49
z3 driver.smt2   # prints one model + get-value trace per puzzle
```

Running the driver does **not** crash Z3; it returns models for every included puzzle. This suggests that the upstream failure needs the host program’s API usage (repeated `solverCheckAndGetModel` + thousands of `modelEval` calls) rather than the raw SMT text alone.

## Next steps

1. Build a minimal harness that replays the dumped constraints through the Z3 C API (or `z3-solver` Python bindings) so we can file a standalone reproducer upstream.
2. Capture a deterministic stack trace (`gdb --args cabal run …`) for the `0..9` command and confirm the failure is still rooted in `Z3_solver_get_model`.

## Replay harness & shrinkray recipe (Oct 27)

- New executable `replay-dump` (`cabal build replay-dump`) replays a dump directory using the same `Z3.Monad` backend as the solver. It expects the original `base.smt2`, `driver.smt2`, and `puzzle-*.smt2` files and reproduces the libz3 assertion in-process.
- Interestingness test (`scripts/test_dump_dir.py`, wrapped by `scripts/run_replay_test.sh`) ensures the driver references at least one non-empty puzzle include so shrinkray keeps a valid-ish corpus.
- Copy the dump you want to minimize (since shrinkray mutates in place), then run the latest shrinkray (installed from HEAD) directly on the directory:

```bash
cp -R stats/crash-live-0-49 stats/crash-shrink-work
shrinkray --ui=basic --formatter=none --volume=quiet \
  --in-place --input-type=arg --parallelism=1 --timeout=5 --seed=5 \
  scripts/run_replay_test.sh stats/crash-shrink-work
```

- Result: `stats/crash-replay-min/` now contains a three-file reproducer (≈400 KB total):

  - `base.smt2` declares a single-state datatype.
  - `driver.smt2` includes `base.smt2` and `puzzle-0004.smt2`.
  - `puzzle-0004.smt2` holds the surviving `(push)/(assert)/(check-sat)` sequence that still crashes libz3.

Repro command: `dist-newstyle/.../replay-dump --dump-dir stats/crash-replay-min` still reports the `ast.cpp:414` assertion.
