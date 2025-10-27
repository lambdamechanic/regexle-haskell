# Z3 Hot Solver Crash Repro

We can trigger the direct Z3 hot backend to segfault consistently by running the new `repro-hot` command against days 30–35 (side 3) without chunking:

```bash
cabal run regexle-haskell -- \
  repro-hot \
  --side 3 \
  --days 30..35 \
  --chunk-size 0
```

This calls `solvePuzzlesZ3DirectHot` once for six puzzles, which reliably ends with `SIGSEGV`/`SIGABRT` on our Linux host. Limiting the chunk size to `5` avoids the crash and matches the production workaround:

```bash
cabal run regexle-haskell -- repro-hot --side 3 --days 30..35 --chunk-size 5
```

The helper uses the same puzzle fetch/cache path as the profiler, so it automatically pulls the necessary puzzle JSONs into `~/.cache/regexle/`. Use `gdb --args` (or `rr`) around the same command to capture a stack trace for `regex-23`.
