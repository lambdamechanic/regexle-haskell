#!/usr/bin/env python3
"""Interestingness test for shrinkray: run replay-dump on a directory."""

from __future__ import annotations

import argparse
import re
import signal
import subprocess
import sys
from pathlib import Path


def run(binary: Path, dump_dir: Path, timeout: float) -> int:
    if not binary.exists():
        print(f"Binary {binary} not found", file=sys.stderr)
        return 2
    if not dump_dir.exists():
        return 1
    driver = dump_dir / "driver.smt2"
    base = dump_dir / "base.smt2"
    puzzles = [p for p in dump_dir.glob("puzzle-*.smt2") if p.is_file()]
    if (
        not driver.exists()
        or not base.exists()
        or driver.stat().st_size == 0
        or base.stat().st_size == 0
        or not any(p.stat().st_size > 0 for p in puzzles)
    ):
        return 1
    try:
        driver_text = driver.read_text()
        base_text = base.read_text()
        puzzle_texts = [p.read_text() for p in puzzles]
    except UnicodeDecodeError:
        return 1
    includes = re.findall(r"\(include \"([^\"]+)\"\)", driver_text)
    if (
        "base.smt2" not in includes
        or not any(name.startswith("puzzle-") for name in includes)
        or driver_text.count("smt2\")") < 2
        or "(declare-datatypes" not in base_text
    ):
        return 1
    if not any((dump_dir / name).exists() for name in includes if name.startswith("puzzle-")):
        return 1
    if not any("(assert" in txt and "(check-sat" in txt for txt in puzzle_texts):
        return 1
    try:
        proc = subprocess.run(
            [str(binary), "--dump-dir", str(dump_dir)],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired:
        return 1

    stderr_text = proc.stderr.decode(errors="ignore")
    if proc.returncode == -signal.SIGSEGV:
        sys.stderr.write(stderr_text)
        return 0
    if proc.returncode != 0 and "assertion violation" in stderr_text.lower():
        sys.stderr.write(stderr_text)
        return 0
    return 1


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("dump_dir")
    parser.add_argument("--binary", required=True)
    parser.add_argument("--timeout", type=float, default=10.0)
    args = parser.parse_args()
    return run(Path(args.binary), Path(args.dump_dir), args.timeout)


if __name__ == "__main__":  # pragma: no cover
    sys.exit(main())
