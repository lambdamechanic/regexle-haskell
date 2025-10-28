#!/usr/bin/env python3
"""Interestingness test: run hot solver from a JSON param file and require SIGSEGV."""

from __future__ import annotations

import argparse
import json
import signal
import subprocess
import sys
from pathlib import Path
from typing import List


def parse_params(path: Path) -> tuple[int, int, List[int]]:
    data = json.loads(path.read_text())
    if not isinstance(data, dict):
        raise ValueError("params must be a JSON object")
    if "side" not in data or "chunk" not in data or "days" not in data:
        raise ValueError("params must include 'side', 'chunk', and 'days'")

    side = int(data["side"])
    chunk = int(data["chunk"])
    days_val = data["days"]
    if isinstance(days_val, list):
        days = sorted({int(d) for d in days_val})
    elif isinstance(days_val, int):
        days = [int(days_val)]
    elif isinstance(days_val, str):
        days = [int(x.strip()) for x in days_val.split(",") if x.strip()]
    else:
        days = []
    days = [d for d in days if d >= 0]
    return side, chunk, days


def main() -> int:
    parser = argparse.ArgumentParser(description="Run hot solver from JSON and expect SIGSEGV")
    parser.add_argument("params", type=Path)
    parser.add_argument("--timeout", type=float, default=600.0)
    args = parser.parse_args()

    try:
        side, chunk, days = parse_params(args.params)
    except (json.JSONDecodeError, ValueError):
        return 1

    if not days:
        return 1

    days_arg = ",".join(str(d) for d in days)
    cmd = [
        "cabal",
        "run",
        "regexle-haskell",
        "--",
        "repro-hot",
        "--side",
        str(side),
        "--days",
        days_arg,
        "--chunk-size",
        str(chunk),
    ]

    try:
        proc = subprocess.run(
            cmd,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            timeout=args.timeout,
        )
    except subprocess.TimeoutExpired:
        return 1

    return 0 if proc.returncode == -signal.SIGSEGV else 1


if __name__ == "__main__":
    sys.exit(main())
