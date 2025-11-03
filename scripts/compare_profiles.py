#!/usr/bin/env python3
"""Compare Haskell PyClone and Python enum_func profile JSON dumps."""
from __future__ import annotations

import argparse
import json
from dataclasses import dataclass
from pathlib import Path
from statistics import mean, pstdev
from typing import Iterable

@dataclass
class ProfileRow:
    day: int
    build: float
    solve: float
    total: float


def load_haskell(path: Path) -> list[ProfileRow]:
    data = json.loads(path.read_text())
    rows: list[ProfileRow] = []
    for idx, day in data["day"].items():
        if data["error"][idx] is not None:
            continue
        rows.append(
            ProfileRow(
                day=int(day),
                build=float(data["build_time"][idx]),
                solve=float(data["solve_time"][idx]),
                total=float(data["total_time"][idx]),
            )
        )
    return sorted(rows, key=lambda r: r.day)


def load_python(path: Path) -> list[ProfileRow]:
    data = json.loads(path.read_text())
    rows: list[ProfileRow] = []
    for idx, day in data["day"].items():
        rows.append(
            ProfileRow(
                day=int(day),
                build=float(data["build_time"][idx]),
                solve=float(data["solve_time"][idx]),
                total=float(data["build_time"][idx]) + float(data["solve_time"][idx]),
            )
        )
    return sorted(rows, key=lambda r: r.day)


def summarize(rows: Iterable[ProfileRow]) -> dict[str, float]:
    rows = list(rows)
    builds = [r.build for r in rows]
    solves = [r.solve for r in rows]
    totals = [r.total for r in rows]
    return {
        "count": len(rows),
        "build_mean_ms": 1000 * mean(builds),
        "build_std_ms": 1000 * pstdev(builds) if len(builds) > 1 else 0.0,
        "solve_mean_ms": 1000 * mean(solves),
        "solve_std_ms": 1000 * pstdev(solves) if len(solves) > 1 else 0.0,
        "total_mean_ms": 1000 * mean(totals),
        "total_std_ms": 1000 * pstdev(totals) if len(totals) > 1 else 0.0,
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("pyclone", type=Path, help="Haskell profile JSON produced by app/Main.hs")
    parser.add_argument("python", type=Path, help="Python enum_func profile JSON")
    args = parser.parse_args()

    haskell_rows = load_haskell(args.pyclone)
    python_rows = load_python(args.python)

    common_days = sorted(set(r.day for r in haskell_rows) & set(r.day for r in python_rows))
    haskell_map = {r.day: r for r in haskell_rows}
    python_map = {r.day: r for r in python_rows}
    aligned = [(haskell_map[d], python_map[d]) for d in common_days]

    haskell_summary = summarize(haskell_map[d] for d in common_days)
    python_summary = summarize(python_map[d] for d in common_days)

    print("Haskell PyClone:", haskell_summary)
    print("Python enum_func lambda:", python_summary)

    solve_deltas = [(h.solve - p.solve) * 1000 for h, p in aligned]
    total_deltas = [(h.total - p.total) * 1000 for h, p in aligned]
    worst_solve = max(zip(common_days, solve_deltas), key=lambda kv: kv[1])
    best_solve = min(zip(common_days, solve_deltas), key=lambda kv: kv[1])
    print(f"Solve delta mean: {mean(solve_deltas):.2f} ms")
    print(f"Total delta mean: {mean(total_deltas):.2f} ms")
    print(f"Worst solve delta: day {worst_solve[0]} -> {worst_solve[1]:.2f} ms")
    print(f"Best solve delta: day {best_solve[0]} -> {best_solve[1]:.2f} ms")


if __name__ == "__main__":
    main()
