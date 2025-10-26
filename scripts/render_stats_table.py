#!/usr/bin/env python3
"""Render stats/combined-benchmark-table.jsonl as a readable table."""

import argparse
import json
from pathlib import Path
from typing import Any

HEADERS = [
    "Key",
    "Label",
    "Platform",
    "Build (s)",
    "Solve (s)",
    "Total (s)",
    "Wall (s)",
    "Solved",
    "Source",
]

FLOAT_FIELDS = {
    "Build (s)": "avg_build",
    "Solve (s)": "avg_solve",
    "Total (s)": "avg_total",
    "Wall (s)": "wall",
}

STRING_FIELDS = {
    "Key": "key",
    "Label": "label",
    "Platform": "platform",
    "Solved": "solved",
    "Source": "source",
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Pretty-print the combined benchmark table JSONL file.")
    parser.add_argument(
        "path",
        nargs="?",
        default=Path("stats/combined-benchmark-table.jsonl"),
        type=Path,
        help="Path to the combined benchmark JSONL file (default: stats/combined-benchmark-table.jsonl)",
    )
    parser.add_argument(
        "--sort",
        choices=["build", "solve", "total", "wall"],
        default="total",
        help="Sort rows by the specified metric (default: total)",
    )
    parser.add_argument(
        "--reverse",
        action="store_true",
        help="Reverse the sort order",
    )
    return parser.parse_args()


def load_rows(path: Path) -> list[dict[str, Any]]:
    rows: list[dict[str, Any]] = []
    for line in path.read_text().splitlines():
        line = line.strip()
        if not line:
            continue
        rows.append(json.loads(line))
    return rows


def build_table(rows: list[dict[str, Any]]) -> list[list[str]]:
    table: list[list[str]] = []
    for row in rows:
        formatted: list[str] = []
        for header in HEADERS:
            if header in FLOAT_FIELDS:
                key = FLOAT_FIELDS[header]
                value = row.get(key)
                formatted.append(f"{value:.6f}" if isinstance(value, (int, float)) else "-")
            else:
                key = STRING_FIELDS[header]
                value = row.get(key, "")
                formatted.append(str(value))
        table.append(formatted)
    return table


def compute_widths(rows: list[list[str]]) -> list[int]:
    widths = [len(header) for header in HEADERS]
    for row in rows:
        for idx, cell in enumerate(row):
            widths[idx] = max(widths[idx], len(cell))
    return widths


def render_table(rows: list[list[str]]) -> str:
    widths = compute_widths(rows)
    lines: list[str] = []
    header_line = " | ".join(header.ljust(widths[idx]) for idx, header in enumerate(HEADERS))
    separator = "-+-".join("-" * width for width in widths)
    lines.append(header_line)
    lines.append(separator)
    for row in rows:
        lines.append(" | ".join(cell.ljust(widths[idx]) for idx, cell in enumerate(row)))
    return "\n".join(lines)


def sort_rows(rows: list[dict[str, Any]], key: str, reverse: bool) -> list[dict[str, Any]]:
    field_map = {
        "build": "avg_build",
        "solve": "avg_solve",
        "total": "avg_total",
        "wall": "wall",
    }
    sort_key = field_map[key]

    def extractor(entry: dict[str, Any]) -> float:
        value = entry.get(sort_key)
        if isinstance(value, (int, float)):
            return float(value)
        return float("inf")

    return sorted(rows, key=extractor, reverse=reverse)


def main() -> None:
    args = parse_args()
    entries = load_rows(args.path)
    sorted_entries = sort_rows(entries, args.sort, args.reverse)
    table = build_table(sorted_entries)
    print(render_table(table))


if __name__ == "__main__":
    main()
