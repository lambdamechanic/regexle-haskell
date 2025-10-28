#!/usr/bin/env python3
import argparse
import os
import signal
import subprocess
import sys
import tarfile
import tempfile
from pathlib import Path


def main() -> int:
    parser = argparse.ArgumentParser(description="Replay tarred dump via replay-dump binary")
    parser.add_argument("--binary", required=True, help="Path to the replay-dump executable")
    parser.add_argument("--timeout", type=float, default=30.0, help="Seconds before killing the run")
    args = parser.parse_args()

    data = sys.stdin.buffer.read()
    if not data:
        return 1

    with tempfile.TemporaryDirectory() as tmpdir:
        tmp_path = Path(tmpdir)
        tar_path = tmp_path / "input.tar"
        tar_path.write_bytes(data)
        extract_dir = tmp_path / "dump"
        extract_dir.mkdir()
        try:
            with tarfile.open(tar_path) as tf:
                tf.extractall(extract_dir)
        except tarfile.TarError:
            return 1

        try:
            proc = subprocess.run(
                [args.binary, "--dump-dir", str(next(extract_dir.iterdir()))],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                timeout=args.timeout,
            )
        except (subprocess.TimeoutExpired, StopIteration):
            return 1

        if proc.returncode == -signal.SIGSEGV:
            sys.stderr.write(proc.stderr.decode(errors="ignore"))
            return 0
        return 1


if __name__ == "__main__":
    sys.exit(main())
