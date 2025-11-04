#!/usr/bin/env bash
set -euo pipefail

if [ "${ALLOW_DIRTY_PUSH:-}" = "1" ]; then
  exit 0
fi

if ! git status --porcelain --untracked-files=no >/dev/null; then
  echo "fatal: not a git repository" >&2
  exit 1
fi

if [ -n "$(git status --porcelain)" ]; then
  cat >&2 <<'EOF'
Cannot run cached build/push while the working tree is dirty.
Please commit, stash, or drop local changes (or set ALLOW_DIRTY_PUSH=1)
before invoking 'make push' or other Attic cache commands.
EOF
  exit 1
fi
