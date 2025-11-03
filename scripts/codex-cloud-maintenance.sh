#!/usr/bin/env bash
set -euxo pipefail

if [ -f "$HOME/.ghcup/env" ]; then
  # shellcheck disable=SC1090
  source "$HOME/.ghcup/env"
fi

cabal update
