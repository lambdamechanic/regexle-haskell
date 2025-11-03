#!/usr/bin/env bash
set -euxo pipefail

if command -v sudo >/dev/null 2>&1; then
  SUDO="sudo"
else
  SUDO=""
fi

$SUDO apt-get update

APT_PACKAGES=(
  build-essential
  clang
  ca-certificates
  curl
  git
  jq
  libffi-dev
  libgmp-dev
  libncurses-dev
  libtinfo5
  pkg-config
  unzip
  wget
  xz-utils
  zlib1g-dev
)

if ! apt-cache show libtinfo5 >/dev/null 2>&1; then
  APT_PACKAGES=( "${APT_PACKAGES[@]/libtinfo5/libtinfo6}" )
fi

$SUDO apt-get install -y --no-install-recommends "${APT_PACKAGES[@]}"

export BOOTSTRAP_HASKELL_NONINTERACTIVE=1
export BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1
export BOOTSTRAP_HASKELL_GHC_VERSION=9.12.2
export BOOTSTRAP_HASKELL_CABAL_VERSION=3.16.0.0
export BOOTSTRAP_HASKELL_ADJUST_BASHRC=no

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | bash

source "$HOME/.ghcup/env"

ghcup set ghc 9.12.2
ghcup set cabal 3.16.0.0

mkdir -p "$HOME/.local/bin"

for rc in "$HOME/.bashrc" "$HOME/.zshrc"; do
  [ -f "$rc" ] || touch "$rc"
  grep -qxF 'source "$HOME/.ghcup/env"' "$rc" || echo 'source "$HOME/.ghcup/env"' >> "$rc"
  grep -qxF 'export PATH="$HOME/.local/bin:$PATH"' "$rc" || echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$rc"
done

if ! python3 -m pip --version >/dev/null 2>&1; then
  python3 -m ensurepip --upgrade || $SUDO apt-get install -y --no-install-recommends python3-pip
fi

python3 -m pip --version >/dev/null 2>&1 || true

export GOBIN="$HOME/.local/bin"

curl --proto '=https' --tlsv1.2 -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash
