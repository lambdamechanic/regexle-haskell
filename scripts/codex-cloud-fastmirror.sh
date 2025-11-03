#!/usr/bin/env bash
set -euxo pipefail

if command -v curl >/dev/null 2>&1; then
  mirrors=$(curl --max-time 10 -fsSL https://mirrors.ubuntu.com/mirrors.txt || true)
else
  echo "curl is required to select a mirror" >&2
  exit 1
fi

if [ -z "${mirrors}" ]; then
  echo "Unable to fetch Ubuntu mirror list; keeping default sources" >&2
  exit 0
fi

preferred=$(printf '%s\n' "${mirrors}" | grep '^https://' | head -n1)
if [ -z "${preferred}" ]; then
  preferred=$(printf '%s\n' "${mirrors}" | head -n1)
fi

if [ -z "${preferred}" ]; then
  echo "No mirror entries found; keeping default sources" >&2
  exit 0
fi

preferred="${preferred%/}"

if command -v sudo >/dev/null 2>&1; then
  SUDO="sudo"
else
  SUDO=""
fi

$SUDO sed -i "s|http://archive.ubuntu.com/ubuntu|${preferred}|g" /etc/apt/sources.list
$SUDO sed -i "s|https://archive.ubuntu.com/ubuntu|${preferred}|g" /etc/apt/sources.list
