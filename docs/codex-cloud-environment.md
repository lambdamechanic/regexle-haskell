## Codex Cloud Environment Setup

This repository ships with a ready-to-copy bootstrap script for the Codex Cloud web environment and a cacheable Dockerfile for local validation.

### Web Environment Script

Use the contents of `scripts/codex-cloud-setup.sh` in the Codex “Environment Setup Script” box. The script:

- installs the Ubuntu build prerequisites Cabal expects;
- installs GHC 9.12.2 and Cabal 3.16.0.0 via `ghcup`, matching our GitHub Actions workflow;
- adds the bd CLI (Beads issue tracker) to `~/.local/bin`.

Optional: prepend `scripts/codex-cloud-fastmirror.sh` if the default `archive.ubuntu.com` mirror is slow in the Codex region.

To refresh the toolchain inside an existing session run:

```bash
source "$HOME/.ghcup/env" && cabal update
```

### Local Validation

`Dockerfile.codex-cloud` layers the setup script on top of `ghcr.io/openai/codex-universal:latest`, so Docker build caches the heavy `apt` and `ghcup` work. The helper script below rebuilds the image and verifies `bd`/`cabal` versions:

```bash
bash scripts/test-codex-cloud-env.sh
```

### Files

| Path | Purpose |
|---|---|
| `scripts/codex-cloud-fastmirror.sh` | Optional mirror rewrite before `apt-get update`. |
| `scripts/codex-cloud-setup.sh` | Primary bootstrap copied into Codex. |
| `scripts/codex-cloud-maintenance.sh` | Lightweight upkeep (`cabal update`). |
| `Dockerfile.codex-cloud` | Cacheable Codex-ready base image layer. |
| `scripts/test-codex-cloud-env.sh` | Builds the Dockerfile, verifies `bd`/`cabal`, and runs `cabal test --enable-tests`. |
