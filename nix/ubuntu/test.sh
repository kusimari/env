#!/bin/bash
# Smoke-tests nix/ubuntu/run.sh inside a bare Ubuntu 24.04 Docker container.
# Usage: ./nix/ubuntu/test.sh [extra args passed to run.sh]
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"

echo "==> Running ubuntu/run.sh in a fresh Ubuntu 24.04 container..."
docker run --rm \
  --privileged \
  -v "$REPO_ROOT:/env:ro" \
  ubuntu:24.04 \
  bash -c '
    set -euo pipefail

    apt-get update -qq
    apt-get install -y curl xz-utils git

    # Install Nix (Determinate Systems - works in containers via --init none)
    curl --proto "=https" --tlsv1.2 -sSf -L https://install.determinate.systems/nix \
      | sh -s -- install linux --no-confirm --init none --extra-conf "sandbox = false"

    source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

    # Clone repo so all files are git-tracked (required for Nix flake path resolution)
    git clone /env /env-rw
    cd /env-rw

    bash nix/ubuntu/run.sh '"$*"'
  '

echo "==> All good."
