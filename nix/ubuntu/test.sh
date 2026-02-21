#!/bin/bash
# Smoke-tests nix/ubuntu/run.sh using a git-cloned copy of the repo.
# Usage: ./nix/ubuntu/test.sh [extra args passed to run.sh]
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
WORK_DIR="$(mktemp -d)"
trap "rm -rf '$WORK_DIR'" EXIT

echo "==> Cloning repo to $WORK_DIR/env-rw..."
git clone "$REPO_ROOT" "$WORK_DIR/env-rw"
cd "$WORK_DIR/env-rw"

echo "==> Running nix/ubuntu/run.sh..."
bash nix/ubuntu/run.sh "$@"

echo "==> All good."
