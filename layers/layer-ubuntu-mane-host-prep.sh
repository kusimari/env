#!/usr/bin/env bash
# env/layers/layer-ubuntu-mane-host-prep.sh — Host OS hardware and driver prep.
#
# Shared sub-script for ubuntu-mane:
#   - Invoked during Layer 1 (layer-1-ubuntu-mane.sh) for Day-1 Genesis
#   - Invoked at the head of Layer 3 (layer-3-ubuntu-mane.sh) for Day-2 Convergence
#
# Ensures host hardware drivers (HPLIP) and backend libraries (python3-gi-cairo)
# required by the host printing and scanning subsystem are present.
# Gracefully skips apt operations if all required packages are already installed,
# ensuring zero sudo prompts on warm runs.
#
# Honors --dry-run.

set -euo pipefail

log() { printf '==> %s\n' "$*"; }

DRY_RUN=0
for arg in "$@"; do
    [[ "$arg" = "--dry-run" ]] && DRY_RUN=1
done

run() {
    if (( DRY_RUN )); then
        printf 'dry-run: %s\n' "$*"
    else
        "$@"
    fi
}

# Host packages required for hardware drivers, print spool, and scanning backend
HOST_PACKAGES=(
    python3-gi-cairo
    hplip
    hplip-gui
)

log "Checking host OS hardware & driver packages$( (( DRY_RUN )) && echo ' (dry-run)')"

missing=()
for pkg in "${HOST_PACKAGES[@]}"; do
    if ! dpkg -s "$pkg" >/dev/null 2>&1; then
        missing+=("$pkg")
    fi
done

if (( ${#missing[@]} > 0 )); then
    log "Missing ${#missing[@]} host package(s): ${missing[*]}"
    echo "Approve sudo when prompted to install host hardware prerequisites..."
    run sudo apt-get update -qq
    run sudo apt-get install -y --no-install-recommends "${missing[@]}"
else
    log "Host hardware packages already satisfied: ${HOST_PACKAGES[*]}"
fi
