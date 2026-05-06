#!/usr/bin/env bash
# env/build-nix/bootstrap-ubuntu-mane.sh — Layer 1 for the
# ubuntu-mane envKind.
#
# Generic prep: apt prereqs for Nix + the Determinate Nix installer
# (multi-user). Exits when done. No chaining; the user runs the
# next layer (bootstrap-common.sh) separately.
#
# Curl-friendly:
#   curl -fsSL <raw-url>/build-nix/bootstrap-ubuntu-mane.sh | bash
#
# --dry-run is honored: logs what would happen, mutates nothing.
#
# No site-specific auth (NFR5). Ubuntu-native packages + Determinate
# installer only.

set -euo pipefail

log() { printf '==> %s\n' "$*"; }
die() { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

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

require_ubuntu() {
    [[ -r /etc/os-release ]] || die "Cannot read /etc/os-release"
    # shellcheck disable=SC1091
    . /etc/os-release
    [[ "${ID:-}" = "ubuntu" ]] || die "Not Ubuntu (ID='${ID:-unknown}')"
}

ensure_apt_prereqs() {
    local needed=(build-essential curl git xz-utils ca-certificates openssh-client)
    local missing=()
    for pkg in "${needed[@]}"; do
        dpkg -s "$pkg" >/dev/null 2>&1 || missing+=("$pkg")
    done
    if (( ${#missing[@]} == 0 )); then
        log "apt prereqs present"
        return
    fi
    log "Installing apt prereqs: ${missing[*]}"
    run sudo apt-get update -qq
    run sudo apt-get install -y --no-install-recommends "${missing[@]}"
}

ensure_nix() {
    if command -v nix >/dev/null 2>&1; then
        log "nix already installed: $(command -v nix)"
        return
    fi
    log "Installing Nix via Determinate installer (multi-user default)"
    if (( DRY_RUN )); then
        printf 'dry-run: curl … | sh -s -- install --no-confirm\n'
        return
    fi
    curl --proto '=https' --tlsv1.2 -sSf -L \
        https://install.determinate.systems/nix \
        | sh -s -- install --no-confirm
    # shellcheck disable=SC1091
    [[ -f /etc/profile.d/nix.sh ]] && . /etc/profile.d/nix.sh || true
}

log "Layer 1 (ubuntu-mane): machine prep$( (( DRY_RUN )) && echo ' (dry-run)')"
require_ubuntu
ensure_apt_prereqs
ensure_nix
log "Layer 1 done. Next: run Layer 2 (bootstrap-common.sh)."
