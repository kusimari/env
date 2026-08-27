#!/usr/bin/env bash
# env/layers/layer-5.sh — Layer 5 (public): get stores.
#
# Ensures ~/dabba/ and ~/workplace/, then clones/fetches one inline
# { ... } block per known store into ~/dabba/<repo-basename>/. See
# project.md for the layer design.
#
# Adding a store: copy an existing { ... } block and edit the
# name/url.
#
# Options:
#   --dry-run        Log planned actions; make no changes.
#   --help, -h       Show this header and exit.
# END-USAGE

set -uo pipefail

# ── Constants ───────────────────────────────────────────────────────
DABBA_ROOT="$HOME/dabba"
WORKPLACE_ROOT="$HOME/workplace"
PUBLIC_USER_NAME="kusimari"
PUBLIC_USER_EMAIL="kusimari@gmail.com"

# ── Defaults ────────────────────────────────────────────────────────
DRY_RUN=0
FAILED=0

source "$(dirname "${BASH_SOURCE[0]}")/layer-5-6-common.sh"

usage() {
    awk '/^# END-USAGE$/{exit} NR>1 && /^#/{sub(/^# ?/,""); print}' \
        "${BASH_SOURCE[0]}"
}

# ── Arg parsing ─────────────────────────────────────────────────────
while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run)       DRY_RUN=1; shift ;;
        --help|-h)       usage; exit 0 ;;
        *) die "Unknown argument: $1 (use --help)" ;;
    esac
done

# ── Flow ────────────────────────────────────────────────────────────

log "Layer 5 (public): stores$( (( DRY_RUN )) && echo ' (dry-run)')"

# Ensure the two roots exist before iterating. workplace is
# mkdir-only by design — no registry, no clones.
for root in "$DABBA_ROOT" "$WORKPLACE_ROOT"; do
    if [[ ! -d "$root" ]]; then
        log "Creating root: $root"
        run mkdir -p "$root"
    fi
done

# Store: kusimari-dabba — personal notes vault (plain Markdown).
# Clones flat into ~/dabba/<repo> and pins the public identity. Comes
# up on every machine (this is the public L5). Get-only: cloned/fetched
# here; there is nothing to build.
(
    name="kusimari-dabba"
    url="git@github.com:kusimari/kusimari-dabba.git"
    clone_base="$(repo_basename "$url")"
    clone_dir="$DABBA_ROOT/$clone_base"

    log "Store: $name (repo: $clone_base)"
    clone_or_fetch "store/$clone_base" "$url" "$clone_dir"
    ensure_git_identity "$clone_dir" "$PUBLIC_USER_NAME" "$PUBLIC_USER_EMAIL"
) || { warn "store/kusimari-dabba: failed (continuing)"; FAILED=1; }

# Add another store the same way: copy the block above, edit name/url.
# Cloud file storage (OneDrive, Google Drive, …) is NOT a store block —
# it is wired manually under ~/dabba/ by the operator (see
# setup-manual-notes.md); L5 does nothing for it.

if (( FAILED )); then
    warn "Layer 5 finished with failures."
    exit 1
fi

log "Layer 5 done."
