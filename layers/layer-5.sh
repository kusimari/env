#!/usr/bin/env bash
# env/layers/layer-5.sh — Layer 5 (public) of the seven-layer
# bootstrap. Stores, get-only.
#
# Ensures two roots and clones/fetches one inline block per known
# store:
#   ~/dabba/            stores (cross-machine, backed-up content)
#   ~/workplace         mkdir-only; humans populate machine-specific work
#
# Each store block clones/fetches flat into ~/dabba/<repo-basename>/
# and pins identity. Stores are knowledge-persistence repos (notes
# vaults, etc.) — they never have a build step, so L5 is their entire
# lifecycle.
#
# Tool workspaces (env-tooling under active churn, e.g. mAId) are NOT
# an L5 concern — Layer 6 (layers/layer-6.sh) owns both cloning and
# building them under ~/tool-workplace/. L5 never touches that root.
#
# Adding a store: copy an existing { ... } block and edit the
# name/url. Each block is wrapped with `|| { warn ...; FAILED=1; }`
# so one bad entry does not abort the rest of the run; the script exits
# non-zero at the end if any block failed.
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

log()  { printf '==> %s\n' "$*"; }
warn() { printf '!!! %s\n' "$*" >&2; }
die()  { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

run() {
    if (( DRY_RUN )); then
        printf 'dry-run: %s\n' "$*"
    else
        "$@"
    fi
}

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

# ── Helpers (kept inline to avoid a cross-script library) ───────────

# Clone $url into $dest, or fetch if already present.
clone_or_fetch() {
    local name="$1" url="$2" dest="$3"
    if [[ -d "$dest/.git" ]]; then
        log "$name: fetching updates"
        run git -C "$dest" fetch --quiet origin
    elif [[ -e "$dest" ]]; then
        die "$dest exists and is not a git checkout"
    else
        log "$name: cloning $url -> $dest"
        run git clone --quiet "$url" "$dest"
    fi
}

# Pin a local commit identity on $dest. Idempotent; diff-checks
# before writing.
ensure_git_identity() {
    local dest="$1" want_name="$2" want_email="$3"
    [[ -d "$dest/.git" ]] || return 0

    local cur_name cur_email
    cur_name="$(git -C "$dest" config --local user.name 2>/dev/null || true)"
    cur_email="$(git -C "$dest" config --local user.email 2>/dev/null || true)"
    if [[ "$cur_name" = "$want_name" && "$cur_email" = "$want_email" ]]; then
        log "git identity pinned: $dest"
        return
    fi
    log "Pinning git identity on $dest: $want_name <$want_email>"
    run git -C "$dest" config --local user.name  "$want_name"
    run git -C "$dest" config --local user.email "$want_email"
}

# Extract the clone basename from a git URL:
#   git@github.com:kusimari/mAId.git          → mAId
#   ssh://host/pkg/Foo.git                    → Foo
#   https://github.com/kusimari/env           → env
repo_basename() {
    local url="$1" base
    base="${url##*/}"
    base="${base%.git}"
    printf '%s' "$base"
}

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
