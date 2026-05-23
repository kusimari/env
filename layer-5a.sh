#!/usr/bin/env bash
# env/layer-5a.sh — Layer 5a (public) of the five-layer bootstrap.
#
# Iterates two registries and ensures three roots:
#   ~/tool-workplace/   workspaces (env-tooling under active churn)
#   ~/dabba/            stores (cross-machine, backed-up content)
#   ~/project-workplace mkdir-only; humans populate machine-specific work
#
# For each workspace entry: clone/fetch the repo into
# ~/tool-workplace/<name>/<repo-basename>/, pin git identity, and
# hand off to the repo's own `install` entry-point.
#
# For each store entry: clone/fetch flat into
# ~/dabba/<repo-basename>/, pin git identity, hand off to install.
#
# This script does not know what any given workspace or store does —
# the content repo owns its own install. L5a only clones and hands off.
#
# Adding a workspace: add a row to the WORKSPACES heredoc.
# Adding a store: add a row to the STORES heredoc.
#
# Options:
#   --dry-run        Log planned actions; make no changes.
#   --skip-install   Clone/fetch only; do not invoke entry-points.
#                    Useful when the entry-point doesn't exist yet
#                    (e.g., a new workspace still being built out).
#   --help, -h       Show this header and exit.
# END-USAGE

set -euo pipefail

# ── Registries ──────────────────────────────────────────────────────
# Format: <name>\t<repo-url>\t<entry-point>
# - name:        directory under the relevant root (workspace name for
#                WORKSPACES; ignored except as a log label for STORES,
#                which clone flat by repo basename).
# - repo-url:    any URL git-clone accepts.
# - entry-point: path (relative to the clone) of an executable
#                the driver runs after clone/fetch.
# Lines starting with # are comments. Blank lines are ignored.
WORKSPACES="$(cat <<'TSV'
# ai-workspace: hosts mAId (and private siblings, if present).
ai-workspace	git@github.com:kusimari/mAId.git	install
TSV
)"

# No public stores today. Add rows here when a public, backed-up
# store gets a repo.
STORES="$(cat <<'TSV'
TSV
)"

# ── Constants ───────────────────────────────────────────────────────
TOOL_WORKPLACE_ROOT="$HOME/tool-workplace"
DABBA_ROOT="$HOME/dabba"
PROJECT_WORKPLACE_ROOT="$HOME/project-workplace"
PUBLIC_USER_NAME="kusimari"
PUBLIC_USER_EMAIL="kusimari@gmail.com"

# ── Defaults ────────────────────────────────────────────────────────
DRY_RUN=0
SKIP_INSTALL=0
FAILURES=()

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
        --skip-install)  SKIP_INSTALL=1; shift ;;
        --help|-h)       usage; exit 0 ;;
        *) die "Unknown argument: $1 (use --help)" ;;
    esac
done

# ── Git helpers (kept inline to avoid a cross-script library) ───────

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

# ── Flow ────────────────────────────────────────────────────────────

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

run_entry_point() {
    local clone_dir="$1" entry="$2" name="$3"
    local entry_path="$clone_dir/$entry"
    if (( SKIP_INSTALL )); then
        log "$name: --skip-install set, not invoking $entry"
        return 0
    fi
    if [[ ! -e "$entry_path" ]]; then
        warn "$name: entry point not found: $entry_path (skipping)"
        return 0
    fi
    if [[ ! -x "$entry_path" ]]; then
        warn "$name: entry point not executable: $entry_path (skipping)"
        return 0
    fi
    log "$name: invoking $entry"
    if (( DRY_RUN )); then
        printf 'dry-run: (cd %s && ./%s)\n' "$clone_dir" "$entry"
    else
        ( cd "$clone_dir" && "./$entry" )
    fi
}

process_workspace() {
    local name="$1" url="$2" entry="$3"
    local ws_dir="$TOOL_WORKPLACE_ROOT/$name"
    local clone_base
    clone_base="$(repo_basename "$url")"
    local clone_dir="$ws_dir/$clone_base"

    log "Workspace: $name (repo: $clone_base)"
    if [[ ! -d "$ws_dir" ]]; then
        log "Creating workspace dir: $ws_dir"
        run mkdir -p "$ws_dir"
    fi

    clone_or_fetch "$name/$clone_base" "$url" "$clone_dir"
    ensure_git_identity "$clone_dir" "$PUBLIC_USER_NAME" "$PUBLIC_USER_EMAIL"
    run_entry_point "$clone_dir" "$entry" "$name/$clone_base"
}

process_store() {
    local name="$1" url="$2" entry="$3"
    local clone_base
    clone_base="$(repo_basename "$url")"
    local clone_dir="$DABBA_ROOT/$clone_base"

    log "Store: $name (repo: $clone_base)"
    clone_or_fetch "store/$clone_base" "$url" "$clone_dir"
    ensure_git_identity "$clone_dir" "$PUBLIC_USER_NAME" "$PUBLIC_USER_EMAIL"
    run_entry_point "$clone_dir" "$entry" "store/$clone_base"
}

# Iterate a registry safely. $1 is human-readable kind ("workspace" /
# "store"), $2 is the registry body, $3 is the per-row processor name.
iterate_registry() {
    local kind="$1" registry="$2" processor="$3"
    while IFS=$'\t' read -r name url entry; do
        [[ -z "$name" || "$name" =~ ^[[:space:]]*# ]] && continue
        if [[ -z "$url" || -z "$entry" ]]; then
            warn "Malformed $kind row: '$name\t$url\t$entry' (skipping)"
            FAILURES+=("$name")
            continue
        fi
        if ! ( "$processor" "$name" "$url" "$entry" ); then
            warn "$name: failed (continuing)"
            FAILURES+=("$name")
        fi
    done <<<"$registry"
}

log "Layer 5a (public): roots and registries$( (( DRY_RUN )) && echo ' (dry-run)')"

# Ensure the three roots exist before iterating. project-workplace is
# mkdir-only by design — no registry, no clones.
for root in "$TOOL_WORKPLACE_ROOT" "$DABBA_ROOT" "$PROJECT_WORKPLACE_ROOT"; do
    if [[ ! -d "$root" ]]; then
        log "Creating root: $root"
        run mkdir -p "$root"
    fi
done

iterate_registry workspace "$WORKSPACES" process_workspace
iterate_registry store     "$STORES"     process_store

if (( ${#FAILURES[@]} > 0 )); then
    warn "Layer 5a finished with failures: ${FAILURES[*]}"
    exit 1
fi

log "Layer 5a done."
