#!/usr/bin/env bash
# env/layers/layer-5a.sh — Layer 5a (public) of the seven-layer
# bootstrap. Get-only.
#
# Ensures three roots and clones/fetches one inline block per known
# workspace and store:
#   ~/tool-workplace/   workspaces (env-tooling under active churn)
#   ~/dabba/            stores (cross-machine, backed-up content)
#   ~/workplace         mkdir-only; humans populate machine-specific work
#
# Each workspace block clones/fetches the repo into
# ~/tool-workplace/<name>/<repo-basename>/ and pins the public git
# identity. Each store block clones/fetches flat into
# ~/dabba/<repo-basename>/ and pins identity.
#
# L5a is GET-ONLY: it clones/fetches and stops. It does NOT run any
# cloned repo's install/setup — that is Layer 6 (layers/layer-6.sh),
# which walks the tool workplaces L5 fetched and runs each one's own
# entry-point. The content repo owns its install; L5 only gets it.
#
# Adding a workspace or store: copy an existing { ... } block and edit
# the name/url. Each block is wrapped with `|| { warn ...; FAILED=1; }`
# so one bad entry does not abort the rest of the run; the script exits
# non-zero at the end if any block failed.
#
# Options:
#   --dry-run        Log planned actions; make no changes.
#   --help, -h       Show this header and exit.
# END-USAGE

set -uo pipefail

# ── Constants ───────────────────────────────────────────────────────
TOOL_WORKPLACE_ROOT="$HOME/tool-workplace"
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

log "Layer 5a (public): roots and registries$( (( DRY_RUN )) && echo ' (dry-run)')"

# Ensure the three roots exist before iterating. workplace is
# mkdir-only by design — no registry, no clones.
for root in "$TOOL_WORKPLACE_ROOT" "$DABBA_ROOT" "$WORKPLACE_ROOT"; do
    if [[ ! -d "$root" ]]; then
        log "Creating root: $root"
        run mkdir -p "$root"
    fi
done

# Workspace: ai-workspace/mAId — hosts mAId (and private siblings on
# the kelasa side, handled by L5b). Get-only: cloned/fetched here;
# built by L6 (runs its install/setup).
{
    name="ai-workspace"
    url="git@github.com:kusimari/mAId.git"
    clone_base="$(repo_basename "$url")"
    ws_dir="$TOOL_WORKPLACE_ROOT/$name"
    clone_dir="$ws_dir/$clone_base"

    log "Workspace: $name (repo: $clone_base)"
    if [[ ! -d "$ws_dir" ]]; then
        log "Creating workspace dir: $ws_dir"
        run mkdir -p "$ws_dir"
    fi
    clone_or_fetch "$name/$clone_base" "$url" "$clone_dir"
    ensure_git_identity "$clone_dir" "$PUBLIC_USER_NAME" "$PUBLIC_USER_EMAIL"
} || { warn "ai-workspace/mAId: failed (continuing)"; FAILED=1; }

# Stores: none today. Add a `{ ... } || { warn ...; FAILED=1; }` block
# here when a public, backed-up store gets a repo. Pattern: mirror a
# workspace block but clone flat into "$DABBA_ROOT/$clone_base".

if (( FAILED )); then
    warn "Layer 5a finished with failures."
    exit 1
fi

log "Layer 5a done."
