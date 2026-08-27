#!/usr/bin/env bash
# env/layers/layer-6.sh — Layer 6 (public) of the seven-layer bootstrap.
# Tools: get + build.
#
# Owns tool workspaces end-to-end: clones/fetches each declared
# workspace under ~/tool-workplace/<name>/<repo-basename>/ (an inline
# { ... } block per workspace, same shape as L5's store blocks), pins
# the public git identity, then discovers and runs every entry-point
# found there. An entry-point is an executable `setup` or `install` at
# either the workspace-root or the sub-repo level — discovered with
# `fd`, so the depth isn't hardcoded. Where both `setup` and `install`
# sit in the same dir, `setup` wins (it is the recommended composer).
#
# The get step needs a declared list (this script's inline blocks) —
# you can't discover what to clone by walking a directory that doesn't
# exist yet. The build step stays registry-free: it walks whatever is
# actually present under ~/tool-workplace/, so a workspace placed there
# by hand (no inline block) is still discovered and built. The content
# repo owns its own install; L6 only clones it and invokes it.
#
# L6 is NOT part of the base env. A bare rebuild through L5 leaves
# ~/tool-workplace/ entirely absent; L6 is an explicit, separate step
# that both creates it and builds what it clones.
#
# Adding a tool workspace: copy an existing { ... } block and edit the
# name/url — same pattern as L5's store blocks. Each get block and each
# build is wrapped so one bad entry does not abort the rest; the script
# exits non-zero at the end if anything failed.
#
# Options:
#   --dry-run        Log planned actions; make no changes.
#   --help, -h       Show this header and exit.
# END-USAGE

set -uo pipefail

# ── Constants ───────────────────────────────────────────────────────
TOOL_WORKPLACE_ROOT="$HOME/tool-workplace"
PUBLIC_USER_NAME="kusimari"
PUBLIC_USER_EMAIL="kusimari@gmail.com"
# Entry-point basenames, in preference order. `setup` wins over
# `install` when both sit in the same directory.
ENTRY_PREFERENCE=(setup install)
# fd --max-depth counts the matched file itself. An entry-point at the
# workspace-root <name>/setup is depth 2; at the sub-repo level
# <name>/<repo>/setup it is depth 3. Cover both.
MAX_DEPTH=3

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
        --dry-run)  DRY_RUN=1; shift ;;
        --help|-h)  usage; exit 0 ;;
        *) die "Unknown argument: $1 (use --help)" ;;
    esac
done

command -v fd >/dev/null 2>&1 || die "fd not found on PATH (required for tool discovery)"

# ── Helpers (get step; same shape as env/layers/layer-5.sh) ─────────

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

repo_basename() {
    local url="$1" base
    base="${url##*/}"
    base="${base%.git}"
    printf '%s' "$base"
}

# Run one entry-point from inside its directory.
build_entry() {
    local entry_path="$1"
    local dir label
    dir="$(dirname "$entry_path")"
    label="${dir#"$TOOL_WORKPLACE_ROOT"/}"
    log "$label: building via ./$(basename "$entry_path")"
    if (( DRY_RUN )); then
        printf 'dry-run: (cd %s && ./%s)\n' "$dir" "$(basename "$entry_path")"
    else
        ( cd "$dir" && "$entry_path" )
    fi
}

# ── Flow ────────────────────────────────────────────────────────────

log "Layer 6 (public): get + build tools$( (( DRY_RUN )) && echo ' (dry-run)')"

if [[ ! -d "$TOOL_WORKPLACE_ROOT" ]]; then
    log "Creating root: $TOOL_WORKPLACE_ROOT"
    run mkdir -p "$TOOL_WORKPLACE_ROOT"
fi

# Workspace: ai-workspace/mAId — hosts mAId (and private siblings on
# the kelasa side, handled by the private L6). Get + build: cloned or
# fetched here, then discovered and built by the walk below.
(
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
) || { warn "ai-workspace/mAId: get failed (continuing)"; FAILED=1; }

# Add another tool workspace the same way: copy the block above, edit
# name/url.

# Discover executable setup/install entry-points anywhere up to
# MAX_DEPTH under the root. Collect the dirs that hold at least one,
# then per-dir let ENTRY_PREFERENCE pick which to run (so a dir with
# both setup+install runs setup only, never twice).
#
# Under --dry-run on a fresh machine the root's `mkdir -p` above is
# itself a no-op, so the root may still not exist here — guard the fd
# call rather than let it print "not a directory" to stderr.
declare -A seen_dir=()
ordered_dirs=()
if [[ -d "$TOOL_WORKPLACE_ROOT" ]]; then
    while IFS= read -r -d '' entry_path; do
        dir="$(dirname "$entry_path")"
        if [[ -z "${seen_dir[$dir]:-}" ]]; then
            seen_dir[$dir]=1
            ordered_dirs+=("$dir")
        fi
    done < <(
        fd --absolute-path --type file --type executable --max-depth "$MAX_DEPTH" \
           '^(setup|install)$' "$TOOL_WORKPLACE_ROOT" --print0
    )
fi

if (( ${#ordered_dirs[@]} == 0 )); then
    warn "No tool entry-points (setup/install) found under $TOOL_WORKPLACE_ROOT."
else
    for dir in "${ordered_dirs[@]}"; do
        for entry in "${ENTRY_PREFERENCE[@]}"; do
            if [[ -x "$dir/$entry" ]]; then
                build_entry "$dir/$entry" \
                    || { warn "${dir#"$TOOL_WORKPLACE_ROOT"/}: build failed (continuing)"; FAILED=1; }
                break
            fi
        done
    done
fi

if (( FAILED )); then
    warn "Layer 6 finished with failures."
    exit 1
fi

log "Layer 6 done."
