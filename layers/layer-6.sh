#!/usr/bin/env bash
# env/layers/layer-6.sh — Layer 6 (public): get + build tools.
#
# Clones/fetches one inline { ... } block per known tool workspace
# into ~/tool-workplace/<name>/<repo-basename>/, then discovers and
# runs each workspace's own setup/install entry-point. See project.md
# for the layer design.
#
# Adding a tool workspace: copy an existing { ... } block and edit
# the name/url.
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

source "$(dirname "${BASH_SOURCE[0]}")/layer-5-6-common.sh"

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
