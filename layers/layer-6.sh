#!/usr/bin/env bash
# env/layers/layer-6.sh — Layer 6 (public) of the seven-layer bootstrap.
# Build the tools.
#
# Walks the tool workplaces L5 cloned under ~/tool-workplace/ and runs
# each workspace's own root entry-point, preferring `setup` over
# `install` when both exist. L6 has no registry of its own — it
# discovers what L5 fetched. The content repo owns its install; L6
# only invokes it.
#
# L6 is NOT part of env setup. A bare rebuild (L1-L5) leaves the tools
# un-built; L6 is an explicit, separate step. The normal fast path is
# running a tool workspace's entry-point from inside it; L6 is the
# run-them-all convenience and the layer the rebuild driver targets
# when tooling should be rebuilt.
#
# Layout: tool workspaces live at ~/tool-workplace/<name>/<repo>/
# (two levels deep — matches how L5a/L5b clone). A repo is buildable
# if it has an executable `setup` or `install` at its root. Repos with
# neither are skipped (a store cloned into ~/dabba is never walked).
#
# Each workspace is wrapped so one bad build does not abort the rest;
# the script exits non-zero at the end if any build failed.
#
# Options:
#   --dry-run        Log planned actions; make no changes.
#   --help, -h       Show this header and exit.
# END-USAGE

set -uo pipefail

# ── Constants ───────────────────────────────────────────────────────
TOOL_WORKPLACE_ROOT="$HOME/tool-workplace"
# Entry-points to try, in preference order. `setup` wins over `install`
# (it is the recommended composer where both exist).
ENTRY_PREFERENCE=(setup install)

# ── Defaults ────────────────────────────────────────────────────────
DRY_RUN=0
FAILED=0

log()  { printf '==> %s\n' "$*"; }
warn() { printf '!!! %s\n' "$*" >&2; }
die()  { printf 'ERROR: %s\n' "$*" >&2; exit 1; }

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

# ── Helpers ─────────────────────────────────────────────────────────

# Echo the preferred entry-point basename present+executable in $dir,
# or nothing if none qualifies.
find_entry_point() {
    local dir="$1" entry
    for entry in "${ENTRY_PREFERENCE[@]}"; do
        if [[ -f "$dir/$entry" && -x "$dir/$entry" ]]; then
            printf '%s' "$entry"
            return 0
        fi
    done
    return 1
}

# Run a tool workspace's own entry-point from inside it.
build_workspace() {
    local clone_dir="$1" label="$2" entry
    if ! entry="$(find_entry_point "$clone_dir")"; then
        log "$label: no setup/install entry-point, skipping"
        return 0
    fi
    log "$label: building via ./$entry"
    if (( DRY_RUN )); then
        printf 'dry-run: (cd %s && ./%s)\n' "$clone_dir" "$entry"
    else
        ( cd "$clone_dir" && "./$entry" )
    fi
}

# ── Flow ────────────────────────────────────────────────────────────

log "Layer 6 (public): build tools$( (( DRY_RUN )) && echo ' (dry-run)')"

if [[ ! -d "$TOOL_WORKPLACE_ROOT" ]]; then
    warn "No tool workplace root at $TOOL_WORKPLACE_ROOT — run L5 first."
    exit 0
fi

# Walk depth-2 repos: ~/tool-workplace/<name>/<repo>/. A buildable repo
# is a git checkout with an executable setup/install at its root.
shopt -s nullglob
found_any=0
for clone_dir in "$TOOL_WORKPLACE_ROOT"/*/*/; do
    clone_dir="${clone_dir%/}"
    [[ -d "$clone_dir/.git" ]] || continue
    found_any=1
    label="${clone_dir#"$TOOL_WORKPLACE_ROOT"/}"
    build_workspace "$clone_dir" "$label" \
        || { warn "$label: build failed (continuing)"; FAILED=1; }
done
shopt -u nullglob

if (( ! found_any )); then
    warn "No tool workspaces found under $TOOL_WORKPLACE_ROOT — run L5 first."
fi

if (( FAILED )); then
    warn "Layer 6 finished with build failures."
    exit 1
fi

log "Layer 6 done."
