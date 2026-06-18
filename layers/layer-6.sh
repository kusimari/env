#!/usr/bin/env bash
# env/layers/layer-6.sh — Layer 6 (public) of the seven-layer bootstrap.
# Build the tools.
#
# Finds every tool entry-point L5 cloned under ~/tool-workplace/ and
# runs it. An entry-point is an executable `setup` or `install` at
# either the workspace-root (~/tool-workplace/<name>/) or the sub-repo
# (~/tool-workplace/<name>/<repo>/) level — discovered with `fd`, so
# the depth isn't hardcoded. Where both `setup` and `install` sit in
# the same dir, `setup` wins (it is the recommended composer). L6 has
# no registry of its own — it runs what L5 fetched. The content repo
# owns its install; L6 only invokes it.
#
# L6 is NOT part of env setup. A bare rebuild (L1-L5) leaves the tools
# un-built; L6 is an explicit, separate step. The normal fast path is
# running a tool workspace's entry-point from inside it; L6 is the
# run-them-all convenience and the layer the rebuild driver targets
# when tooling should be rebuilt.
#
# Each entry-point is wrapped so one bad build does not abort the rest;
# the script exits non-zero at the end if any build failed.
#
# Options:
#   --dry-run        Log planned actions; make no changes.
#   --help, -h       Show this header and exit.
# END-USAGE

set -uo pipefail

# ── Constants ───────────────────────────────────────────────────────
TOOL_WORKPLACE_ROOT="$HOME/tool-workplace"
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

log "Layer 6 (public): build tools$( (( DRY_RUN )) && echo ' (dry-run)')"

if [[ ! -d "$TOOL_WORKPLACE_ROOT" ]]; then
    warn "No tool workplace root at $TOOL_WORKPLACE_ROOT — run L5 first."
    exit 0
fi

# Discover executable setup/install entry-points anywhere up to
# MAX_DEPTH under the root. Collect the dirs that hold at least one,
# then per-dir let ENTRY_PREFERENCE pick which to run (so a dir with
# both setup+install runs setup only, never twice).
declare -A seen_dir=()
ordered_dirs=()
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

if (( ${#ordered_dirs[@]} == 0 )); then
    warn "No tool entry-points (setup/install) found under $TOOL_WORKPLACE_ROOT — run L5 first."
    exit 0
fi

for dir in "${ordered_dirs[@]}"; do
    for entry in "${ENTRY_PREFERENCE[@]}"; do
        if [[ -x "$dir/$entry" ]]; then
            build_entry "$dir/$entry" \
                || { warn "${dir#"$TOOL_WORKPLACE_ROOT"/}: build failed (continuing)"; FAILED=1; }
            break
        fi
    done
done

if (( FAILED )); then
    warn "Layer 6 finished with build failures."
    exit 1
fi

log "Layer 6 done."
