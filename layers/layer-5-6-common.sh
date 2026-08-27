#!/usr/bin/env bash
# env/layers/layer-5-6-common.sh — shared helpers for layer-5.sh and
# layer-6.sh. Sourced, not executed; caller must set DRY_RUN first.
#
# Not sourced by L1/L2 — those stay single-file and curl-pipeable
# (see project.md's Curl-able column), so they keep their own copies.

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
