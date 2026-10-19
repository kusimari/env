#!/usr/bin/env bash
# env/layers/layer-common.sh — shared helpers for layers that can
# source a sibling file. Sourced, not executed; caller must set
# DRY_RUN first.
#
# Sourced today by layer-5.sh and layer-6.sh, whose log/warn/die/run/
# clone_or_fetch/ensure_git_identity/repo_basename are identical.
# update_default_branch is L6-only: stores (L5) stay fetch-only, since
# a store checkout can carry local work.
# Deliberately NOT sourced by:
#   - layer-1-*.sh / layer-2.sh — must stay single-file and
#     curl-pipeable (`curl url | bash` has no sibling file to source),
#     so they keep their own copies of log/warn/die/run.
#   - layer-2.sh's own clone_or_fetch/ensure_git_identity — same names
#     as the ones here, but a different shape (branch-switching,
#     hardcoded identity); not the same function, so not folded in.
#   - the layer-3 family (layer-3-common.sh, layer-3-post-nix-common.sh)
#     — already has its own differently-scoped `pn_log`/`pn_warn`
#     (distinct on purpose; not the same functions as these).
# usage() is never moved here either, in any script: its
# ${BASH_SOURCE[0]} self-reference must resolve to the caller's own
# file, not this one.

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

# Put $dest on its remote default branch (origin/HEAD: main, mainline,
# ...) and fast-forward it to the fetched tip, so a build runs the latest
# default branch, never a feature branch left checked out. Refuses a
# dirty tree rather than switch away from uncommitted work. Run after
# clone_or_fetch; a no-op when $dest is not a checkout yet (dry-run).
update_default_branch() {
    local name="$1" dest="$2" head branch cur
    [[ -d "$dest/.git" ]] || return 0

    head="$(git -C "$dest" symbolic-ref --quiet --short refs/remotes/origin/HEAD 2>/dev/null || true)"
    branch="${head#origin/}"
    if [[ -z "$branch" ]]; then
        for branch in main mainline ""; do
            [[ -z "$branch" ]] && break
            git -C "$dest" show-ref --quiet --verify "refs/remotes/origin/$branch" && break
        done
    fi
    [[ -n "$branch" ]] || { warn "$name: no default branch found on origin"; return 1; }

    if [[ -n "$(git -C "$dest" status --porcelain)" ]]; then
        warn "$name: uncommitted changes — left on $(git -C "$dest" branch --show-current), not updated"
        return 1
    fi
    cur="$(git -C "$dest" branch --show-current)"
    if [[ "$cur" != "$branch" ]]; then
        log "$name: switching ${cur:-detached} -> $branch"
        run git -C "$dest" switch --quiet "$branch" || return 1
    fi
    log "$name: fast-forwarding $branch to origin/$branch"
    run git -C "$dest" merge --ff-only --quiet "origin/$branch"
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
