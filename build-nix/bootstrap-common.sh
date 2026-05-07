#!/usr/bin/env bash
# env/build-nix/bootstrap-common.sh — Layer 2 of the four-layer
# bootstrap.
#
# Ensures ~/env-workplace exists, checks GitHub SSH reachability,
# clones or fetches env + mAId. That's it. No chaining — when this
# script finishes, the next step (Layer 3 nix build) is something
# the user runs directly. See env/README.md for the full layer
# design.
#
# Runs either from a checkout or via curl pipe; auto-detects.
#
# Options:
#   --env-branch NAME    Branch to use for env (default: main).
#                        On initial clone: cloned at that branch.
#                        On re-run: switches to it if the clone is on
#                        a different branch. Fails if the working
#                        tree has uncommitted changes.
#   --maid-branch NAME   Same semantics for mAId.
#   --dry-run            Log planned actions; make no changes.
#   --help, -h           Show this header and exit.
# END-USAGE

set -euo pipefail

# ── Constants ────────────────────────────────────────────────────────
ENV_REPO="git@github.com:kusimari/env.git"
MAID_REPO="git@github.com:kusimari/mAId.git"
WORKSPACE="$HOME/env-workplace"
ENV_CLONE="$WORKSPACE/env"
MAID_CLONE="$WORKSPACE/mAId"
DEFAULT_KEY="$HOME/.ssh/id_ed25519"

# ── Defaults ─────────────────────────────────────────────────────────
DRY_RUN=0
ENV_BRANCH=""
MAID_BRANCH=""

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

# ── Arg parsing ──────────────────────────────────────────────────────
# Accepts both `--flag VALUE` and `--flag=VALUE` (GNU style).
while [[ $# -gt 0 ]]; do
    case "$1" in
        --env-branch)     [[ $# -ge 2 ]] || die "--env-branch requires a name"
                          ENV_BRANCH="$2";      shift 2 ;;
        --env-branch=*)   ENV_BRANCH="${1#*=}"; shift   ;;
        --maid-branch)    [[ $# -ge 2 ]] || die "--maid-branch requires a name"
                          MAID_BRANCH="$2";      shift 2 ;;
        --maid-branch=*)  MAID_BRANCH="${1#*=}"; shift   ;;
        --dry-run)        DRY_RUN=1; shift ;;
        --help|-h)        usage; exit 0 ;;
        *) die "Unknown argument: $1 (use --help)" ;;
    esac
done

# ── Steps ────────────────────────────────────────────────────────────

ensure_workspace() {
    if [[ -d "$WORKSPACE" ]]; then
        log "Workspace exists: $WORKSPACE"
    else
        log "Creating workspace: $WORKSPACE"
        run mkdir -p "$WORKSPACE"
    fi
}

ensure_github_ssh() {
    if GIT_SSH_COMMAND="ssh -o BatchMode=yes -o ControlPath=none" \
        git ls-remote "$ENV_REPO" HEAD >/dev/null 2>&1; then
        log "GitHub SSH reachable"
        return
    fi

    warn "GitHub SSH not reachable"
    if (( DRY_RUN )); then
        log "Dry-run: would generate SSH key and prompt for GitHub registration"
        return
    fi

    if [[ ! -e "$DEFAULT_KEY" ]]; then
        log "Generating SSH key: $DEFAULT_KEY"
        mkdir -p "$(dirname "$DEFAULT_KEY")"
        chmod 700 "$(dirname "$DEFAULT_KEY")"
        ssh-keygen -t ed25519 -f "$DEFAULT_KEY" -N '' \
            -C "bootstrap-common@$(hostname)" >/dev/null
    elif [[ ! -f "$DEFAULT_KEY" ]]; then
        die "$DEFAULT_KEY exists but is not a regular file; remove or move it, then re-run"
    fi

    local pub="${DEFAULT_KEY}.pub"
    local key
    key="$(cat "$pub" 2>/dev/null || echo '<pub key missing; re-run>')"
    cat >&2 <<EOF

⚠️  Manual action required
    1. Add this public key to GitHub: https://github.com/settings/keys

       $key

    2. Re-run this script when done.
EOF
    exit 1
}

clone_or_fetch() {
    local name="$1" url="$2" dest="$3" branch="${4:-}"
    if [[ -d "$dest/.git" ]]; then
        log "$name: fetching updates"
        run git -C "$dest" fetch --quiet origin
        if [[ -n "$branch" ]]; then
            switch_branch "$name" "$dest" "$branch"
        fi
    elif [[ -e "$dest" ]]; then
        die "$dest exists and is not a git checkout"
    else
        if [[ -n "$branch" ]]; then
            log "$name: cloning $url (branch: $branch) -> $dest"
            run git clone --quiet --branch "$branch" "$url" "$dest"
        else
            log "$name: cloning $url -> $dest"
            run git clone --quiet "$url" "$dest"
        fi
    fi
}

# Switch an existing clone to $branch if it isn't already on it.
# Refuses to clobber uncommitted tracked changes — the user must
# commit or stash first. Untracked files are left alone.
switch_branch() {
    local name="$1" dest="$2" branch="$3"
    local current
    current="$(git -C "$dest" rev-parse --abbrev-ref HEAD 2>/dev/null || echo '<detached>')"
    if [[ "$current" = "$branch" ]]; then
        log "$name: already on $branch"
        return
    fi
    if [[ -n "$(git -C "$dest" status --porcelain --untracked-files=no 2>/dev/null)" ]]; then
        die "$dest has uncommitted changes on '$current'; commit or stash before switching to $branch"
    fi
    log "$name: switching from $current to $branch"
    run git -C "$dest" checkout --quiet "$branch"
}

# Pin commit identity for public repos (env + mAId). Without this,
# git auto-assigns $USER@$HOSTNAME, which can leak corporate
# dev-desktop hostnames into public commit metadata. Local config
# only — global git config is never touched. Idempotent.
ensure_git_identity() {
    local dest="$1"
    local want_name="kusimari"
    local want_email="kusimari@gmail.com"
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

# ── Flow ─────────────────────────────────────────────────────────────

log "Layer 2: sync$( (( DRY_RUN )) && echo ' (dry-run)')"
ensure_workspace
ensure_github_ssh
clone_or_fetch "env"  "$ENV_REPO"  "$ENV_CLONE"  "$ENV_BRANCH"
clone_or_fetch "mAId" "$MAID_REPO" "$MAID_CLONE" "$MAID_BRANCH"
ensure_git_identity "$ENV_CLONE"
ensure_git_identity "$MAID_CLONE"
log "Layer 2 done."
