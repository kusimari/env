#!/usr/bin/env bash
# env/build-nix/post-nix-common.sh — Layer 3 tail.
#
# Sourced by _common.sh after `home-manager switch` (or equivalent)
# finishes. At that point nix-managed binaries exist on disk, but the
# calling shell hasn't re-sourced .zshrc, so we prepend the per-user
# nix profile bin dirs to PATH ourselves before invoking anything.
#
# Keep this file to idempotent, interactive-friendly post-activation
# nudges. Anything that needs secrets (tokens, keychain writes) belongs
# here, not in nix store paths.

# No `set -e`: a failed nudge must not abort the rest of the post-nix flow.

pn_log()  { printf '==> %s\n' "$*"; }
pn_warn() { printf '!!! %s\n' "$*" >&2; }

# Make nix-installed binaries reachable regardless of shell init state.
# ~/.nix-profile/bin covers single-user installs (al2/al2023/ubuntu);
# /etc/profiles/per-user/$USER/bin covers multi-user + nix-darwin.
pn_prime_path() {
    local profile_dirs=(
        "$HOME/.nix-profile/bin"
        "/etc/profiles/per-user/$(whoami)/bin"
    )
    for d in "${profile_dirs[@]}"; do
        if [[ -d "$d" && ":$PATH:" != *":$d:"* ]]; then
            PATH="$d:$PATH"
        fi
    done
    export PATH
}

# gh auth: the token lives in ~/.config/gh/hosts.yml (or the OS keychain
# if gh was compiled with support). Either way it's per-user, portable
# via that file, and outside the nix store. We just make sure the user
# has logged in once; we never write the token ourselves.
pn_ensure_gh_auth() {
    if ! command -v gh >/dev/null 2>&1; then
        pn_warn "gh not on PATH yet; skipping auth check (re-run this script after the next shell opens)"
        return 0
    fi

    if gh auth status -h github.com >/dev/null 2>&1; then
        pn_log "gh authenticated for github.com"
        return 0
    fi

    cat >&2 <<'EOF'

⚠️  Manual action required
    gh is installed but not authenticated. On this headless-friendly box,
    run:

        gh auth login -h github.com -p ssh -w

    Pick "Login with a web browser"; gh prints a one-time code and URL.
    Open the URL on any device (laptop/phone), paste the code, approve.
    Token persists in ~/.config/gh/hosts.yml across shells and reboots.

    Re-run the build script when done to re-verify.
EOF
    return 1
}

# ── Flow ──────────────────────────────────────────────────────────────
pn_log "Post-nix checks"
pn_prime_path
pn_ensure_gh_auth || true
pn_log "Post-nix checks done."
