#!/usr/bin/env bash
# Run gittree elisp tests in emacs --batch against a throwaway fixture repo.
#
# Usage:
#   ./emacs/tests/run.sh
#
# Exits 0 on all-pass, non-zero otherwise. Prints per-test PASS/FAIL lines.
# Uses nix-shell to get emacs; no home-manager switch required.

set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
REPO_EMACS_DIR=$(cd "$SCRIPT_DIR/.." && pwd)

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

# Build the fixture repo in-place. Elisp helpers.el also knows how to
# build it, but doing it in bash lets us capture the first SHA for TC6.
cd "$WORK"
git init -q -b main
git config user.email "test@example.com"
git config user.name "test"
git config commit.gpgsign false

printf 'alpha v1\nline two\n' > alpha.md
printf 'beta content\n' > beta.md
mkdir -p dir
printf 'shared v1\n' > dir/shared.md
git add -A
git commit -q -m "first"
SHA1=$(git rev-parse HEAD)

printf 'alpha v2 modified\nline two\nthird line\n' > alpha.md
rm -f beta.md
printf 'gamma new file\n' > gamma.md
printf 'other new\n' > dir/other.md
git add -A
git commit -q -m "second"

# Run the test suite. We load core-gittree.el directly — stubs inside
# helpers.el cover the treemacs/vdiff surface we don't exercise here.
# `nix shell` is used so the harness runs on a fresh clone without
# needing a home-manager switch.
run_emacs() {
    if command -v emacs >/dev/null 2>&1; then
        emacs "$@"
    else
        nix shell nixpkgs#emacs -c emacs "$@"
    fi
}

GITTREE_TEST_SHA1="$SHA1" run_emacs --batch \
    -L "$REPO_EMACS_DIR/tests" \
    -l "$REPO_EMACS_DIR/tests/stubs.el" \
    -l "$REPO_EMACS_DIR/core-gittree.el" \
    -l "$REPO_EMACS_DIR/tests/helpers.el" \
    -l "$REPO_EMACS_DIR/tests/test-gittree.el" \
    -f gittree-run-tests
