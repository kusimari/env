#!/bin/bash
# emacs-gittree — launch emacs in gittree mode, optionally comparing two refs.
#
# Usage:
#   emacs-gittree                              # open gittree-mode, pick files in the tree
#   emacs-gittree <ref>                        # <ref> vs working (right panel shows diff)
#   emacs-gittree <ref-a> <ref-b>              # <ref-a> vs <ref-b>; pick file from tree
#   emacs-gittree <ref-a> <ref-b> <file>       # open that file immediately, diffed
#
# Refs may be literal 'working' (on-disk), ':0' (staged), 'HEAD', or any git ref.
# Tokens other than the literals are passed through to `git show <ref>:<path>`.

set -euo pipefail

# Escape a string for embedding in an elisp string literal.
elisp_quote() {
    local s="$1"
    s="${s//\\/\\\\}"
    s="${s//\"/\\\"}"
    printf '%s' "$s"
}

case "$#" in
    0)
        exec emacs --eval "(gittree-mode 1)"
        ;;
    1)
        ref_a=$(elisp_quote "$1")
        exec emacs --eval "(gittree-launch \"${ref_a}\" \"working\" nil)"
        ;;
    2)
        ref_a=$(elisp_quote "$1")
        ref_b=$(elisp_quote "$2")
        exec emacs --eval "(gittree-launch \"${ref_a}\" \"${ref_b}\" nil)"
        ;;
    3)
        ref_a=$(elisp_quote "$1")
        ref_b=$(elisp_quote "$2")
        file=$(elisp_quote "$3")
        exec emacs --eval "(gittree-launch \"${ref_a}\" \"${ref_b}\" \"${file}\")"
        ;;
    *)
        echo "Usage: emacs-gittree [<ref-a> [<ref-b> [<file>]]]" >&2
        exit 2
        ;;
esac
