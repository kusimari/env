#!/bin/bash
# Common body for Layer-3 build scripts (ubuntu-mane.sh,
# darwin-kelasa.sh, al2-kelasa.sh, al2023-kelasa.sh). The L3 wrapper
# must export these before sourcing this file:
#   SED_INPLACE_FLAG  — '' on macOS, unset on Linux
#   NIX_COMMAND       — the nix command to run (platform-specific)
#   NIX_ECHO_MESSAGE  — banner printed before the nix command

# Not using `set -e`: placeholders must be restored even if nix fails.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[1]}")" && pwd)"
USER_HOST_FILE="$SCRIPT_DIR/../home/user-host.nix"
FLAKE_DIR="$SCRIPT_DIR/.."

USER_VALUE=$(whoami)
HOSTNAME_VALUE=$(hostname)

replace_placeholders() {
    local from_user="$1" to_user="$2"
    local from_host="$3" to_host="$4"
    if [[ "${SED_INPLACE_FLAG+x}" = "x" ]]; then
        # macOS: SED_INPLACE_FLAG is set (even if empty)
        sed -i "$SED_INPLACE_FLAG" \
            "s|user = \"${from_user}\";|user = \"${to_user}\";|g; s|hostName = \"${from_host}\";|hostName = \"${to_host}\";|g" \
            "$USER_HOST_FILE"
    else
        # Linux
        sed -i \
            "s|user = \"${from_user}\";|user = \"${to_user}\";|g; s|hostName = \"${from_host}\";|hostName = \"${to_host}\";|g" \
            "$USER_HOST_FILE"
    fi
}

echo "Replacing placeholders with actual values..."
replace_placeholders "replace-user" "$USER_VALUE" "replace-hostname" "$HOSTNAME_VALUE"

echo "$NIX_ECHO_MESSAGE"

cd "$FLAKE_DIR" || { echo "Error: cannot cd to flake directory: $FLAKE_DIR"; exit 1; }
eval "$NIX_COMMAND"

# Clean up stray target-dir templates from `home-manager init`.
# Passing a flake-ref like `.#al2023-kelasa` as init's positional arg
# makes it create a literal `.#al2023-kelasa/` directory with template
# home.nix and flake.nix inside — the activation uses the repo flake,
# not those templates, so they're cruft.
find "$FLAKE_DIR" -maxdepth 1 -type d -name '.#*' -exec rm -rf {} +

echo "Restoring placeholders..."
replace_placeholders "$USER_VALUE" "replace-user" "$HOSTNAME_VALUE" "replace-hostname"

echo "Done!"

printf '\n\033[1;33m=== Post-install setup notes ===\033[0m\n'
cat "$FLAKE_DIR/setup-notes.md"
