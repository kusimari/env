#!/bin/bash

# Common functionality for both nix-darwin and nix-ubuntu run.sh scripts
# This script expects the following parameters to be set by the calling script:
#   SED_INPLACE_FLAG: either '' (for macOS) or nothing (for Linux)
#   NIX_COMMAND: the nix command to run (platform-specific)
#   NIX_ECHO_MESSAGE: the echo message to display before running nix command
#
# Arguments:
#   $1 - (optional) path to pre-nix setup script
#   $2 - (optional) path to post-nix setup script

# Note: Not using 'set -e' to ensure placeholders are always restored even if nix fails

# Get the directory where the calling script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[1]}")" && pwd)"
USER_HOST_FILE="$SCRIPT_DIR/../home/user-host.nix"
FLAKE_DIR="$SCRIPT_DIR/.."

echo "Replacing placeholders with actual values..."
# Get values - no escaping needed since we're replacing entire assignments
USER_VALUE=$(whoami)
HOSTNAME_VALUE=$(hostname)

if [[ "${SED_INPLACE_FLAG+x}" = "x" ]]; then
    # macOS: SED_INPLACE_FLAG is set (even if empty)
    sed -i "$SED_INPLACE_FLAG" "s|user = \"replace-user\";|user = \"$USER_VALUE\";|g; s|hostName = \"replace-hostname\";|hostName = \"$HOSTNAME_VALUE\";|g" "$USER_HOST_FILE"
else
    # Linux: SED_INPLACE_FLAG is not set
    sed -i "s|user = \"replace-user\";|user = \"$USER_VALUE\";|g; s|hostName = \"replace-hostname\";|hostName = \"$HOSTNAME_VALUE\";|g" "$USER_HOST_FILE"
fi

# Execute pre-nix setup script if provided as argument
# The script should write shell initialization to ~/.pre-nix-rc
if [[ -n "$1" ]]; then
    if [[ -f "$1" ]]; then
        echo "Executing pre-nix setup script: $1"
        if [[ -x "$1" ]]; then
            source "$1"
            echo "Pre-nix setup script executed successfully"
            if [[ -f "$HOME/.pre-nix-rc" ]]; then
                echo "Pre-nix shell initialization file created at ~/.pre-nix-rc"
            fi
        else
            echo "Error: Pre-nix setup script is not executable: $1"
            exit 1
        fi
    else
        echo "Error: Pre-nix setup script not found: $1"
        exit 1
    fi
else
    # No pre-nix setup script provided, create dummy file
    echo "# No pre-nix setup configured" > "$HOME/.pre-nix-rc"
    echo "Created dummy ~/.pre-nix-rc file"
fi

echo "$NIX_ECHO_MESSAGE"

# cd to the flake directory so NIX_COMMAND can reference the flake as "."
cd "$FLAKE_DIR" || { echo "Error: cannot cd to flake directory: $FLAKE_DIR"; exit 1; }
eval "$NIX_COMMAND"

# Execute post-nix setup script if provided as second argument
if [[ -n "$2" ]]; then
    if [[ -f "$2" ]]; then
        echo "Executing post-nix setup script: $2"
        if [[ -x "$2" ]]; then
            source "$2"
            echo "Post-nix setup script executed successfully"
        else
            echo "Error: Post-nix setup script is not executable: $2"
        fi
    else
        echo "Error: Post-nix setup script not found: $2"
    fi
fi

# Clean up stray target-dir templates from `home-manager init`.
# Passing a flake-ref like `.#al2023-kelasa` as init's positional arg makes
# it create a literal `.#al2023-kelasa/` directory with template home.nix
# and flake.nix inside — the activation uses the repo flake, not those
# templates, so they're cruft. Scoped to the flake dir at depth 1.
find "$FLAKE_DIR" -maxdepth 1 -type d -name '.#*' -exec rm -rf {} +

echo "Restoring placeholders..."
if [[ "${SED_INPLACE_FLAG+x}" = "x" ]]; then
    # macOS: SED_INPLACE_FLAG is set (even if empty)
    sed -i "$SED_INPLACE_FLAG" "s|user = \"$USER_VALUE\";|user = \"replace-user\";|g; s|hostName = \"$HOSTNAME_VALUE\";|hostName = \"replace-hostname\";|g" "$USER_HOST_FILE"
else
    # Linux: SED_INPLACE_FLAG is not set
    sed -i "s|user = \"$USER_VALUE\";|user = \"replace-user\";|g; s|hostName = \"$HOSTNAME_VALUE\";|hostName = \"replace-hostname\";|g" "$USER_HOST_FILE"
fi

echo "Done!"

printf '\n\033[1;33m=== Post-install setup notes ===\033[0m\n'
cat "$FLAKE_DIR/setup-notes.md"