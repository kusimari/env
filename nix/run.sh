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
if [[ "${SED_INPLACE_FLAG+x}" = "x" ]]; then
    # macOS: SED_INPLACE_FLAG is set (even if empty)
    sed -i "$SED_INPLACE_FLAG" "s/replace-user/$(whoami)/g; s/replace-hostname/$(hostname)/g" "$USER_HOST_FILE"
else
    # Linux: SED_INPLACE_FLAG is not set
    sed -i "s/replace-user/$(whoami)/g; s/replace-hostname/$(hostname)/g" "$USER_HOST_FILE"
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

# Run the platform-specific nix command
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

echo "Restoring placeholders..."
if [[ "${SED_INPLACE_FLAG+x}" = "x" ]]; then
    # macOS: SED_INPLACE_FLAG is set (even if empty)
    sed -i "$SED_INPLACE_FLAG" "s/$(whoami)/replace-user/g; s/$(hostname)/replace-hostname/g" "$USER_HOST_FILE"
else
    # Linux: SED_INPLACE_FLAG is not set
    sed -i "s/$(whoami)/replace-user/g; s/$(hostname)/replace-hostname/g" "$USER_HOST_FILE"
fi

echo "Done!"