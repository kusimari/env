#!/bin/bash

# stuff installed outside nix - zsh with chsh of user

# nix install
  # curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# nix setup
  # run home-manager - temporarily replace placeholder values, run nix, then revert with sed
  # nix flake update

# Note: Not using 'set -e' to ensure placeholders are always restored even if nix fails

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
USER_HOST_FILE="$SCRIPT_DIR/../home/user-host.nix"
FLAKE_DIR="$SCRIPT_DIR"

echo "Replacing placeholders with actual values..."
sed -i "s/replace-user/$(whoami)/g; s/replace-hostname/$(hostname)/g" "$USER_HOST_FILE"

echo "Running home-manager switch..."
# Option 1: For initial setup
# nix run home-manager/master -- init --switch "$FLAKE_DIR"

# Option 2: For subsequent runs (uncomment this and comment the line above if home-manager is already installed)
home-manager switch --flake "$FLAKE_DIR"

echo "Restoring placeholders..."
sed -i "s/$(whoami)/replace-user/g; s/$(hostname)/replace-hostname/g" "$USER_HOST_FILE"

echo "Done!"