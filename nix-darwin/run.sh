#!/bin/bash

# stuff installed outside nix - brew and toolbox. see their docs, see loginShellInit below

# nix install
  # https://docs.determinate.systems/
  # get a determinate pkg for macos installation

# nix setup
  # run darwin as user - temporarily replace placeholder values, run nix, then revert with sed
  # nix flake update

# Note: Not using 'set -e' to ensure placeholders are always restored even if nix fails

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
USER_HOST_FILE="$SCRIPT_DIR/../home/user-host.nix"
FLAKE_DIR="$SCRIPT_DIR"

echo "Replacing placeholders with actual values..."
sed -i '' "s/replace-user/$(whoami)/g; s/replace-hostname/$(hostname)/g" "$USER_HOST_FILE"

echo "Running nix-darwin switch..."

# Option 1: Standard nix-darwin
sudo -H nix run nix-darwin -- switch --flake "$FLAKE_DIR"

# Option 2: Specific nix-darwin version (uncomment if needed)
# sudo -H nix run nix-darwin/nix-darwin-25.05#darwin-rebuild -- switch --flake "$FLAKE_DIR"

echo "Restoring placeholders..."
sed -i '' "s/$(whoami)/replace-user/g; s/$(hostname)/replace-hostname/g" "$USER_HOST_FILE"

echo "Done!"