#!/bin/bash

# stuff installed outside nix - brew and toolbox. see their docs, see loginShellInit below

# nix install
  # https://docs.determinate.systems/
  # get a determinate pkg for macos installation

# nix setup
  # run darwin as user - temporarily replace placeholder values, run nix, then revert with sed

# Platform-specific configuration for common-run.sh
export SED_INPLACE_FLAG=''  # macOS requires empty string after -i
export NIX_COMMAND='sudo -H nix run nix-darwin -- switch --flake "$FLAKE_DIR#darwin"'
export NIX_ECHO_MESSAGE="Running nix-darwin switch..."

# Option 2: Specific nix-darwin version (uncomment if needed)
# export NIX_COMMAND='sudo -H nix run nix-darwin/nix-darwin-25.05#darwin-rebuild -- switch --flake "$FLAKE_DIR#darwin"'

# Execute common functionality
source "$(dirname "${BASH_SOURCE[0]}")/../run.sh" "$@"