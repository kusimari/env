#!/bin/bash

# stuff installed outside nix - zsh with chsh of user

# nix install
  # curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# nix setup
  # run home-manager - temporarily replace placeholder values, run nix, then revert with sed

# Platform-specific configuration for common-run.sh
# Linux sed uses -i without additional flag (don't export SED_INPLACE_FLAG)
export NIX_COMMAND='home-manager switch --flake "$FLAKE_DIR"'
export NIX_ECHO_MESSAGE="Running home-manager switch..."

# Option 1: For initial setup (uncomment if needed)
# export NIX_COMMAND='nix run home-manager/master -- init --switch "$FLAKE_DIR"'

# Execute common functionality
source "$(dirname "${BASH_SOURCE[0]}")/../common/common-run.sh" "$@"