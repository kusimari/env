#!/bin/bash

# stuff installed outside nix - zsh with chsh of user

# nix install
  # curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# nix setup
  # run home-manager - temporarily replace placeholder values, run nix, then revert with sed

# Platform-specific configuration for common-run.sh
# Linux sed uses -i without additional flag (don't export SED_INPLACE_FLAG)

# For AL2: Use nix run command for initial setup (after fixing nixbld group)
export NIX_COMMAND='nix run home-manager/master -- init --switch "$FLAKE_DIR#kelasa-al2"'
export NIX_ECHO_MESSAGE="Running home-manager init with nix run..."

# Standard command (use after initial setup)
# export NIX_COMMAND='home-manager switch --flake "$FLAKE_DIR#kelasa-al2"'
# export NIX_ECHO_MESSAGE="Running home-manager switch..."

# Execute common functionality
source "$(dirname "${BASH_SOURCE[0]}")/../run.sh" "$@"