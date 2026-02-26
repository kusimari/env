#!/bin/bash

# Prerequisites for Amazon Linux 2:
# - Nix installed with SINGLE-USER mode: sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install) --no-daemon
# - SSL certificates fixed (use ./fix-git-ssl.sh if needed)
# - zsh with chsh of user (manual step)

# nix setup
  # run home-manager - temporarily replace placeholder values, run nix, then revert with sed

# Platform-specific configuration for common-run.sh
# Linux sed uses -i without additional flag (don't export SED_INPLACE_FLAG)

# For AL2: Use nix run command for initial setup with backup (cd to flake dir first)
export NIX_COMMAND='cd "$FLAKE_DIR" && nix --extra-experimental-features "nix-command flakes" run home-manager/master -- init --switch -b backup ".#kelasa-al2"'
export NIX_ECHO_MESSAGE="Running home-manager init with backup..."

# Standard command (use after initial setup)
# export NIX_COMMAND='cd "$FLAKE_DIR" && home-manager switch --flake ".#kelasa-al2"'
# export NIX_ECHO_MESSAGE="Running home-manager switch..."

# Execute common functionality
source "$(dirname "${BASH_SOURCE[0]}")/run.sh" "$@"