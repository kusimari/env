#!/bin/bash

# Prerequisites for Amazon Linux 2:
# - Nix installed with SINGLE-USER mode: sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install) --no-daemon
# - SSL certificates fixed (run bootstrap-al2.sh)
# - zsh with chsh of user (run bootstrap-al2.sh)

# nix setup
  # run home-manager - temporarily replace placeholder values, run nix, then revert with sed

# Platform-specific configuration for _common.sh
# Linux sed uses -i without additional flag (don't export SED_INPLACE_FLAG)

# For AL2: Use nix run command for initial setup with backup
export NIX_COMMAND='nix --extra-experimental-features "nix-command flakes" run home-manager/master -- init --switch -b bak ".#al2-kelasa"'
export NIX_ECHO_MESSAGE="Running home-manager init with backup..."

# Standard command (use after initial setup)
# export NIX_COMMAND='home-manager switch --flake ".#al2-kelasa"'
# export NIX_ECHO_MESSAGE="Running home-manager switch..."

# Execute common functionality
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh" "$@"
