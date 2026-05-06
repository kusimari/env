#!/bin/bash

# Prerequisites for al2023-kelasa: run Layer 1
# (bootstrap-al2023-kelasa.sh in a kelasa-specific env repo) first.
# It installs single-user Nix, sets up SSL certificates, and
# configures sudoers for /nix dir creation.

# nix setup
  # run home-manager - temporarily replace placeholder values, run nix, then revert with sed

# Platform-specific configuration for _common.sh
# Linux sed uses -i without additional flag (don't export SED_INPLACE_FLAG)

# For AL2023: Use nix run command for initial setup with backup
export NIX_COMMAND='nix --extra-experimental-features "nix-command flakes" run home-manager/master -- init --switch -b bak ".#al2023-kelasa"'
export NIX_ECHO_MESSAGE="Running home-manager init with backup..."

# Standard command (use after initial setup)
# export NIX_COMMAND='home-manager switch --flake ".#al2023-kelasa"'
# export NIX_ECHO_MESSAGE="Running home-manager switch..."

# Execute common functionality
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh"
