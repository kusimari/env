#!/bin/bash

# Prerequisites for Amazon Linux 2023:
# - Nix installed (single-user or multi-user via determinate installer)
# - SSL certificates configured
# - zsh with chsh of user

# Platform-specific configuration for _common.sh
# AL2023 has better systemd support, can potentially use multi-user nix
# For consistency with AL2, start with single-user approach

# For initial setup with backup:
export NIX_COMMAND='nix --extra-experimental-features "nix-command flakes" run home-manager/master -- init --switch -b bak ".#al2023-kelasa"'
export NIX_ECHO_MESSAGE="Running home-manager init with backup for AL2023..."

# Standard command (use after initial setup, can uncomment and replace above):
# export NIX_COMMAND='home-manager switch --flake ".#al2023-kelasa"'
# export NIX_ECHO_MESSAGE="Running home-manager switch for AL2023..."

# Execute common functionality
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh" "$@"
