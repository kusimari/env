#!/bin/bash

# Cloud VM configuration build script for local testing
# This tests the nixosConfigurations.cloud build without deploying

# Platform-specific configuration for _common.sh
# Linux sed uses -i without additional flag (don't export SED_INPLACE_FLAG)
export NIX_COMMAND='nix build "$FLAKE_DIR#nixosConfigurations.cloud.config.system.build.toplevel"'
export NIX_ECHO_MESSAGE="Building nixosConfigurations.cloud for testing..."

# Execute common functionality
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh" "$@"
