#!/bin/bash
# env/layers/layer-3-ubuntu-mane.sh — Layer 3: The Environment Setup
#
# Desktop convergence for ubuntu-mane:
#   1. Pre-Nix host hardware prep (layer-ubuntu-mane-host-prep.sh)
#   2. Nix userland activation (home-manager switch via layer-3-common.sh)
#   3. Post-Nix host debloat (layer-3-ubuntu-mane-debloat.sh)

# Platform-specific configuration for layer-3-common.sh
# Linux sed uses -i without additional flag (don't export SED_INPLACE_FLAG)
export NIX_COMMAND='home-manager switch -b bak --flake ".#ubuntu-mane"'
export NIX_ECHO_MESSAGE="Running home-manager switch..."

# Option 1: For initial setup (uncomment if needed)
# export NIX_COMMAND='nix run home-manager/master -- init --switch ".#ubuntu-mane"'

# Host OS hardware & driver prep sub-script — ensures host printing/scanning backend is ready
bash "$(dirname "${BASH_SOURCE[0]}")/layer-ubuntu-mane-host-prep.sh" "$@"

# Execute common functionality
# shellcheck disable=SC1091
source "$(dirname "${BASH_SOURCE[0]}")/layer-3-common.sh"

# Host debloat sub-script — purges default Ubuntu desktop bloatware post-switch
bash "$(dirname "${BASH_SOURCE[0]}")/layer-3-ubuntu-mane-debloat.sh" "$@"
