#!/bin/bash

# AL2 Setup and Build Script
# This script:
# 1. Installs nix if not present
# 2. Configures SSL certificates
# 3. Installs/configures zsh
# 4. Runs home-manager build via _common.sh

set -euo pipefail

echo "=== AL2 Nix Setup and Build ==="
echo ""

# Step 1: Install Nix if not present
echo "Step 1: Nix installation..."
if ! command -v nix >/dev/null 2>&1; then
  echo "Installing single-user Nix for AL2..."
  sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install) --no-daemon

  # Source nix profile for this session
  if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
    . ~/.nix-profile/etc/profile.d/nix.sh
  fi
  echo "✓ Nix installed"
else
  echo "✓ Nix already installed at $(which nix)"
fi

# Step 2: SSL certificate configuration
echo ""
echo "Step 2: SSL certificate configuration..."
sudo mkdir -p /etc/nix
if [ ! -f /etc/nix/nix.conf ] || ! grep -q "ssl-cert-file" /etc/nix/nix.conf; then
  echo "Configuring nix.conf..."
  sudo tee /etc/nix/nix.conf > /dev/null << EOF
trusted-users = root $(whoami)
experimental-features = nix-command flakes
ssl-cert-file = /etc/ssl/certs/ca-bundle.crt
EOF
  echo "✓ nix.conf configured"
else
  echo "✓ nix.conf already configured"
fi

export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
if ! grep -q "SSL_CERT_FILE" ~/.bashrc 2>/dev/null; then
  echo 'export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt' >> ~/.bashrc
fi

# Only update CA certs if older than 7 days (avoid unnecessary network calls)
if [ ! -f /var/cache/.ca-certs-updated ] || [ "$(find /var/cache/.ca-certs-updated -mtime +7 2>/dev/null)" ]; then
  echo "Updating CA certificates..."
  sudo yum update -y ca-certificates >/dev/null 2>&1
  sudo touch /var/cache/.ca-certs-updated
fi
echo "✓ SSL certificates configured"

# Step 3: Shell configuration
echo ""
echo "Step 3: Shell configuration..."
if [ "$SHELL" != "$(which zsh 2>/dev/null)" ]; then
  echo "Installing and configuring zsh..."
  sudo yum install -y zsh >/dev/null 2>&1
  chsh -s $(which zsh)
  echo "✓ zsh configured (requires logout/login to take effect)"
else
  echo "✓ zsh already configured"
fi

echo ""
echo "=== Running home-manager build ==="
echo ""

# Platform-specific configuration for _common.sh
# For initial setup with backup:
export NIX_COMMAND='nix --extra-experimental-features "nix-command flakes" run home-manager/master -- init --switch -b bak ".#al2-kelasa"'
export NIX_ECHO_MESSAGE="Running home-manager init with backup for AL2..."

# Standard command (use after initial setup, can uncomment and replace above):
# export NIX_COMMAND='home-manager switch --flake ".#al2-kelasa"'
# export NIX_ECHO_MESSAGE="Running home-manager switch for AL2..."

# Execute common functionality
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh" "$@"
