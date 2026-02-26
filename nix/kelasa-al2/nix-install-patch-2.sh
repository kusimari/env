#!/bin/bash

# Nix installation and SSL patch script for Amazon Linux 2
# This script uses standard nixos.org installer and fixes SSL certificate issues

set -e  # Exit on any error

echo "🚀 Nix installation and SSL patch for Amazon Linux 2..."

# Step 1: Fix SSL certificates for Amazon Linux 2
echo "🔒 Updating SSL certificates for Amazon Linux 2..."
sudo yum update -y ca-certificates || sudo yum install -y ca-certificates
echo "✅ SSL certificates updated"

# Step 2: Install Nix using standard nixos.org installer
echo "📦 Installing Nix with standard installer..."
sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install) --daemon
echo "✅ Nix installation completed"

echo ""
echo "🎉 Nix installation and SSL patch complete!"
echo "💡 You may need to restart your shell or run:"
echo "    source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
echo ""
echo "You can now run: ./run.sh"