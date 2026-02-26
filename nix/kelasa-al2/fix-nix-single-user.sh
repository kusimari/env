#!/bin/bash

# Fix Nix single-user mode setup for Amazon Linux 2
# This script switches Nix from multi-user to single-user mode to avoid nixbld group issues

set -e  # Exit on any error

echo "🔧 Setting up Nix single-user mode..."

# Step 1: Stop and disable the nix-daemon
echo "🛑 Stopping nix-daemon..."
sudo systemctl stop nix-daemon || echo "⚠️  nix-daemon was not running"
sudo systemctl disable nix-daemon || echo "⚠️  nix-daemon was not enabled"

# Step 2: Configure environment for single-user mode
echo "⚙️  Configuring single-user mode..."
echo 'export NIX_REMOTE=""' >> ~/.bashrc
echo 'export NIX_REMOTE=""' >> ~/.profile

# Step 3: Set single-user mode for current session
export NIX_REMOTE=""

# Step 4: Verify the configuration
echo "✅ Verifying configuration..."
echo "NIX_REMOTE is set to: '$NIX_REMOTE'"
echo ""
echo "Nix daemon status:"
sudo systemctl status nix-daemon --no-pager -l || echo "✅ nix-daemon is stopped"

# Step 5: Test if Nix works in single-user mode
echo ""
echo "🧪 Testing Nix in single-user mode..."
if NIX_REMOTE="" nix run nixpkgs#hello; then
    echo "✅ Nix single-user mode is working!"
else
    echo "❌ Nix test failed"
    exit 1
fi

echo ""
echo "🎉 Nix single-user mode setup complete!"
echo "💡 Note: You may need to restart your shell or run 'source ~/.bashrc'"
echo ""
echo "You can now run:"
echo "  export NIX_REMOTE=\"\""
echo "  nix run home-manager/master -- init --switch \".#kelasa-al2\""