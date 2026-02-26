#!/bin/bash

# Fix Nix single-user mode setup for Amazon Linux 2
# This script switches Nix from multi-user to single-user mode to avoid nixbld group issues

set -e  # Exit on any error

echo "🔧 Setting up Nix single-user mode..."

# Step 1: Create Nix config directory
echo "📁 Creating Nix config directory..."
mkdir -p ~/.config/nix

# Step 2: Configure Nix to use single-user mode
echo "⚙️  Configuring single-user mode..."
echo "use-daemon = false" >> ~/.config/nix/nix.conf

# Step 3: Stop and disable the nix-daemon
echo "🛑 Stopping nix-daemon..."
sudo systemctl stop nix-daemon || echo "⚠️  nix-daemon was not running"
sudo systemctl disable nix-daemon || echo "⚠️  nix-daemon was not enabled"

# Step 4: Verify the configuration
echo "✅ Verifying configuration..."
echo "Nix config file contents:"
cat ~/.config/nix/nix.conf

echo ""
echo "Nix daemon status:"
sudo systemctl status nix-daemon --no-pager -l || echo "✅ nix-daemon is stopped"

# Step 5: Test if Nix works in single-user mode
echo ""
echo "🧪 Testing Nix in single-user mode..."
if nix run nixpkgs#hello; then
    echo "✅ Nix single-user mode is working!"
else
    echo "❌ Nix test failed"
    exit 1
fi

echo ""
echo "🎉 Nix single-user mode setup complete!"
echo "You can now run: nix run home-manager/master -- init --switch \".#kelasa-al2\""