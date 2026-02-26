#!/bin/bash

# Fix nixbld group and users for Amazon Linux 2
# This script creates the missing nixbld group and users for multi-user Nix

set -e  # Exit on any error

echo "🔧 Setting up nixbld group and users for multi-user Nix..."

# Step 1: Check if nixbld group already exists
echo "📋 Checking existing nixbld group..."
if getent group nixbld >/dev/null 2>&1; then
    echo "✅ nixbld group already exists"
else
    echo "➕ Creating nixbld group..."
    sudo groupadd nixbld
    echo "✅ nixbld group created"
fi

# Step 2: Create nixbld users (typically 1-32)
echo ""
echo "👥 Creating nixbld users..."
for i in $(seq 1 10); do
    username="nixbld$i"
    if getent passwd "$username" >/dev/null 2>&1; then
        echo "✅ User $username already exists"
    else
        echo "➕ Creating user $username..."
        sudo useradd \
            -g nixbld \
            -G nixbld \
            -d /var/empty \
            -s $(which nologin) \
            -c "Nix build user $i" \
            -M \
            "$username"
        echo "✅ User $username created"
    fi
done

# Step 3: Verify the setup
echo ""
echo "🔍 Verifying nixbld setup..."
echo "nixbld group members:"
getent group nixbld

echo ""
echo "nixbld users:"
getent passwd | grep nixbld | head -5

# Step 4: Start and enable nix-daemon
echo ""
echo "🚀 Starting nix-daemon..."
sudo systemctl enable nix-daemon
sudo systemctl start nix-daemon
sudo systemctl status nix-daemon --no-pager -l

# Step 5: Test basic Nix functionality
echo ""
echo "🧪 Testing Nix with proper multi-user setup..."
if nix run nixpkgs#hello; then
    echo "✅ Nix multi-user mode is working!"
else
    echo "❌ Nix test failed"
    exit 1
fi

echo ""
echo "🎉 nixbld group and users setup complete!"
echo "You can now run: nix run home-manager/master -- init --switch \".#kelasa-al2\""