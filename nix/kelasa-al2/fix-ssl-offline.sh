#!/bin/bash

# Alternative SSL fix: disable substituters temporarily for Amazon Linux 2

set -e  # Exit on any error

echo "🔧 Alternative SSL fix: using offline/build-from-source approach..."

# Step 1: Configure Nix to build from source instead of downloading
echo "⚙️  Configuring Nix for offline/build-from-source mode..."

# Add current user as trusted and disable substituters
sudo mkdir -p /etc/nix
sudo tee /etc/nix/nix.conf << EOF
# Trusted users
trusted-users = root $(whoami)

# Experimental features
experimental-features = nix-command flakes

# Disable binary cache substituters to avoid SSL issues
substituters =
require-sigs = false

# Build everything from source
builders =

# Other settings
max-jobs = auto
cores = 0
EOF

echo "✅ Configured Nix for build-from-source mode (no binary cache)"

# Step 2: Restart nix-daemon
echo "🔄 Restarting nix-daemon..."
sudo systemctl restart nix-daemon
sleep 2

# Step 3: Test with a simple build
echo "🧪 Testing build-from-source approach..."
if nix --extra-experimental-features "nix-command flakes" eval --expr '1 + 1'; then
    echo "✅ Basic nix evaluation working without binary cache"
else
    echo "❌ Nix evaluation failed even without binary cache"
    exit 1
fi

echo ""
echo "🎉 Offline/build-from-source configuration complete!"
echo "⚠️  Note: This will build packages from source (slower but avoids SSL issues)"
echo ""
echo "Try running: ./run.sh"
echo ""
echo "💡 To re-enable binary cache later, remove 'substituters =' line from /etc/nix/nix.conf"