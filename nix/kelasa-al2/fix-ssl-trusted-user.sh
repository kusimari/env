#!/bin/bash

# Fix SSL certificates and trusted user settings for Nix on Amazon Linux 2

set -e  # Exit on any error

echo "🔒 Fixing SSL certificates and trusted user settings for Nix..."

# Step 1: Add current user as trusted user in system Nix configuration
echo "👤 Adding current user as trusted Nix user..."
sudo mkdir -p /etc/nix

# Check if nix.conf exists and backup
if [ -f /etc/nix/nix.conf ]; then
    sudo cp /etc/nix/nix.conf /etc/nix/nix.conf.backup
    echo "✅ Backed up existing /etc/nix/nix.conf"
fi

# Add trusted users and SSL settings to system config
sudo tee /etc/nix/nix.conf << EOF
# Trusted users (allows all settings)
trusted-users = root $(whoami)

# Experimental features
experimental-features = nix-command flakes

# SSL certificate configuration
ssl-cert-file = /etc/ssl/certs/ca-bundle.crt

# Build settings
max-jobs = auto
cores = 0
EOF

echo "✅ Updated system Nix configuration with trusted user: $(whoami)"

# Step 2: Update certificates and set environment variables
echo "🔧 Setting up SSL environment..."
sudo yum update -y ca-certificates

# Set SSL_CERT_FILE environment variable
export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
echo "export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt" >> ~/.bashrc
echo "export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt" >> ~/.profile

echo "✅ SSL environment configured"

# Step 3: Restart nix-daemon to pick up new config
echo "🔄 Restarting nix-daemon to apply changes..."
sudo systemctl restart nix-daemon
sleep 2
sudo systemctl status nix-daemon --no-pager -l

echo "✅ nix-daemon restarted"

# Step 4: Test the fix
echo "🧪 Testing SSL fix..."

# Test 1: Basic nix evaluation
echo "Testing basic nix evaluation..."
if SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt nix --extra-experimental-features "nix-command flakes" eval --expr '1 + 1'; then
    echo "✅ Basic nix evaluation working"
else
    echo "❌ Basic nix evaluation failed"
fi

# Test 2: Try to download something small
echo "Testing package download..."
if SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt nix --extra-experimental-features "nix-command flakes" eval --expr 'builtins.currentTime'; then
    echo "✅ Nix can evaluate and potentially download"
else
    echo "❌ Nix evaluation with download failed"
fi

echo ""
echo "🎉 SSL and trusted user configuration complete!"
echo "💡 Important: You may need to start a new shell session for all changes to take effect"
echo ""
echo "Try running: ./run.sh"