#!/bin/bash

# Comprehensive SSL certificate fix for Amazon Linux 2 and Nix
# This script tries multiple approaches to fix SSL certificate issues

set -e  # Exit on any error

echo "🔒 Comprehensive SSL certificate fix for Amazon Linux 2..."

# Step 1: Update all certificate packages
echo "📦 Updating certificate packages..."
sudo yum update -y ca-certificates
sudo yum install -y ca-certificates openssl
echo "✅ Certificate packages updated"

# Step 2: Set SSL_CERT_FILE environment variable system-wide
echo "🔧 Setting SSL_CERT_FILE environment variable..."
SSL_CERT_PATH="/etc/ssl/certs/ca-bundle.crt"

# Check if the certificate bundle exists
if [ -f "$SSL_CERT_PATH" ]; then
    echo "Found SSL certificate bundle at: $SSL_CERT_PATH"

    # Add to system-wide environment
    echo "export SSL_CERT_FILE=$SSL_CERT_PATH" | sudo tee -a /etc/environment

    # Add to user's shell profiles
    echo "export SSL_CERT_FILE=$SSL_CERT_PATH" >> ~/.bashrc
    echo "export SSL_CERT_FILE=$SSL_CERT_PATH" >> ~/.profile

    # Set for current session
    export SSL_CERT_FILE="$SSL_CERT_PATH"

    echo "✅ SSL_CERT_FILE set to $SSL_CERT_PATH"
else
    echo "⚠️  Standard certificate bundle not found, trying alternatives..."

    # Try alternative locations
    for cert_path in "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/cert.pem" "/usr/share/pki/ca-trust-source/anchors/ca-bundle.crt"; do
        if [ -f "$cert_path" ]; then
            echo "Found alternative certificate bundle at: $cert_path"
            export SSL_CERT_FILE="$cert_path"
            echo "export SSL_CERT_FILE=$cert_path" >> ~/.bashrc
            echo "export SSL_CERT_FILE=$cert_path" >> ~/.profile
            echo "✅ SSL_CERT_FILE set to $cert_path"
            break
        fi
    done
fi

# Step 3: Configure Nix-specific SSL settings
echo "🔧 Configuring Nix SSL settings..."
mkdir -p ~/.config/nix

# Create or update nix.conf with SSL settings
cat >> ~/.config/nix/nix.conf << EOF
# SSL certificate configuration for Amazon Linux 2
ssl-cert-file = ${SSL_CERT_FILE:-/etc/ssl/certs/ca-bundle.crt}
# Enable experimental features
experimental-features = nix-command flakes
EOF

echo "✅ Nix SSL configuration updated"

# Step 4: Test SSL connectivity
echo "🧪 Testing SSL connectivity..."
echo "Current SSL_CERT_FILE: $SSL_CERT_FILE"

# Test with curl first
echo "Testing HTTPS connectivity with curl..."
if curl -s --connect-timeout 10 https://cache.nixos.org >/dev/null; then
    echo "✅ HTTPS connectivity working with curl"
else
    echo "❌ HTTPS connectivity failed with curl"
fi

# Test with Nix
echo "Testing Nix with SSL configuration..."
if SSL_CERT_FILE="$SSL_CERT_FILE" nix --extra-experimental-features "nix-command flakes" eval --expr '1 + 1'; then
    echo "✅ Nix evaluation working with SSL configuration"
else
    echo "❌ Nix evaluation failed - may need to restart shell"
fi

echo ""
echo "🎉 SSL configuration complete!"
echo "💡 Important: Restart your shell or run:"
echo "    source ~/.bashrc"
echo "    export SSL_CERT_FILE=$SSL_CERT_FILE"
echo ""
echo "Then try: ./run.sh"