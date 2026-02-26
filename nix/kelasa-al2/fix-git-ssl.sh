#!/bin/bash

# Fix SSL certificates for git operations on Amazon Linux 2

echo "🔒 Fixing SSL certificates for git operations on AL2..."

# Step 1: Update SSL certificates
echo "📦 Updating SSL certificates..."
sudo yum update -y ca-certificates openssl

# Step 2: Configure git to use system certificates
echo "⚙️  Configuring git SSL settings..."
git config --global http.sslCAInfo /etc/ssl/certs/ca-bundle.crt
git config --global http.sslverify true

# Step 3: Set SSL environment variables for current session
echo "🌐 Setting SSL environment variables..."
export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
export CURL_CA_BUNDLE=/etc/ssl/certs/ca-bundle.crt

# Add to bash profiles
echo 'export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt' >> ~/.bashrc
echo 'export CURL_CA_BUNDLE=/etc/ssl/certs/ca-bundle.crt' >> ~/.bashrc

# Step 4: Test git SSL connectivity
echo "🧪 Testing git SSL connectivity..."
if git ls-remote https://github.com/kusimari/env.git HEAD; then
    echo "✅ Git SSL connectivity working!"
else
    echo "❌ Git SSL still failing - try alternative certificate path"

    # Try alternative certificate paths
    for cert_path in "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/cert.pem"; do
        if [ -f "$cert_path" ]; then
            echo "Trying alternative certificate: $cert_path"
            git config --global http.sslCAInfo "$cert_path"
            if git ls-remote https://github.com/kusimari/env.git HEAD; then
                echo "✅ Git working with: $cert_path"
                export SSL_CERT_FILE="$cert_path"
                export CURL_CA_BUNDLE="$cert_path"
                break
            fi
        fi
    done
fi

echo ""
echo "🎉 Git SSL configuration complete!"
echo "💡 Restart shell or run: source ~/.bashrc"