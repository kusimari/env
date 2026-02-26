#!/bin/bash

# Fix terminal encoding and locale on Amazon Linux 2

echo "🔤 Fixing terminal encoding and locale for AL2..."

# Install glibc-langpack for UTF-8 support
echo "📦 Installing UTF-8 locale support..."
sudo yum install -y glibc-langpack-en

# Generate locale if needed
echo "🌐 Setting up UTF-8 locale..."
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Add to system environment
echo 'export LANG=en_US.UTF-8' | sudo tee -a /etc/environment
echo 'export LC_ALL=en_US.UTF-8' | sudo tee -a /etc/environment

# Test locale
echo "🧪 Testing locale settings..."
locale
echo ""
echo "Current LANG: $LANG"
echo "Current LC_ALL: $LC_ALL"

echo ""
echo "✅ Terminal encoding fix complete!"
echo "💡 Restart your shell: exec zsh"