#!/bin/bash
# SSH Setup Script - Universal pattern for all systems
# Runs during home-manager activation to setup SSH config
# - Adds Include directive for nix-managed config
# - Creates SSH key interactively if missing
# - Uploads key to GitHub (if GH_TOKEN set)

SSH_DIR="$HOME/.ssh"
SSH_CONFIG="$SSH_DIR/config"
KNOWN_HOSTS="$SSH_DIR/known_hosts"
KNOWN_HOSTS_NIX="$SSH_DIR/known_hosts_nix"
GITHUB_KEY="$SSH_DIR/github_id"
INCLUDE_LINE="Include ~/.ssh/config_nix"

# Backup function for safety
backup_file() {
  local file="$1"
  if [[ -f "$file" ]]; then
    cp "$file" "$file.pre-activation-backup"
  fi
}

# Restore function for rollback
restore_file() {
  local file="$1"
  if [[ -f "$file.pre-activation-backup" ]]; then
    mv "$file.pre-activation-backup" "$file"
  fi
}

echo "=== SSH Setup ==="

# Ensure .ssh directory exists
if [[ ! -d "$SSH_DIR" ]]; then
  mkdir -p "$SSH_DIR"
  chmod 700 "$SSH_DIR"
fi

# Backup config before modifications
backup_file "$SSH_CONFIG"

# Create config if it doesn't exist
if [[ ! -f "$SSH_CONFIG" ]]; then
  touch "$SSH_CONFIG"
  chmod 600 "$SSH_CONFIG"
  echo "✓ Created SSH config"
fi

# Add Include directive at bottom if not present
if ! grep -qF "$INCLUDE_LINE" "$SSH_CONFIG" 2>/dev/null; then
  echo "" >> "$SSH_CONFIG"
  echo "$INCLUDE_LINE" >> "$SSH_CONFIG"
  echo "✓ Added Include directive to SSH config"
else
  echo "✓ Include directive already present"
fi

# Validate Include directive was added correctly
if ! grep -qF "$INCLUDE_LINE" "$SSH_CONFIG" 2>/dev/null; then
  echo "✗ Failed to add Include directive, rolling back"
  restore_file "$SSH_CONFIG"
  exit 1
fi

# Create known_hosts if it doesn't exist
if [[ ! -f "$KNOWN_HOSTS" ]]; then
  touch "$KNOWN_HOSTS"
  chmod 600 "$KNOWN_HOSTS"
  echo "✓ Created known_hosts"
fi

# Create/recreate known_hosts_nix if it doesn't exist or is a symlink
# (home-manager may create it as a symlink, which is read-only)
if [[ -L "$KNOWN_HOSTS_NIX" ]]; then
  rm "$KNOWN_HOSTS_NIX"
fi

if [[ ! -f "$KNOWN_HOSTS_NIX" ]]; then
  touch "$KNOWN_HOSTS_NIX"
  chmod 600 "$KNOWN_HOSTS_NIX"
fi

# Fetch GitHub public keys to nix-managed known_hosts_nix
# This ensures keys are always up-to-date from GitHub's servers
echo "Fetching GitHub public keys..."
# ssh-keyscan outputs keys to stdout, connection info to stderr
# -T flag sets timeout (10 seconds)
# Fetch keys for both hostname and IP addresses to keep system known_hosts clean
TEMP_KEYS=$(mktemp)
# Use absolute /usr/bin paths since PATH is limited during home-manager activation
# (nix is installed but /usr/bin utilities are more reliable during bootstrap)
# Resolve github.com to IPs and scan both hostname and IPs
GITHUB_IPS=$(/usr/bin/host github.com 2>/dev/null | grep "has address" | /usr/bin/awk '{print $NF}')
if /usr/bin/timeout 10 /usr/bin/ssh-keyscan -T 10 github.com $GITHUB_IPS > "$TEMP_KEYS" 2>/dev/null && [[ -s "$TEMP_KEYS" ]]; then
  mv "$TEMP_KEYS" "$KNOWN_HOSTS_NIX"
  chmod 600 "$KNOWN_HOSTS_NIX"
  echo "✓ Added GitHub public keys to known_hosts_nix"
else
  rm -f "$TEMP_KEYS"
  echo "⚠️  Failed to fetch GitHub keys (network issue?). Keeping existing keys if any."
fi

# Handle GitHub SSH key
if [[ ! -f "$GITHUB_KEY" ]]; then
  echo ""
  echo "⚠️  GitHub SSH key not found"

  # Check if we're in interactive mode
  if [[ -t 0 ]]; then
    read -p "Create GitHub SSH key now? (y/n): " -r
    echo

    if [[ $REPLY =~ ^[Yy]$ ]]; then
      read -p "Enter your email for the SSH key: " -r KEY_EMAIL
      echo

      # Generate key (uses system ssh-keygen)
      echo "Generating SSH key..."
      ssh-keygen -t ed25519 -f "$GITHUB_KEY" -C "$KEY_EMAIL" -N ""

      if [[ -f "$GITHUB_KEY" ]]; then
        echo "✓ SSH key created"

        # Try to upload to GitHub if GH_TOKEN is set
        if [[ -n "${GH_TOKEN:-}" ]]; then
          echo "Authenticating with GitHub..."
          if echo "$GH_TOKEN" | gh auth login --with-token 2>/dev/null; then
            echo "Uploading key to GitHub..."
            if gh ssh-key add "$GITHUB_KEY.pub" --title "$(hostname)-$(date +%Y%m%d)" 2>/dev/null; then
              echo "✓ Key uploaded to GitHub"
            else
              echo "⚠️  Failed to upload key, please upload manually"
              echo "   Public key: $GITHUB_KEY.pub"
              echo "   Upload at: https://github.com/settings/ssh/new"
            fi
          fi
        else
          echo ""
          echo "To upload the key to GitHub:"
          echo "  1. Copy: cat $GITHUB_KEY.pub"
          echo "  2. Visit: https://github.com/settings/ssh/new"
          echo "  3. Paste and save"
          read -p "Press Enter after uploading..."
        fi

        # Test connection
        echo "Testing GitHub connection..."
        if ssh -T git@github.com 2>&1 | grep -q "successfully authenticated"; then
          echo "✓ GitHub SSH connection successful"
        else
          echo "⚠️  Connection test failed (key may need time to propagate)"
        fi
      else
        echo "✗ Failed to create SSH key"
      fi
    else
      echo "Skipping SSH key creation"
      echo "  Create later: ssh-keygen -t ed25519 -f $GITHUB_KEY"
      echo "  Upload: https://github.com/settings/ssh/new"
    fi
  else
    echo "  Not in interactive mode, skipping key creation"
    echo "  Create: ssh-keygen -t ed25519 -f $GITHUB_KEY -C 'your-email'"
    echo "  Upload: https://github.com/settings/ssh/new"
  fi
else
  echo "✓ GitHub SSH key exists"
fi

# Clean up backup if everything succeeded
if [[ -f "$SSH_CONFIG.pre-activation-backup" ]]; then
  rm "$SSH_CONFIG.pre-activation-backup"
fi

echo "=== SSH Setup Complete ==="
