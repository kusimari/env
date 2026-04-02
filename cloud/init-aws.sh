#!/bin/bash
#
# AWS EC2 User Data Script for NixOS AMI
# This script runs on first boot of an official NixOS AMI
# The AMI already has NixOS installed, so we can directly apply our configuration
#

set -euo pipefail

# Configuration
FLAKE_CONFIG="github:kusimari/env#cloud"
USERNAME="kusimari"
LOG_FILE="/var/log/cloud-init-custom.log"

# Logging function
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
}

log "=== AWS NixOS Cloud Init Started ==="

# Wait for network to be ready
log "Waiting for network connectivity..."
timeout=60
while ! curl -s --connect-timeout 5 https://api.github.com >/dev/null 2>&1; do
    if [ $timeout -le 0 ]; then
        log "ERROR: Network connectivity timeout"
        exit 1
    fi
    sleep 5
    ((timeout-=5))
done
log "Network connectivity confirmed"

# Apply NixOS configuration first (this creates the user)
log "Applying NixOS configuration from: $FLAKE_CONFIG"
nixos-rebuild switch --flake "$FLAKE_CONFIG" || {
    log "ERROR: nixos-rebuild failed"
    exit 1
}
log "NixOS configuration applied successfully"

# Import SSH keys from GitHub (after user exists from nixos-rebuild)
log "Importing SSH keys from GitHub for user: $USERNAME"
mkdir -p /home/$USERNAME/.ssh
curl -fsSL "https://api.github.com/users/$USERNAME/keys" | \
    jq -r '.[].key' > /home/$USERNAME/.ssh/authorized_keys || {
    log "ERROR: Failed to import SSH keys from GitHub"
    exit 1
}
chmod 600 /home/$USERNAME/.ssh/authorized_keys
chmod 700 /home/$USERNAME/.ssh
chown -R $USERNAME:users /home/$USERNAME/.ssh
log "SSH keys imported successfully"

# Create success marker
touch /var/lib/cloud-init-success
echo "Cloud VM initialized at $(date)" > /var/lib/cloud-init-success

log "=== AWS NixOS Cloud Init Completed Successfully ==="
log "System Information:"
log "Hostname: $(hostname)"
log "NixOS Version: $(nixos-version)"
log "User: $USERNAME"
log "SSH Service: $(systemctl is-active sshd)"
log "Cloud VM is ready for use!"
