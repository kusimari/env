#!/bin/bash
#
# GCP Compute Engine Startup Script for Ubuntu -> NixOS Conversion
# This script runs on first boot of Ubuntu 24.04 LTS and converts it to NixOS
# using nixos-infect, then applies our configuration
#
# Flow:
# 1. Ubuntu boot → run nixos-infect → reboot
# 2. NixOS boot → GCP re-runs startup script → detect /etc/NIXOS → apply flake config
#

set -euo pipefail

# Configuration
FLAKE_CONFIG="github:kusimari/env#cloud"
USERNAME="kusimari"
LOG_FILE="/var/log/cloud-init-custom.log"
NIXOS_INFECT_URL="https://raw.githubusercontent.com/elitak/nixos-infect/master/nixos-infect"

# Logging function
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
}

# ============================================================
# Post-reboot path: NixOS is running, apply our flake config
# ============================================================
if [[ -f /etc/NIXOS ]]; then
    log "=== GCP NixOS Post-Reboot Configuration ==="

    # Skip if already completed successfully
    if [[ -f /var/lib/cloud-init-success ]]; then
        log "Cloud init already completed, skipping"
        exit 0
    fi

    log "Detected NixOS system, applying configuration..."

    # Wait for network
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

    log "=== GCP NixOS Cloud Init Completed Successfully ==="
    log "System Information:"
    log "Hostname: $(hostname)"
    log "NixOS Version: $(nixos-version 2>/dev/null || echo 'Unknown')"
    log "User: $USERNAME"
    log "SSH Service: $(systemctl is-active sshd)"
    log "Cloud VM is ready for use!"

    exit 0
fi

# ============================================================
# First boot path: Ubuntu → nixos-infect conversion
# ============================================================
log "=== GCP Ubuntu -> NixOS Conversion Started ==="

# Update package database
log "Updating Ubuntu package database..."
export DEBIAN_FRONTEND=noninteractive
apt-get update

# Install required packages for nixos-infect
log "Installing dependencies..."
apt-get install -y curl sudo jq

# Download and prepare nixos-infect
log "Downloading nixos-infect..."
curl -fsSL "$NIXOS_INFECT_URL" -o /root/nixos-infect
chmod +x /root/nixos-infect

# Create a minimal NixOS configuration for nixos-infect
# This is the bootstrap config — our flake config replaces it after reboot
log "Creating basic NixOS configuration for nixos-infect..."
mkdir -p /etc/nixos

cat > /etc/nixos/configuration.nix <<'NIXEOF'
{ config, pkgs, ... }:
{
  imports = [ ./hardware-configuration.nix ];

  # Enable SSH with key-only access
  services.openssh.enable = true;
  services.openssh.settings.PermitRootLogin = "prohibit-password";

  # Enable flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Network configuration
  networking.useDHCP = true;
  networking.firewall.allowedTCPPorts = [ 22 ];

  system.stateVersion = "24.05";
}
NIXEOF

log "Starting nixos-infect conversion..."
log "After reboot, GCP will re-run this startup script which will detect NixOS and apply the flake config"

# Set environment variables for nixos-infect
export NIX_CHANNEL=nixos-24.05

# Run nixos-infect (this will reboot the system)
/root/nixos-infect --no-reboot || {
    log "ERROR: nixos-infect failed"
    exit 1
}

log "nixos-infect completed, rebooting to NixOS..."
reboot
