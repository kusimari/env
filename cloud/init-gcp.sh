#!/bin/bash
#
# GCP Compute Engine Startup Script for Ubuntu -> NixOS Conversion
# This script runs on first boot of Ubuntu 24.04 LTS and converts it to NixOS
# using nixos-infect, then applies our configuration
#

set -euo pipefail

# Configuration
REPO_URL="https://github.com/kusimari/env"
FLAKE_CONFIG="github:kusimari/env#cloud"
USERNAME="kusimari"
LOG_FILE="/var/log/cloud-init-custom.log"
NIXOS_INFECT_URL="https://raw.githubusercontent.com/elitak/nixos-infect/master/nixos-infect"

# Logging function
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$LOG_FILE"
}

# Check if we're post-reboot (NixOS is running)
if [[ -f /etc/NIXOS ]]; then
    log "=== GCP NixOS Post-Reboot Configuration ==="

    # We're now running NixOS after nixos-infect conversion
    log "Detected NixOS system, applying configuration..."

    # Import SSH keys from GitHub
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

    # Apply NixOS configuration
    log "Applying NixOS configuration from: $FLAKE_CONFIG"
    nixos-rebuild switch --flake "$FLAKE_CONFIG" || {
        log "ERROR: nixos-rebuild failed"
        exit 1
    }

    log "NixOS configuration applied successfully"

    # Create success marker
    touch /var/lib/cloud-init-success
    echo "Cloud VM initialized at $(date)" > /var/lib/cloud-init-success

    # Clean up the systemd service
    systemctl disable nixos-cloud-init.service || true
    rm -f /etc/systemd/system/nixos-cloud-init.service

    log "=== GCP NixOS Cloud Init Completed Successfully ==="
    log "System Information:"
    log "Hostname: $(hostname)"
    log "NixOS Version: $(nixos-version 2>/dev/null || echo 'Unknown')"
    log "User: $USERNAME"
    log "SSH Service: $(systemctl is-active sshd)"
    log "Cloud VM is ready for use!"

    exit 0
fi

# We're running Ubuntu, start nixos-infect process
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

# Create systemd service for post-reboot configuration
log "Creating post-reboot systemd service..."
cat > /tmp/nixos-cloud-init.service <<EOF
[Unit]
Description=NixOS Cloud Init Post-Reboot Configuration
After=network-online.target
Wants=network-online.target

[Service]
Type=oneshot
User=root
ExecStart=$0
RemainAfterExit=yes
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
EOF

# The service will be installed during nixos-infect, so we'll prepare it
mkdir -p /etc/systemd/system
cp /tmp/nixos-cloud-init.service /etc/systemd/system/
systemctl daemon-reload
systemctl enable nixos-cloud-init.service

# Create a minimal NixOS configuration for nixos-infect
log "Creating basic NixOS configuration for nixos-infect..."
mkdir -p /etc/nixos

cat > /etc/nixos/configuration.nix <<EOF
{ config, pkgs, ... }:
{
  imports = [ ./hardware-configuration.nix ];

  # Enable SSH
  services.openssh.enable = true;
  services.openssh.settings.PermitRootLogin = "yes";

  # Enable flakes
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Create initial user
  users.users.$USERNAME = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    shell = pkgs.bash;  # Will be changed by our flake config
  };

  # Enable sudo without password for wheel group (temporary)
  security.sudo.wheelNeedsPassword = false;

  # Network configuration
  networking.useDHCP = true;
  networking.firewall.allowedTCPPorts = [ 22 ];

  system.stateVersion = "24.05";
}
EOF

log "Starting nixos-infect conversion..."
log "This will reboot the system to complete the conversion to NixOS"

# Set environment variables for nixos-infect
export NIX_CHANNEL=nixos-24.05

# Run nixos-infect (this will reboot the system)
/root/nixos-infect --no-reboot || {
    log "ERROR: nixos-infect failed"
    exit 1
}

log "nixos-infect completed, rebooting to NixOS..."
reboot
