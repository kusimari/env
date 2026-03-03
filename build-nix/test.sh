#!/bin/bash

# Evaluate the flake to catch Nix errors without building anything.
# Tests both Linux homeConfigurations (ubuntu, kelasa-al2).

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
USER_HOST_FILE="$SCRIPT_DIR/../home/user-host.nix"
FLAKE_DIR="$SCRIPT_DIR/.."

USER_VALUE=$(whoami)
HOSTNAME_VALUE=$(hostname)

restore_placeholders() {
    sed -i "s|user = \"$USER_VALUE\";|user = \"replace-user\";|g; s|hostName = \"$HOSTNAME_VALUE\";|hostName = \"replace-hostname\";|g" "$USER_HOST_FILE"
}

trap restore_placeholders EXIT

echo "Replacing placeholders..."
sed -i "s|user = \"replace-user\";|user = \"$USER_VALUE\";|g; s|hostName = \"replace-hostname\";|hostName = \"$HOSTNAME_VALUE\";|g" "$USER_HOST_FILE"

echo ""
echo "Evaluating homeConfigurations.ubuntu..."
nix eval "$FLAKE_DIR#homeConfigurations.ubuntu.activationPackage.name"

echo "Evaluating homeConfigurations.kelasa-al2..."
nix eval "$FLAKE_DIR#homeConfigurations.kelasa-al2.activationPackage.name"

echo ""
echo "Flake OK."
