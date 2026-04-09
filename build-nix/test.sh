#!/bin/bash

# Evaluate the flake to catch Nix errors without building anything.
# Tests both Linux homeConfigurations (ubuntu-mane, al2-kelasa).

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
echo "Evaluating homeConfigurations.ubuntu-mane..."
nix eval "$FLAKE_DIR#homeConfigurations.ubuntu-mane.activationPackage.name"

echo "Evaluating homeConfigurations.al2-kelasa..."
nix eval "$FLAKE_DIR#homeConfigurations.al2-kelasa.activationPackage.name"

echo ""
echo "Flake OK."
