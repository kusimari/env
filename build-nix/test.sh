#!/bin/bash

# Build the flake to catch both evaluation and builder errors.
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
echo "Building homeConfigurations.ubuntu-mane..."
nix build "$FLAKE_DIR#homeConfigurations.ubuntu-mane.activationPackage" --no-link

echo "Building homeConfigurations.al2-kelasa..."
nix build "$FLAKE_DIR#homeConfigurations.al2-kelasa.activationPackage" --no-link

echo ""
echo "Flake OK."

echo ""
echo "Running emacs gittree tests..."
if bash "$FLAKE_DIR/emacs/tests/run.sh"; then
    echo "Emacs tests OK."
else
    echo "Emacs tests FAILED."
    exit 1
fi
