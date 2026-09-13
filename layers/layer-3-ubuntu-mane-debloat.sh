#!/usr/bin/env bash
# env/layers/layer-3-ubuntu-mane-debloat.sh — Sub-script for
# ubuntu-mane Layer 3.
#
# Purges default Ubuntu desktop bloatware after Nix declarative
# userland has been activated. Gracefully degrades if packages are
# absent, allowing clean execution across Ubuntu versions and
# minimal base installs.
#
# Run standalone or chained at the tail of layer-3-ubuntu-mane.sh.
# Honors --dry-run.

set -euo pipefail

log() { printf '==> %s\n' "$*"; }

DRY_RUN=0
for arg in "$@"; do
    [[ "$arg" = "--dry-run" ]] && DRY_RUN=1
done

run() {
    if (( DRY_RUN )); then
        printf 'dry-run: %s\n' "$*"
    else
        "$@"
    fi
}

# Extensible list of default Ubuntu desktop bloat packages.
# Add new Canonical desktop additions here.
BLOAT_PACKAGES=(
    # Office
    libreoffice-common
    libreoffice-core
    # Mail & Communication
    thunderbird
    # Audio / Video players (Nix provides DigiKam / Viewnior)
    rhythmbox
    totem
    totem-common
    # Torrent & Remote Desktop
    transmission-gtk
    transmission-common
    remmina
    # GNOME Games
    gnome-sudoku
    gnome-mines
    gnome-mahjongg
    aisleriot
    # Host file manager & desktop icons (replaced by Nix Thunar)
    nautilus
    nautilus-share
    gnome-shell-extension-desktop-icons-ng
    # Default viewers/editors (replaced by Emacs / Viewnior)
    gedit
    eog
)

log "Checking for default Ubuntu desktop bloatware$( (( DRY_RUN )) && echo ' (dry-run)')"

installed=()
for pkg in "${BLOAT_PACKAGES[@]}"; do
    if dpkg -s "$pkg" >/dev/null 2>&1; then
        installed+=("$pkg")
    fi
done

if (( ${#installed[@]} > 0 )); then
    log "Found ${#installed[@]} bloat package(s) installed: ${installed[*]}"
    echo "Approve sudo when prompted to purge host bloatware..."

    run sudo apt-get purge -y --auto-remove "${installed[@]}"
    run sudo apt-get clean
else
    log "Host packages already lean. No bloatware packages found."
fi

# Clean userland desktop entries bloat (e.g. Chrome PWAs masquerading as apps, broken symlinks)
user_apps_dir="$HOME/.local/share/applications"
if [[ -d "$user_apps_dir" ]]; then
    # Purge Chrome web-app desktop files masquerading as native apps
    shopt -s nullglob
    pwa_files=("$user_apps_dir"/chrome-*.desktop)
    if (( ${#pwa_files[@]} > 0 )); then
        log "Purging ${#pwa_files[@]} web-app shortcut(s) from $user_apps_dir"
        for pwa in "${pwa_files[@]}"; do
            run rm -f "$pwa"
        done
    fi

    # Remove broken symlinks
    while IFS= read -r -d '' broken; do
        log "Removing broken symlink in $user_apps_dir: $(basename "$broken")"
        run rm -f "$broken"
    done < <(find -L "$user_apps_dir" -maxdepth 1 -type l -print0)
    shopt -u nullglob

    if command -v update-desktop-database >/dev/null 2>&1; then
        run update-desktop-database "$user_apps_dir"
    fi
fi

log "Debloat complete. Host and userland are lean."
