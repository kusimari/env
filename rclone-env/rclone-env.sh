#!/usr/bin/env bash
set -euo pipefail

CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/rclone-env"
REMOTES_FILE="$CONFIG_DIR/remotes"

_ensure_config() {
  if [[ ! -f "$REMOTES_FILE" ]]; then
    mkdir -p "$CONFIG_DIR"
    cat > "$REMOTES_FILE" <<'EOF'
# rclone-env remotes config
# Format: rclone-remote     mount-point
# Example:
# gdrive:             ~/mnt/gdrive
# myserver:photos     ~/mnt/photos
EOF
    echo "Created config file at $REMOTES_FILE"
  fi
}

_resolve_path() {
  local p="$1"
  echo "${p/#\~/$HOME}"
}

_parse_remotes() {
  grep -v '^\s*#' "$REMOTES_FILE" | grep -v '^\s*$' | awk '{print $1, $2}'
}

_is_mounted() {
  local mp="$1"
  mount | grep -qF " $mp " 2>/dev/null || mount | grep -qF " $mp" 2>/dev/null
}

_get_mountpoint_for() {
  local target="$1"
  local mountpoint=""
  while IFS=' ' read -r remote mp; do
    [[ "$remote" == "$target" ]] && mountpoint="$mp" && break
  done < <(_parse_remotes)
  echo "$mountpoint"
}

cmd_list() {
  _ensure_config
  local found=0
  while IFS=' ' read -r remote mountpoint; do
    [[ -z "$remote" ]] && continue
    found=1
    local mp
    mp=$(_resolve_path "$mountpoint")
    if _is_mounted "$mp"; then
      echo "  [mounted]   $remote -> $mp"
    else
      echo "  [unmounted] $remote -> $mp"
    fi
  done < <(_parse_remotes)
  [[ $found -eq 0 ]] && echo "No remotes configured. Use 'rclone-env add' to add one."
}

cmd_mount() {
  _ensure_config
  local target="${1:-}"

  if [[ -z "$target" ]]; then
    local -a options=()
    while IFS=' ' read -r remote mountpoint; do
      [[ -z "$remote" ]] && continue
      local mp
      mp=$(_resolve_path "$mountpoint")
      _is_mounted "$mp" || options+=("$remote")
    done < <(_parse_remotes)

    if [[ ${#options[@]} -eq 0 ]]; then
      echo "All remotes are already mounted."
      return 0
    fi

    echo "Select remote to mount:"
    select target in "${options[@]}"; do
      [[ -n "$target" ]] && break
    done
  fi

  local mountpoint
  mountpoint=$(_get_mountpoint_for "$target")
  if [[ -z "$mountpoint" ]]; then
    echo "Error: remote '$target' not found in config." >&2
    exit 1
  fi

  local mp
  mp=$(_resolve_path "$mountpoint")

  if _is_mounted "$mp"; then
    echo "$target is already mounted at $mp"
    return 0
  fi

  mkdir -p "$mp"
  echo "Mounting $target -> $mp"
  rclone mount "$target" "$mp" --daemon
  echo "Mounted."
}

cmd_umount() {
  _ensure_config
  local target="${1:-}"

  if [[ -z "$target" ]]; then
    local -a options=()
    while IFS=' ' read -r remote mountpoint; do
      [[ -z "$remote" ]] && continue
      local mp
      mp=$(_resolve_path "$mountpoint")
      _is_mounted "$mp" && options+=("$remote") || true
    done < <(_parse_remotes)

    if [[ ${#options[@]} -eq 0 ]]; then
      echo "No remotes are currently mounted."
      return 0
    fi

    echo "Select remote to unmount:"
    select target in "${options[@]}"; do
      [[ -n "$target" ]] && break
    done
  fi

  local mountpoint
  mountpoint=$(_get_mountpoint_for "$target")
  if [[ -z "$mountpoint" ]]; then
    echo "Error: remote '$target' not found in config." >&2
    exit 1
  fi

  local mp
  mp=$(_resolve_path "$mountpoint")

  if ! _is_mounted "$mp"; then
    echo "$target is not mounted."
    return 0
  fi

  echo "Unmounting $target at $mp"
  if [[ "$(uname)" == "Darwin" ]]; then
    umount "$mp"
  else
    fusermount -u "$mp"
  fi
  echo "Unmounted."
}

cmd_add() {
  _ensure_config
  echo "Launching rclone config to set up a new remote..."
  rclone config
  echo ""
  read -rp "Enter the rclone remote name (e.g. 'gdrive:' or 'myserver:photos'): " remote_name
  read -rp "Enter the local mount point (e.g. ~/mnt/gdrive): " mount_point
  echo "$remote_name    $mount_point" >> "$REMOTES_FILE"
  echo "Added: $remote_name -> $mount_point"
}

cmd_remove() {
  local target="${1:-}"
  if [[ -z "$target" ]]; then
    echo "Usage: rclone-env remove <remote>" >&2
    exit 1
  fi
  _ensure_config
  local tmp
  tmp=$(mktemp)
  grep -v "^[[:space:]]*${target}[[:space:]]" "$REMOTES_FILE" > "$tmp" || true
  mv "$tmp" "$REMOTES_FILE"
  echo "Removed $target from config (rclone.conf unchanged)."
}

# Main
subcommand="${1:-}"
shift || true

case "$subcommand" in
  list|status) cmd_list ;;
  mount)       cmd_mount "${1:-}" ;;
  umount)      cmd_umount "${1:-}" ;;
  add)         cmd_add ;;
  remove)      cmd_remove "${1:-}" ;;
  *)
    echo "Usage: rclone-env <command> [args]"
    echo ""
    echo "Commands:"
    echo "  list, status       Show all configured remotes and their mount status"
    echo "  mount [remote]     Mount a remote (interactive prompt if no arg)"
    echo "  umount [remote]    Unmount a remote (interactive prompt if no arg)"
    echo "  add                Configure a new remote interactively"
    echo "  remove <remote>    Remove a remote from config (does not modify rclone.conf)"
    exit 1
    ;;
esac
