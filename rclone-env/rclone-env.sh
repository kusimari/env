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

# All remotes known to rclone
_rclone_remotes() {
  rclone listremotes 2>/dev/null
}

# Mount point for a remote from our config file (empty if not set)
_get_mountpoint_for() {
  local target="$1"
  [[ -f "$REMOTES_FILE" ]] || return
  grep -v '^\s*#' "$REMOTES_FILE" | grep -v '^\s*$' | awk -v r="$target" '$1 == r {print $2; exit}'
}

# All remotes in our config file with their mount points
_parse_remotes() {
  [[ -f "$REMOTES_FILE" ]] || return
  grep -v '^\s*#' "$REMOTES_FILE" | grep -v '^\s*$' | awk '{print $1, $2}'
}

_is_mounted() {
  local mp="$1"
  mount | grep -qF " $mp " 2>/dev/null || mount | grep -qF " $mp" 2>/dev/null
}

cmd_list() {
  local found=0
  while IFS= read -r remote; do
    [[ -z "$remote" ]] && continue
    found=1
    local mountpoint mp
    mountpoint=$(_get_mountpoint_for "$remote")
    if [[ -z "$mountpoint" ]]; then
      echo "  [no mount]  $remote"
    else
      mp=$(_resolve_path "$mountpoint")
      if _is_mounted "$mp"; then
        echo "  [mounted]   $remote -> $mp"
      else
        echo "  [unmounted] $remote -> $mp"
      fi
    fi
  done < <(_rclone_remotes)
  [[ $found -eq 0 ]] && echo "No rclone remotes configured. Run 'rclone config' or 'rclone-env add' to add one."
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
  local -a unmapped=()
  while IFS= read -r remote; do
    [[ -z "$(_get_mountpoint_for "$remote")" ]] && unmapped+=("$remote")
  done < <(_rclone_remotes)

  local remote_name
  if [[ ${#unmapped[@]} -gt 0 ]]; then
    echo "Existing remotes without a mount point:"
    select remote_name in "${unmapped[@]}" "Configure a new remote"; do
      [[ -n "$remote_name" ]] && break
    done
    if [[ "$remote_name" == "Configure a new remote" ]]; then
      rclone config
      read -rp "Enter the rclone remote name just configured (e.g. 'gdrive:'): " remote_name
    fi
  else
    echo "Launching rclone config to set up a new remote..."
    rclone config
    read -rp "Enter the rclone remote name just configured (e.g. 'gdrive:'): " remote_name
  fi

  read -rp "Enter the local mount point for $remote_name (e.g. ~/mnt/gdrive): " mount_point
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
