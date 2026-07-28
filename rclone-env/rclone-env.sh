#!/usr/bin/env bash
set -euo pipefail

RCLONE_FLAGS=(
  --transfers 16
  --checkers 16
  --drive-chunk-size 256M
  --stats 1s
  --progress
  --fast-list
)

CHECK_FLAGS=(
  --checksum
  --checkers 16
  --drive-chunk-size 256M
  --stats 1s
  --progress
  --fast-list
)

cmd_remotes() {
  rclone listremotes
}

cmd_add() {
  rclone config
}

cmd_ls() {
  local path="${1:-}"
  if [[ -z "$path" ]]; then
    echo "Usage: rclone-env ls <remote:path>" >&2
    exit 1
  fi
  rclone lsd "$path"
}

cmd_check() {
  local src="${1:-}" dst="${2:-}"
  if [[ -z "$src" || -z "$dst" ]]; then
    echo "Usage: rclone-env check <source> <dest>" >&2
    exit 1
  fi
  rclone check "${CHECK_FLAGS[@]}" "$src" "$dst"
}

cmd_copy() {
  local src="${1:-}" dst="${2:-}"
  if [[ -z "$src" || -z "$dst" ]]; then
    echo "Usage: rclone-env copy <source> <dest>" >&2
    exit 1
  fi
  echo "Dry run first..."
  rclone copy --dry-run "$src" "$dst"
  echo ""
  read -rp "Proceed with copy? [y/N] " confirm
  [[ "$confirm" =~ ^[Yy]$ ]] || { echo "Aborted."; exit 0; }
  rclone copy "${RCLONE_FLAGS[@]}" --checksum "$src" "$dst"
}

cmd_backends() {
  local tmp
  tmp=$(mktemp)
  rclone config providers 2>/dev/null | jq -c '.[]' > "$tmp"

  local selected
  selected=$(jq -r '"\(.Name)\t\(.Description)"' "$tmp" \
    | fzf --exact \
          --delimiter='\t' \
          --with-nth=1 \
          --preview="jq -C --arg n {1} 'select(.Name == \$n)' $tmp" \
          --preview-window=right:60% \
    | cut -f1)

  if [[ -n "$selected" ]]; then
    jq -C --arg n "$selected" 'select(.Name == $n)' "$tmp"
  fi
  rm -f "$tmp"
}

cmd_browse() {
  local path="${1:-}"
  if [[ -z "$path" ]]; then
    echo "Usage: rclone-env browse <remote:path>" >&2
    exit 1
  fi
  rclone ncdu "$path"
}

cmd_sync() {
  local src="${1:-}" dst="${2:-}"
  if [[ -z "$src" || -z "$dst" ]]; then
    echo "Usage: rclone-env sync <source> <dest>" >&2
    exit 1
  fi
  echo "Dry run first..."
  rclone sync --dry-run "$src" "$dst"
  echo ""
  read -rp "Proceed with sync? [y/N] " confirm
  [[ "$confirm" =~ ^[Yy]$ ]] || { echo "Aborted."; exit 0; }
  rclone sync "${RCLONE_FLAGS[@]}" --checksum "$src" "$dst"
}

MOUNT_FLAGS=(
  --daemon
  --vfs-cache-mode full
  --dir-cache-time 24h
)

# True when $1 is a currently-mounted path. Uses `mount` output, which
# lists the mount point on both macOS and Linux.
is_mounted() {
  mount | grep -q " on ${1%/} "
}

cmd_mount() {
  local remote="${1:-}" mnt="${2:-}"
  if [[ -z "$remote" || -z "$mnt" ]]; then
    echo "Usage: rclone-env mount <remote:path> <mount-point>" >&2
    exit 1
  fi
  # Idempotent: already mounted is a no-op, not an error.
  if is_mounted "$mnt"; then
    echo "Already mounted: $mnt"
    return 0
  fi
  mkdir -p "$mnt"
  # macOS has no FUSE by default; rclone's built-in NFS server + the OS
  # NFS client mounts without macFUSE. Linux uses stock fusermount.
  if [[ "$(uname -s)" == "Darwin" ]]; then
    rclone nfsmount "${MOUNT_FLAGS[@]}" "$remote" "$mnt"
  else
    rclone mount "${MOUNT_FLAGS[@]}" "$remote" "$mnt"
  fi
  echo "Mounted $remote at $mnt"
}

cmd_umount() {
  local mnt="${1:-}"
  if [[ -z "$mnt" ]]; then
    echo "Usage: rclone-env umount <mount-point>" >&2
    exit 1
  fi
  if ! is_mounted "$mnt"; then
    echo "Not mounted: $mnt"
    return 0
  fi
  if [[ "$(uname -s)" == "Darwin" ]]; then
    umount "$mnt" || diskutil unmount force "$mnt"
  else
    fusermount -u "$mnt"
  fi
  echo "Unmounted $mnt"
}

cmd_status() {
  local remotes
  remotes=$(rclone listremotes 2>/dev/null)
  echo "Configured remotes:"
  if [[ -z "$remotes" ]]; then
    echo "  (none)"
  else
    printf '%s\n' "$remotes" | sed 's/^/  /'
  fi
  echo ""
  # `mount` doesn't reliably name the backing remote (esp. macOS NFS
  # mounts to localhost), so report the active rclone/fuse/nfs mounts
  # rather than claim a per-remote mapping.
  local active
  active=$(mount | grep -iE 'fuse\.rclone|rclone|nfs' || true)
  echo "Active rclone/fuse/nfs mounts:"
  if [[ -z "$active" ]]; then
    echo "  (none)"
  else
    printf '%s\n' "$active" | sed 's/^/  /'
  fi
}

# Main
subcommand="${1:-}"
shift || true

case "$subcommand" in
  remotes|list) cmd_remotes ;;
  add)          cmd_add ;;
  ls)           cmd_ls "${1:-}" ;;
  check)        cmd_check "${1:-}" "${2:-}" ;;
  copy)         cmd_copy "${1:-}" "${2:-}" ;;
  sync)         cmd_sync "${1:-}" "${2:-}" ;;
  mount)        cmd_mount "${1:-}" "${2:-}" ;;
  umount)       cmd_umount "${1:-}" ;;
  status)       cmd_status ;;
  browse)       cmd_browse "${1:-}" ;;
  backends)     cmd_backends ;;
  *)
    echo "Usage: rclone-env <command> [args]"
    echo ""
    echo "Commands:"
    echo "  remotes            List configured rclone remotes"
    echo "  add                Add a new remote interactively"
    echo "  ls <remote:path>   List directories on a remote"
    echo "  check <src> <dst>  Check differences between source and dest"
    echo "  copy <src> <dst>   Dry-run preview then copy with optimised defaults"
    echo "  sync <src> <dst>   Dry-run preview then sync with optimised defaults"
    echo "  mount <remote:path> <mount-point>  Mount a remote (idempotent)"
    echo "  umount <mount-point>               Unmount a mount point"
    echo "  status                             Show remotes and active mounts"
    echo "  browse <remote:>   Interactive TUI browser for a remote (rclone ncdu)"
    echo "  backends            Browse all supported storage backends via fzf"
    exit 1
    ;;
esac
