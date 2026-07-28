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
  # With --daemon, rclone waits for the background mount to report ready.
  # On macOS/BSD that wait is a constant sleep (not an early-exit poll),
  # so the default 1m would stall the terminal for a full minute.
  --daemon-wait 5s
  --vfs-cache-mode full
  --dir-cache-time 24h
)

# Mount points only, from `mount` output. Two formats to strip:
#   Linux: <dev> on <mnt> type <fs> (opts)
#   macOS: <dev> on <mnt> (fs, opts)
mount_points() {
  mount | sed -e 's/ type [^ ]* (.*$//' -e 's/ (.*$//' -e 's/^.* on //'
}

# True when $1 is a currently-mounted path. Compares whole lines with a
# fixed string (-xF): a path is not a regex, and `.` or `*` in a real
# path would otherwise match a different mount and wrongly report it
# mounted. Resolves the path first so a symlinked mount point (e.g.
# /home -> /local/home) matches what the kernel reports.
is_mounted() {
  local target="${1%/}"
  target="$(cd "$target" 2>/dev/null && pwd -P)" || target="${1%/}"
  mount_points | grep -qxF "$target"
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
  # rclone blocks until the daemon reports ready (see --daemon-wait), so
  # say what's happening before the pause rather than only after it.
  echo "Mounting $remote at $mnt (waiting for the mount to become ready)..."
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
    # Fall back to a lazy unmount if the mount is busy (matches the
    # macOS force fallback).
    fusermount -u "$mnt" || fusermount -uz "$mnt"
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
  # `mount` doesn't reliably name the backing remote, so report the
  # rclone-created mounts rather than claim a per-remote mapping. Match
  # only what rclone produces: `fuse.rclone` on Linux, and a localhost
  # NFS mount on macOS (rclone nfsmount serves 127.0.0.1). Bare `nfs`
  # would wrongly catch corporate NFS home dirs on work machines.
  local active
  active=$(mount | grep -iE 'fuse\.rclone|rclone|127\.0\.0\.1:|localhost:' || true)
  echo "Active rclone mounts:"
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
