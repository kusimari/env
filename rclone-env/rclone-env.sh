#!/usr/bin/env bash
set -euo pipefail

RCLONE_FLAGS=(
  --transfers 16
  --checkers 16
  --drive-chunk-size 256M
  --stats 1s
  --progress
)

CHECK_FLAGS=(
  --checksum
  --checkers 16
  --drive-chunk-size 256M
  --stats 1s
  --progress
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

# Main
subcommand="${1:-}"
shift || true

case "$subcommand" in
  remotes|list) cmd_remotes ;;
  add)          cmd_add ;;
  ls)           cmd_ls "${1:-}" ;;
  check)        cmd_check "${1:-}" "${2:-}" ;;
  copy)         cmd_copy "${1:-}" "${2:-}" ;;
  *)
    echo "Usage: rclone-env <command> [args]"
    echo ""
    echo "Commands:"
    echo "  remotes            List configured rclone remotes"
    echo "  add                Add a new remote interactively"
    echo "  ls <remote:path>   List directories on a remote"
    echo "  check <src> <dst>  Check differences between source and dest"
    echo "  copy <src> <dst>   Dry-run preview then copy with optimised defaults"
    exit 1
    ;;
esac
