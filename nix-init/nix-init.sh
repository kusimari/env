#!/usr/bin/env bash
set -euo pipefail

if [[ -f flake.nix ]]; then
  echo "flake.nix already exists, aborting." >&2
  exit 1
fi

if [[ -n "${1:-}" ]]; then
  template="$1"
else
  template=$(nix flake show templates --json 2>/dev/null \
    | jq -r '.templates | to_entries[] | "\(.key)\t\(.value.description)"' \
    | fzf --delimiter='\t' --with-nth=1 --preview='echo {2}' --preview-window=bottom:2:wrap \
    | cut -f1)
  [[ -z "$template" ]] && { echo "No template selected." >&2; exit 1; }
fi

nix flake init --template "templates#$template"
echo "use flake" >> .envrc
direnv allow
