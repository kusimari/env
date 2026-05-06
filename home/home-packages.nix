# Packages installed into home.packages for every environment.
#
# This file is one of two declaration sites for tier-1 + tier-2
# invariants (the other is programs.*.enable in home.nix).
#
#   Tier 1 — installed via nix on every env.
#   Tier 2 — wanted on every env; some envs exclude the nix install
#            (via `lib.optionals (<envKind-predicate>) [...]`) and
#            provide the same binary through their own post-install
#            tooling. `env-verify` checks the binary is on PATH.
{ pkgs, lib, envKind }:

with pkgs;
# Tier 1 — always installed via nix.
[
  tree
  git
  htop

  rclone
  exiftool

  claude-code
  gemini-cli-bin
  gh

  # Terminal utilities
  ripgrep  # fast regex search across files (rg), also used by emacs consult
  fd       # fast file finder, also used by emacs consult
  jq       # JSON processor, used by rclone-env backends

  # rclone-env: list, browse, check, copy, sync across rclone remotes
  (pkgs.writeShellScriptBin "rclone-env" (builtins.readFile ../rclone-env/rclone-env.sh))

  # nix-init: init a nix flake with direnv in the current directory
  (pkgs.writeShellScriptBin "nix-init" (builtins.readFile ../nix-init/nix-init.sh))
]
# Tier 2 — wanted on every env but not installable via nix on some envs.
# No entries today. When the next such package appears, wrap it in
# `lib.optionals (<envKind-predicate>) [ ... ]` here and install it via
# that env's own post-install tooling. `env-verify` picks it up automatically.
# Example shape:
#   ++ lib.optionals (envKind != "kelasa") [ some-pkg ]
