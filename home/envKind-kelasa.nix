{ config, lib, pkgs, ... }:
{
  # kelasa-only packages and config (not installed on mane environments)

  # bubblewrap: runtime dependency of toolbox-installed codex (L4). Codex
  # 0.144+ sandboxes every shell command it runs through `bwrap`; with no
  # bwrap on PATH it panics at linux-sandbox/src/launcher.rs and falls back
  # to a slow, degraded exec path. Linux-only: darwin-kelasa codex uses the
  # macOS Seatbelt sandbox instead, and mane envs have no codex. Nix's
  # profile bin is first on PATH, so codex resolves this bwrap by lookup.
  home.packages = lib.optionals pkgs.stdenv.isLinux [ pkgs.bubblewrap ];

  # Overwrite any pre-existing ~/.config/nix/nix.conf instead of backing it up.
  # Layer 1 bootstrap scripts also write this file; without force, home-manager
  # tries to rename the existing file to .bak and aborts when a stale .bak
  # lingers from a prior activation. Contents are fully reproducible from
  # Layer 1 + this flake, so nothing is worth preserving in a backup.
  # Only needed on Linux kelasa envs (AL2/AL2023), not darwin-kelasa.
  xdg.configFile = lib.mkIf pkgs.stdenv.isLinux {
    "nix/nix.conf".force = true;
  };
}
