{ config, lib, pkgs, ... }:
{
  # kelasa-only packages and config (not installed on mane environments)

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
