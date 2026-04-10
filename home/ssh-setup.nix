# SSH Setup Module - Universal pattern for all systems
# Provides:
# - Nix-managed SSH config (config_nix) with GitHub settings
# - Activation script that ensures Include directive, fetches GitHub keys, creates SSH keys
{ config, lib, pkgs, ... }:
{
  # Nix-managed SSH configuration
  # System ~/.ssh/config includes this file via "Include ~/.ssh/config_nix" directive
  # This allows system processes to write to the main config while nix manages its own section
  home.file.".ssh/config_nix".text = ''
    # Nix-managed GitHub SSH configuration
    Host github.com
      IdentityFile ~/.ssh/github_id
  '';

  # Activation script - runs ssh-setup.sh directly from nix store
  # This runs on ALL systems during home-manager switch
  # Fetches GitHub public keys at activation time (not hardcoded)
  # No files copied to home directory - keeps it clean
  home.activation.setupSSHConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
    bash ${./ssh-setup.sh}
  '';
}
