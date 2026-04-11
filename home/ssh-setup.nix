# SSH Setup - GitHub config and activation script
{ config, lib, pkgs, ... }:
{
  # GitHub SSH config included by system ~/.ssh/config
  home.file.".ssh/config_nix".text = ''
    Host github.com
      IdentityFile ~/.ssh/github_id
      UserKnownHostsFile ~/.ssh/known_hosts ~/.ssh/known_hosts_nix
  '';

  # Runs during home-manager switch to setup SSH (adds Include directive, fetches keys)
  home.activation.setupSSHConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
    bash ${./ssh-setup.sh}
  '';
}
