# SSH Setup - GitHub config, activation script, and autossh (persistent connections)
{ config, lib, pkgs, ... }:
{
  # autossh -M 0 -t <remote> -- tmux new-session -As <session-name>
  home.packages = [ pkgs.autossh ];

  # Included by system ~/.ssh/config via "Include ~/.ssh/config_nix"
  home.file.".ssh/config_nix".text = ''
    Host github.com
      IdentityFile ~/.ssh/github_id
      UserKnownHostsFile ~/.ssh/known_hosts ~/.ssh/known_hosts_nix

    Host *
      ServerAliveInterval 30
      ServerAliveCountMax 3
  '';

  # Runs during home-manager switch to setup SSH (adds Include directive, fetches keys)
  home.activation.setupSSHConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
    bash ${./ssh-setup.sh}
  '';
}
