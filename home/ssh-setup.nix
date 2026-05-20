# SSH Setup - GitHub config, activation script, and mosh (persistent connections)
{ config, lib, pkgs, ... }:
{
  home.packages = [ pkgs.mosh ];

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

  # mw: mosh to a remote host and attach (or create) a tmux session
  programs.zsh.initContent = lib.mkOrder 600 ''
    mw() {
      if [[ $# -ne 2 ]]; then
        echo "Usage: mw <host> <session>" >&2
        return 1
      fi
      mosh "$1" -- tmux new-session -As "$2"
    }
  '';
}
