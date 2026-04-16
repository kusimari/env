{ config, lib, pkgs, ... }:

let
  # Custom derivation to install tmux-sessions-view script in nix store
  # Mirrors the structure of tmux-resurrect plugin
  tmuxSessionsExtra = pkgs.runCommand "tmux-sessions-extra" {} ''
    mkdir -p $out/share/tmux-plugins/tmux-sessions-extra/scripts
    cp ${./tmux-sessions-view.py} $out/share/tmux-plugins/tmux-sessions-extra/scripts/view.sh
    chmod +x $out/share/tmux-plugins/tmux-sessions-extra/scripts/view.sh
  '';
in {
  # Python required for tmux-sessions-view.py
  home.packages = [ pkgs.python3 tmuxSessionsExtra ];

  # Tmux configuration with session persistence plugins
  programs.tmux = {
    enable = true;

    plugins = with pkgs.tmuxPlugins; [
      resurrect   # Save/restore sessions
      continuum   # Auto-save every 15 minutes
    ];

    # Consolidated configuration: styling + plugin settings
    extraConfig = builtins.readFile ./tmux.conf;
  };

  # Tmux session persistence shell aliases - all in one place
  programs.zsh.shellAliases = {
    tmux-sessions-save = "tmux run-shell ${pkgs.tmuxPlugins.resurrect}/share/tmux-plugins/resurrect/scripts/save.sh";
    tmux-sessions-restore = "tmux run-shell ${pkgs.tmuxPlugins.resurrect}/share/tmux-plugins/resurrect/scripts/restore.sh";
    tmux-sessions-view = "tmux run-shell ${tmuxSessionsExtra}/share/tmux-plugins/tmux-sessions-extra/scripts/view.sh";
  };
}
