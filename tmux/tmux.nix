{ config, lib, pkgs, ... }:

with lib;

let
  # Custom tmux plugin following nixpkgs conventions
  sessions-view = pkgs.tmuxPlugins.mkTmuxPlugin {
    pluginName = "sessions-view";
    version = "1.0.0";
    src = ./.;

    # Override installPhase to install Python script in scripts/ subdirectory
    installPhase = ''
      runHook preInstall

      target=$out/share/tmux-plugins/sessions-view
      mkdir -p $target/scripts
      cp tmux-sessions-view.py $target/scripts/view.sh
      chmod +x $target/scripts/view.sh

      runHook postInstall
    '';

    meta = with lib; {
      description = "View tmux-resurrect sessions in hierarchical format";
      homepage = "https://github.com/kusimari/env";
      license = licenses.mit;
      platforms = platforms.unix;
    };
  };
in
{
  # Declare python3 dependency for sessions-view script
  home.packages = [ pkgs.python3 ];

  # Tmux configuration with session persistence plugins
  programs.tmux = {
    enable = true;

    plugins = with pkgs.tmuxPlugins; [
      resurrect      # Save/restore sessions
      continuum      # Auto-save every 15 minutes
      sessions-view  # View saved sessions (custom plugin)
    ];

    # Consolidated configuration: styling + plugin settings
    extraConfig = builtins.readFile ./tmux.conf;
  };

  # Tmux session persistence shell aliases - all in one place
  programs.zsh.shellAliases = {
    tmux-sessions-save = "tmux run-shell ${pkgs.tmuxPlugins.resurrect}/share/tmux-plugins/resurrect/scripts/save.sh";
    tmux-sessions-restore = "tmux run-shell ${pkgs.tmuxPlugins.resurrect}/share/tmux-plugins/resurrect/scripts/restore.sh";
    tmux-sessions-view = "tmux run-shell ${sessions-view}/share/tmux-plugins/sessions-view/scripts/view.sh";
  };
}
