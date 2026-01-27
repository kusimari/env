{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.gittree;

  # Config stored in nix store - immutable and reproducible
  # Lazygit expects config at $XDG_CONFIG_HOME/lazygit/config.yml
  configDir = pkgs.writeTextDir "lazygit/config.yml" (builtins.readFile ./lazygit-config.yml);
in
{
  options.programs.gittree = {
    enable = mkEnableOption "gittree - Enhanced lazygit with side-by-side terminal diffs";

    commandName = mkOption {
      type = types.str;
      default = "lg";
      description = "Name of the global command to launch lazygit with enhanced configuration";
    };
  };

  config = mkIf cfg.enable {
    # Install required packages
    home.packages = with pkgs; [
      lazygit    # Main TUI for git with side-by-side diffs
      delta      # Syntax highlighting for git diffs (used by lazygit config)

      # Global lazygit wrapper with side-by-side view
      (writeShellScriptBin cfg.commandName ''
        #!/bin/bash

        # gittree - Enhanced Lazygit Launcher
        # Usage: ${cfg.commandName}    (from anywhere on the system)

        # Use config directly from nix store - immutable and reproducible
        export XDG_CONFIG_HOME="${configDir}"

        # Change to git repository root if in a subdirectory
        if git rev-parse --git-dir > /dev/null 2>&1; then
            cd "$(git rev-parse --show-toplevel)"
        fi

        # Launch lazygit with our config
        ${lazygit}/bin/lazygit "$@"
      '')
    ];
  };
}