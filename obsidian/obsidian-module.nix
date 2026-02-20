{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.obsidian;
in
{
  options.programs.obsidian = {
    enable = mkEnableOption "obsidian - Note-taking app with git-backed vault";

    notesRepo = mkOption {
      type = types.str;
      example = "git@github.com:user/obsidian-vault.git";
      description = "Git repository URL for the Obsidian notes vault to clone on activation";
    };

    notesPath = mkOption {
      type = types.str;
      default = config.home.homeDirectory + "/obsidian";
      description = "Local path where the notes repository will be cloned. Defaults to ~/obsidian";
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      obsidian
    ];

    # Clone the notes repository on activation if it does not already exist
    home.activation.cloneObsidianNotes = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      if [ ! -d "${cfg.notesPath}" ]; then
        ${pkgs.git}/bin/git clone "${cfg.notesRepo}" "${cfg.notesPath}"
      fi
    '';
  };
}
