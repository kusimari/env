{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.programs.rclone-env;
in {
  options.programs.rclone-env = {
    enable = mkEnableOption "rclone-env - mount/unmount rclone remotes";
  };

  config = mkIf cfg.enable {
    home.packages = [
      (pkgs.writeShellScriptBin "rclone-mount" (builtins.readFile ./rclone-mount.sh))
    ];

    # Zsh completion: place _rclone-mount in ~/.zfunc
    home.file.".zfunc/_rclone-mount".source = ./_rclone-mount;

    # Ensure ~/.zfunc is on fpath before compinit
    programs.zsh.initExtra = ''
      fpath=(~/.zfunc $fpath)
    '';
  };
}
