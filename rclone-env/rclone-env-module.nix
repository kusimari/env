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
      (pkgs.writeShellScriptBin "rclone-env" (builtins.readFile ./rclone-env.sh))
    ];

    # Zsh completion: place _rclone-env in ~/.zfunc
    home.file.".zfunc/_rclone-env".source = ./_rclone-env;

    # Ensure ~/.zfunc is on fpath before compinit
    programs.zsh.initExtraBeforeCompInit = ''
      fpath=(~/.zfunc $fpath)
    '';
  };
}
