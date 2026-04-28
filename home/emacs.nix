{ pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-nox;
  };

  # Load core.el from the source tree (symlinked via .config/emacs below).
  home.file.".emacs".text = ''
    (load-file "~/.config/emacs/core.el")
  '';

  home.file.".config/emacs".source = ../emacs;

  # emacs-gittree: launch emacs in gittree mode, optionally comparing two refs.
  home.packages = [
    (pkgs.writeShellScriptBin "emacs-gittree" (builtins.readFile ../gittree/emacs-gittree.sh))
  ];
}
