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

  # Emacs runtime tools:
  # - gnutar + gzip: package.el uses `tar xf` to extract MELPA tarballs; on
  #   minimal images (e.g., AL2023 headless) the system tar/gzip may not be
  #   on the PATH nix sees, causing "Failed to install <pkg>: tar not found".
  # - emacs-gittree: CLI wrapper that launches gittree-mode, optionally
  #   comparing two refs.
  home.packages = [
    pkgs.gnutar
    pkgs.gzip
    (pkgs.writeShellScriptBin "emacs-gittree" (builtins.readFile ../gittree/emacs-gittree.sh))
  ];
}
