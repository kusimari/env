{ pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-nox;
    # Install markdown-mode declaratively so `.md` files get the mode
    # immediately on first launch — no MELPA fetch, no runtime tar/gzip
    # dependency. Other packages in core.el still come from MELPA via
    # my-use-package, but markdown-mode is tiny and failures here are
    # high-friction (every README open breaks). Keep it as a baseline.
    extraPackages = epkgs: [
      epkgs.markdown-mode
    ];
  };

  # Load core.el from the source tree (symlinked via .config/emacs below).
  home.file.".emacs".text = ''
    (load-file "~/.config/emacs/core.el")
  '';

  home.file.".config/emacs".source = ../emacs;

  # emacs-gittree: CLI wrapper that launches gittree-mode, optionally
  # comparing two refs.
  home.packages = [
    (pkgs.writeShellScriptBin "emacs-gittree" (builtins.readFile ../gittree/emacs-gittree.sh))
  ];
}
