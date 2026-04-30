{ lib, pkgs, ... }:
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

  # emacs-gittree: CLI wrapper that launches gittree-mode, optionally
  # comparing two refs.
  home.packages = [
    (pkgs.writeShellScriptBin "emacs-gittree" (builtins.readFile ../gittree/emacs-gittree.sh))
  ];

  # Wipe ~/.emacs.d/elpa when the emacs source in the repo changes, so
  # MELPA re-fetches packages freshly instead of fighting stale
  # byte-compiles from an older emacs build. Hash-gated so routine
  # home-manager switches that don't touch emacs/ stay fast.
  #
  # Pattern mirrors home/ssh-setup.nix's activation hook. Runs after
  # writeBoundary so ~/.config/emacs is already in place when we hash.
  home.activation.emacsElpaRefresh = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    set -eu
    SOURCE_DIR="$HOME/.config/emacs"
    STATE="$HOME/.emacs.d/.emacs-source-hash"
    if [ -d "$SOURCE_DIR" ]; then
      CURRENT=$(find -L "$SOURCE_DIR" -type f \( -name '*.el' -o -name '*.el.gz' \) -print0 \
                | sort -z | xargs -0 ${pkgs.coreutils}/bin/sha256sum \
                | ${pkgs.coreutils}/bin/sha256sum \
                | cut -d' ' -f1)
      PREV=""
      [ -f "$STATE" ] && PREV=$(cat "$STATE")
      if [ "$CURRENT" != "$PREV" ]; then
        echo "emacs source changed — wiping ~/.emacs.d/elpa so MELPA re-installs cleanly"
        rm -rf "$HOME/.emacs.d/elpa"
        mkdir -p "$HOME/.emacs.d"
        printf '%s' "$CURRENT" > "$STATE"
      fi
    fi
  '';
}
