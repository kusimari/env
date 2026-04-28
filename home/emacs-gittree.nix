{ pkgs, ... }:
{
  # emacs-gittree: launch emacs in gittree mode, optionally comparing two refs.
  # Separated into its own module so the package declaration lives next to the
  # emacs config in the import tree (home.nix imports ../emacs/core.el's
  # shell script from here) rather than inside home.packages up top.
  home.packages = [
    (pkgs.writeShellScriptBin "emacs-gittree" (builtins.readFile ../gittree/emacs-gittree.sh))
  ];
}
