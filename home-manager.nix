{ lib, config, pkgs, ... }:
{
  # shell with solarized dark
  # tmux
  # emacs with zenburn

  # changes in each release.
  home.stateVersion = "23.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    emacs-nox
    tree
    git
    htop
  ];

  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;

    oh-my-zsh = {
      enable = true;
      plugins = [ "direnv" ];
      theme = "robbyrussell";
    };
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  # run commands to setup things done with brew etc
  # no vscode.extensions as that requires install thru nix, but here installed using brew
  home.activation.install-vscode-extensions = lib.hm.dag.entryAfter ["installPackages"] ''
    eval "$(/opt/homebrew/bin/brew shellenv)"
    code --install-extension rust-lang.rust-analyzer
    code --install-extension ms-python.python 
    code --install-extension jnoortheen.nix-ide
  '';
  programs.vscode = {
    enable = true;
    userSettings = {
      "editor.minimap.autohide" = true;
      "editor.inlayHints.enabled" = "on";

      "workbench.colorTheme" =  "Solarized Dark";
    };
  };
}