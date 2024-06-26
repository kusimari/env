{ config, pkgs, ... }:
{
  # shell with solarized dark
  # emacs with zenburn

  # changes in each release.
  home.stateVersion = "24.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # https://github.com/nix-community/home-manager/issues/1341#issuecomment-2049723843

  home.packages = with pkgs; [
    tree
    git
    htop
  ];

  programs.tmux.enable = true;

  programs.alacritty = {
    enable = true;
    settings.import = [ pkgs.alacritty-theme.solarized_dark ];
    settings = {
      window.decorations = "Full";
      window.option_as_alt = "OnlyLeft";
    };
  };

  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    sessionVariables = {
      # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/tmux
      ZSH_TMUX_AUTOSTART = "true";
      ZSH_TMUX_AUTOCONNECT = "false";
    };
    oh-my-zsh = {
      enable = true;
      plugins = [ "direnv" "tmux" ];
      theme = "robbyrussell";
    };
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  programs.vscode = {
    enable = true;
    extensions = with pkgs.open-vsx; [
      rust-lang.rust-analyzer
      ms-python.python
      jnoortheen.nix-ide
    ] ++ (with pkgs.vscode-marketplace; [
      ryanolsonx.zenburn
    ]);
    userSettings = {
      "editor.minimap.autohide" = true;
      "editor.inlayHints.enabled" = "offUnlessPressed";

      "workbench.colorTheme" =  "Zenburn";
    };
  };

  programs.ssh = {
    enable = true;
    matchBlocks = {
      "github.com" = {
        identityFile = "~/.ssh/github_id";
      };
    };
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-nox;
  };
  home.file.".emacs".source = ./emacs.el;
}