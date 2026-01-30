{ config, pkgs, nixgl, ... }:
{
  # Import custom modules directly
  imports = [
    ../gittree/gittree-module.nix
  ];

  # shell with solarized dark

  # changes in each release.
  home.stateVersion = "25.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;


  # https://github.com/nix-community/home-manager/issues/1341#issuecomment-2049723843

  home.packages = with pkgs; [
    tree
    git  # todo: set userEmail and userName locally
    htop
    ripgrep  # also used by emacs consult
    fd       # also used by emacs consult

    rclone
    exiftool

    claude-code
    gemini-cli-bin
  ];

  programs.tmux = {
    enable = true;
    # To test config changes without rebuilding:
    # tmux source-file <tmux.conf-path>
    extraConfig = builtins.readFile ./tmux.conf;
  };

  # on Ubuntu unfortunately have to run with nixGL
  # nix run --impure github:nix-community/nixGL -- program
  # but that ain't functional too
  programs.alacritty = {
    enable = true;
    settings.general.import = [ pkgs.alacritty-theme.solarized_dark ];
    settings = {
      window.decorations = "Full";
      window.option_as_alt = "OnlyLeft";
    };
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    sessionVariables = {
      # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/tmux
      ZSH_TMUX_AUTOSTART = "true";
      ZSH_TMUX_AUTOCONNECT = "false";
      ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=236";
      ZSH_AUTOSUGGEST_STRATEGY = "(completion history)";
    };
    oh-my-zsh = {
      enable = true;
      plugins = [ "direnv" "tmux" ];
      theme = "robbyrussell";
    };
    completionInit = "rclone completion zsh";
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  programs.vscode = {
    # https://github.com/LnL7/nix-darwin/issues/1315
    # added nix to the "allow full disk access" security list
    enable = false;
    profiles.default = {
      extensions = with pkgs.open-vsx; [
        rust-lang.rust-analyzer
        ms-python.python
        jnoortheen.nix-ide
      ] ++ (with pkgs.vscode-marketplace; [
        ryanolsonx.zenburn
        saoudrizwan.claude-dev
      ]);
      userSettings = {
        "editor.minimap.autohide" = true;
        "editor.inlayHints.enabled" = "offUnlessPressed";
        "workbench.colorTheme" =  "Zenburn";
      };
    };
  };

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    matchBlocks = {
      "github.com" = {
        identityFile = "~/.ssh/github_id";
      };
      "*" = {
        identityFile = "~/.ssh/amazon_id_ecdsa";
      };
    };
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-nox;
  };
  # emacs configuration - create directory with all emacs files and load from there
  home.file.".emacs".text = ''
    ;; Load core.el from nix store emacs directory
    (load-file "~/.config/emacs/core.el")
  '';
  
  home.file.".config/emacs".source = ../emacs;

  # Terminal Live Git Diff - Enhanced git workflow with lazygit
  programs.gittree = {
    enable = true;
    commandName = "lg";
  };

}
