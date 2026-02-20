{ config, pkgs, nixgl, ... }:
{
  # Import custom modules directly
  imports = [
    ../../gittree/gittree-module.nix
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

    rclone
    exiftool

    claude-code
    gemini-cli-bin

    # Terminal utilities
    ripgrep  # fast regex search across files (rg), also used by emacs consult
    fd       # fast file finder, also used by emacs consult
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
      ZSH_TMUX_AUTOSTART = "false";
      ZSH_TMUX_AUTOCONNECT = "false";
      ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE = "fg=#666666";
      ZSH_AUTOSUGGEST_STRATEGY = "(completion history)";
    };
    initContent = ''
      # Source pre-nix setup shell initialization
      if [[ -f ~/.pre-nix-rc ]]; then
        source ~/.pre-nix-rc
      fi
    '';
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

  # Terminal utilities with shell integration
  # fuzzy finder for files, history, and piped input
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  # cat clone with syntax highlighting and git integration
  programs.bat.enable = true;

  # modern ls replacement with icons, git status, and tree view
  programs.eza = {
    enable = true;
    enableZshIntegration = true;  # aliases ls/ll/la/lt etc.
  };

  # smarter cd that learns your most-used directories
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;  # adds z/zi commands
  };

  # shell history search/sync with context (replaces ctrl-r)
  programs.atuin = {
    enable = true;
    enableZshIntegration = true;
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
    };
  };

  programs.git = {
    enable = true;
    settings.alias = {
      lg = "log --oneline --graph --all --decorate";
    };
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-nox;
  };
  # emacs configuration - create directory with all emacs files and load from there
  home.file.".emacs".text = ''
    (load-file "~/.config/emacs/core.el")
  '';

  home.file.".config/emacs".source = ../../emacs;
  home.shellAliases.emacs-gittree = ''emacs --eval "(gittree-mode 1)"'';

  # Terminal UI for git - installs lazygit with delta and custom config
  programs.gittree = {
    enable = true;
    commandName = "lg";
  };

}
