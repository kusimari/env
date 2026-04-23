{ config, lib, pkgs, envKind, ... }:
{
  # Import custom modules and environment-specific config.
  # envKind is "mane" or "kelasa", passed via extraSpecialArgs in flake.nix.
  # Edit home/mane.nix or home/kelasa.nix to add environment-specific packages.
  imports = [
    ../gittree/gittree-module.nix
    ../tmux/tmux.nix
    ./${envKind}.nix
    ./ssh-setup.nix
  ];

  # shell with solarized dark

  # changes in each release.
  home.stateVersion = "25.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.sessionVariables = {
    EDITOR = "emacs";
  };

  # https://github.com/nix-community/home-manager/issues/1341#issuecomment-2049723843

  home.packages = with pkgs; [
    tree
    git
    htop

    rclone
    exiftool

    claude-code
    gemini-cli-bin
    gh

    # Terminal utilities
    ripgrep  # fast regex search across files (rg), also used by emacs consult
    fd       # fast file finder, also used by emacs consult
    jq       # JSON processor, used by rclone-env backends

    # rclone-env: list, browse, check, copy, sync across rclone remotes
    (pkgs.writeShellScriptBin "rclone-env" (builtins.readFile ../rclone-env/rclone-env.sh))

    # nix-init: init a nix flake with direnv in the current directory
    (pkgs.writeShellScriptBin "nix-init" (builtins.readFile ../nix-init/nix-init.sh))
  ];


  programs.alacritty = {
    enable = true;
    # On Ubuntu (non-NixOS), nixGL is required to expose the host OpenGL drivers.
    # nixGLIntel covers Mesa-based systems (Intel/AMD). For Nvidia, swap with nixGLNvidia.
    # symlinkJoin preserves the .desktop file so app launchers (e.g. Synapse) find it,
    # while replacing only the binary with the nixGL wrapper.
    package = if pkgs.stdenv.isLinux
      then
        let
          wrapper = pkgs.writeShellScript "alacritty-nixgl" ''
            exec ${pkgs.nixgl.nixGLIntel}/bin/nixGLIntel ${pkgs.alacritty}/bin/alacritty "$@"
          '';
        in pkgs.symlinkJoin {
          name = "alacritty-nixgl";
          paths = [ pkgs.alacritty ];
          postBuild = ''
            rm $out/bin/alacritty
            ln -s ${wrapper} $out/bin/alacritty
          '';
        }
      else pkgs.alacritty;
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
    initContent = lib.mkOrder 550 ''
      fpath=(~/.zfunc $fpath)
    '';
    # Determinate Nix adds nix-daemon.sh to /etc/zshrc (interactive only).
    # Source it in .zshenv so non-login shells (e.g. Tailscale SSH) get Nix in PATH.
    envExtra = ''
      if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
        . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
      fi

      # Generic hook system: source pre/post-nix RC files if they exist
      # These files can be managed by external setup scripts or other repos
      if [[ -f ~/.pre-nix-rc ]]; then
        source ~/.pre-nix-rc
      fi
      if [[ -f ~/.post-nix-rc ]]; then
        source ~/.post-nix-rc
      fi
    '';
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

  home.file.".config/emacs".source = ../emacs;
  home.shellAliases.emacs-gittree = ''emacs --eval "(gittree-mode 1)"'';

  # Terminal UI for git - installs lazygit with delta and custom config
  programs.gittree = {
    enable = true;
    commandName = "lg";
  };

  # zsh completions — placed in ~/.zfunc which is in fpath (see initContent above)
  # Generated at build time to avoid runtime permission issues with system completion dirs
  home.file.".zfunc/_rclone".source = pkgs.runCommand "_rclone" {} ''
    ${pkgs.rclone}/bin/rclone completion zsh $out
  '';
  home.file.".zfunc/_rclone-env".source = ../rclone-env/_rclone-env;

}
