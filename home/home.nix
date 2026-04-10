{ config, lib, pkgs, envKind, ... }:
{
  # Import custom modules and environment-specific config.
  # envKind is "mane" or "kelasa", passed via extraSpecialArgs in flake.nix.
  # Edit home/mane.nix or home/kelasa.nix to add environment-specific packages.
  imports = [
    ../gittree/gittree-module.nix
    ./${envKind}.nix
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

  programs.tmux = {
    enable = true;
    # To test config changes without rebuilding:
    # tmux source-file <tmux.conf-path>
    extraConfig = builtins.readFile ./tmux.conf;
  };

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
      # Source pre-nix setup shell initialization
      if [[ -f ~/.pre-nix-rc ]]; then
        source ~/.pre-nix-rc
      fi
    '';
    # Determinate Nix adds nix-daemon.sh to /etc/zshrc (interactive only).
    # Source it in .zshenv so non-login shells (e.g. Tailscale SSH) get Nix in PATH.
    envExtra = ''
      if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
        . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
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

  # SSH configuration - separate nix-managed files
  # System ~/.ssh/config includes this file via "Include ~/.ssh/config_nix" directive
  # This allows system processes to write to the main config while nix manages its own section
  home.file.".ssh/config_nix".text = ''
    # Nix-managed GitHub SSH configuration
    Host github.com
      IdentityFile ~/.ssh/github_id
      # Check both system and nix known_hosts files
      UserKnownHostsFile ~/.ssh/known_hosts ~/.ssh/known_hosts_nix
  '';

  # Nix-managed known_hosts with GitHub public keys
  home.file.".ssh/known_hosts_nix".text = ''
    github.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl
    github.com ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCj7ndNxQowgcQnjshcLrqPEiiphnt+VTTvDP6mHBL9j1aNUkY4Ue1gvwnGLVlOhGeYrnZaMgRK6+PKCUXaDbC7qtbW8gIkhL7aGCsOr/C56SJMy/BCZfxd1nWzAOxSDPgVsmerOBYfNqltV9/hWCqBywINIR+5dIg6JTJ72pcEpEjcYgXkE2YEFXV1JHnsKgbLWNlhScqb2UmyRkQyytRLtL+38TGxkxCflmO+5Z8CSSNY7GidjMIZ7Q4zMjA2n1nGrlTDkzwDCsw+wqFPGQA179cnfGWOWRVruj16z6XyvxvjJwbz0wQZ75XK5tKSb7FNyeIEs4TT4jk+S4dhPeAUC5y+bDYirYgM4GC7uEnztnZyaVWQ7B381AK4Qdrwt51ZqExKbQpTUNn+EjqoTwvqNj4kqx5QUCI0ThS/YkOxJCXmPUWZbhjpCg56i+2aB6CmK2JGhn57K5mj0MNdBXA4/WnwH6XoPWJzK5Nyu2zB3nAZp+S5hpQs+p1vN1/wsjk=
    github.com ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBEmKSENjQEezOmxkZMy7opKgwFB9nkt5YRrYMjNuG5N87uRgg6CLrbo5wAdT/y6v0mKV0U2w0WZ2YB/++Tpockg=
  '';

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
