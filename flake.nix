{
  description = "Juice's unified darwin/ubuntu system";

  inputs = {
    # Package sets
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # nix-darwin (macOS system configuration)
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # home-manager (user environment management)
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Overlays
    alacritty-theme.url = "github:alexghr/alacritty-theme.nix";
    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    claude-code.url = "github:sadjow/claude-code-nix";

    # Linux-specific
    nixgl.url = "github:nix-community/nixGL";
  };

  outputs = inputs@{ self, nixpkgs, nix-darwin, home-manager, ... }:
  let
    inherit (import ./home/user-host.nix) user hostName;

    # ── System-level configurations ────────────────────────────────────────
    # Shared across all platforms: overlays, nix settings.
    # Note: programs.zsh.enable is a darwin system option; see darwinConfiguration.
    commonConfiguration = {
      nixpkgs.overlays = [
        inputs.nix-vscode-extensions.overlays.default
        inputs.alacritty-theme.overlays.default
        inputs.claude-code.overlays.default
      ];
      nix.settings.experimental-features = "nix-command flakes";
      nixpkgs.config.allowUnfree = true;
    };

    # darwin-only system settings
    darwinConfiguration = { ... }: {
      nix.enable = false; # determinate needs this
      programs.zsh.enable = true;
      system = {
        configurationRevision = self.rev or self.dirtyRev or null;
        stateVersion = 4;
      };

      nixpkgs.hostPlatform = "aarch64-darwin";

      homebrew = {
        enable = true;
        user = "${user}";
        onActivation.cleanup = "uninstall";
        onActivation.autoUpdate = true;
        casks = [
          "raycast"
          "google-chrome"
          "porting-kit"
        ];
      };

      security.pam.services.sudo_local.touchIdAuth = true;
      security.pam.services.sudo_local.reattach = true;
    };

    # Linux base: nixGL, google-chrome, rofi desktop files.
    # rofi requires a desktop environment — works on ubuntu-mane, not applicable on al2-kelasa.
    linuxConfiguration = { pkgs, lib, ... }: {
      nix.package = pkgs.nix;
      nixpkgs.overlays = [ inputs.nixgl.overlays.default ];
      home.packages = [ pkgs.google-chrome ];
      home.file = lib.mapAttrs' (name: _: {
        name  = ".local/share/applications/${name}";
        value.source = ./rofi-desktop + "/${name}";
      }) (builtins.readDir ./rofi-desktop);

      # rofi: bind shortcut to "rofi -show drun" in GNOME Settings → Keyboard → Custom Shortcuts.
      programs.rofi = {
        enable = true;
        extraConfig.show-icons = true;
        theme = builtins.toString (pkgs.writeText "rofi-theme.rasi" ''
          @theme "Arc-Dark"
          * {
            font: "Monospace 24";
          }
          window {
            width:  50%;
            height: 50%;
          }
        '');
      };
    };

    # Shared configuration for AL2/AL2023 kelasa machines
    al2KelasaConfiguration = { pkgs, ... }: {
      home.username = "${user}";
      home.homeDirectory = "/home/${user}";

      # Fix PATH for single-user Nix installation
      home.sessionPath = [
        "/home/${user}/.nix-profile/bin"
      ];

      # Fix terminal encoding
      home.packages = [ pkgs.glibcLocales ];
      home.sessionVariables = {
        LANG = "en_US.UTF-8";
        LC_ALL = "en_US.UTF-8";
        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      };
    };

  in {
    # darwin-kelasa: work macOS machine
    darwinConfigurations.darwin-kelasa = nix-darwin.lib.darwinSystem {
      modules = [
        commonConfiguration
        darwinConfiguration
        home-manager.darwinModules.home-manager {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.backupFileExtension = "bak";
          home-manager.extraSpecialArgs = { envKind = "kelasa"; };
          users.users.${user} = {
            name = "${user}";
            home = "/Users/${user}";
          };
          home-manager.users.${user} = import ./home/home.nix;
        }
      ];
    };

    darwinPackages = self.darwinConfigurations.darwin-kelasa.pkgs;

    # ubuntu-mane: home Ubuntu machine
    homeConfigurations.ubuntu-mane = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      extraSpecialArgs = { envKind = "mane"; };
      modules = [
        commonConfiguration
        linuxConfiguration
        {
          home.username = "${user}";
          home.homeDirectory = "/home/${user}";
        }
        ./home/home.nix
      ];
    };

    # al2-kelasa: office Amazon Linux 2 machine
    homeConfigurations.al2-kelasa = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      extraSpecialArgs = { envKind = "kelasa"; };
      modules = [
        commonConfiguration
        linuxConfiguration
        al2KelasaConfiguration
        ./home/home.nix
      ];
    };

    # al2023-kelasa: office Amazon Linux 2023 machine
    homeConfigurations.al2023-kelasa = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      extraSpecialArgs = { envKind = "kelasa"; };
      modules = [
        commonConfiguration
        linuxConfiguration
        al2KelasaConfiguration
        ./home/home.nix
      ];
    };
  };
}
