{
  description = "Juice's unified darwin/ubuntu system";

  inputs = {
    # Package sets - standardized on stable
    nixpkgs.url = "github:NixOS/nixpkgs";

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

    # Ubuntu-specific
    nixgl.url = "github:nix-community/nixGL";
  };

  outputs = inputs@{ self, nixpkgs, nix-darwin, home-manager, ... }:
  let
    inherit (import ./home/user-host.nix) user hostName;

    # Shared across both platforms
    commonConfiguration = {
      nixpkgs.overlays = [
        inputs.nix-vscode-extensions.overlays.default
        inputs.alacritty-theme.overlays.default
        inputs.claude-code.overlays.default
      ];
      programs.zsh.enable = true;
      nix.settings.experimental-features = "nix-command flakes";
      nixpkgs.config.allowUnfree = true;
    };

    # macOS-specific configuration
    darwinConfiguration = { ... }: {
      nix.enable = false; # determinate needs this
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

    # Ubuntu-specific configuration
    ubuntuConfiguration = { pkgs, ... }: {
      nix.package = pkgs.nix;
    };

  in {
    darwinConfigurations.darwin = nix-darwin.lib.darwinSystem {
      modules = [
        commonConfiguration
        darwinConfiguration
        home-manager.darwinModules.home-manager {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.backupFileExtension = "bak";
          users.users.${user} = {
            name = "${user}";
            home = "/Users/${user}";
          };
          home-manager.users.${user} = import ./home/home.nix;
        }
      ];
    };

    darwinPackages = self.darwinConfigurations.darwin.pkgs;

    homeConfigurations.ubuntu = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [
        commonConfiguration
        ubuntuConfiguration
        {
          home.username = "${user}";
          home.homeDirectory = "/home/${user}";
        }
        ./home/home.nix
      ];
    };
  };
}
