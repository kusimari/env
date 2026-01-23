{
  description = "Juice's darwin system";

  inputs = {
    # Package sets
    nixpkgs.url = "github:NixOS/nixpkgs";
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # home-manager and overlays
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    alacritty-theme = {
      url = "github:alexghr/alacritty-theme.nix";
    };
    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    claude-code.url = "github:sadjow/claude-code-nix";
  };

  outputs = inputs@{ self, nixpkgs, nix-darwin, home-manager,  ... }:
  let
    inherit (import ../home/user-host.nix) user hostName;
    hostPlatform = "aarch64-darwin";

    configuration = { pkgs, ... }: {
      nix = {
        enable = false; # determinate needs this
        settings.experimental-features = "nix-command flakes";
      };
      system = {
        configurationRevision = self.rev or self.dirtyRev or null;
        stateVersion = 4;
      };

      programs.zsh = {
        enable = true;
        loginShellInit = ''
                export PATH=$PATH:/Users/${user}/.toolbox/bin
                        eval "$(/opt/homebrew/bin/brew shellenv)"
        '';
      };

      # non nixable tools
      nixpkgs.config.allowUnfree = true;
      nixpkgs.hostPlatform = "${hostPlatform}";
      
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

      environment.systemPackages = with pkgs; [];

      # mac specific configs
      security.pam.services.sudo_local.touchIdAuth = true;


    };
  in  {
    darwinConfigurations.${hostName} = nix-darwin.lib.darwinSystem {
      modules = [ 
        {nixpkgs.overlays = import ../common-inputs.nix {inputs = inputs;};}

        configuration

        home-manager.darwinModules.home-manager {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          users.users.${user} = { name = "${user}";
                                  home = "/Users/${user}";
                                };
          home-manager.users.${user} = import ../home/home.nix;
        }
      ];
    };

    # Expose the package set, including overlays, for convenience.
    darwinPackages = self.darwinConfigurations.${hostName}.pkgs;
  };
}
