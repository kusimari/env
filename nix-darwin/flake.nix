# stuff installed outside nix - brew and toolbox. see their docs, see loginShellInit below

# nix install
  # curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# nix setup
  # nix run nix-darwin -- switch --flake ~/env/nix-darwin
  # darwin-rebuild switch --flake ~/env/nix-darwin
  # nix flake update

{
  description = "Juice's darwin system";

  inputs = {
    # Package sets
    nixpkgs.url = "github:NixOS/nixpkgs";
    nixpkgs-unstable.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # home-manager and overlays
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";

    alacritty-theme.url = "github:alexghr/alacritty-theme.nix";

    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs = inputs@{ self, nixpkgs, nix-darwin, home-manager, ... }:
  let
    inherit (import ../config-local.nix) user hostName;
    hostPlatform = "aarch64-darwin";

    configuration = { pkgs, ... }: {
      # stuff that nix-darwin init created
      programs.zsh.enable = true;
      services.nix-daemon.enable = true;
      nix.settings.experimental-features = "nix-command flakes";
      system.configurationRevision = self.rev or self.dirtyRev or null;
      system.stateVersion = 4;

      programs.zsh.loginShellInit = ''
        export PATH=$PATH:/Users/${user}/.toolbox/bin
        eval "$(/opt/homebrew/bin/brew shellenv)"
      '';

      # Non nixable tools
      nixpkgs.config.allowUnfree = true;
      homebrew = { enable = true;
	                 onActivation.cleanup = "uninstall";
                   casks = [ "raycast"
                  ];};

      environment.systemPackages = with pkgs; [];

      nixpkgs.hostPlatform = "${hostPlatform}";

      # mac specific configs
      # https://mynixos.com/nix-darwin/option/security.pam.enableSudoTouchIdAuth after system update
      # reapply nix-darwin configuration
      security.pam.enableSudoTouchIdAuth = true;
    };
  in  {
    darwinConfigurations.${hostName} = nix-darwin.lib.darwinSystem {
      modules = [ 
        configuration

        { nixpkgs.overlays = [ inputs.nix-vscode-extensions.overlays.default
                               inputs.alacritty-theme.overlays.default
        ];}

        home-manager.darwinModules.home-manager {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          users.users.${user} = { name = "${user}";
                                  home = "/Users/${user}";
                                };
          home-manager.users.${user} = import ../home-manager.nix;
        }
      ];
    };

    # Expose the package set, including overlays, for convenience.
    darwinPackages = self.darwinConfigurations.${hostName}.pkgs;
  };
}