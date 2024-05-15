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
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-23.11-darwin";
    nixpkgs-unstable.url = github:NixOS/nixpkgs/nixpkgs-unstable;

    # Environment/system management
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager/release-23.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";    
  };

  outputs = inputs@{ self, nixpkgs, nix-darwin, home-manager, ... }:
  let
    inherit (import ../config-local.nix) user hostName;
    hostPlatform = "aarch64-darwin";

    configuration = { pkgs, ... }: {
      # enable nix in shell
      programs.zsh.enable = true;


      # add toolbox and brew
      programs.zsh.loginShellInit = ''
        export PATH=$PATH:/Users/${user}/.toolbox/bin
        eval "$(/opt/homebrew/bin/brew shellenv)"
      '';

      # Non nixable tools
      nixpkgs.config.allowUnfree = true;
      homebrew = {
        enable = true;
	      onActivation.cleanup = "uninstall";
        casks = [
           "visual-studio-code"
           "iterm2"
        ]; 
      };

      # system wide installations
      environment.systemPackages = with pkgs; [
      ];

      # Auto upgrade nix package and the daemon service.
      services.nix-daemon.enable = true;
      # Necessary for using flakes on this system.
      nix.settings.experimental-features = "nix-command flakes";
      # Set Git commit hash for darwin-version.
      system.configurationRevision = self.rev or self.dirtyRev or null;
      # Used for backwards compatibility, please read the changelog before changing.
      # $ darwin-rebuild changelog
      system.stateVersion = 4;

      nixpkgs.hostPlatform = "${hostPlatform}";

      # mac specific configs
      security.pam.enableSudoTouchIdAuth = true;
    };
  in
  {
    # Build darwin flake using:
    # $ darwin-rebuild build --flake .#$[hostname]
    darwinConfigurations.${hostName} = nix-darwin.lib.darwinSystem {
      modules = [ 
        configuration
        home-manager.darwinModules.home-manager {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          users.users.${user} = {
            name = "${user}";
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