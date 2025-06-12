# stuff installed outside nix - zsh with chsh of user

# nix install
  # curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# nix setup
  # nix run home-manager/master -- init --switch ~/env/nix-ubuntu/
  # home-manager switch --flake ~/env/nix-ubuntu/
  # nix flake update

{
  description = "Juice's ubuntu system";

  inputs = {
    # Package sets
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:NixOS/nixpkgs";
    nixpkgs-unstable.url = github:NixOS/nixpkgs/nixpkgs-unstable;

    # home-manager and overlays
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs-unstable";

    alacritty-theme.url = "github:alexghr/alacritty-theme.nix";

    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    nixgl.url = "github:nix-community/nixGL";
  };


  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
  let
    inherit (import ../config-local.nix) user hostName;
    system = "x86_64-linux";

    configuration = { pkgs, ... }: {
      programs.zsh.enable = true;
      nix.settings.experimental-features = "nix-command flakes";
      nixpkgs.config.allowUnfree = true;
      nix.package = pkgs.nix;

    };

    pkgs = nixpkgs.legacyPackages.${system};

  in {
    homeConfigurations."${user}" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [
            configuration

            {nixpkgs.overlays = [ inputs.nix-vscode-extensions.overlays.default
                                  inputs.alacritty-theme.overlays.default ];}
            {
                
                home.username = "${user}";
                home.homeDirectory = "/home/${user}";
            }

            ../home/home.nix
        ];
      };
    };
}
