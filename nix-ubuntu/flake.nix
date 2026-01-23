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
    inherit (import ../home/user-host.nix) user hostName;
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

            {nixpkgs.overlays = import ../common-inputs.nix {inputs = inputs;};}

            {
                
                home.username = "${user}";
                home.homeDirectory = "/home/${user}";
            }

            ../home/home.nix
        ];
      };
    };
}
