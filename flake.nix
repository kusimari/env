{
  # Package layering — read before adding packages.
  #
  #   Tier 1 — nix-managed, every env
  #     Where: home/home.nix (home.packages list and programs.*.enable)
  #     How:   pkgs.<name> in the list, or a home-manager module.
  #
  #   Tier 2 — every env, nix on some, external bootstrap on others
  #     Where: same files as tier 1, wrapped in
  #              `lib.optionals (<envKind-predicate>) [...]`
  #     How:   envs the predicate admits get the nix install; excluded envs
  #            must provide the same binary on PATH through their own
  #            post-install tooling. `nix run .#env-verify` checks PATH.
  #
  #   Tier 3 — per-env differences
  #     Where: envKinds/<name>/home.nix (user-level) or envKinds/<name>/ (system-level).
  #     How:   only the env(s) that want it see it. No verifier coverage.
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
    # Antigravity CLI overlay
    antigravity = {
      url = "github:jacopone/antigravity-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Linux-specific
    nixgl.url = "github:nix-community/nixGL";
  };

  outputs = inputs@{ self, nixpkgs, nix-darwin, home-manager, ... }:
  let
    inherit (import ./home/user-host.nix) user hostName;

    # ── Modular configurations ─────────────────────────────────────────────
    # Common base: universal overlays & Linux base
    commonConfiguration = import ./common/common.nix { inherit inputs; };
    linuxBaseConfiguration = import ./common/linux.nix { inherit inputs; };

    # Machine-class configurations: kelasa & mane
    al2KelasaConfiguration = import ./envKinds/kelasa/al2.nix { inherit user; };
    darwinConfiguration = import ./envKinds/kelasa/darwin.nix { inherit self user; };
    ubuntuManeConfiguration = ./envKinds/mane/graphical.nix;

    # Shared module list for headless Amazon Linux kelasa machines (al2 / al2023).
    al2KelasaModules = [
      commonConfiguration
      linuxBaseConfiguration
      al2KelasaConfiguration
      ./home/home.nix
    ];

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

    # env-verify: on-demand PATH check for tier-1 + tier-2 invariants.
    # Implementation in ./env-verify.nix; kept out of this flake to keep
    # install-time and verification-time concerns separate.
    apps = import ./env-verify.nix { inherit nixpkgs home-manager inputs; };

    # ubuntu-mane: home Ubuntu machine (graphical desktop)
    homeConfigurations.ubuntu-mane = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      extraSpecialArgs = { envKind = "mane"; };
      modules = [
        commonConfiguration
        linuxBaseConfiguration
        ubuntuManeConfiguration
        {
          home.username = "${user}";
          home.homeDirectory = "/home/${user}";
        }
        ./home/home.nix
      ];
    };

    # al2-kelasa: office Amazon Linux 2 machine (headless SSH)
    homeConfigurations.al2-kelasa = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      extraSpecialArgs = { envKind = "kelasa"; };
      modules = al2KelasaModules;
    };

    # al2023-kelasa: office Amazon Linux 2023 machine (headless SSH)
    homeConfigurations.al2023-kelasa = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      extraSpecialArgs = { envKind = "kelasa"; };
      modules = al2KelasaModules;
    };
  };
}
