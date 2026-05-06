{
  # Package layering — read before adding packages.
  #
  #   Tier 1 — nix-managed, every env
  #     Where: home/home.nix (programs.*.enable) and home/home-packages.nix
  #     How:   home-manager modules or pkgs.<name> in the list.
  #
  #   Tier 2 — every env, nix on some, external bootstrap on others
  #     Where: same files as tier 1, wrapped in
  #              `lib.optionals (<envKind-predicate>) [...]`
  #     How:   envs the predicate admits get the nix install; excluded envs
  #            must provide the same binary on PATH through their own
  #            post-install tooling. `nix run .#env-verify` checks PATH.
  #
  #   Tier 3 — per-env differences
  #     Where: home/envKind-<name>.nix (user-level) or the <envKind>Configuration
  #            attrset in this file (system-level).
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

    # Linux base: shared by headless and graphical targets.
    # nixGL provides host OpenGL drivers for GUI apps on non-NixOS distros;
    # headless kelasa machines don't actually use it at runtime but pulling in
    # the overlay is essentially free and keeps the module tree symmetric.
    linuxBaseConfiguration = { pkgs, ... }: {
      nix.package = pkgs.nix;
      nixpkgs.overlays = [ inputs.nixgl.overlays.default ];
    };

    # Graphical Linux add-on: chrome + rofi + rofi desktop files.
    # Applied only to ubuntu-mane. AL2/AL2023 kelasa are headless SSH-only
    # and have no desktop environment for these to act on.
    linuxGraphicalConfiguration = { pkgs, lib, ... }: {
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

    # Shared module list for headless Amazon Linux kelasa machines.
    # al2-kelasa and al2023-kelasa have identical nix configs today; if they
    # diverge later, fork this into separate lists.
    al2KelasaModules = [
      commonConfiguration
      linuxBaseConfiguration
      al2KelasaConfiguration
      ./home/home.nix
    ];

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

    # env-verify: on-demand check that every tier-1 + tier-2 invariant is on PATH.
    #
    # Invariant list is derived — not hand-maintained — by evaluating the
    # home-manager configuration for each envKind and subtracting what the
    # per-env (tier-3) files contribute. Anything added through
    # programs.*.enable in home.nix or into home-packages.nix is picked up
    # automatically; anything added only in home/envKind-*.nix is excluded.
    apps = let
      systems = [ "x86_64-linux" "aarch64-darwin" ];

      mkPkgs = system: import nixpkgs {
        inherit system;
        overlays = [
          inputs.nix-vscode-extensions.overlays.default
          inputs.alacritty-theme.overlays.default
          inputs.claude-code.overlays.default
        ];
        config.allowUnfree = true;
      };

      mkApp = system: let
        pkgs = mkPkgs system;
        lib  = pkgs.lib;

        # Never activated; we only read config.home.packages. Stubs keep
        # homeManagerConfiguration happy regardless of target user.
        mkConfig = envKind: home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = { inherit envKind; };
          modules = [
            ./home/home.nix
            {
              home.username      = "env-verify";
              home.homeDirectory = "/tmp/env-verify";
              home.stateVersion  = "25.05";
            }
          ];
        };

        # Only check packages that declare an executable we can look up by name.
        # Packages without meta.mainProgram (e.g. oh-my-zsh, nix-zsh-completions,
        # purely runtime/data packages) are skipped — there's nothing to `command -v`.
        nameOf  = p: p.meta.mainProgram or "";
        toNames = ps: lib.unique (builtins.filter (n: n != "") (map nameOf ps));

        manePkgs   = (mkConfig "mane").config.home.packages;
        kelasaPkgs = (mkConfig "kelasa").config.home.packages;

        tier3      = envFile: (import envFile { inherit pkgs; }).home.packages or [];
        maneOnly   = tier3 ./home/envKind-mane.nix;
        kelasaOnly = tier3 ./home/envKind-kelasa.nix;

        invariants = lib.subtractLists
          (toNames maneOnly ++ toNames kelasaOnly)
          (toNames (manePkgs ++ kelasaPkgs));

        script = pkgs.writeShellApplication {
          name = "env-verify";
          runtimeInputs = [ pkgs.coreutils ];
          text = ''
            echo "Checking ${toString (builtins.length invariants)} invariant(s)..."
            missing=0
            for name in ${lib.concatStringsSep " " invariants}; do
              if path=$(command -v "$name" 2>/dev/null); then
                printf "  ok  %-24s -> %s\n" "$name" "$path"
              else
                printf "  MISS %-24s -> not found\n" "$name"
                missing=$((missing + 1))
              fi
            done
            if [ "$missing" -gt 0 ]; then
              echo ""
              echo "$missing invariant(s) missing."
              echo "Tier-2 invariants are installed by your environment's own"
              echo "post-install tooling - re-run that, or ensure the binaries"
              echo "are on PATH, then re-run this check."
              exit 1
            fi
            echo "All invariants present."
          '';
        };
      in { type = "app"; program = "${script}/bin/env-verify"; };
    in builtins.listToAttrs
         (map (s: { name = s; value = { env-verify = mkApp s; }; }) systems);

    # ubuntu-mane: home Ubuntu machine (graphical desktop)
    homeConfigurations.ubuntu-mane = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      extraSpecialArgs = { envKind = "mane"; };
      modules = [
        commonConfiguration
        linuxBaseConfiguration
        linuxGraphicalConfiguration
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
