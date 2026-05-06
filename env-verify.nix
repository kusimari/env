# env-verify: on-demand PATH check for tier-1 + tier-2 invariants.
# Tier layering is documented in flake.nix.
#
# Invariant list is derived — not hand-maintained — by evaluating the
# home-manager configuration for each envKind and subtracting what the
# per-env (tier-3) files contribute. Anything added through
# programs.*.enable in home.nix or inline in home.nix's home.packages is
# picked up automatically; anything added only in home/envKind-*.nix is
# excluded.
{ nixpkgs, home-manager, inputs }:

let
  systems = [ "x86_64-linux" "aarch64-darwin" ];

  # Apply the same overlays as commonConfiguration in flake.nix so the eval
  # sees the same claude-code / alacritty-theme / vscode-extensions packages
  # as real home-manager activations.
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
    # purely runtime/data packages) are skipped — nothing to `command -v`.
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
in
  builtins.listToAttrs
    (map (s: { name = s; value = { env-verify = mkApp s; }; }) systems)
