# Common configurations shared across platforms and envKinds
{ inputs }:

{
  # Shared across all platforms: overlays, nix settings.
  commonConfiguration = { ... }: {
    nixpkgs.overlays = [
      inputs.nix-vscode-extensions.overlays.default
      inputs.alacritty-theme.overlays.default
      inputs.claude-code.overlays.default
      inputs.antigravity.overlays.default
      # Skip direnv's upstream test suite. Its zsh integration tests
      # occasionally hang inside the sandbox on aarch64-darwin; the
      # package itself is fine. Remove this overlay once nixpkgs
      # ships a cached direnv build we actually pull.
      (_final: prev: {
        direnv = prev.direnv.overrideAttrs (_old: { doCheck = false; });
      })
    ];
    nix.settings.experimental-features = "nix-command flakes";
    nixpkgs.config.allowUnfree = true;
  };

  # Linux base: shared by headless and graphical Linux targets.
  # nixGL provides host OpenGL drivers for GUI apps on non-NixOS distros;
  # headless kelasa machines don't actually use it at runtime but pulling in
  # the overlay is essentially free and keeps the module tree symmetric.
  linuxCommonConfiguration = { pkgs, ... }: {
    nix.package = pkgs.nix;
    nixpkgs.overlays = [ inputs.nixgl.overlays.default ];
  };
}
