{ inputs }:
{
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
}
