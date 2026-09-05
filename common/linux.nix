{ inputs }:
{ pkgs, ... }: {
  nix.package = pkgs.nix;
  nixpkgs.overlays = [ inputs.nixgl.overlays.default ];
}
