{ self, user }:
{ ... }: {
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
      "obsidian"
      "claude" # GUI app; no nixpkgs equivalent on any envKind
    ];
  };

  security.pam.services.sudo_local.touchIdAuth = true;
  security.pam.services.sudo_local.reattach = true;
}
