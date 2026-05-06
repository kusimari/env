{ pkgs, ... }:
{
  # mane-only packages and config (not installed on kelasa environments)
  home.packages = [
    pkgs.tailscale  # VPN — mane only
  ];
}
