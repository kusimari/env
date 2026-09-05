{ user }:
{ pkgs, ... }: {
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
}
