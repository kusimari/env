{ pkgs, lib, ... }:
let
  # DigiKam wrapped with KDE/Qt image format plugins (HEIC/HEIF, AVIF, RAW, etc.)
  digikam-wrapped = pkgs.symlinkJoin {
    name = "digikam";
    paths = [ pkgs.digikam ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/digikam \
        --prefix QT_PLUGIN_PATH : "${lib.makeSearchPath "lib/qt-6/plugins" [ pkgs.kdePackages.kimageformats pkgs.kdePackages.qtimageformats ]}" \
        --set-default QT_XCB_GL_INTEGRATION none
      wrapProgram $out/bin/showfoto \
        --prefix QT_PLUGIN_PATH : "${lib.makeSearchPath "lib/qt-6/plugins" [ pkgs.kdePackages.kimageformats pkgs.kdePackages.qtimageformats ]}" \
        --set-default QT_XCB_GL_INTEGRATION none
    '';
  };
in {
  home.packages = [
    pkgs.google-chrome
    digikam-wrapped
    pkgs.libheif

    # Declarative userland apps
    pkgs.thunar
    pkgs.thunar-archive-plugin
    pkgs.xarchiver
    pkgs.viewnior
  ];

  # Declarative default applications (MIME associations)
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "inode/directory" = "thunar.desktop";
      "image/jpeg" = "viewnior.desktop";
      "image/png" = "viewnior.desktop";
      "image/webp" = "viewnior.desktop";
      "image/gif" = "viewnior.desktop";
      "application/zip" = "xarchiver.desktop";
      "application/x-tar" = "xarchiver.desktop";
      "application/x-gzip" = "xarchiver.desktop";
      "application/x-bzip2" = "xarchiver.desktop";
      "application/x-xz" = "xarchiver.desktop";
      "application/x-7z-compressed" = "xarchiver.desktop";
      "application/x-rar" = "xarchiver.desktop";
    };
  };
  home.file = lib.mapAttrs' (name: _: {
    name  = ".local/share/applications/${name}";
    value.source = ../../rofi-desktop + "/${name}";
  }) (builtins.readDir ../../rofi-desktop);

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
}
