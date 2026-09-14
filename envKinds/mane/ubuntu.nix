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

  # Thunar wrapped with GVFS GIO modules for Trash/virtual URI schemes and archive plugin
  thunar-with-plugins = pkgs.thunar.override {
    thunarPlugins = [ pkgs.thunar-archive-plugin ];
  };

  thunar-wrapped = pkgs.symlinkJoin {
    name = "thunar";
    paths = [ thunar-with-plugins ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/thunar \
        --prefix GIO_EXTRA_MODULES : "${pkgs.gvfs}/lib/gio/modules" \
        --prefix XDG_DATA_DIRS : "${pkgs.gvfs}/share"
      wrapProgram $out/bin/thunar-settings \
        --prefix GIO_EXTRA_MODULES : "${pkgs.gvfs}/lib/gio/modules" \
        --prefix XDG_DATA_DIRS : "${pkgs.gvfs}/share"
    '';
  };
in {
  home.packages = [
    pkgs.google-chrome
    digikam-wrapped
    pkgs.libheif

    # Declarative userland apps
    thunar-wrapped
    pkgs.gvfs
    pkgs.xarchiver
    pkgs.viewnior
    pkgs.evince
    pkgs.libreoffice
    pkgs.simple-scan
    pkgs.gnome-calculator
    pkgs.dust
    pkgs.imagemagick
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
      "application/pdf" = "org.gnome.Evince.desktop";
      "application/postscript" = "org.gnome.Evince.desktop";
      "application/x-dvi" = "org.gnome.Evince.desktop";
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
