# Feature: digikam-desktop

## Git Setup

- Branch: feat/digikam-desktop
- Base: main

## Feature Brief

Install DigiKam with full HEIC/HEIF, AVIF, RAW, and extended format support on Ubuntu-mane via Nix, cleanly integrated with the graphical desktop environment.

## Handoff

- **Phase:** review
- **Ready for:** review
- **Carry forward:** Wrapped digikam in `linuxGraphicalConfiguration` provides full HEIC/AVIF/RAW support via `kimageformats` and `qtimageformats` without touching headless kelasa targets or universal `home.nix`.
- **Deliberately left:** Headless targets (`al2-kelasa`, `al2023-kelasa`) and universal `home.nix` are untouched because digikam is a graphical desktop application for `ubuntu-mane`.

## Requirements

- DigiKam desktop application is installed and launchable on `ubuntu-mane`.
- DigiKam and Showfoto display and process HEIC/HEIF images without errors.
- DigiKam displays and processes extended formats (AVIF, JPEG-XL, RAW formats like CR2/CR3/DNG/NEF/ARW, WebP) supported by KDE and Qt image format plugins.
- Standalone `heif-convert` and `heif-info` CLI utilities are available on PATH on `ubuntu-mane`.
- Headless kelasa configurations (`al2-kelasa`, `al2023-kelasa`) remain unaffected and build cleanly.

## Test Strategy

- `layers/test-flake.sh` evaluates and builds `homeConfigurations.ubuntu-mane.activationPackage` and `homeConfigurations.al2-kelasa.activationPackage` to verify evaluation, dependencies, and builder success across targets.
- Smoke test `digikam` binary wrapper to verify `QT_PLUGIN_PATH` correctly contains `kimageformats` and `qtimageformats` plugin directories.
- Run `digikam -v` and `digikam --help` to verify executable startup and library linkage without crashes.
- Verify `heif-info` and `heif-convert` are accessible in the package output.

## Design

- Rationale: DigiKam is a Tier 3 Linux GUI application specific to `ubuntu-mane`. Per project conventions, Linux graphical desktop applications belong in `linuxGraphicalConfiguration` in `flake.nix` (co-located with `google-chrome` and `rofi`).
- Packaging approach: Use `symlinkJoin` and `makeWrapper` on `pkgs.digikam` to create a wrapped derivation that sets `--prefix QT_PLUGIN_PATH : ...` pointing to `${pkgs.kdePackages.kimageformats}/lib/qt-6/plugins` and `${pkgs.kdePackages.qtimageformats}/lib/qt-6/plugins`.
- This preserves the desktop entry files (`org.kde.digikam.desktop`, `org.kde.showfoto.desktop`), app icons, and ensures that when launched from GNOME/Rofi application menus, `digikam` and `showfoto` always find the format plugins.
- Add `pkgs.libheif` to `linuxGraphicalConfiguration.home.packages` to provide standalone HEIF CLI utilities on PATH.

## Implementation Plan

- [x] Create wrapped `digikam` package with `kimageformats` and `qtimageformats` in `flake.nix` under `linuxGraphicalConfiguration`
- [x] Add wrapped `digikam` and `pkgs.libheif` to `linuxGraphicalConfiguration.home.packages`
- [x] Run `bash layers/test-flake.sh` to verify build integrity for `ubuntu-mane` and `al2-kelasa`
- [x] Verify binary execution and plugin resolution

## Session Log

- 2026-09-05: Feature started. Grounded on Qt6 image format plugin requirements and confirmed `kimageformats` enables HEIC/AVIF/RAW in Qt6 `QImageReader`.
- 2026-09-05: Wrapped `digikam` and `showfoto` with `kdePackages.kimageformats` and `kdePackages.qtimageformats` in `linuxGraphicalConfiguration` in `flake.nix`. Added `libheif` CLI tools. Built and verified cleanly via `layers/test-flake.sh`. Verified binary wrapper execution and plugin paths.

## Decision Log

- Co-locate digikam wrapper and libheif in `linuxGraphicalConfiguration` in `flake.nix` rather than `home/envKind-mane.nix` or `home/home.nix` to preserve cohesion for Linux graphical desktop applications and keep headless targets minimal.
