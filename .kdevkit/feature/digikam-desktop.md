# Feature: digikam-desktop

## Git Setup

- Branch: feat/digikam-desktop
- Base: main

## Feature Brief

Install DigiKam with full HEIC/HEIF, AVIF, RAW, and extended format support on Ubuntu-mane via Nix, cleanly integrated with the graphical desktop environment.

## Handoff

- **Phase:** review
- **Ready for:** reviewer verification and merge
- **Carry forward:** Modular structure separates universal/platform configurations (`common/common.nix`, `common/linux.nix`) from identity-class configurations under `envKinds/` (`mane/` and `kelasa/`). Wrapped digikam lives in `envKinds/mane/graphical.nix`.
- **Deliberately left:** Headless targets (`al2-kelasa`, `al2023-kelasa`) and universal `home.nix` are untouched because digikam is a graphical desktop application for `ubuntu-mane`.

## Requirements

- DigiKam desktop application is installed and launchable on `ubuntu-mane`.
- DigiKam and Showfoto display and process HEIC/HEIF images without errors.
- DigiKam displays and processes extended formats (AVIF, JPEG-XL, RAW formats like CR2/CR3/DNG/NEF/ARW, WebP) supported by KDE and Qt image format plugins.
- Standalone `heif-convert` and `heif-info` CLI utilities are available on PATH on `ubuntu-mane`.
- Headless kelasa configurations (`al2-kelasa`, `al2023-kelasa`) remain unaffected and build cleanly.
- `flake.nix` configurations are modularized into `common/` and `envKinds/` directory structure matching `envKind` conventions.

## Test Strategy

- `layers/test-flake.sh` evaluates and builds `homeConfigurations.ubuntu-mane.activationPackage` and `homeConfigurations.al2-kelasa.activationPackage` to verify evaluation, dependencies, and builder success across targets.
- Smoke test `digikam` binary wrapper to verify `QT_PLUGIN_PATH` correctly contains `kimageformats` and `qtimageformats` plugin directories.
- Run `digikam -v` and `digikam --help` to verify executable startup and library linkage without crashes.
- Verify `heif-info` and `heif-dec` are accessible in the package output.

## Design

- Rationale: DigiKam is a Tier 3 Linux GUI application specific to `ubuntu-mane`. Per review feedback, `flake.nix` is modularized into dedicated files organized by universal/platform foundations (`common/`) and identity classes (`envKinds/`).
- Modular structure:
  - `common/common.nix`: Shared overlays (claude-code, alacritty, etc.), nix settings, allowUnfree.
  - `common/linux.nix`: Shared Linux base (nixGL overlay, pkgs.nix).
  - `envKinds/mane/graphical.nix`: Linux graphical desktop (Google Chrome, Rofi, wrapped DigiKam with `kimageformats`/`qtimageformats`, `libheif`, rofi desktop files).
  - `envKinds/mane/home.nix`: User-level mane packages (Tailscale, Google Antigravity).
  - `envKinds/kelasa/al2.nix`: AL2/AL2023 machine config (glibcLocales, sessionPath, user/home setup).
  - `envKinds/kelasa/darwin.nix`: Darwin system config (TouchID, Homebrew casks).
  - `envKinds/kelasa/home.nix`: User-level kelasa packages (Bubblewrap, nix.conf override).
- Packaging approach: Use `symlinkJoin` and `makeWrapper` on `pkgs.digikam` in `envKinds/mane/graphical.nix` to create a wrapped derivation setting `--prefix QT_PLUGIN_PATH : ...` pointing to `${pkgs.kdePackages.kimageformats}/lib/qt-6/plugins` and `${pkgs.kdePackages.qtimageformats}/lib/qt-6/plugins`.
- Preserves desktop entry files (`org.kde.digikam.desktop`, `org.kde.showfoto.desktop`), app icons, and adds `pkgs.libheif` to provide standalone HEIF CLI utilities on PATH.

## Implementation Plan

- [x] Create wrapped `digikam` package with `kimageformats` and `qtimageformats`
- [x] Add wrapped `digikam` and `pkgs.libheif` to graphical packages
- [x] Create `common/` modules (`common.nix`, `linux.nix`)
- [x] Create `envKinds/mane/` modules (`graphical.nix`, `home.nix`)
- [x] Create `envKinds/kelasa/` modules (`al2.nix`, `darwin.nix`, `home.nix`)
- [x] Update `home/home.nix` to import `../envKinds/${envKind}/home.nix` and retire `home/envKind-*.nix`
- [x] Refactor `flake.nix` to consume modular configurations
- [x] Update `env-verify.nix` to reference `envKinds/<name>/home.nix`
- [x] Run `bash layers/test-flake.sh` to verify build integrity for `ubuntu-mane` and `al2-kelasa`
- [x] Verify `nix flake check` passes without evaluation regressions

## Session Log

- 2026-09-05: Feature started. Grounded on Qt6 image format plugin requirements and confirmed `kimageformats` enables HEIC/AVIF/RAW in Qt6 `QImageReader`.
- 2026-09-05: Wrapped `digikam` and `showfoto` with `kdePackages.kimageformats` and `kdePackages.qtimageformats`. Added `libheif` CLI tools. Built and verified cleanly via `layers/test-flake.sh`.
- 2026-09-05: Review comment received requesting modular reorganization of `flake.nix` into `common/` and `envKinds/<envKind-name>/` structure. Updated design and plan.
- 2026-09-05: Implemented modular architecture: split configs into `common/` and `envKinds/`, updated `flake.nix`, `home/home.nix`, and `env-verify.nix`. Validated with `test-flake.sh` and `nix flake check`.

## Decision Log

- Reorganize `flake.nix` configurations into `common/` (platform/universal) and `envKinds/<envKind-name>/` (identity classes) to consolidate all tier-3 configuration and keep `flake.nix` lean.
- Co-locate digikam wrapper and libheif in `envKinds/mane/graphical.nix` so all Linux graphical desktop tooling is self-contained.
