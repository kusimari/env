# Feature: userland-apps-office-print

## Git Setup

- Branch: feat/userland-apps-office-print
- Base: main

## Feature Brief

Add declarative userland applications for office productivity, PDF editing, and document scanning on `ubuntu-mane`. Provide LibreOffice suite (with LibreOffice Draw for vector and text PDF editing) and Document Scanner (`simple-scan`) declaratively through Nix. Resolve the host print queue crash and Apport errors by eliminating host GIO environment pollution, aligning the debloat package list, and integrating HP Device Manager (`hp-toolbox`) on the host.

## Handoff

- **Phase:** planning
- **Ready for:** Planning Review Gate
- **Carry forward:** Host printing backend crashed previously due to purged `python3-gi-cairo` and `GIO_EXTRA_MODULES` exporting Nix GLIBC 2.38+ modules to host Ubuntu 22.04 (GLIBC 2.35) binaries.
- **Deliberately left:** Host OS updates/upgrades and APT remote cleanups are handled by the operator outside `env` rather than introducing a sidecar maintenance script.

## Requirements

- **Office & PDF Editing Experience**:
  - The user can launch LibreOffice applications (Writer, Calc, Impress, Draw) directly from Rofi or Thunar.
  - The user can open existing PDF files in LibreOffice Draw to edit text, alter formatting, add objects, and re-export to PDF.
  - Document viewer (Evince) remains available as the fast default viewer for reading PDFs and PostScript files.
- **Scanning Experience**:
  - The user can launch Document Scanner (`simple-scan`) directly from Rofi or by triggering the "Scan" action in HP Device Manager (`hp-toolbox`).
  - Scanning supports multi-page capture, rotating, cropping, and saving directly to PDF or image formats.
- **Printing & Queue Stability**:
  - Print jobs dispatched from desktop apps (Chrome, Evince, LibreOffice) spool cleanly without Apport system crash popups.
  - Host Python/GTK utilities (including `system-config-printer` and `hp-toolbox`) run cleanly without GLIBC symbol errors or Cairo context errors.
- **Clean Desktop Integration**:
  - LibreOffice apps and Document Scanner are automatically indexed by Rofi (`rofi -show drun`).
  - Headless kelasa environments (`al2-kelasa`, `al2023-kelasa`) and darwin configurations remain unaffected.

## Test Strategy

- Flake evaluation: `bash layers/test-flake.sh` verifies flake evaluation across `ubuntu-mane` and `al2-kelasa`.
- Binary and desktop entry verification: Assert `libreoffice` and `simple-scan` binaries exist in the built Nix profile and have valid `.desktop` files in `share/applications/`.
- Session variable isolation: Verify `GIO_EXTRA_MODULES` is no longer exported in `home.sessionVariables`, preventing GLIBC version conflicts on host binaries.
- Debloat alignment: Verify `layers/layer-3-ubuntu-mane-debloat.sh --dry-run` does not target `simple-scan`.

## Design

- **Architecture & Ecosystem Fit**:
  - Thin Host OS owns hardware drivers, display server, and base printing/scanning daemons (CUPS, SANE, HPLIP driver).
  - Host GUI tool `hplip-gui` (`hp-toolbox`) provides hardware status, ink monitoring, and print queue management for the HP OfficeJet Pro 9010.
  - Nix owns userland desktop applications: `pkgs.libreoffice` and `pkgs.simple-scan`.
  - `hp-toolbox` delegates scanning to the desktop's registered scanner frontend (`simple-scan`).
- **Nix Declarative Packaging (`envKinds/mane/ubuntu.nix`)**:
  - Add `pkgs.libreoffice` to `home.packages`.
  - Add `pkgs.simple-scan` to `home.packages`.
  - Drop `home.sessionVariables.GIO_EXTRA_MODULES = "${pkgs.gvfs}/lib/gio/modules"`. Thunar is already wrapped directly with its GIO modules; exporting this globally in user sessions leaked Nix GLIBC 2.38+ binaries to host Python/GTK tools running against Ubuntu 22.04's GLIBC 2.35.
- **Debloat Alignment (`layers/layer-3-ubuntu-mane-debloat.sh`)**:
  - Remove `simple-scan` from `BLOAT_PACKAGES` so `layer-3-ubuntu-mane-debloat.sh` does not trigger host purge of scanning libraries.
- **Host Remediation (Operator Action via `sudo`)**:
  - Restore host printer backend and install HP Device Manager: `sudo apt-get install -y --no-install-recommends python3-gi-cairo hplip-gui`.
  - Clean obsolete PPAs and dead `xenial` remotes from `/etc/apt/sources.list.d/` and `/etc/apt/sources.list`.

## Implementation Plan

- [ ] Update `envKinds/mane/ubuntu.nix`: add `pkgs.libreoffice` and `pkgs.simple-scan` to `home.packages`.
- [ ] Remove `GIO_EXTRA_MODULES` export from `home.sessionVariables` in `envKinds/mane/ubuntu.nix`.
- [ ] Remove `simple-scan` from `BLOAT_PACKAGES` in `layers/layer-3-ubuntu-mane-debloat.sh`.
- [ ] Run `bash layers/test-flake.sh` to verify flake build integrity.
- [ ] Verify `layers/layer-3-ubuntu-mane-debloat.sh --dry-run` executes cleanly.
- [ ] Verify Rofi indexing and desktop file generation.

## Decision Log

- Chose `pkgs.libreoffice` over standalone PDF editors (`xournalpp`, `pdfarranger`) to provide a full office suite (Writer, Calc, Impress) alongside LibreOffice Draw's direct text/vector PDF editing capabilities.
- Chose `pkgs.simple-scan` as the dedicated scanner frontend because `hp-toolbox` functions as a device manager and delegates actual scan acquisition to the system's scanning utility.
- Dropped the proposed host sidecar script (`ubuntu-host-update.sh`) in favor of direct operator maintenance outside `env`, keeping the repository focused on declarative configuration.
- Removed global `GIO_EXTRA_MODULES` session variable export to fix host binary GLIBC incompatibility; Thunar is already isolated via wrapper.

## Session Log

- 2026-09-13: Diagnosed host printer error as missing `python3-gi-cairo` and GLIBC version collision caused by `GIO_EXTRA_MODULES`.
- 2026-09-13: Clarified relationship between HP Device Manager (`hp-toolbox`) and Document Scanner (`simple-scan`).
- 2026-09-13: Authored feature spec `userland-apps-office-print.md`.
