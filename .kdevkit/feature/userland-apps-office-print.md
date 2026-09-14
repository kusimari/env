# Feature: userland-apps-office-print

## Git Setup

- Branch: feat/userland-apps-office-print
- Base: main

## Feature Brief

Add declarative userland applications for office productivity, PDF editing, and document scanning on `ubuntu-mane`. Provide LibreOffice suite (with LibreOffice Draw for vector and text PDF editing) and Document Scanner (`simple-scan`) declaratively through Nix. Establish a shared, idempotent host hardware/OS preparation script (`layers/layer-ubuntu-mane-host-prep.sh`) invoked by both Layer 1 (genesis) and Layer 3 (environment setup) to ensure host printing and scanning drivers (`hplip`, `hplip-gui`, `python3-gi-cairo`) are present on both fresh installs and day-2 rebuilds. Resolve the host print queue crash and Apport errors by eliminating host GIO environment pollution, aligning the debloat package list, and documenting the "Environment Setup" role of Layer 3 in `project.md`.

## Handoff

Phase: closed

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
- **Deterministic Machine Lifecycle & Layer Coherence**:
  - A brand-new machine running Layer 1 installs host printing/imaging drivers alongside base system tools.
  - An existing machine rebuilding via Layer 3 (or `layer-run`) automatically ensures host hardware dependencies are met before the Nix generation switch.
  - Day-2 executions incur zero overhead and zero interactive `sudo` prompts when host dependencies are already satisfied.
- **Clean Desktop Integration**:
  - LibreOffice apps and Document Scanner are automatically indexed by Rofi (`rofi -show drun`).
  - Headless kelasa environments (`al2-kelasa`, `al2023-kelasa`) and darwin configurations remain unaffected.

## Test Strategy

- Flake evaluation: `bash layers/test-flake.sh` verifies flake evaluation across `ubuntu-mane` and `al2-kelasa`.
- Host prep idempotence: Verify `layers/layer-ubuntu-mane-host-prep.sh --dry-run` and normal run execute cleanly and exit in milliseconds when packages are already installed.
- Debloat alignment: Verify `layers/layer-3-ubuntu-mane-debloat.sh --dry-run` does not target `simple-scan`.
- Binary and desktop entry verification: Assert `libreoffice` and `simple-scan` binaries exist in the built Nix profile and have valid `.desktop` files in `share/applications/`.
- Session variable isolation: Verify `GIO_EXTRA_MODULES` is no longer exported in `home.sessionVariables`, preventing GLIBC version conflicts on host binaries.

## Design

- **Architecture & Ecosystem Fit**:
  - Thin Host OS owns hardware drivers, display server, and base printing/scanning daemons (CUPS, SANE, HPLIP driver).
  - Host GUI tool `hplip-gui` (`hp-toolbox`) provides hardware status, ink monitoring, and print queue management for the HP OfficeJet Pro 9010.
  - Nix owns userland desktop applications: `pkgs.libreoffice` and `pkgs.simple-scan`.
  - `hp-toolbox` delegates scanning to the desktop's registered scanner frontend (`simple-scan`).
- **Shared Host Preparation Script (`layers/layer-ubuntu-mane-host-prep.sh`)**:
  - Standalone, executable bash script with `--dry-run` support and `dpkg -s` checks.
  - Enforces presence of `python3-gi-cairo`, `hplip`, and `hplip-gui`.
  - Invoked during Layer 1 (`layer-1-ubuntu-mane.sh`) for Day-1 Genesis.
  - Invoked at the head of Layer 3 (`layer-3-ubuntu-mane.sh`) for Day-2 Convergence before the Nix switch.
  - Checks if packages are already installed; if all present, exits immediately without `sudo` prompt.
- **Layer 3 Framing ("The Environment Setup")**:
  - Layer 3 is framed as "Environment Setup" for `ubuntu-mane`, orchestrating the 3-step cycle:
    1. Pre-Nix host OS appliance prep (`layer-ubuntu-mane-host-prep.sh`).
    2. Nix userland generation switch (`home-manager switch`).
    3. Post-Nix host OS debloat (`layer-3-ubuntu-mane-debloat.sh`).
  - Documented in `project.md` why this is kept under Layer 3 rather than introducing sub-layers like L3.1/L3.2 (preserving the stable 1–7 numbering contract in `layer-run` across all platforms and targets).
- **Nix Declarative Packaging (`envKinds/mane/ubuntu.nix`)**:
  - Add `pkgs.libreoffice` to `home.packages`.
  - Add `pkgs.simple-scan` to `home.packages`.
  - Drop `home.sessionVariables.GIO_EXTRA_MODULES = "${pkgs.gvfs}/lib/gio/modules"`. Thunar is already wrapped directly with its GIO modules; exporting this globally in user sessions leaked Nix GLIBC 2.38+ binaries to host Python/GTK tools running against Ubuntu 22.04's GLIBC 2.35.
- **Debloat Alignment (`layers/layer-3-ubuntu-mane-debloat.sh`)**:
  - Remove `simple-scan` from `BLOAT_PACKAGES` so `layer-3-ubuntu-mane-debloat.sh` does not trigger host purge of scanning libraries.

## Implementation Plan

- [x] Create `layers/layer-ubuntu-mane-host-prep.sh` with `dpkg -s` check, `--dry-run`, and package installation for `python3-gi-cairo`, `hplip`, and `hplip-gui`.
- [x] Wire `layers/layer-1-ubuntu-mane.sh` to invoke `layer-ubuntu-mane-host-prep.sh`.
- [x] Wire `layers/layer-3-ubuntu-mane.sh` to invoke `layer-ubuntu-mane-host-prep.sh` before `layer-3-common.sh`.
- [x] Remove `simple-scan` from `BLOAT_PACKAGES` in `layers/layer-3-ubuntu-mane-debloat.sh`.
- [x] Update `envKinds/mane/ubuntu.nix`: add `pkgs.libreoffice` and `pkgs.simple-scan`, and remove `GIO_EXTRA_MODULES` export.
- [x] Document Layer 3 "Environment Setup" architecture and L3.1/L3.2 rationale in `.kdevkit/project.md`.
- [x] Run `bash layers/test-flake.sh` to verify build integrity across targets.
- [x] Verify `layers/layer-ubuntu-mane-host-prep.sh --dry-run` and `layers/layer-3-ubuntu-mane-debloat.sh --dry-run`.

## Decision Log

- Chose `pkgs.libreoffice` over standalone PDF editors (`xournalpp`, `pdfarranger`) to provide a full office suite (Writer, Calc, Impress) alongside LibreOffice Draw's direct text/vector PDF editing capabilities.
- Chose `pkgs.simple-scan` as the dedicated scanner frontend because `hp-toolbox` functions as a device manager and delegates actual scan acquisition to the system's scanning utility.
- Preserved the L1–L7 layer numbering contract in `project.md` and `layer-run` rather than creating sub-layers (L3.1, L3.2). Layer 3 acts as "Environment Setup" encapsulating pre-Nix host prep, Nix userland switch, and post-Nix debloat.
- Extracted host hardware preparation into a shared script (`layer-ubuntu-mane-host-prep.sh`) invoked by both Layer 1 (Day-1 Genesis) and Layer 3 (Day-2 Convergence).
- Removed global `GIO_EXTRA_MODULES` session variable export to fix host binary GLIBC incompatibility; Thunar is already isolated via wrapper.
- Elevated official layer naming across the repo (`layer-run`, `project.md`, `README.md`, layer scripts): L0 (Curl Bootstrap), L1 (Host Genesis), L2 (Env Repo Genesis), L3 (The Environment Setup), L4 (Enterprise Overlays), L5 (Stores), L6 (Tools), L7 (Projects).

## Session Log

- 2026-09-13: Diagnosed host printer error as missing `python3-gi-cairo` and GLIBC version collision caused by `GIO_EXTRA_MODULES`.
- 2026-09-13: Clarified relationship between HP Device Manager (`hp-toolbox`) and Document Scanner (`simple-scan`).
- 2026-09-13: Authored initial feature spec `userland-apps-office-print.md` and opened PR #54.
- 2026-09-13: Addressed layer architecture feedback: unified host hardware preparation into a shared script across L1 and L3, and framed L3 as "The Environment Setup" with rationale in `project.md`. Consolidated spec for dev.
- 2026-09-13: Implemented shared host prep script, wired L1 and L3, un-blacklisted simple-scan in debloat, added libreoffice and simple-scan to Nix userland, removed GIO leak, documented architecture in project.md, and verified clean flake build via test-flake.sh.
- 2026-09-13: Applied unified layer naming convention (Host Genesis, Env Repo Genesis, The Environment Setup, Enterprise Overlays, Stores, Tools, Projects) across `layer-run` (purpose dictionary, usage help text, execution banners), `project.md` (table column), `README.md` (philosophy section), and layer script headers.
- 2026-09-13: Reconciled in-flight markers, updated Handoff to closed, and verified closure criteria.
