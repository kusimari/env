# Feature: vscode-on-mane

## Git Setup

- Branch: feat/vscode-on-mane
- Base: main

## Feature Brief

Enable declarative Visual Studio Code installation and configuration for personal workstation (`mane`) environments via Nix and Home Manager. Provide the `code` binary, desktop application entry (accessible via application launcher/Rofi), and curated default extensions and user settings on `ubuntu-mane`, while maintaining Tier 3 isolation so headless work environments (`al2-kelasa`, `al2023-kelasa`) and darwin configurations remain lean and unaffected.

## Handoff

- **Phase:** review
- **Ready for:** human review (Agent-dev Review Gate)
- **Carry forward:** VS Code is successfully installed and verified under Tier 3 in `envKinds/mane/ubuntu.nix`. Spurious background update notifications are suppressed with `enableUpdateCheck = false` and `enableExtensionUpdateCheck = false`.
- **Deliberately left:** Headless kelasa environments remain free of VS Code and GUI packages; workspaces supply language servers via scoped direnv/mise.

## Requirements

- **Application Availability & Launch Experience**:
  - The `code` binary is installed in the Nix user profile and available on PATH in all interactive terminal sessions on `mane` machines.
  - Visual Studio Code appears in desktop application menus and is indexed by Rofi (`rofi -show drun`) for launch via application shortcut.
  - Visual Studio Code launches cleanly in the graphical desktop environment.
- **Preconfigured Editor Environment**:
  - Essential language and tooling extensions are installed declaratively: Rust Analyzer (`rust-lang.rust-analyzer`), Python (`ms-python.python`), Nix IDE (`jnoortheen.nix-ide`), Zenburn color theme (`ryanolsonx.zenburn`), and Cline / Claude Dev (`saoudrizwan.claude-dev`).
  - Pre-configured user settings are applied automatically: Zenburn color theme active by default, minimap auto-hidden, and inlay hints configured to `offUnlessPressed`.
  - Spurious update notifications on read-only Nix store derivations are suppressed (`enableUpdateCheck = false`, `enableExtensionUpdateCheck = false`).
- **Target Isolation & Invariant Cleanliness**:
  - Headless kelasa environments (`al2-kelasa`, `al2023-kelasa`) and darwin configurations remain completely free of VS Code and GUI dependencies.
  - Post-activation invariant verification (`nix run .#env-verify`) passes without expecting or requiring `code` on non-mane targets.

## Test Strategy

- Flake evaluation: `bash layers/test-flake.sh` verifies flake evaluation and activation package builds for both `ubuntu-mane` and `al2-kelasa`.
- Flake schema check: `nix flake check` passes cleanly without warnings or errors.
- Invariant isolation: Verify `env-verify` derivation does not list `code` under `invariants`, ensuring headless `kelasa` targets remain green.
- Package output verification: Verify `homeConfigurations.ubuntu-mane` exposes `programs.vscode` with `enable = true`, package `pkgs.vscode`, and the declared extension derivations.

## Design

- **Architecture & Ecosystem Fit**:
  - Scoped to Tier 3 (machine-class differences). Graphical desktop applications for `mane` live in `envKinds/mane/ubuntu.nix`.
  - In Home Manager, `programs.vscode` declaratively manages the application binary (`pkgs.vscode`), desktop entry file, extensions (`profiles.default.extensions`), and user settings (`profiles.default.userSettings`).
  - Home Manager 25.05 uses `profiles.default` for extension and settings declarations.
  - Overlays: `nix-vscode-extensions` overlay is already wired into `commonConfiguration` (`envKinds/common.nix`) and provides `pkgs.open-vsx` and `pkgs.vscode-marketplace`.
  - Migration from `home/home.nix`: Relocated `programs.vscode` definition from `home/home.nix` into `envKinds/mane/ubuntu.nix` with `enable = true`.
  - Keeping VS Code in `envKinds/mane/ubuntu.nix` mirrors all other desktop applications (`google-chrome`, `libreoffice`, `simple-scan`, `thunar`), eliminates dead code from the universal `home/home.nix`, and guarantees `env-verify.nix` does not treat `code` as an invariant for headless servers.
- **Extensions & Settings**:
  - `open-vsx`: `rust-lang.rust-analyzer`, `ms-python.python`, `jnoortheen.nix-ide`
  - `vscode-marketplace`: `ryanolsonx.zenburn`, `saoudrizwan.claude-dev`
  - User Settings: `editor.minimap.autohide: true`, `editor.inlayHints.enabled: "offUnlessPressed"`, `workbench.colorTheme: "Zenburn"`.
  - Update suppression: `enableUpdateCheck = false`, `enableExtensionUpdateCheck = false`.

## Implementation Plan

- [x] Relocate `programs.vscode` configuration from `home/home.nix` to `envKinds/mane/ubuntu.nix` with `enable = true`.
- [x] Clean up `home/home.nix` by removing the disabled `programs.vscode` block.
- [x] Update `project.md` documentation regarding `programs.vscode` status under `home/home.nix` and `envKinds/mane/ubuntu.nix`.
- [x] Run `bash layers/test-flake.sh` to verify build integrity across `ubuntu-mane` and `al2-kelasa`.
- [x] Verify `env-verify` invariant purity (ensure `code` is not leaked to kelasa).

## Session Log

- 2026-09-16: Received request to install Visual Studio Code on mane machines through Nix setup.
- 2026-09-16: Grounded codebase: inspected `flake.nix`, `home/home.nix`, `envKinds/mane/ubuntu.nix`, `envKinds/mane/home.nix`, and `env-verify.nix`.
- 2026-09-16: Analyzed invariant implications in `env-verify.nix`, identifying that enabling vscode in `home/home.nix` leaks `code` as an invariant to headless kelasa machines, whereas placing it in `envKinds/mane/ubuntu.nix` maintains clean Tier 3 isolation.
- 2026-09-16: Verified extension availability from `open-vsx` and `vscode-marketplace`. Authored initial feature spec `vscode-on-mane.md` and opened PR #55.
- 2026-09-16: User gave planning approval to proceed with build. Consolidated spec and advanced handoff to dev phase.
- 2026-09-16: Implemented `programs.vscode` in `envKinds/mane/ubuntu.nix` with `enable = true`, extensions, and settings. Removed dormant definition from `home/home.nix`. Updated `project.md`.
- 2026-09-16: Executed `bash layers/test-flake.sh` — verified clean build of VS Code 1.116.0, all extensions, and settings on `ubuntu-mane`, and clean headless evaluation of `al2-kelasa`.
- 2026-09-16: Verified `env-verify` invariant purity — confirmed `code` is not in invariants and does not leak to `kelasa`.
- 2026-09-16: Dispatched fresh-context Code Review Gate subagent — received PASS WITH NOTES; applied update suppression (`enableUpdateCheck = false`, `enableExtensionUpdateCheck = false`).
- 2026-09-16: Re-ran `nix flake check` and test suites — clean pass.

## Decision Log

- Decided to treat Visual Studio Code as a Tier 3 application scoped to `ubuntu-mane` in `envKinds/mane/ubuntu.nix` rather than enabling it conditionally in `home/home.nix`. Rationale: Defining `programs.vscode` in `home/home.nix` causes `env-verify.nix` to evaluate `code` as an invariant binary that must exist across all targets (including headless AL2/AL2023), causing verification failures on remote servers. Keeping it in `envKinds/mane/ubuntu.nix` mirrors all other desktop applications (`google-chrome`, `libreoffice`, `simple-scan`).
- Suppressed background update checks (`enableUpdateCheck = false`, `enableExtensionUpdateCheck = false`) to avoid spurious notification popups on immutable Nix store installations.
