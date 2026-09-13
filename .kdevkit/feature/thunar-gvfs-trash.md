# Feature: thunar-gvfs-trash

## Git Setup

- Branch: fix/thunar-gvfs-trash
- Base: main

## Feature Brief

Restore GVFS and Trash support in Thunar on `ubuntu-mane`. When Thunar is installed as a standalone package via Nix on Ubuntu, its binary wrapper lacks `pkgs.gvfs` in `GIO_EXTRA_MODULES`, causing GIO URI scheme probing to fail for `trash://`, `computer://`, `recent://`, and `network://`. This feature wraps Thunar with GVFS GIO extension modules and integrates `thunar-archive-plugin` so Trash appears in the sidepane without warnings.

## Handoff

Phase: plan
Ready for: dev
Carry forward: Host already runs gvfsd/gvfsd-trash; Nix wrapper must point GIO_EXTRA_MODULES to pkgs.gvfs/lib/gio/modules
Deliberately left: Headless kelasa environments do not require graphical GVFS or Thunar

## Requirements

- **GVFS Support in Thunar**:
  - `thunar` launches without the warning banner: `"It looks like gvfs is not available. Important features including trash support... will not work"`.
  - The **Trash** (`trash:///`) shortcut is displayed in the left sidepane under Places.
  - Deleting files in Thunar offers "Move to Trash" and moves items to the desktop trash folder.
- **Archive Integration in File Manager**:
  - Right-clicking files and folders in Thunar displays archive actions ("Create Archive...", "Extract Here", "Extract To...").
- **Session-Wide Virtual Filesystem & Scheme Support**:
  - Graphical desktop applications and file choosers have access to desktop virtual filesystem schemes (`trash://`, `recent://`, `network://`).
- **Compatibility**:
  - Headless kelasa targets (`al2-kelasa`, `al2023-kelasa`, `darwin-kelasa`) remain unaffected.
  - `layers/test-flake.sh` passes cleanly without evaluation errors.

## Test Strategy

- Flake evaluation: `layers/test-flake.sh` verifies flake build integrity for both `ubuntu-mane` and `al2-kelasa`.
- Wrapper inspection: verify `thunar` wrapper script in the Nix store contains `--prefix GIO_EXTRA_MODULES` pointing to `gvfs/lib/gio/modules`.
- Scheme probing: verify `g_vfs_get_supported_uri_schemes()` or `gio info trash:///` recognizes the `trash` scheme under the Nix environment.
- Live verification:
  - Run `bash layers/layer-3-ubuntu-mane.sh` to activate generation.
  - Launch `thunar` and verify the warning banner is gone.
  - Verify "Trash" appears under Places in the sidepane.
  - Right-click a file to verify "Move to Trash" is available.
  - Right-click an archive/folder to verify archive plugin context actions appear.

## Design

- **Root Cause Analysis**:
  - In Nixpkgs, `thunar` is wrapped only with `dconf` and `xfconf` GIO modules.
  - On standalone Home Manager installations on non-NixOS Linux, `services.gvfs` does not exist as a Home Manager service. While Ubuntu's host session runs the D-Bus daemon (`/usr/libexec/gvfsd` and `/usr/libexec/gvfsd-trash`), Nix binaries cannot load Ubuntu's host `/usr/lib/x86_64-linux-gnu/gio/modules/libgvfsdbus.so` due to glibc incompatibility (GLIBC 2.38 requirement mismatch).
  - Without `libgvfsdbus.so` from `pkgs.gvfs`, GIO's `g_vfs_get_supported_uri_schemes()` returns only `['file', 'resource']`.
  - In Thunar's `thunar-shortcuts-model.c`, `thunar_g_vfs_is_uri_scheme_supported("trash")` checks for `"trash"` in that list. Since it is absent, Thunar hides the Trash entry from the sidepane and warns that GVFS is unavailable.
- **Technical Approach**:
  - In `envKinds/mane/ubuntu.nix`, define a wrapped Thunar package (`thunar-wrapped`) using `pkgs.symlinkJoin` and `pkgs.makeWrapper`:
    - Override `pkgs.thunar` with `thunarPlugins = [ pkgs.thunar-archive-plugin ]`.
    - Join with `pkgs.gvfs`.
    - Wrap `bin/thunar` and `bin/thunar-settings` with `--prefix GIO_EXTRA_MODULES : "${pkgs.gvfs}/lib/gio/modules"` and `--prefix XDG_DATA_DIRS : "${pkgs.gvfs}/share"`.
  - Add `pkgs.gvfs` to `home.packages`.
  - Set `home.sessionVariables.GIO_EXTRA_MODULES = "${pkgs.gvfs}/lib/gio/modules";` so graphical userland tools inherit GIO URI scheme awareness.

## Implementation Plan

- [ ] Define `thunar-wrapped` with `pkgs.gvfs` and `pkgs.thunar-archive-plugin` in `envKinds/mane/ubuntu.nix`
- [ ] Export `GIO_EXTRA_MODULES` in `home.sessionVariables` in `envKinds/mane/ubuntu.nix`
- [ ] Run `bash layers/test-flake.sh` to verify build integrity for all targets
- [ ] Activate generation via `bash layers/layer-3-ubuntu-mane.sh` and verify Thunar sidepane trash functionality

## Decision Log

- Wrapped Thunar directly via `symlinkJoin` and `makeWrapper` (mirroring the `digikam-wrapped` pattern in `ubuntu.nix`) rather than relying purely on shell environment variables. This guarantees that GUI launchers, desktop shortcuts, and D-Bus invocations always inherit `GIO_EXTRA_MODULES`.
- Wired `thunar-archive-plugin` into `thunarPlugins` override to ensure plugins are in `THUNARX_DIRS`.

## Session Log

- 2026-09-13: Investigated Thunar GVFS and Trash issue on `ubuntu-mane`. Identified missing `libgvfsdbus.so` in `GIO_EXTRA_MODULES` causing `thunar_g_vfs_is_uri_scheme_supported("trash")` to return false. Verified that wrapping Thunar with `pkgs.gvfs` exposes all URI schemes (`trash`, `computer`, `recent`, `sftp`, `network`). Authored feature spec.
