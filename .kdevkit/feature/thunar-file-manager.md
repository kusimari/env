# Feature: thunar-file-manager

## Git Setup

- Branch: feat/thunar-file-manager
- Base: main

## Feature Brief

Add the lightweight Thunar file manager to the `ubuntu-mane` Nix configuration to replace Nautilus with a lean, responsive GUI file explorer that does not leave persistent background daemons in memory.

## Handoff

Phase: planning
Ready for: user review of spec and application inventory
Carry forward: public repo hygiene — no internal markers

## Requirements

- Thunar is installed declaratively via Nix on `ubuntu-mane`.
- Thunar desktop entry is available and searchable via Rofi (`rofi -show drun`).
- Thunar launches quickly, respects GTK styling, and exits cleanly without persistent background processes.
- Directory MIME handler (`inode/directory`) can be configured to default to Thunar.
- Headless kelasa environments (`al2-kelasa`, `al2023-kelasa`) and darwin configurations remain unaffected.
- `layers/test-flake.sh` passes cleanly.

## Test Strategy

- `layers/test-flake.sh` to verify flake build integrity for both `ubuntu-mane` and `al2-kelasa`.
- Verify `thunar` binary on PATH and `thunar --version`.
- Verify `thunar.desktop` is registered in `~/.nix-profile/share/applications/`.
- Test launching via Rofi and direct invocation.

## Design

- In `envKinds/mane/ubuntu.nix`, add `pkgs.xfce.thunar` and `pkgs.xfce.thunar-archive-plugin` to `home.packages`.
- Thunar is a Tier 3 Linux GUI package specific to `ubuntu-mane`.
- Does not affect `envKinds/common.nix` or headless machines.
- Once verified, `xdg-mime default thunar.desktop inode/directory` sets it as the default handler for folder browsing from Chrome and other applications.

## Implementation Plan

- [ ] Add `pkgs.xfce.thunar` and `pkgs.xfce.thunar-archive-plugin` to `envKinds/mane/ubuntu.nix`
- [ ] Run `bash layers/test-flake.sh` to verify build integrity
- [ ] Rebuild Home Manager generation (`bash layers/layer-3-ubuntu-mane.sh`)
- [ ] Verify `thunar` in Rofi and launch behavior
- [ ] Set Thunar as default file manager via `xdg-mime`

## Session Log

- 2026-09-12: Feature started in worktree `feat-thunar`. Grounded on user-facing application inventory across APT, Snap, and Nix. Spec authored.
