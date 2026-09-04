# Antigravity CLI Installation

## How to use this session file
This file tracks the design, implementation, and verification for installing the Antigravity CLI in the \`env\` flake.

## Feature Brief
Migrate the Google Antigravity CLI package source to the dedicated \`jacopone/antigravity-nix\` flake overlay and scope its installation to \`mane\` (personal workstation) environments under Tier 3 (\`home/envKind-mane.nix\`), removing the package from Tier 1 global packages (\`home/home.nix\`).

## Context & Motivation
- Previously, the CLI was pulled via an ad-hoc unmerged \`nixpkgs\` branch (\`deftdawg/nixpkgs/add-antigravity-cli-package\`) via \`callPackage\`, and placed in Tier 1 (\`home/home.nix\`).
- The upstream packaging is now maintained via the \`jacopone/antigravity-nix\` flake input and standard overlay.
- Antigravity CLI is intended for personal environments (\`mane\`) rather than every machine class (\`kelasa\`), moving it from Tier 1 (nix-managed, every env) to Tier 3 (per-env differences in \`home/envKind-mane.nix\`).

## Requirements
- [x] Add \`antigravity\` flake input pointing to \`github:jacopone/antigravity-nix\` with \`inputs.nixpkgs.follows = "nixpkgs"\`.
- [x] Remove obsolete \`nixpkgs-antigravity-cli\` flake input and manual \`callPackage\` overlay.
- [x] Apply \`inputs.antigravity.overlays.default\` in \`flake.nix\` (\`commonConfiguration.nixpkgs.overlays\`).
- [x] Keep \`env-verify.nix\`'s \`mkPkgs\` overlay list in sync with \`commonConfiguration\`.
- [x] Remove \`antigravity-cli\` from global Tier 1 packages in \`home/home.nix\`.
- [x] Add \`pkgs.google-antigravity-cli\` to \`home/envKind-mane.nix\` (Tier 3).
- [x] Update \`flake.lock\` to lock the new input.
- [x] Verify configuration against the Test Gate (\`nix flake check\`, \`test-flake.sh\`, shellcheck, rclone tests).

## Design
1. **Flake Overlay Integration**:
   - \`flake.nix\`:
     \`\`\`nix
     antigravity = {
       url = "github:jacopone/antigravity-nix";
       inputs.nixpkgs.follows = "nixpkgs";
     };
     \`\`\`
   - \`commonConfiguration.nixpkgs.overlays\`:
     \`\`\`nix
     inputs.antigravity.overlays.default
     \`\`\`
2. **Tier-3 Scoping (\`home/envKind-mane.nix\`)**:
   - Tier-3 packages only affect targeted environments (\`ubuntu-mane\`). Excluded on \`kelasa\` targets without affecting global PATH invariants evaluated by \`env-verify\`.
   - \`home/envKind-mane.nix\`:
     \`\`\`nix
     home.packages = [
       pkgs.tailscale
       pkgs.google-antigravity-cli
     ];
     \`\`\`
3. **Verification Tooling Sync (\`env-verify.nix\`)**:
   - \`env-verify.nix\` mirrors \`commonConfiguration\` overlays to evaluate \`mane\` and \`kelasa\` packages. Adding \`inputs.antigravity.overlays.default\` prevents attribute missing errors during \`nix flake check\`.

## Implementation Plan
1. ✅ **Update flake inputs and overlays** - Add \`antigravity\` input to \`flake.nix\` and \`env-verify.nix\`, removing legacy inputs.
2. ✅ **Scope package to mane** - Remove from \`home/home.nix\` and add \`pkgs.google-antigravity-cli\` to \`home/envKind-mane.nix\`.
3. ✅ **Update flake.lock** - Re-lock inputs.
4. ✅ **Test Gate verification** - Run \`nix flake check\` and \`layers/test-flake.sh\`.

## Session Log
### 2026-09-03 - Initial Spec & Verification
- Code changes integrated across \`flake.nix\`, \`flake.lock\`, \`home/home.nix\`, and \`home/envKind-mane.nix\`.
- Synced overlays in \`env-verify.nix\`.
- Validated with \`nix flake check\` and \`layers/test-flake.sh\`. All tests pass.
