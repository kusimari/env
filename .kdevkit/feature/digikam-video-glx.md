# Feature: digikam-video-glx

## Git Setup

- Branch: fix/digikam-video-glx
- Base: main

## Feature Brief

Wrap DigiKam and Showfoto with `QT_XCB_GL_INTEGRATION=none` to prevent fatal GLX initialization crashes when previewing or playing video files on Ubuntu-mane under X11.

## Handoff

Phase: closed

## Requirements

- DigiKam launched via application launchers (Rofi, GNOME desktop) or terminal does not crash when previewing video files.
- Showfoto has the same wrapper applied for consistency.
- Existing `QT_PLUGIN_PATH` (with `kimageformats` and `qtimageformats`) remains intact.
- If a user explicitly sets `QT_XCB_GL_INTEGRATION` in their shell, their explicit choice is respected (`--set-default`).
- `test-flake.sh` and `nix flake check` pass cleanly.

## Test Strategy

- `layers/test-flake.sh` to verify build integrity for both `ubuntu-mane` and `al2-kelasa`.
- Inspect generated wrapper script to verify `--set-default QT_XCB_GL_INTEGRATION none` produces `QT_XCB_GL_INTEGRATION=${QT_XCB_GL_INTEGRATION:-none}`.
- Verify binary execution with `digikam -v`.

## Design

- In `envKinds/mane/ubuntu.nix`, update `wrapProgram` calls for `digikam` and `showfoto` to pass `--set-default QT_XCB_GL_INTEGRATION none`.
- This ensures Qt XCB uses software raster rendering instead of attempting to negotiate GLX visual/FBConfig across hybrid GPU drivers on X11.

## Implementation Plan

- [x] Update `envKinds/mane/ubuntu.nix` wrapper with `--set-default QT_XCB_GL_INTEGRATION none`
- [x] Run `bash layers/test-flake.sh` to verify flake build
- [x] Verify `nix flake check` and `shellcheck`
- [x] Push to `origin fix/digikam-video-glx` and open PR

## Session Log

- 2026-09-08: Investigated crash when previewing video files. Traced to `qglx_findConfig: Failed to finding matching FBConfig` / `Could not initialize GLX` calling `qFatal` / `abort()`. Confirmed `QT_XCB_GL_INTEGRATION=none` resolves the crash.
- 2026-09-08: Added `--set-default QT_XCB_GL_INTEGRATION none` to both `digikam` and `showfoto` wrappers in `envKinds/mane/ubuntu.nix`. Verified wrapper generation and test-flake / flake-check quality gates.
- 2026-09-08: Opened PR #50 and verified all gates. Reconciled and closed out spec.

## Decision Log

- Use `--set-default QT_XCB_GL_INTEGRATION none` rather than `--set` so users can still override with hardware acceleration if desired.
