# Tmux Pane Highlighting

## How to use this session file
This file tracks the progress of the "Tmux Pane Highlighting" feature.
- **When starting:** Read this file to understand the current state, requirements, and plan.
- **During work:** Update the "Implementation Plan" status (📋 -> ⏳ -> ✅) as you complete steps.
- **When ending:** Add a new entry to the "Session Log" with the date, achievements, and current status. Update the requirements if they change.

## Feature Brief
Highlight the active pane in tmux to make it easily distinguishable from inactive panes. This improves visibility and reduces confusion when working with multiple panes.

## Requirements
- [x] Configure `pane-active-border-style` to `fg=yellow` for the active pane.
- [x] Use `pane-border-lines double` for inactive pane borders.
- [x] Display `pane-border-indicators` as `arrows`.
- [x] Set `pane-border-status` to `top` to create a header for each pane.
- [x] Configure `pane-border-format` to show the pane number and current path (`" #P #{pane_current_path}"`).
- [x] Implement configuration via Home Manager in `home/home.nix`.
- [x] Verify changes.

## Implementation Plan
1. ✅ **Analyze Configuration** - Confirm tmux is managed via Home Manager in `home/home.nix`.
2. ✅ **Modify Configuration** - Update `home/home.nix` with `extraConfig` to set pane styles.
3. ✅ **Verify & Apply** - Changes applied to `home/home.nix`. User rebuilt and reloaded.
4. ✅ **Enhance Border** - Enabled `pane-border-status top`.
5. ✅ **Refine Styling** - Iterated on styling to reach final configuration.

**Status Legend:** 📋 Not Started | ⏳ In Progress | ✅ Complete

## Testing Instructions
You can test the tmux configuration by rebuilding your home manager or darwin environment:

```bash
# Apply via Home Manager / Darwin
darwin-rebuild switch
# or
home-manager switch
```

Then restart tmux or reload the config:
```bash
tmux source-file ~/.config/tmux/tmux.conf
# or simply kill the server and restart
tmux kill-server && tmux
```

## Session Log
### 2026-01-20 - Final Configuration
- **Action:** Finalized tmux configuration in `home/home.nix` based on user feedback.
- **Goal:** Implement a clear and functional pane highlighting scheme.
- **Final Configuration:**
    - `pane-active-border-style`: `fg=yellow`
    - `pane-border-lines`: `double`
    - `pane-border-indicators`: `arrows`
    - `pane-border-status`: `top`
    - `pane-border-format`: `" #P #{pane_current_path}"`
- **Current State:** Feature complete and verified by the user.

### 2026-01-20 - Revised Formatting (No Conditional)
- **Action:** Modified `home/home.nix` to simplify tmux pane formatting. Removed conditional `pane-border-format`.
- **Goal:** Achieve desired "Status Bar" styling for active/inactive panes without conditional logic in `pane-border-format`.
- **Details:**
    - `pane-border-style` set to `fg=colour247,bg=colour238` (light grey text on dark grey background) for inactive panes.
    - `pane-active-border-style` updated to `fg=black,bg=yellow` (black text on yellow background) for active panes.
    - `pane-border-format` set to `"#P"`, allowing it to inherit colors from the border styles.
- **Current State:** Configuration updated in `home/home.nix`.
- **Next Step:** User is performing `darwin-rebuild switch` and reloading tmux to verify.

### 2026-01-20 - Refinement: Status Bar Styling
- **Action:** Updated `home/home.nix` with "Status Bar" inspired styling.
- **Goal:** Make pane headers look similar to the tmux status line for visual consistency.
- **Details:**
    - Active Pane: Black text on yellow background.
    - Inactive Pane: Light grey text on dark grey background.
- **Current State:** Configuration updated.
- **Next Step:** User is performing `darwin-rebuild switch` and reloading tmux to verify.

### 2026-01-17 - Configuration Verified & Applied
- **Action:** Updated `home/home.nix` with verified `extraConfig`.
- **Configuration Details:**
  - `pane-border-status top`: Enables top border.
  - `pane-border-style`: Dark grey (colour238) for inactive.
  - `pane-active-border-style`: Blue for active.
  - `pane-border-format`: Custom format using conditional logic `#{?pane_active,...}` to style the pane number (`#P`) in bold blue for active panes and grey for inactive ones.
- **Verification:** Web search confirmed `pane-border-format` and conditional syntax.
- **Re-Verification:** Double-checked `#{?pane_active,...}` syntax on user request. Confirmed it is the standard method for conditional styling in tmux.
- **Current State:** Changes applied to `home/home.nix`. Ready for system rebuild.
- **Next Step (Manual):** User will manually test the configuration via `darwin-rebuild switch`.
- **Pending Task:** Once verified, move the `extraConfig` to a separate `home/tmux.conf` file and load it using `builtins.readFile` (or similar) to keep `home.nix` clean.

### 2026-01-17 - Restart: Fresh Implementation
- **Action:** Removed `home/tmux.conf`. User cleared `extraConfig` in `home/home.nix`.
- **Goal:** Implement pane highlighting requirements from scratch.
- **Current State:** Clean slate. Ready to implement `extraConfig` in `home/home.nix`.

### 2026-01-17 - REVERTED: Back to Inline extraConfig
- **Test Result:** Darwin rebuild with `builtins.readFile ./tmux.conf` approach **FAILED**
- **Action Taken:** Reverted to inline `extraConfig` in `home/home.nix`
  - Removed `builtins.readFile ./tmux.conf`
  - Added inline tmux configuration directly in `extraConfig = ''...''`
  - Configuration includes: pane border styling + `pane-border-status top`
- **Current State:** Ready for testing with darwin rebuild
- **Next Step:** Run `darwin-rebuild switch` to test if inline config works properly

**🚨 PENDING: Test darwin rebuild with inline extraConfig to confirm functionality**

### 2026-01-17 - Simplification: External Config & Yellow Background
- **Objective:** Simplify config and enable easier testing without full system rebuild.
- **Changes:**
  - Created separate `tmux.conf` file in `/Users/gorantls/env/home/` for easier testing
  - Simplified to focus on just top border with yellow background (like tmux status line)
  - Updated `home.nix` to use `builtins.readFile ./tmux.conf` instead of inline `extraConfig`
  - Added testing instructions to session file
- **Benefits:**
  - Can test config directly with `tmux -f /path/to/tmux.conf new-session`
  - No need to rebuild entire darwin/home-manager for config tweaks
  - Yellow background provides better visual distinction like status line
- **Current status:** Ready for testing. Config available at `/Users/gorantls/env/home/tmux.conf`

### 2026-01-17 - Enhancement: Border Visibility
- **Feedback:** User wants a "full surround" line (top, bottom, left, right).
- **Research:** Tmux shares borders. Native "outer" borders (left/right edges of terminal) aren't supported.
- **Decision:** Enable `pane-border-status top`. This puts a line at the top of *every* pane (including the top-most one).
    - This creates a "header" for each pane.
    - The bottom of the top pane (which is the top of the bottom pane) is already drawn.
    - This results in more visible "lines" than before.
- **Action:** Update `home/home.nix` with `set -g pane-border-status top`.

### 2026-01-17 - Implementation Complete
- **Session Focus:** Implementation of tmux pane highlighting.
- **Achievements:**
    - Modified `home/home.nix` to include `pane-border-style` and `pane-active-border-style`.
    - Active pane will be highlighted in blue, inactive in dark grey (colour238).
- **Current status:** Implementation complete. Pending user application (e.g., `darwin-rebuild switch` or `home-manager switch`).

### 2026-01-17 - Session Start
- **Session Focus:** Initial research and planning.
- **Achievements:**
    - Created session file.
    - Researched tmux configuration for pane highlighting.
    - Identified `home/home.nix` as the configuration source.
- **Current status:** Ready to implement changes in `home/home.nix`.

## Useful References
<!-- Instructions: Add helpful URLs, docs, tutorials as you find them -->
