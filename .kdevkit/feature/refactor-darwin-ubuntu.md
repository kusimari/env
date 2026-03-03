# Refactor Darwin Ubuntu

## How to use this session file
This session file contains comprehensive specifications generated through structured interviews. Each section serves dual purposes:
1. **Decision Record**: Captures user thoughts, preferences, and rationale for future reference
2. **Agent Instructions**: Provides clear, actionable guidance for coding agents to implement the feature

Coding agents should read the entire specification before beginning work, paying particular attention to the Requirements, Technical Design, and Test Strategy sections.

## Feature Brief
Merge separate darwin/flake.nix and ubuntu/flake.nix files into one unified flake.nix that supports both platforms through flake fragments, while maintaining existing run.sh script functionality and standardizing on stable nixpkgs.

## Requirements Specification

### Functional Requirements
- **FR1**: Create unified flake.nix at `/nix/flake.nix` (root level)
- **FR2**: Support both `darwinConfigurations` and `homeConfigurations` output types in single flake
- **FR3**: Maintain functionality of existing `darwin/run.sh` and `ubuntu/run.sh` scripts
- **FR4**: Enable platform selection through flake fragments (`--flake .#darwin` vs `--flake .#ubuntu`)
- **FR5**: Standardize on stable nixpkgs (remove nixpkgs-unstable complexity)
- **FR6**: Fix existing bug where Ubuntu flake references claude-code overlay without input definition
- **FR7**: Absorb `/nix/common/common-inputs.nix` functionality into unified flake
- **FR8**: Preserve `/nix/common/common-run.sh` functionality (still needed by both run scripts)

### Non-Functional Requirements
- **NFR1**: No functionality loss compared to current separate flakes
- **NFR2**: Maintainable code structure for future platform additions
- **NFR3**: Preserve existing build performance
- **NFR4**: Follow Nix flake best practices and conventions

### User Stories
- **As a developer**, I want to modify platform configurations in one place so that I can maintain consistency across platforms
- **As a system administrator**, I want to rebuild nix using the same run.sh scripts so that my workflow remains unchanged
- **As a maintainer**, I want a single flake file so that shared dependencies and overlays are easier to manage

### Success Criteria
- Both `darwin/run.sh` and `ubuntu/run.sh` work identically to before the refactor
- Single `/nix/flake.nix` file exists with no separate platform flakes
- `/nix/common/common-inputs.nix` can be deleted (functionality absorbed)
- `/nix/common/common-run.sh` continues to work (still needed)
- No build errors on either platform
- Identical system/home configurations before and after refactor

### Constraints
- Must maintain backward compatibility with existing run scripts during transition
- Cannot break existing CI/CD processes (if any)
- Must preserve platform-specific differences (darwin system config vs ubuntu home-manager only)

## Technical Design Specification

### Architecture Overview
Unified flake.nix will contain both `darwinConfigurations` and `homeConfigurations` outputs, with platform selection handled through flake fragments passed by the respective run.sh scripts.

### Technical Decisions
- **Flake Location**: `/nix/flake.nix` (root level for cleaner structure)
- **Platform Selection**: Flake fragments approach (`--flake .#darwin` vs `--flake .#ubuntu`)
- **Input Strategy**: Standardize on stable nixpkgs only (remove nixpkgs-unstable)
- **Parameter Passing**: Update run scripts to pass platform-specific fragment identifiers
- **Common Directory**: Absorb `common-inputs.nix` into flake, preserve `common-run.sh`

### Data Model
**Current Architecture**:
```
/nix/
├── darwin/flake.nix (2,536 bytes) - nix-darwin system config
├── ubuntu/flake.nix (1,522 bytes) - home-manager only config
├── common/common-inputs.nix (shared overlays)
└── common/common-run.sh (shared script logic)
```

**Target Architecture**:
```
/nix/
├── flake.nix (unified, ~3,000 bytes)
├── run.sh (shared run logic, moved from common/)
├── darwin/run.sh (updated to use fragments)
├── ubuntu/run.sh (updated to use fragments)
└── home/ (unchanged shared config)
```

### API/Interface Design
**Flake Outputs Structure**:
```nix
{
  darwinConfigurations = {
    default = nix-darwin.lib.darwinSystem { /* darwin config */ };
  };

  homeConfigurations = {
    default = home-manager.lib.homeManagerConfiguration { /* ubuntu config */ };
  };
}
```

**Run Script Interface**:
- Darwin: `--flake "$FLAKE_DIR#darwin"` or similar fragment
- Ubuntu: `--flake "$FLAKE_DIR#ubuntu"` or similar fragment

### Security Model
- Preserve existing security model (nix-darwin system-level vs home-manager user-level)
- Maintain sudo requirements for darwin (system changes) vs no-sudo for ubuntu (user changes)

### Dependencies
**Unified Input Set** (standardized on stable):
- `nixpkgs` (stable, 24.05-darwin branch)
- `nix-darwin` (follows nixpkgs)
- `home-manager` (follows nixpkgs)
- `alacritty-theme`
- `nix-vscode-extensions` (follows nixpkgs)
- `claude-code` (available to both platforms)
- `nixgl` (ubuntu-specific for graphics support)

## Test Strategy Specification

### Test Plan Overview
Manual testing approach with systematic verification of both platforms to ensure identical functionality before and after refactor.

### Test Cases
**TC1: Darwin Build Test**
- Execute `darwin/run.sh` with unified flake
- Verify successful nix-darwin system rebuild
- Confirm all darwin-specific packages and configurations present

**TC2: Ubuntu Build Test**
- Execute `ubuntu/run.sh` with unified flake
- Verify successful home-manager rebuild
- Confirm all ubuntu-specific packages and configurations present

**TC3: Functionality Preservation Test**
- Compare system state before and after refactor on both platforms
- Verify identical package sets, configurations, and home-manager state
- Test all custom overlays (vscode-extensions, alacritty-theme, claude-code)

**TC4: Bug Fix Verification**
- Confirm claude-code overlay works on Ubuntu (fixes existing latent bug)
- Verify no build errors related to missing inputs

### Edge Cases & Error Handling
- **Invalid fragment**: Graceful error when wrong fragment passed
- **Missing platform config**: Clear error messages for unsupported configurations
- **Input resolution**: Proper handling of platform-specific inputs like nixgl

### Performance Criteria
- Build time should be comparable to current separate flakes
- No significant increase in evaluation time
- Minimal flake.lock differences (mostly input consolidation)

### Automation Strategy
Manual testing only - automated testing not required for this refactor as it's primarily structural reorganization.

## Implementation Plan Specification

### Development Approach
Incremental replacement approach with immediate testing and cleanup.

### Task Breakdown
**Status Legend:** 📋 Not Started | ⏳ In Progress | ✅ Complete

1. ✅ **Create unified flake.nix** - Merge platform configurations with fragment support, absorb common-inputs.nix overlays
2. ✅ **Update darwin/run.sh** - Modify to use flake fragment for platform selection
3. ✅ **Update ubuntu/run.sh** - Modify to use flake fragment for platform selection
4. 📋 **Test darwin platform** - Verify darwin/run.sh works with unified flake
5. 📋 **Test ubuntu platform** - Verify ubuntu/run.sh works with unified flake
6. ✅ **Remove obsolete common-inputs.nix** - Deleted file and common/ directory (functionality in unified flake)
7. ✅ **Remove obsolete platform flakes** - Deleted darwin/flake.nix and ubuntu/flake.nix

### Dependencies & Sequencing
- Task 1 must complete before tasks 2-3
- Tasks 2-3 can be done in parallel
- Tasks 4-5 depend on completion of corresponding run script updates
- Tasks 6-7 only after successful testing of both platforms
- Task 6 and 7 can be done in parallel (cleanup phase)

### Risk Assessment
**Low Risk Refactor**:
- Structural change only, no functional modifications
- Git rollback available for any issues
- Platform configurations remain identical in behavior
- Shared home.nix unchanged
- Common-run.sh preserved (still needed)

### Integration Plan
No integration complexity - this is internal build system refactoring with no external dependencies.

### Deployment Strategy
No deployment needed - local development environment refactor only.

## Session Log
<!-- Instructions: Add entry for each work session. Newest at top -->

### 2026-02-20 - Session Resume
- **Goal**: Review current status, assist with remaining tasks 4-5 (platform testing)
- Reviewed session file — tasks 1-3, 6-7 are ✅ complete
- Remaining: Task 4 (darwin test) and Task 5 (ubuntu test) require manual platform verification
- Note: `nix/home/home.nix` has uncommitted changes adding terminal utilities (fzf, bat, eza, zoxide, atuin) — unrelated to this refactor feature
- **Status**: Awaiting user direction on next steps

### 2026-02-17 - Review & Testing Session
- **Goal**: Review implementation status, assist with remaining platform testing (tasks 4-5)
- Reviewed session file and confirmed tasks 1-3, 6-7 are complete
- Remaining: Tasks 4 (darwin test) and 5 (ubuntu test) require manual platform verification
- Note: `emacs/core-gittree.el` modified in working tree (unrelated to this feature)
- Fixed file watch: replaced file-notify (broken on macOS with atomic writes) with modtime polling

### 2026-02-17 - Implementation Session
- **Goal**: Implement tasks 1-3 (create unified flake.nix, update both run.sh scripts)
- Reviewed all existing files: darwin/flake.nix, ubuntu/flake.nix, common/common-inputs.nix, common/common-run.sh, both run.sh scripts, home.nix
- Key findings:
  - Confirmed FR6 bug: ubuntu/flake.nix missing `claude-code` input but common-inputs.nix references it
  - ubuntu/flake.nix has `nixpkgs-unstable` that will be removed per decision
  - FLAKE_DIR in common-run.sh needs to point to parent dir (nix/) instead of platform dir
  - Fragment names: `darwinConfigurations.darwin` and `homeConfigurations.ubuntu`
- **Results**:
  - ✅ Task 1: Created `/nix/flake.nix` with unified inputs, `commonOverlays` list (absorbs common-inputs.nix), darwin + ubuntu outputs
  - ✅ Task 2: Updated darwin/run.sh NIX_COMMAND to use `#darwin` fragment
  - ✅ Task 3: Updated ubuntu/run.sh NIX_COMMAND to use `#ubuntu` fragment
  - ✅ Updated common-run.sh FLAKE_DIR from `$SCRIPT_DIR` to `$SCRIPT_DIR/..` (points to unified flake location)
  - ✅ Moved `common/common-run.sh` to `nix/run.sh` — cleaner location alongside unified flake
  - ✅ Updated both platform run.sh scripts to source `../run.sh` instead of `../common/common-run.sh`
  - Fixes applied: FR6 bug fixed (claude-code overlay now available to ubuntu), nixpkgs-unstable removed
- **Next steps**: Tasks 4-5 (manual platform testing), then tasks 6-7 (cleanup obsolete files — common/ directory can be fully removed)

### 2026-02-17 - Initial Specification Creation
- Completed comprehensive requirements interview
- Analyzed current flake architecture and directory structure
- Defined technical design with flake fragments approach
- Established manual testing strategy
- Updated task breakdown to handle common folder files properly
- Current status: Ready for implementation when session resumes

## Useful References
<!-- Instructions: Add helpful URLs, docs, tutorials as you find them -->
- [Nix Flakes Documentation](https://nixos.wiki/wiki/Flakes) - Official flake syntax and patterns
- [nix-darwin Documentation](https://daiderd.com/nix-darwin/) - Darwin system configuration
- [Home Manager Manual](https://nix-community.github.io/home-manager/) - User environment management

## Decision Log
<!-- Record of key decisions made during specification and development -->

### 2026-02-17 - Unified Flake Architecture
**Context**: Need to merge separate darwin and ubuntu flakes while maintaining existing workflow
**Decision**: Use flake fragments with unified flake.nix at root level
**Rationale**: Simpler than platform detection, explicit parameter passing, follows Nix conventions
**Alternatives**: Platform detection within flake, environment variables, separate entry points
**Impact**: Cleaner architecture, easier maintenance, obsoletes common-inputs.nix

### 2026-02-17 - Standardize on Stable Nixpkgs
**Context**: Ubuntu currently uses nixpkgs-unstable while Darwin uses stable
**Decision**: Standardize both platforms on stable nixpkgs
**Rationale**: Simpler input management, more predictable builds, reduced complexity
**Alternatives**: Keep unstable for Ubuntu, move both to unstable
**Impact**: Ubuntu may lose access to some newer packages but gains build stability

### 2026-02-17 - Preserve Common Run Script
**Context**: Decision on what to do with common directory files
**Decision**: Keep common-run.sh, remove common-inputs.nix
**Rationale**: common-run.sh provides shared logic still needed by both platforms, common-inputs.nix functionality absorbed into unified flake
**Alternatives**: Move all common logic into platform scripts, keep all common files
**Impact**: Maintains clean separation of shared script logic while consolidating flake inputs

### 2026-02-17 - Manual Testing Only
**Context**: Need validation approach for the refactor
**Decision**: Manual testing of both run scripts, no automated tests
**Rationale**: Structural refactor with no functional changes, git rollback available
**Alternatives**: Automated comparison testing, extensive backup procedures
**Impact**: Faster implementation, relies on git for safety net