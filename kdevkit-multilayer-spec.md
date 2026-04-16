# kdevkit Multi-Layer Architecture Specification

**Version**: 0.1.0  
**Date**: 2026-04-16  
**Status**: Draft

---

## 1. Executive Summary

This spec defines a multi-layered architecture for kdevkit that enables:
- **System-wide** coding agent configurations that work across all projects
- **Project-specific** configurations that inherit and override system defaults
- **Local divergence** with periodic reconciliation back to the upstream kdevkit git
- **Memory and rules** that accumulate in local environments without polluting the upstream git

---

## 2. Problem Statement

### Current State
- kdevkit exists as a git repository with reusable instruction sets (markdown files)
- Users curl files from the repo to use with coding agents
- All customizations are project-local or ephemeral

### Issues
1. **No system-wide defaults**: Git conventions, coding practices, and agent behaviors should be consistent across all projects for a given user
2. **No inheritance model**: Projects can't inherit from system-level configurations
3. **No divergence management**: Local customizations can't be easily reconciled with upstream
4. **No memory persistence**: Agent memory and learned behaviors are per-conversation, not persistent across system/project contexts
5. **Tight coupling**: kdevkit must be installed into each project; no separation of system vs. project concerns

---

## 3. Proposed Architecture

### 3.1 Five-Layer Model

```
┌─────────────────────────────────────────────────────┐
│ Layer 5: Project Feature Files                      │
│ (.kdevkit/feature/*.md)                            │
│ - Ephemeral, specific to current work               │
│ - Never synced to upstream                          │
└─────────────────────────────────────────────────────┘
                        ↓ inherits from
┌─────────────────────────────────────────────────────┐
│ Layer 4: Local Project kdevkit                      │
│ (~/<project>/.kdevkit/)                             │
│ - Overrides and extensions for this project         │
│ - Can diverge from Layer 2                          │
│ - Reconcilable via kdevkit-sync                     │
└─────────────────────────────────────────────────────┘
                        ↓ inherits from
┌─────────────────────────────────────────────────────┐
│ Layer 3: Upstream Project kdevkit                   │
│ (git: kusimari/kdevkit, path: project/)             │
│ - Canonical project-level templates and workflows   │
│ - Git-managed, versioned                            │
└─────────────────────────────────────────────────────┘
                        ↓ inherits from
┌─────────────────────────────────────────────────────┐
│ Layer 2: Local System kdevkit                       │
│ (~/.kdevkit/)                                       │
│ - User's personalized system-wide settings          │
│ - Git conventions, coding style, agent preferences  │
│ - Can diverge from Layer 1                          │
│ - Reconcilable via kdevkit-sync                     │
└─────────────────────────────────────────────────────┘
                        ↓ inherits from
┌─────────────────────────────────────────────────────┐
│ Layer 1: Upstream System kdevkit                    │
│ (git: kusimari/kdevkit, path: system/)              │
│ - Canonical system-level templates and workflows    │
│ - Git-managed, versioned                            │
└─────────────────────────────────────────────────────┘
```

### 3.2 Directory Structure

#### Upstream Git (kusimari/kdevkit)
```
kdevkit/
├── system/                      # Layer 1: System-wide templates
│   ├── README.md
│   ├── git-practices.md         # Git conventions (branches, commits, PRs)
│   ├── coding-standards.md      # Language-agnostic coding standards
│   ├── agent-behavior.md        # Default agent behavior and preferences
│   ├── memory-templates/        # Templates for agent memory structure
│   └── skills/                  # Custom skills for system-level use
├── project/                     # Layer 3: Project templates
│   ├── README.md
│   ├── feature-dev.md           # (existing) Feature development workflow
│   ├── agent-dev-loop.md        # (existing) Dev loop setup
│   ├── project-template.md      # Template for project.md
│   └── templates/               # Feature file templates
├── tools/                       # Tooling for kdevkit itself
│   ├── kdevkit-sync            # Reconciliation tool (CLI)
│   ├── kdevkit-init            # Initialization tool
│   └── kdevkit-inspect         # Divergence inspection tool
├── build.js                     # (existing) Build script
├── tests/                       # (existing) Test suite
└── README.md                    # (existing) Main README
```

#### Local System (~/.kdevkit/) - Layer 2
```
~/.kdevkit/
├── upstream/                    # Linked/copied from Layer 1
│   └── .git-ref                # Records upstream commit SHA
├── local/                       # Local divergence
│   ├── git-practices.md        # Overrides system/git-practices.md
│   ├── coding-standards.md     # Overrides system/coding-standards.md
│   └── custom-skills/          # User-specific skills
├── memory/                      # Agent memory (not synced to upstream)
│   ├── user.md
│   ├── feedback.md
│   └── reference.md
├── config.yaml                  # kdevkit configuration
└── .divergence.log             # Tracks local changes
```

#### Local Project (~/<project>/.kdevkit/) - Layer 4
```
<project>/.kdevkit/
├── upstream/                    # Linked/copied from Layer 3
│   └── .git-ref                # Records upstream commit SHA
├── local/                       # Local project-specific divergence
│   ├── project.md              # Overrides project template
│   └── dev-loop-custom.md      # Custom dev loop additions
├── feature/                     # Layer 5: Feature files (ephemeral)
│   ├── auth-system.md
│   └── api-refactor.md
├── memory/                      # Project-specific agent memory
│   ├── project.md
│   ├── feedback.md
│   └── architecture.md
├── config.yaml                  # Project-level kdevkit config
└── .divergence.log             # Tracks local changes
```

---

## 4. Key Components

### 4.1 Inheritance Model

Each layer inherits from the layer below it:
- **Merge strategy**: Local files override upstream files by name
- **Composition**: Files can declare `extends: <upstream-file>` to merge rather than replace
- **Resolution order**: Layer 5 → 4 → 3 → 2 → 1 (most specific to most general)

Example `.kdevkit/local/git-practices.md`:
```yaml
---
extends: system/git-practices.md
override: true
---

# Git Practices (Local Override)

<!-- This file extends system/git-practices.md and overrides specific sections -->

## Commit Message Format
(custom format here)

<!-- All other sections inherited from system/git-practices.md -->
```

### 4.2 kdevkit-sync Tool

A CLI tool for reconciling local divergence with upstream.

#### Usage
```bash
# Inspect divergence at system level
kdevkit-sync inspect --scope system

# Inspect divergence at project level
kdevkit-sync inspect --scope project

# Push local system changes to upstream
kdevkit-sync push --scope system --files git-practices.md

# Pull upstream changes to local system
kdevkit-sync pull --scope system

# Interactive reconciliation (shows diffs, asks what to do)
kdevkit-sync reconcile --scope project
```

#### Reconciliation Workflow
1. **Detect divergence**: Compare local files with upstream commit SHA
2. **Categorize changes**:
   - `memory/*` → Never sync (always local)
   - `feature/*` → Never sync (always local)
   - `local/*` → Candidates for upstream push
   - `config.yaml` → Never sync (local configuration)
3. **Interactive review**:
   - Show diffs
   - Ask: "Push to upstream?", "Keep local?", "Merge?"
4. **Execute**: Create PR to upstream git or update local state

### 4.3 kdevkit-init Tool

Initializes kdevkit at system or project level.

#### Usage
```bash
# Initialize system-level kdevkit
kdevkit-init system

# Initialize project-level kdevkit
kdevkit-init project

# Initialize with specific upstream version
kdevkit-init system --version v1.2.3
```

#### Initialization Process
1. Check if `~/.kdevkit/` (system) or `.kdevkit/` (project) exists
2. If not, create directory structure
3. Fetch upstream files from git (via API or curl)
4. Record upstream commit SHA in `.git-ref`
5. Create default `config.yaml`
6. Create empty `memory/` and `local/` directories

### 4.4 kdevkit-inspect Tool

Analyzes the current kdevkit state and suggests improvements.

#### Usage
```bash
# Inspect current system state
kdevkit-inspect system

# Inspect current project state
kdevkit-inspect project

# Suggest what should be pushed upstream
kdevkit-inspect suggest-push
```

#### Inspection Report
```
kdevkit System Inspection Report
=================================

Upstream: kusimari/kdevkit @ 9f6b305
Local:    ~/.kdevkit/

Divergence Summary:
  - local/git-practices.md: 47 lines changed from upstream
  - local/coding-standards.md: NEW FILE (no upstream equivalent)
  - memory/*: 12 files (never synced)

Suggestions:
  ✓ Consider pushing git-practices.md changes to upstream
  ✓ coding-standards.md could be useful for others
  ⚠ Upstream has 3 commits since last sync (run 'kdevkit-sync pull')

Memory Growth:
  - memory/ size: 45KB
  - Oldest entry: 2026-01-12
  - Entries: 23 files
```

### 4.5 Agent Integration

Coding agents (Claude Code, etc.) should load kdevkit context at startup.

#### Load Sequence
1. **System context** (if `~/.kdevkit/` exists):
   - Load `upstream/` files (Layer 1)
   - Apply `local/` overrides (Layer 2)
   - Load `memory/` (system-level agent memory)
2. **Project context** (if `.kdevkit/` exists in project):
   - Load `upstream/` files (Layer 3)
   - Apply `local/` overrides (Layer 4)
   - Load `memory/` (project-level agent memory)
3. **Feature context** (if specified):
   - Load `.kdevkit/feature/<name>.md` (Layer 5)

#### Configuration Integration

Add to Claude Code's `CLAUDE.md` (or equivalent):
```markdown
# kdevkit Integration

At session start:
1. Check for `~/.kdevkit/config.yaml`
2. If exists, run: kdevkit-load --scope system
3. Check for `.kdevkit/config.yaml` in project
4. If exists, run: kdevkit-load --scope project

At session end:
- Auto-save relevant learnings to appropriate memory layer
```

---

## 5. Configuration Format

### 5.1 config.yaml (System Level)

```yaml
kdevkit:
  version: 0.1.0
  upstream:
    repo: kusimari/kdevkit
    ref: main
    path: system
    commit: 9f6b305abc...
  
  local:
    auto_sync: false          # Auto-pull upstream changes
    sync_prompt: true         # Prompt before syncing
    reconcile_on_exit: true   # Check divergence on agent exit
  
  memory:
    auto_save: true           # Auto-save agent memory
    max_size_kb: 1024         # Memory size limit
    retention_days: 90        # Clean up old entries
  
  overrides:
    - git-practices.md
    - coding-standards.md
```

### 5.2 config.yaml (Project Level)

```yaml
kdevkit:
  version: 0.1.0
  upstream:
    repo: kusimari/kdevkit
    ref: main
    path: project
    commit: 9f6b305abc...
  
  local:
    auto_sync: false
    sync_prompt: true
  
  memory:
    auto_save: true
    inherit_system: true      # Inherit system memory
  
  feature:
    auto_persist: true        # Auto-update feature files
    template: default         # Which feature template to use
  
  overrides:
    - project.md
```

---

## 6. Reconciliation Strategy

### 6.1 Divergence Detection

Track changes in `.divergence.log`:
```json
{
  "tracking_since": "2026-04-01T00:00:00Z",
  "upstream_ref": "9f6b305abc...",
  "local_changes": [
    {
      "file": "local/git-practices.md",
      "status": "modified",
      "first_changed": "2026-04-05T14:32:00Z",
      "change_count": 7,
      "suggested_action": "push_upstream"
    },
    {
      "file": "local/coding-standards.md",
      "status": "new",
      "first_changed": "2026-04-10T09:15:00Z",
      "change_count": 1,
      "suggested_action": "push_upstream"
    }
  ],
  "memory_stats": {
    "total_files": 23,
    "total_size_kb": 45,
    "oldest_entry": "2026-01-12T08:00:00Z"
  }
}
```

### 6.2 Push Strategy

When pushing local changes to upstream:

1. **File categorization**:
   - System-level: `~/.kdevkit/local/*.md` → `kdevkit/system/*.md`
   - Project-level: `<project>/.kdevkit/local/*.md` → `kdevkit/project/*.md`

2. **PR creation**:
   - Branch: `user/<username>/sync-<scope>-<timestamp>`
   - Title: `sync(system): reconcile local changes from <username>`
   - Body: Includes diff summary, rationale, and change count

3. **Merge strategy**:
   - Small changes (< 50 lines): Auto-merge if tests pass
   - Large changes: Require review
   - Breaking changes: Flag for maintainer review

### 6.3 Pull Strategy

When pulling upstream changes to local:

1. **Conflict detection**:
   - Compare upstream changes with local divergence
   - Flag conflicts where both changed the same file

2. **Merge options**:
   - `keep_local`: Preserve local version
   - `take_upstream`: Overwrite with upstream version
   - `merge`: Three-way merge (manual resolution)

3. **Backup**:
   - Before overwriting, backup to `.kdevkit/.backups/<timestamp>/`

---

## 7. Memory Management

### 7.1 Memory Layers

Memory is **never synced to upstream git**. It stays local:
- System memory: `~/.kdevkit/memory/`
- Project memory: `<project>/.kdevkit/memory/`

### 7.2 Memory Inheritance

Project-level agents can read system-level memory:
```yaml
# In project config.yaml
memory:
  inherit_system: true
  inherit_patterns:
    - user.md          # Always inherit user profile
    - feedback.md      # Inherit feedback about agent behavior
    - !reference.md    # Don't inherit external references
```

### 7.3 Memory Reconciliation

While memory isn't pushed to upstream, there should be a tool to extract generalizable patterns:

```bash
# Analyze memory for patterns worth codifying
kdevkit-inspect memory-patterns

# Suggest converting memory to explicit rules
kdevkit-inspect suggest-rules
```

Output:
```
Memory Pattern Analysis
=======================

Detected Pattern:
  - feedback.md mentions "always run tests before commit" 7 times
  - Suggestion: Add to system/agent-behavior.md as explicit rule

Detected Pattern:
  - project.md describes auth architecture in 3 projects
  - Suggestion: Create project/templates/auth-template.md

Would you like to:
  1. Generate rule file from pattern
  2. Ignore this pattern
  3. Review next pattern
```

---

## 8. Build and Packaging

### 8.1 Built Artifacts

kdevkit should produce multiple build outputs:

```
build/
├── kdevkit-system.md          # Single-file system bundle
├── kdevkit-project.md         # Single-file project bundle
├── kdevkit-sync               # CLI tool (bash or Node.js)
├── kdevkit-init               # CLI tool
└── kdevkit-inspect            # CLI tool
```

### 8.2 Build Process

Update `build.js` to support multiple outputs:

```javascript
// build.js
const TARGETS = {
  system: {
    manifest: ['system/git-practices.md', 'system/coding-standards.md', ...],
    output: 'build/kdevkit-system.md'
  },
  project: {
    manifest: ['project/feature-dev.md', 'project/agent-dev-loop.md', ...],
    output: 'build/kdevkit-project.md'
  }
};
```

### 8.3 Installation via build-nix

Integration with your nix setup:

```nix
# In your build-nix configuration
{ pkgs, ... }:
{
  home.packages = [
    (pkgs.writeScriptBin "kdevkit-sync" (builtins.readFile ./kdevkit/tools/kdevkit-sync))
    (pkgs.writeScriptBin "kdevkit-init" (builtins.readFile ./kdevkit/tools/kdevkit-init))
    (pkgs.writeScriptBin "kdevkit-inspect" (builtins.readFile ./kdevkit/tools/kdevkit-inspect))
  ];

  home.file.".kdevkit/upstream".source = pkgs.fetchFromGitHub {
    owner = "kusimari";
    repo = "kdevkit";
    rev = "main";
    sha256 = "...";
  };

  home.activation.kdevkit = lib.hm.dag.entryAfter ["writeBoundary"] ''
    if [ ! -d "$HOME/.kdevkit" ]; then
      $DRY_RUN_CMD ${pkgs.kdevkit-init}/bin/kdevkit-init system --auto
    fi
  '';
}
```

---

## 9. Usage Examples

### 9.1 Initial Setup

```bash
# On a new machine managed by build-nix
# kdevkit-init is automatically run via nix activation

# Verify installation
kdevkit-inspect system
# Output: System kdevkit initialized at ~/.kdevkit/

# Customize git practices
emacs ~/.kdevkit/local/git-practices.md

# Later, push customizations upstream
kdevkit-sync reconcile --scope system
```

### 9.2 Starting a New Project

```bash
cd ~/projects/my-new-app

# Initialize project-level kdevkit
kdevkit-init project

# Start coding agent with kdevkit
claude-code

# Agent automatically loads:
# 1. System context from ~/.kdevkit/
# 2. Project context from .kdevkit/
```

### 9.3 Working on a Feature

```bash
# In coding agent session
> Follow kdevkit workflow for feature: auth-system

# Agent:
# - Loads system context (Layer 2 + 1)
# - Loads project context (Layer 4 + 3)
# - Creates .kdevkit/feature/auth-system.md (Layer 5)
# - Applies git practices
# - Starts development

# After several days of work, check divergence
kdevkit-inspect project
# Output: local/project.md has diverged (added auth architecture notes)

# Decide if this should be pushed upstream
kdevkit-sync inspect --scope project
# Shows diff, suggests pushing

# Push if appropriate
kdevkit-sync push --scope project --files local/project.md
# Creates PR to kusimari/kdevkit
```

### 9.4 Sharing Improvements

```bash
# After accumulating valuable local changes
kdevkit-sync reconcile --scope system

# Interactive prompt:
# File: local/git-practices.md
# Changes: Added "conventional commits" enforcement
# Push to upstream? [y/n/diff/skip]
> y

# File: memory/feedback.md
# Changes: 47 entries about agent behavior
# Push to upstream? [y/n/diff/skip]
> n (memory is always local)

# kdevkit-sync creates PR:
# - Branch: user/gorantls/sync-system-20260416
# - Title: sync(system): add conventional commits enforcement
# - Body: [detailed changes]
```

---

## 10. Implementation Plan

### Phase 1: Repository Restructuring
- [ ] Create `system/` and `project/` directories in kdevkit git
- [ ] Move existing files to appropriate locations
- [ ] Update `build.js` to support multiple targets
- [ ] Update CI to build both `kdevkit-system.md` and `kdevkit-project.md`

### Phase 2: Basic Tooling
- [ ] Implement `kdevkit-init` (system and project initialization)
- [ ] Implement `kdevkit-inspect` (basic divergence detection)
- [ ] Implement config file parsing (`config.yaml`)

### Phase 3: Sync Mechanism
- [ ] Implement `kdevkit-sync pull` (upstream → local)
- [ ] Implement `kdevkit-sync inspect` (show divergence)
- [ ] Implement `kdevkit-sync push` (local → upstream via PR)

### Phase 4: Memory Integration
- [ ] Define memory directory structure
- [ ] Implement memory inheritance (system → project)
- [ ] Implement memory pattern detection

### Phase 5: Agent Integration
- [ ] Create agent loader (loads system + project context)
- [ ] Update CLAUDE.md templates to include kdevkit loading
- [ ] Test with Claude Code CLI and web

### Phase 6: Nix Integration
- [ ] Package kdevkit tools for nix
- [ ] Create home-manager module
- [ ] Add automatic initialization to build-nix

---

## 11. Open Questions

1. **Versioning**: Should system and project kdevkit have separate version numbers?
2. **Tool language**: Implement tools in bash (portable) or Node.js (consistent with build.js)?
3. **Memory size limits**: What's reasonable for system/project memory before requiring cleanup?
4. **Upstream PR approval**: Who approves sync PRs? Maintainers only, or auto-merge for trusted users?
5. **Cross-project sharing**: Should there be a way to share project-level configs between projects?
6. **Skills and agents**: How should custom skills/agents be structured and synced?
7. **Testing**: How to test the reconciliation logic without polluting real upstream?

---

## 12. Security and Privacy Considerations

1. **Memory content**: Memory may contain sensitive project details. Never sync to upstream.
2. **Config files**: May contain tokens, API keys. Never sync `config.yaml`.
3. **Feature files**: May contain business logic, IP. Never sync `feature/` directory.
4. **Divergence logs**: May expose file names and change patterns. Don't expose publicly.
5. **PR content**: When pushing to upstream, review for sensitive data before creating PR.

---

## 13. Success Criteria

- [ ] Can initialize system-level kdevkit on any machine
- [ ] Can initialize project-level kdevkit in any project
- [ ] Local customizations persist across agent sessions
- [ ] Can easily reconcile and push valuable changes upstream
- [ ] Agent automatically loads appropriate context layers
- [ ] Memory grows intelligently without manual maintenance
- [ ] Clear separation between local and upstream content
- [ ] No friction when working on multiple projects

---

## 14. Future Enhancements

- **kdevkit marketplace**: Share community templates and skills
- **Team profiles**: Share org-level kdevkit configs (beyond personal)
- **Auto-sync**: Optionally auto-push certain changes (e.g., memory-derived rules)
- **Dependency tracking**: If project kdevkit depends on specific system version
- **Migration tools**: Upgrade from old kdevkit structure to new multi-layer model
- **Web UI**: Visual diff and reconciliation interface
- **Analytics**: Track which rules/templates are most effective

---

## Appendix A: File Format Specifications

### A.1 .git-ref Format

```json
{
  "repo": "kusimari/kdevkit",
  "ref": "main",
  "path": "system",
  "commit": "9f6b305abc123def456...",
  "fetched_at": "2026-04-16T10:30:00Z"
}
```

### A.2 .divergence.log Format

(See section 6.1)

### A.3 Frontmatter for Override Files

```yaml
---
extends: system/git-practices.md
override: true
version: 1.0.0
last_modified: 2026-04-16T10:30:00Z
---
```

---

## Appendix B: Comparison with Alternatives

### vs. Git Submodules
- **kdevkit**: Focuses on reconcilable divergence, not strict versioning
- **Submodules**: Strict version locking, no local divergence support

### vs. Dotfiles Management
- **kdevkit**: Structured inheritance, reconciliation, and agent integration
- **Dotfiles**: Flat file syncing, no layering or divergence management

### vs. Ansible/Chef/Puppet
- **kdevkit**: Lightweight, markdown-based, agent-focused
- **Config management**: Heavy, imperative, system-wide automation

---

**End of Specification**
