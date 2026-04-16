# kdevkit Multi-Layer Architecture - Simplified Specification

**Version**: 0.3.0  
**Date**: 2026-04-16  
**Status**: Draft - Simplified based on Q&A

---

## Core Principles

1. **Keep it simple** - No config files unless absolutely necessary
2. **Use standard tools** - Memory sync is just git, use it directly
3. **Auto-detect everything** - Agent types, overrides, divergence
4. **Explicit is better than implicit** - Clear file purposes, no magic

---

## Directory Structure

### System-level (~/.kdevkit/)

```
~/.kdevkit/
├── upstream/                      # Cached copy from kusimari/kdevkit/system/
│   ├── .git-ref                   # { repo, ref, commit, fetched_at }
│   ├── git-practices.md           # Verbatim from GitHub
│   ├── coding-standards.md
│   └── agent-behavior.md
├── local/                         # Your customizations (overrides)
│   └── git-practices.md           # (Optional) Your custom version
└── memory/                        # Your personal agent memory
    ├── .git/                      # (Optional) Make it a git repo to sync
    ├── user.md
    ├── feedback.md
    └── reference.md
```

### Project-level (<project>/.kdevkit/)

```
<project>/.kdevkit/
├── upstream/                      # Cached copy from kusimari/kdevkit/project/
│   ├── .git-ref
│   ├── feature-dev.md
│   ├── agent-dev-loop.md
│   └── project-template.md
├── local/                         # Project-specific customizations
│   └── project.md                 # COMMITTED - describes this project
├── feature/                       # COMMITTED - feature context files
│   ├── auth-system.md
│   └── api-refactor.md
└── memory/                        # Project memory
    ├── project.md                 # GITIGNORED (or committed if team wants)
    ├── feedback.md
    └── architecture.md
```

### Agent config in project root

```
<project>/
├── CLAUDE.md                      # COMMITTED - Claude Code instructions
├── .kdevkit/                      # (structure above)
└── .gitignore
```

---

## File Purposes Explained

### upstream/ - Cached Baseline

**Purpose**: Verbatim copy of GitHub kdevkit content for offline use and divergence detection.

**When synced**: When you run `kdevkit sync pull`

**Example upstream/.git-ref**:
```json
{
  "repo": "kusimari/kdevkit",
  "ref": "main",
  "path": "system",
  "commit": "9f6b305abc...",
  "fetched_at": "2026-04-16T10:30:00Z"
}
```

### local/ - Your Customizations

**Purpose**: Files that override upstream defaults.

**Inheritance**: If `local/git-practices.md` exists, it completely replaces `upstream/git-practices.md`

**Reconciliation**: When upstream changes, `kdevkit sync pull` compares and warns of conflicts

**Example**:
```
~/.kdevkit/local/git-practices.md    # Your custom git conventions
<project>/.kdevkit/local/project.md  # This project's description
```

### feature/ - Feature Context (Project Only)

**Purpose**: Describes features being worked on. Similar to ADRs (Architecture Decision Records).

**Committed**: YES - team shares feature context

**Created by**: Coding agents during feature development workflow

**Example feature/auth-system.md**:
```markdown
# Feature: Authentication System

## Status
In Progress

## Context
Need OAuth2 + JWT authentication for API

## Design
- OAuth2 via Auth0
- JWT tokens with 1hr expiry
- Refresh token rotation

## Session Log
- 2026-04-15: Implemented OAuth2 flow
- 2026-04-16: Added JWT middleware

## Open Questions
- Should we support social login?
```

### memory/ - Agent Memory

**Purpose**: Persistent memory that accumulates across agent sessions

**System memory** (`~/.kdevkit/memory/`):
- Personal preferences and learnings
- Can sync across your machines (make it a git repo)
- Private (don't share)

**Project memory** (`<project>/.kdevkit/memory/`):
- Project-specific context and learnings
- Choice: gitignore (personal) or commit (team-shared)
- Up to team preference

**Syncing system memory across machines**:
```bash
# Machine 1:
cd ~/.kdevkit/memory
git init
git remote add origin git@github.com:you/kdevkit-memory-private.git
git add .
git commit -m "init memory"
git push

# Machine 2:
cd ~/.kdevkit
rm -rf memory
git clone git@github.com:you/kdevkit-memory-private.git memory
```

No special tooling needed - just use git directly!

---

## How Agent Loading Works

### 1. Init Time (One-time Setup)

```bash
cd <project>
kdevkit init project
```

**What happens**:
1. Creates `.kdevkit/` structure
2. Fetches upstream files from GitHub
3. Creates `.kdevkit/local/project.md` from template
4. Detects agent config (CLAUDE.md, GEMINI.md, etc.)
5. Injects import block into agent config

**Resulting CLAUDE.md**:
```markdown
<!-- kdevkit:auto-import -->
# kdevkit Context Loading

At session start, run:
```
! kdevkit load both
```

This loads system and project kdevkit context including:
- Git practices and coding standards
- Project description and conventions  
- Feature workflows and dev-loop instructions
- Memory from past sessions

The output is markdown that will be in your context.
<!-- /kdevkit:auto-import -->

# Project: My Cool App

... your project-specific instructions ...
```

### 2. Session Start (Every Time)

**User starts coding**:
```bash
claude-code
```

**Claude's workflow**:
```
1. Read CLAUDE.md
2. See: ! kdevkit load both
3. Execute via Bash tool
4. Receive merged markdown content
5. Load into context
6. Ready to work with full kdevkit structure in mind
```

### 3. What kdevkit load outputs

```bash
$ kdevkit load both
```

**Output** (to stdout, in markdown):
```markdown
# kdevkit System Context

## Git Practices
[Content from ~/.kdevkit/local/git-practices.md OR upstream/git-practices.md]

## Coding Standards  
[Content from ~/.kdevkit/upstream/coding-standards.md]

## Agent Behavior
[Content from ~/.kdevkit/upstream/agent-behavior.md]

---

# kdevkit Project Context

## Project Description
[Content from .kdevkit/local/project.md]

## Feature Development Workflow
[Content from .kdevkit/upstream/feature-dev.md]

## Dev Loop Instructions
[Content from .kdevkit/upstream/agent-dev-loop.md]

---

# System Memory

## User Profile
[Content from ~/.kdevkit/memory/user.md]

## Feedback
[Content from ~/.kdevkit/memory/feedback.md]

---

# Project Memory

## Project Context
[Content from .kdevkit/memory/project.md]

## Architecture Notes
[Content from .kdevkit/memory/architecture.md]
```

The agent reads this as markdown context.

---

## Commands Reference

### kdevkit init

**Initialize kdevkit at system or project level**

```bash
# System-level (run once per machine)
kdevkit init system

# Project-level (run once per project)
cd <project>
kdevkit init project
```

**What it does**:
- Creates directory structure
- Fetches upstream files from GitHub
- Creates templates (project.md for projects)
- Detects and updates agent configs (CLAUDE.md, etc.)
- Updates .gitignore

**Options**:
```bash
--auto              # Non-interactive mode
--version <ref>     # Fetch specific version (default: main)
```

### kdevkit load

**Output merged kdevkit content for agent consumption**

```bash
# Load only system context
kdevkit load system

# Load only project context  
kdevkit load project

# Load both (most common)
kdevkit load both
```

**Output format**: Markdown to stdout

**Options**:
```bash
--no-memory         # Skip memory sections
--format json       # Output as JSON instead of markdown
```

### kdevkit sync

**Sync with upstream or push changes**

```bash
# Pull latest from upstream
kdevkit sync pull system
kdevkit sync pull project

# Inspect divergence
kdevkit sync status system
kdevkit sync status project

# Push local changes to upstream (creates PR)
kdevkit sync push system git-practices.md
```

**Pull workflow**:
1. Fetch latest from GitHub
2. Update `upstream/` directory
3. Update `.git-ref` with new commit
4. Compare with `local/` files
5. Warn if conflicts detected

**Push workflow** (for contributing back to kdevkit):
1. Detect GitHub authentication (GH_TOKEN or gh CLI)
2. Fork kusimari/kdevkit (if not already forked)
3. Create branch: `sync-system-YYYYMMDD` or `sync-project-YYYYMMDD`
4. Commit local files to appropriate path (system/ or project/)
5. Create PR via GitHub API
6. Return PR URL

### kdevkit inspect

**Inspect current kdevkit state**

```bash
# Show system status
kdevkit inspect system

# Show project status
kdevkit inspect project

# Show divergence details
kdevkit inspect divergence system

# Check if agent configs are up-to-date
kdevkit inspect agent-configs
```

**Example output**:
```
kdevkit Project Status
======================

Upstream: kusimari/kdevkit @ 9f6b305 (fetched 2 days ago)
Local:    /home/user/myproject/.kdevkit/

Upstream Files:
  ✓ feature-dev.md
  ✓ agent-dev-loop.md
  ✓ project-template.md

Local Files:
  • project.md (5KB)

Features:
  • auth-system
  • api-refactor

Memory:
  Files: 3
  Size:  12KB

Agent Configs:
  ✓ CLAUDE.md (import block present, up-to-date)

Divergence:
  ⚠ Upstream is 2 commits ahead
  Run: kdevkit sync pull project
```

### kdevkit update

**Update agent config import blocks**

```bash
# Check if import blocks need updating
kdevkit update check

# Update all agent configs in current project
kdevkit update agent-configs
```

**Use case**: When kdevkit changes how `kdevkit load` works, update the import blocks in CLAUDE.md etc.

---

## Configuration

### No config.yaml by default

Most things are auto-detected:
- Upstream repo: from `.git-ref`
- Overrides: any file in `local/`
- Agent type: detect CLAUDE.md, GEMINI.md, etc.
- Divergence: compare `local/` vs `upstream/` on the fly

### Optional config.yaml

If you need custom behavior, create `.kdevkit/config.yaml`:

```yaml
kdevkit:
  version: "0.3.0"
  
  # Override default upstream (rare)
  upstream:
    repo: "myorg/custom-kdevkit"  # Default: kusimari/kdevkit
    ref: "main"                   # Default: main
  
  # Behavior (all optional, sensible defaults)
  sync:
    auto_pull: false              # Auto-pull on load?
    check_upstream_on_load: true  # Check if upstream is ahead?
  
  load:
    include_memory: true          # Include memory in output?
    format: "markdown"            # markdown or json
```

**When config.yaml is checked**:
- If exists: read and apply settings
- If not exists: use defaults
- Never required

---

## GitHub Authentication (for sync push)

### Token-based (for SSH-only terminals)

```bash
# 1. Create token at: https://github.com/settings/tokens
#    Scopes: repo, workflow

# 2. Set environment variable
export GH_TOKEN=ghp_xxxxxxxxxxxxxxxxxxxx

# 3. Add to shell config
echo 'export GH_TOKEN=ghp_xxxxxxxxxxxxxxxxxxxx' >> ~/.zshrc

# 4. Now kdevkit sync push works
kdevkit sync push system git-practices.md
```

### Via gh CLI

```bash
# Alternative: authenticate gh CLI
echo $GH_TOKEN | gh auth login --with-token

# kdevkit will use gh CLI automatically
kdevkit sync push system git-practices.md
```

**How kdevkit checks for auth**:
1. Check for `$GH_TOKEN` environment variable
2. Try `gh auth token` command
3. If neither: print error with instructions

---

## Workflows

### Workflow 1: Initial Setup on New Machine

```bash
# 1. Install kdevkit (via nix or pip)
# (handled by build-nix)

# 2. Initialize system kdevkit
kdevkit init system
# Creates ~/.kdevkit/ with upstream templates

# 3. (Optional) Sync your memory from another machine
cd ~/.kdevkit
rm -rf memory
git clone git@github.com:you/kdevkit-memory-private.git memory

# 4. (Optional) Customize system settings
vim ~/.kdevkit/local/git-practices.md

# Done! Now every project can use kdevkit
```

### Workflow 2: Adding kdevkit to Existing Project

```bash
# 1. Go to project
cd ~/projects/my-app

# 2. Initialize project kdevkit
kdevkit init project
# Creates .kdevkit/ and updates CLAUDE.md

# 3. Describe your project
vim .kdevkit/local/project.md

# 4. Commit kdevkit structure
git add .kdevkit/ CLAUDE.md .gitignore
git commit -m "feat: add kdevkit structure"

# 5. Start coding with agent
claude-code
# Agent automatically loads kdevkit context
```

### Workflow 3: Working on a Feature

```bash
# In coding agent session:
> Follow kdevkit workflow for feature: auth-system

# Agent:
# 1. Loads system context (git practices, coding standards)
# 2. Loads project context (project.md, dev-loop)
# 3. Creates/loads .kdevkit/feature/auth-system.md
# 4. Follows feature-dev workflow
# 5. Updates feature file as work progresses

# Feature file is committed:
git add .kdevkit/feature/auth-system.md
git commit -m "feat(auth): add OAuth2 flow"
```

### Workflow 4: Customizing and Contributing Back

```bash
# 1. Customize system-level git practices
vim ~/.kdevkit/local/git-practices.md
# Add your preferred commit message format

# 2. Use it for a while, refine it

# 3. Decide to contribute back to kdevkit
kdevkit sync push system git-practices.md

# Behind the scenes:
# - Forks kusimari/kdevkit (if needed)
# - Creates branch: sync-system-20260416
# - Commits your file to system/git-practices.md
# - Creates PR via GitHub API
# - Returns: https://github.com/kusimari/kdevkit/pull/123

# 4. PR gets reviewed and merged
# 5. Other users benefit from your improvements
```

### Workflow 5: Syncing Changes from Upstream

```bash
# Check if upstream has changes
kdevkit inspect system
# Output: ⚠ Upstream is 5 commits ahead

# Pull changes
kdevkit sync pull system
# Fetches latest, updates upstream/, checks for conflicts

# If conflicts detected:
kdevkit sync status system
# Shows: local/git-practices.md differs from new upstream

# Review and resolve:
# Option A: Keep your version (do nothing)
# Option B: Merge changes manually
# Option C: Take upstream version:
cp ~/.kdevkit/upstream/git-practices.md ~/.kdevkit/local/git-practices.md
```

### Workflow 6: Sharing Project Memory with Team

```bash
# Default: memory is gitignored (personal)
cat .gitignore
# .kdevkit/memory/

# To share with team:
# 1. Remove from .gitignore
sed -i '/\.kdevkit\/memory/d' .gitignore

# 2. Commit memory
git add .kdevkit/memory/
git commit -m "docs: add shared project memory"

# Now team members get project memory when they clone
```

---

## Python Implementation Structure

```
kdevkit/
├── tools/
│   ├── kdevkit                   # Symlink to kdevkit.py
│   ├── kdevkit.py                # Main entry point (dispatcher)
│   ├── kdevkit_init.py           # Init command
│   ├── kdevkit_load.py           # Load command
│   ├── kdevkit_sync.py           # Sync command
│   ├── kdevkit_inspect.py        # Inspect command
│   ├── kdevkit_update.py         # Update command
│   └── kdevkit_common.py         # Shared utilities
├── system/                       # System-level templates
│   ├── git-practices.md
│   ├── coding-standards.md
│   └── agent-behavior.md
├── project/                      # Project-level templates
│   ├── feature-dev.md
│   ├── agent-dev-loop.md
│   └── project-template.md
├── tests/
│   ├── test_init.py
│   ├── test_load.py
│   └── test_sync.py
└── README.md
```

**Design principles**:
- Pure Python 3 stdlib (no external dependencies)
- Follow tmux script pattern (dataclasses, type hints, clean structure)
- Each command is a separate module
- Common utilities in kdevkit_common.py
- Main entry point dispatches to subcommands

---

## What Goes Where - Summary Table

| Item | Location | Committed? | Synced How? |
|------|----------|-----------|-------------|
| Upstream templates | `upstream/` | NO (gitignored or fetched) | `kdevkit sync pull` |
| System customizations | `~/.kdevkit/local/` | Optional (dotfiles repo) | Manual git |
| Project customizations | `.kdevkit/local/` | YES | Project git |
| Feature files | `.kdevkit/feature/` | YES | Project git |
| System memory | `~/.kdevkit/memory/` | Optional (private repo) | Manual git |
| Project memory | `.kdevkit/memory/` | Optional (team choice) | Project git (if shared) |
| Agent configs | `CLAUDE.md`, etc. | YES | Project git |
| Git ref | `.git-ref` | NO (gitignored) | Auto-updated on pull |
| Config | `config.yaml` | NO (gitignored) | N/A (local prefs) |

---

## Key Design Decisions

1. **upstream/ is cached GitHub content** - enables offline work and fast divergence detection
2. **No config.yaml by default** - auto-detect everything, add config only if needed
3. **Memory is just directories** - use git directly to sync, no special tooling
4. **Feature files are committed** - they're project context, not ephemeral work
5. **Agent config updated at init** - one-time injection of import block
6. **kdevkit load outputs markdown** - agent reads it into context at session start
7. **Token-based GitHub auth** - works on SSH-only terminals via GH_TOKEN
8. **Python with stdlib only** - no external dependencies, following tmux pattern

---

## Next Steps

1. Implement Python tools (kdevkit_init.py, kdevkit_load.py, etc.)
2. Test locally on this machine
3. Create PRs to kusimari/kdevkit to add system/ and project/ structure
4. Package for nix
5. Deploy to build-nix for automatic installation

---

**End of Simplified Specification**
