# Design: Reproducible Project Workspace Management

## Overview

A system to capture and recreate project workspaces across machines. Supports capturing existing workspaces into version-controlled metadata and recreating them from that metadata on fresh machines.

## Vocabulary

### Directory Structure
```
~/env-workplace/                      # Environment storage (system-level)
├── env/                              # Public generic environment configs
│   └── workspace-tools/              # Generic workspace management tools
└── <private-env>/                    # Private environment (corporate/personal)
    ├── desktop/
    │   └── bootstrap-*.sh            # Machine bootstrap scripts
    └── projects/                     # Project metadata storage
        └── <project-name>/           # Per-project metadata

~/workplace/                          # Work directory (project workspaces)
└── <project-workspace>/              # Individual project workspace
    ├── [git repos]
    ├── [corp-workspaces]             # Corporate workspace tools (build systems, etc.)
    └── [environment configs]
```

### Key Terms

- **env-workplace**: `~/env-workplace/` - System environment storage (generic + private)
- **workplace**: `~/workplace/` - Top-level directory for all project work
- **project-workspace**: `~/workplace/<name>/` - A specific project's working directory
- **project metadata**: `~/env-workplace/<private-env>/projects/<name>/` - Configuration to recreate a workspace

### Per-Project Metadata Files

- **workspace.env**: Shell-sourceable environment variables and metadata
- **workspace-init**: Python script to create/wire the project-workspace
- **Nix/flake.nix**: Nix development environment definition
- **tool-configs/**: Tool-specific workspace configurations (symlinked into workspace)
  - `tool-configs/kiro/` - Kiro configuration
  - `tool-configs/claude/` - Claude Code configuration  
  - `tool-configs/<other>/` - Other tool configurations

## Core Concepts

### Two-Phase Architecture

**Phase 1: CAPTURE (workspace → metadata)**
```
Input:  ~/workplace/<project-workspace>/
        ├── [git repos with .git/config]
        ├── [corp-workspaces]
        ├── .envrc
        └── [symlinks]

Tool:   workspace-capture <source-workspace-dir> <output-metadata-dir>

Output: <output-metadata-dir>/<project-name>/
        ├── workspace.env          (metadata)
        ├── workspace-init         (recreation script: Python)
        ├── Nix/                   (copied/referenced)
        └── tool-configs/          (copied: kiro, claude, etc.)
```

**Phase 2: SETUP (metadata → workspace)**
```
Input:  <projects-dir>/<project-name>/
        ├── workspace.env
        ├── workspace-init
        ├── Nix/flake.nix
        └── tool-configs/

Tool:   workspace-setup <projects-dir>/<project-name> [workplace-dir]

Output: ~/workplace/<project-workspace>/
        ├── [git repos cloned]
        ├── [corp-workspaces created]
        ├── [symlinks created]
        └── .envrc
```

## Functional Requirements

**FR1: Workspace Capture**
- Given: Existing project-workspace at `~/workplace/<name>/`
- When: User runs `workspace-capture <source-workspace-dir> <output-metadata-dir>`
- Then: System generates metadata at `<output-metadata-dir>/<project-name>/`
- Captured data:
  - Git repositories (URLs, branches, providers)
  - Corporate workspaces (if applicable)
  - Symlink structure
  - Nix environment reference
  - Tool configurations
  - Project pattern (single-git, corp-workspace, multi-git, manual)

**FR2: Workspace Recreation**
- Given: Project metadata at `<projects-dir>/<name>/`
- When: User runs `workspace-setup <projects-dir>/<name> [workplace-dir]`
- Then: System creates project-workspace at specified location
- Recreation includes:
  - Creating workplace directory structure
  - Cloning git repositories with correct branches
  - Setting up corporate workspaces (if applicable)
  - Creating symlinks
  - Generating .envrc for direnv
  - Running direnv allow

**FR3: Bulk Recreation**
- Given: Multiple project metadata directories
- When: User runs init-all command (via nix flake)
- Then: All projects recreated in ~/workplace/

**FR4: Pattern Support**
- **single-git**: One git repository
- **corp-workspace**: Multiple corporate workspaces, no git repos
- **multi-git**: Multiple git repositories
- **manual**: Custom setup (user-defined)

**FR5: Path Portability**
- All scripts use `~` or `$HOME` instead of hardcoded usernames
- Works across different machines with different usernames
- Supports different platforms (Linux, Darwin/macOS)

**FR6: Git Provider Support**
- Corporate git providers (internal hosting with credentials)
- GitHub (SSH keys)
- Personal git providers

## Design Principles

### 1. Declarative Metadata

**workspace.env** is a shell-sourceable file:
```bash
# Identity
PROJECT_NAME="myproject"
PROJECT_DESCRIPTION="..."
PROJECT_PATTERN="single-git"

# Location (portable)
WORKPLACE_DIR="$HOME/workplace/myproject"

# Nix environment
FLAKE_PATH="./project-myproject/Nix"

# Data sources
GIT_REPOS=(...)
CORP_WORKSPACES=(...)
SYMLINKS=(...)
```

### 2. Imperative Recreation (Python)

**workspace-init** is a Python script:
1. Parse `workspace.env`
2. Create `$WORKPLACE_DIR`
3. Clone git repos
4. Create corp-workspaces (if applicable)
5. Create symlinks using `os.symlink()`
6. Generate .envrc
7. Run direnv allow via `subprocess`

**Why Python?**
- Nix environment includes Python
- Better readability for complex logic
- Easier error handling
- Standard library: `os`, `subprocess`, `pathlib`

### 3. Explicit Registration

Top-level orchestration (flake.nix) explicitly lists projects:
```nix
projects = {
  project1 = ./project1;
  project2 = ./project2;
};
```

Provides:
- `nix run .#init-<project>` - Setup individual project
- `nix run .#init-all` - Setup all projects
- `nix develop .#<project>` - Enter project environment

### 4. Separation of Concerns

```
Generic (in env/workspace-tools/):
├── workspace-capture (if no private dependencies)
├── workspace-setup (if no private dependencies)
└── Design documentation

Private (in <private-env>/projects/):
├── git-integrations/ (credential helpers)
├── flake.nix (top-level orchestrator)
└── <project>/ (project metadata)
```

### 5. Tool Configs Storage

Tool configurations (kiro, claude, etc.) are:
- Stored in `<project-metadata>/tool-configs/<tool>/`
- NOT stored within git repos in the workspace
- Symlinked into workspace (e.g., `.kiro` → `project-name/tool-configs/kiro`)

## workspace.env Schema

```bash
# === Identity ===
PROJECT_NAME="myproject"                # Required: unique identifier
PROJECT_DESCRIPTION="My project"        # Optional: human description
PROJECT_PATTERN="single-git"            # Required: pattern type

# === Location (use $HOME for portability) ===
WORKPLACE_DIR="$HOME/workplace/myproject"

# === Nix Environment ===
FLAKE_PATH="./project-myproject/Nix"    # Relative to WORKPLACE_DIR

# === Git Repositories ===
# Format: repo-name|provider|account|credential-helper|role|region|branch
GIT_REPOS=(
  "my-repo|github|myorg||||main"
  "other-repo|corporate-git|account|helper|role|region|branch"
)

# === Corporate Workspaces ===
# Format: workspace-name|versionset|platform|package1,package2,...
CORP_WORKSPACES=(
  "workspace1|versionset1||package1"
  "workspace2|versionset2|platform|package1,package2"
)

# === Symlinks ===
# Format: link-name::target
# Target can use $HOME, $WORKPLACE_DIR variables
SYMLINKS=(
  "project-myproject::$HOME/env-workplace/<private-env>/projects/myproject"
  ".tool1::project-myproject/tool-configs/tool1"
  ".tool2::project-myproject/tool-configs/tool2"
)

# === Metadata (optional) ===
CREATED_DATE="2026-04-23"
LAST_MODIFIED="2026-04-23"
```

## Implementation Language: Python

All workspace management tools are implemented in Python for:
- Better readability and maintainability
- Rich standard library (`os`, `subprocess`, `pathlib`, `shutil`)
- Easier error handling and validation
- Already available in nix environment

## Non-Functional Requirements

**Idempotency**
- Scripts can be run multiple times safely
- Detect existing state and skip/update appropriately
- Backup files before overwriting

**Extensibility**
- Easy to add new project patterns
- Easy to add new git providers
- Migration path to TOML/JSON if needed

**Error Handling**
- Clear error messages when dependencies missing
- Validation of metadata before recreation
- Rollback on failure (where possible)

## Usage Examples

### Capture Existing Workspace

```bash
# Capture existing project workspace into metadata
workspace-capture ~/workplace/myproject ~/env-workplace/<private-env>/projects

# Result: ~/env-workplace/<private-env>/projects/myproject/
#   ├── workspace.env
#   ├── workspace-init
#   ├── Nix/flake.nix
#   └── tool-configs/
```

### Recreate Workspace on New Machine

```bash
# Setup single project
cd ~/env-workplace/<private-env>/projects
nix run .#init-myproject

# Setup all projects
nix run .#init-all

# Result: ~/workplace/myproject/ recreated with all repos, configs, symlinks
```

## Success Criteria

- ✅ Fresh machine can recreate all workspaces with `nix run .#init-all`
- ✅ Capturing workspace takes < 5 minutes (mostly automated)
- ✅ Scripts use `$HOME`, work across platforms
- ✅ Idempotent and safe to re-run
- ✅ Clear error messages
- ✅ Easy to add new projects (capture + minor edits)
