# Design: Reproducible Project Workspace Management

## Core Principle

**Simplicity through Natural Language**: Instead of complex parsers, scripts, and configuration formats, we use markdown files with clear instructions that coding agents can execute.

## Key Files

- **workspace.md** (per-project) - Natural language setup instructions stored in project metadata
- **workspace-capture-instruction.md** (generic) - Instructions for coding agent to capture existing workspace
- **workspace-setup-instruction.md** (generic) - Instructions for coding agent to recreate workspace from workspace.md

## Directory Structure

```
~/env-workplace/
├── env/
│   └── workspace-tools/
│       ├── DESIGN.md (this file)
│       ├── workspace-capture-instruction.md
│       └── workspace-setup-instruction.md
│
└── <private-env>/
    ├── git-integrations/
    │   └── git-codecommit/setup (credential helper script)
    │
    └── projects/
        └── <project-name>/
            ├── workspace.md               ← Main config (natural language)
            ├── Nix/flake.nix              ← Development environment
            ├── tool-configs/              ← Tool configurations
            │   ├── kiro/
            │   └── claude/
            └── CLAUDE.md                  ← Project-specific AI instructions

~/workplace/
└── <project-name>/                        ← Recreated workspace
    ├── project-<name> → (symlink to metadata)
    ├── .kiro → project-<name>/tool-configs/kiro
    ├── .claude → project-<name>/tool-configs/claude
    ├── .envrc
    └── [git-repo]/                        ← Cloned repository
```

## Why This Approach?

Coding agents excel at interpreting natural language instructions. No need for custom parsers, complex scripts, or rigid config formats. workspace.md is both human-readable and agent-executable.
