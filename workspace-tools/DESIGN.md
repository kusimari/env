# Design: Reproducible Project Workspace Management

## Overview

A coding-agent-friendly system to capture and recreate project workspaces across machines. Uses markdown instruction files that can be interpreted by either humans or coding agents.

## Core Principle

**Simplicity through Natural Language**: Instead of complex parsers, scripts, and configuration formats, we use markdown files with clear instructions that coding agents can execute.

## Key Files

### 1. workspace.md (Per-Project)

A markdown document stored in project metadata that describes how to recreate the workspace. Contains:

- **Overview**: What is this project?
- **Prerequisites**: What tools are needed?
- **Git Repositories**: List of repos with setup commands
- **Tool Configurations**: What tool configs exist in metadata
- **Symlinks**: Exact commands to create symlinks
- **Environment Setup**: How to setup .envrc and direnv
- **Verification**: Commands to verify setup worked
- **Notes**: Any gotchas or special considerations

Example location: `~/env-workplace/<private-env>/projects/myproject/workspace.md`

### 2. workspace-capture-instruction.md (Generic)

Instructions for a coding agent on how to capture an existing workspace into a workspace.md file.

Agent tasks:
- Analyze workspace directory
- Find git repos and extract connection info
- Identify symlinks and tool configurations
- Generate workspace.md with setup instructions
- Copy tool configs and nix files to metadata directory
- Self-reflect on potential issues

Location: `~/env-workplace/env/workspace-tools/workspace-capture-instruction.md`

### 3. workspace-setup-instruction.md (Generic)

Instructions for a coding agent on how to recreate a workspace from workspace.md.

Agent tasks:
- Read and parse workspace.md
- Verify prerequisites
- Execute git clone commands
- Create symlinks as instructed
- Setup .envrc and run direnv
- Run verification commands
- Report completion status

Location: `~/env-workplace/env/workspace-tools/workspace-setup-instruction.md`

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

## Workflow

### Capture Existing Workspace

1. User invokes coding agent with: "Use workspace-capture-instruction.md to capture my workspace"
2. Agent asks for source workspace dir and output metadata dir
3. Agent analyzes workspace, extracts repo info, finds symlinks
4. Agent generates workspace.md with clear setup instructions
5. Agent copies tool configs and nix files to metadata
6. Agent reports what was captured and potential issues

### Recreate Workspace

1. User invokes coding agent with: "Use workspace-setup-instruction.md to setup my workspace"
2. Agent asks for project metadata directory
3. Agent reads workspace.md and understands requirements
4. Agent executes each section: prerequisites, git clone, symlinks, envrc
5. Agent runs verification commands
6. Agent reports completion and any issues

## Why This Approach?

### Advantages

**Human-Readable**:
- Anyone can read workspace.md and understand the setup
- No need to parse complex formats (TOML, JSON, env files)
- Easy to edit manually if needed

**Agent-Friendly**:
- Coding agents excel at interpreting natural language instructions
- No custom parsers needed
- Agents can self-reflect and catch issues

**Maintainable**:
- Workspace changes? Just update the markdown instructions
- No code to maintain (beyond the instruction files themselves)
- Version control friendly (markdown diffs are readable)

**Flexible**:
- Easy to add new sections or special cases
- Can include conditional logic in natural language
- Platform-specific notes are just... notes

**Portable**:
- Works with any coding agent (Claude, GPT, etc.)
- No language runtime dependencies (Python, Bash, etc.)
- Just markdown + a coding agent

### Why Not Code?

Complex parsers and scripts:
- Harder to maintain
- More failure modes
- Requires specific runtime (Python, Bash)
- Harder for humans to modify
- Rigid structure

## Example: Git Repository Section

Instead of:
```bash
GIT_REPOS=(
  "repo|provider|account|role|region|branch"
)
```

We write:
```markdown
### Repository: MyRepo

- **Provider**: codecommit
- **Account**: 123456
- **Branch**: main
- **Setup Command**:
  ```bash
  bash ~/env-workplace/corp-env/git-codecommit/setup \
    MyRepo 123456 Conduit Role us-west-2 main
  ```
```

Clear, readable, and the agent knows exactly what to execute.

## Principles

1. **Instructions over Configuration**: Write what to do, not structured data
2. **Idempotency**: Instructions should be safe to run multiple times
3. **Self-Documentation**: The workspace.md explains itself
4. **Verification**: Always include commands to verify setup worked
5. **Notes Section**: Capture gotchas and context for humans

## Tool Configurations

Tool configurations (kiro, claude, etc.) are:
- Stored in `<project-metadata>/tool-configs/<tool>/`
- NOT stored inside git repositories
- Symlinked into workspace (e.g., `.kiro` → `project-name/tool-configs/kiro`)
- Kept in sync across machines via the metadata repository

## Success Criteria

- ✅ Humans can read workspace.md and manually recreate workspace
- ✅ Coding agents can execute workspace.md instructions automatically
- ✅ Capturing workspace takes < 5 minutes (mostly automated by agent)
- ✅ Recreating workspace is fully automated
- ✅ No complex parsers or scripts to maintain
- ✅ Easy to add new projects or modify existing ones
- ✅ Works across platforms (Linux, macOS)
