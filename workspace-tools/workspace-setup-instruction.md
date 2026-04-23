# Workspace Setup Instructions

You are a coding agent helping to recreate a project workspace from its workspace.md configuration file.

## Input Required from User

**IMPORTANT**: Always ask for these at the start:

1. **Project metadata directory** - Path to the project's metadata containing workspace.md
   - Ask: "Which project should I setup? (provide path to project metadata directory)"
   - Example: `~/env-workplace/Gorantls-env/projects/trace`
   - Validate: Check that directory exists and contains workspace.md

2. **Target workspace directory** (optional) - Where to create the workspace
   - Ask: "Where should I create the workspace? (press Enter for default: ~/workplace/<project-name>)"
   - Default: `~/workplace/<project-name>` (extracted from metadata dir basename)
   - Validate: Check parent directory exists

## Setup Process

### 1. Read workspace.md

Read the workspace.md file from the project metadata directory:

```bash
cat <metadata-dir>/workspace.md
```

Parse and understand:
- Prerequisites needed
- Git repositories to clone
- Symlinks to create
- Environment configuration
- Tool configurations available

### 2. Verify Prerequisites

Check that required tools are installed:

```bash
command -v nix
command -v direnv
command -v git
# [Other tools from Prerequisites section]
```

If missing, inform user and exit with clear instructions on what to install.

### 3. Create Workspace Directory

```bash
mkdir -p <workspace-dir>
cd <workspace-dir>
```

### 4. Clone Git Repositories

For each repository listed in workspace.md, execute the setup commands provided in the file.

**Important**: 
- Use the exact commands from workspace.md
- For codecommit repos, ensure the git-codecommit/setup script exists
- For github repos, ensure SSH keys are configured
- Check if repo already exists before cloning (idempotent)
- Report progress for each repository

### 5. Create Symlinks

Execute the symlink commands from workspace.md:

```bash
# Example from workspace.md:
ln -s $HOME/env-workplace/<private-env>/projects/<project-name> project-<project-name>
ln -s project-<project-name>/tool-configs/kiro .kiro
# ... etc
```

**Important**:
- Check if symlink already exists
- If existing symlink points to wrong target, ask user before updating
- If non-symlink file exists with same name, error and ask user

### 6. Setup Environment

Create `.envrc` as specified in workspace.md, then:

```bash
direnv allow .
```

### 7. Run Verification Steps

Execute the verification commands from workspace.md to ensure everything is set up correctly.

Report any verification failures clearly.

### 8. Report Completion

Inform user:
- Workspace created at: `<workspace-dir>`
- Git repositories cloned: [list]
- Symlinks created: [list]
- Environment activated: [yes/no]

Next steps:
```bash
cd <workspace-dir>
direnv reload
# Ready to work!
```

## Error Handling

If any step fails:
1. Report the error clearly
2. Show what succeeded before the failure
3. Provide next steps for user to fix manually
4. Don't continue if critical step fails (git clone, prerequisites missing)

## Idempotency

The setup should be safe to run multiple times:
- Skip cloning if git repo already exists
- Skip symlinks if they already point to correct target
- Backup .envrc before overwriting if it exists with different content
