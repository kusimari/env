# Workspace Capture Instructions

You are a coding agent helping to capture an existing project workspace into a reproducible configuration.

## Input Required from User

**IMPORTANT**: Always ask for these at the start:

1. **Source workspace directory** - The existing workspace to capture
   - Ask: "Which workspace directory should I capture?"
   - Example: `~/workplace/myproject` or `/home/user/workplace/trace`
   - Validate: Check that directory exists

2. **Output metadata directory** - Where to store the captured configuration
   - Ask: "Where should I store the project metadata?"
   - Example: `~/env-workplace/Gorantls-env/projects` or `/local/home/user/env-workplace/Gorantls-env/projects`
   - Validate: Check that directory exists

3. **Project name** (optional) - Name for this project
   - Ask: "Project name? (press Enter to use directory name)"
   - Default: basename of source directory
   - This determines the output directory: `<output-dir>/<project-name>/`

## Check for Existing Project

Before capturing, check if the project already exists:

```bash
ls -la <output-dir>/<project-name>/
```

If project exists:
- Inform user: "Project '<project-name>' already exists at <output-dir>/<project-name>/"
- Ask: "Should I update the existing project or create a backup?"
  - **Update**: Overwrite workspace.md, merge/update tool-configs
  - **Backup**: Move existing to `<project-name>.backup.<timestamp>`, then create fresh

If updating:
- Preserve existing Nix/flake.nix unless user wants to regenerate
- Merge tool-configs (don't delete existing configs)
- Update workspace.md with new information

## Capture Process

### 1. Analyze the Workspace

Navigate to the source workspace directory and examine:

```bash
cd <source-workspace-dir>
ls -la
```

Look for:
- **Git repositories**: Directories with `.git/` subdirectories
- **Symlinks**: Files/directories that are symbolic links
- **Environment files**: `.envrc`, `.env`, etc.
- **Tool configurations**: `.kiro/`, `.claude/`, etc. (but NOT inside git repos)

### 2. Extract Git Repository Information

For each git repository found:

```bash
cd <repo-dir>
git remote get-url origin
git branch --show-current
git config --local --list | grep -E "(codecommit|credential)"
```

Capture:
- Repository name
- Remote URL
- Current branch
- Provider (codecommit, github, git.amazon, etc.)
- Any credential configuration (ada provider, role, region, account)

### 3. Extract Symlinks

```bash
find . -maxdepth 1 -type l
```

For each symlink, capture:
- Link name (relative to workspace)
- Target (use `readlink -f` for absolute path, then convert to portable format using `$HOME`)

### 4. Extract Environment Configuration

Look for `.envrc`:
```bash
cat .envrc
```

Identify the nix flake path if present (e.g., `use flake ./project-name/Nix`)

### 5. Identify Tool Configurations

Look for tool-specific configurations that should be captured (NOT inside git repos):
- `.kiro/` or `kiro/`
- `.claude/` or `claude/`
- Other tool directories

Ask user: "Which of these tool configuration directories should be captured in the metadata?"

### 6. Self-Reflection

Before generating the workspace.md, reflect on:

- **Missing dependencies**: Are there build systems or corporate tools mentioned in READMEs that need special setup?
- **Environment variables**: Are there any ENV vars beyond what's in .envrc?
- **Platform-specific paths**: Any hardcoded paths that won't work on other machines?
- **Secrets**: Any credentials or sensitive files that shouldn't be captured?

Inform the user about potential issues or things that might need manual attention.

## Generate workspace.md

Create `<output-dir>/<project-name>/workspace.md` with the following structure:

```markdown
# Workspace: <project-name>

## Overview

Brief description of this project workspace.

## Prerequisites

- Nix with flakes enabled
- direnv
- [Any other tools needed]

## Git Repositories

For each repository:

### Repository: <repo-name>

- **Provider**: codecommit / github / git.amazon
- **URL/Account**: [connection info]
- **Branch**: <branch-name>
- **Setup Command**:
  ```bash
  # For codecommit
  bash ~/env-workplace/<private-env>/git-codecommit/setup <repo-name> <account> <ada-provider> <role> <region> <branch>
  
  # For github
  git clone git@github.com:<org>/<repo-name>.git
  cd <repo-name> && git checkout <branch>
  ```

## Tool Configurations

List any tool configuration directories that were captured:

- **kiro**: Stored in `<project-metadata>/tool-configs/kiro/`
- **claude**: Stored in `<project-metadata>/tool-configs/claude/`

## Symlinks

Create these symlinks in the workspace:

```bash
cd $HOME/workplace/<project-name>

# Link to project metadata
ln -s $HOME/env-workplace/<private-env>/projects/<project-name> project-<project-name>

# Link to tool configs
ln -s project-<project-name>/tool-configs/kiro .kiro
ln -s project-<project-name>/tool-configs/claude .claude

# [Other symlinks...]
```

## Environment Setup

Create `.envrc`:

```bash
cat > .envrc << 'EOF'
use flake ./project-<project-name>/Nix
EOF

direnv allow
```

## Verification

After setup, verify:

```bash
# Check git repos
ls -la <repo-name>/.git

# Check symlinks
readlink .kiro  # Should point to project-*/tool-configs/kiro

# Check environment
direnv reload
which <expected-tool>  # Should be from nix
```

## Notes

[Any special considerations, manual steps, or gotchas]
```

## Copy Tool Configurations

For each tool configuration directory the user wants to capture:

```bash
mkdir -p <output-dir>/<project-name>/tool-configs
cp -r <source-workspace>/<tool-dir> <output-dir>/<project-name>/tool-configs/
```

## Copy Nix Configuration

If there's a flake.nix in the workspace or linked:

```bash
mkdir -p <output-dir>/<project-name>/Nix
cp <source-workspace>/flake.nix <output-dir>/<project-name>/Nix/
cp <source-workspace>/flake.lock <output-dir>/<project-name>/Nix/ 2>/dev/null || true
```

## Summary

Report to the user:
- Created workspace.md at: `<output-dir>/<project-name>/workspace.md`
- Copied tool configs to: `<output-dir>/<project-name>/tool-configs/`
- Copied nix config to: `<output-dir>/<project-name>/Nix/`
- Next step: Review workspace.md and test recreation on another machine
