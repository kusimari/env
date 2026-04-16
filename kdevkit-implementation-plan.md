# kdevkit Multi-Layer Implementation Plan

**Version**: 0.1.0  
**Date**: 2026-04-16  
**Based on**: kdevkit-multilayer-spec.md

---

## Overview

This plan implements the multi-layer kdevkit architecture in phases, with each phase delivering concrete value and being independently testable. We'll use GitHub's API (via curl/web fetch) to interact with the kusimari/kdevkit repository without cloning it locally.

---

## Phase 1: Repository Restructuring (1-2 PRs)

**Goal**: Reorganize kdevkit git repo to support system/project separation.

### 1.1 Directory Restructuring

**Changes to kusimari/kdevkit**:
```
OLD STRUCTURE                    NEW STRUCTURE
├── .kdevkit/                    ├── system/                 (NEW)
│   ├── feature/                 │   ├── README.md
│   └── project.md               │   ├── git-practices.md
├── agent-dev-loop.md            │   ├── coding-standards.md
├── feature-dev.md               │   └── agent-behavior.md
├── echo.md                      ├── project/                (NEW)
├── build.js                     │   ├── README.md
├── tests/                       │   ├── feature-dev.md      (moved)
└── README.md                    │   ├── agent-dev-loop.md   (moved)
                                 │   ├── echo.md             (moved)
                                 │   ├── project-template.md (new)
                                 │   └── templates/          (new)
                                 ├── tools/                  (NEW)
                                 │   ├── kdevkit-sync
                                 │   ├── kdevkit-init
                                 │   └── kdevkit-inspect
                                 ├── build.js                (updated)
                                 ├── tests/                  (updated)
                                 └── README.md               (updated)
```

**Tasks**:
- [ ] Create `system/` directory with initial content
  - Extract git conventions from `feature-dev.md` → `system/git-practices.md`
  - Create `system/coding-standards.md` (new, based on common patterns)
  - Create `system/agent-behavior.md` (new, extracted from feature-dev.md)
  - Create `system/README.md` (explains system-level kdevkit)
  
- [ ] Create `project/` directory
  - Move `feature-dev.md` → `project/feature-dev.md`
  - Move `agent-dev-loop.md` → `project/agent-dev-loop.md`
  - Move `echo.md` → `project/echo.md`
  - Extract `.kdevkit/project.md` content → `project/project-template.md`
  - Create `project/README.md` (explains project-level kdevkit)
  - Create `project/templates/` directory with `feature-template.md`

- [ ] Update build system
  - Modify `build.js` to support multiple targets (`system`, `project`)
  - Update `MANIFEST` structure to handle both directories
  - Generate `build/kdevkit-system.md` and `build/kdevkit-project.md`
  
- [ ] Update tests
  - Modify `tests/build.sh` to validate both system and project builds
  - Add tests for file organization (system files don't reference project, etc.)
  
- [ ] Update main README.md
  - Document new structure
  - Update usage examples to reference system vs. project

**Deliverable**: PR #1 to kusimari/kdevkit with restructured repo

### 1.2 Extract Content

**Create system/git-practices.md**:
```markdown
# Git Practices

This file defines git conventions for all projects using kdevkit.

## Branch Naming
Format: `<type>/<short-description>`

Types: feat, fix, chore, docs, refactor, test

## Commit Messages
Conventional Commits format: `type(scope): subject`

Rules:
- Imperative mood ("add" not "added")
- Lowercase subject line
- No period at end
- Subject ≤ 72 characters
- Body explains *why*, not *what*
- Every commit must leave repo in working state

## Pull Requests
- Title follows commit message format
- Body explains motivation and approach
- Keep PRs small and focused
- Default merge strategy: squash merge

## Commit Discipline
- Commit at coherent completion points
- Each commit must pass build and tests
- No WIP commits on main branch

## Hygiene
Never commit:
- Commented-out code
- Debug print statements
- Temporary files
- Secrets or credentials
- .env files with actual values

## Scope
- Changes stay within project root
- Never modify global git config
- Never write outside project directory
```

**Create system/coding-standards.md**:
```markdown
# Coding Standards

Language-agnostic coding standards for all projects.

## Code Quality Principles

### Simplicity First
- Don't add features beyond what was asked
- Don't refactor code you didn't need to change
- Three similar lines > premature abstraction
- No speculative abstractions for hypothetical future requirements

### Error Handling
- Only validate at system boundaries (user input, external APIs)
- Trust internal code and framework guarantees
- Don't add error handling for scenarios that can't happen

### Comments and Documentation
- Only add comments where logic isn't self-evident
- Don't add docstrings to code you didn't change
- Code should be self-documenting when possible

### Abstractions
- Don't create helpers/utilities for one-time operations
- Right amount of complexity = what the task requires
- No half-finished implementations either

### Backwards Compatibility
- Delete unused code completely
- No backwards-compatibility hacks (_vars, re-exports, removed comments)
- If certain something is unused, delete it

## Security
- Never introduce: command injection, XSS, SQL injection, OWASP Top 10
- If you write insecure code, immediately fix it
- Prioritize safe, secure, and correct code

## Testing Philosophy
- Type checking and test suites verify code correctness, not feature correctness
- For UI/frontend: test in browser before claiming completion
- Test golden path and edge cases
- Monitor for regressions

## Code Review
- Changes should be minimal and focused
- Avoid mixing refactoring with feature work
- Self-review before requesting review
```

**Create system/agent-behavior.md**:
```markdown
# Agent Behavior

Default behavior patterns for coding agents.

## Session Behavior

### Persistence
- Update session files after each meaningful unit of work
- Don't batch updates
- Record decisions, completed tasks, resolved questions

### Phase Gating
- Stop between phases
- Wait for explicit instruction before proceeding
- Exception: YOLO mode (disabled by default)

### Assumptions
- If phase input is ambiguous: present brief plan, wait for approval
- If clear: act immediately
- Never guess at requirements

### YOLO Mode
- Disabled by default
- When enabled: chains phases, skips assumption plans
- Toggle: "yolo" / "yolo off"

## Communication Style

### Brevity
- Responses should be short and concise
- No emoji unless explicitly requested
- No colon before tool calls

### Code References
- Use `file_path:line_number` format
- Use `owner/repo#123` for GitHub issues/PRs

### Summaries
- Don't summarize what you just did (user can read diff)
- Focus on next steps or blockers

## Work Approach

### Reading Before Writing
- Read files before suggesting modifications
- Understand existing code before changes
- Don't propose changes to unread code

### File Creation
- Prefer editing existing files over creating new ones
- Only create files absolutely necessary for the goal
- Never create .md or README files unless requested

### Estimates
- Don't give time estimates
- Focus on what needs to be done, not duration

### Failure Response
- Diagnose why before switching tactics
- Read errors and check assumptions
- Try focused fix
- Don't retry identical action blindly
- Don't abandon viable approach after single failure
- Escalate only when genuinely stuck after investigation

### Scope Control
- Bug fix doesn't need surrounding cleanup
- Simple feature doesn't need extra configurability
- Don't add docstrings/comments/types to unchanged code
- Don't add error handling for impossible scenarios
- No feature flags for straightforward changes
```

**Create project/project-template.md**:
```markdown
# Project: [Project Name]

## Purpose
[Brief description of what this project does]

## Tech Stack
[List main technologies, languages, frameworks]

## Constraints
[Any hard constraints, requirements, or limitations]

## Key Paths

| Path | Role |
|------|------|
| [path] | [description] |

## Development Workflow

### Build
[How to build the project]

### Test
[How to run tests]

### Run
[How to run the project locally]

### Deploy
[How to deploy, if applicable]

## Project Conventions
[Any project-specific conventions not covered by system-level standards]

## Architecture Notes
[High-level architecture overview]
```

**Deliverable**: Same PR or follow-up PR with content extraction

---

## Phase 2: Basic Tooling (3-5 PRs)

**Goal**: Create `kdevkit-init`, `kdevkit-inspect`, and config file support.

### 2.1 kdevkit-init Tool

**Implementation choice**: Start with bash for portability.

**File**: `tools/kdevkit-init`

```bash
#!/usr/bin/env bash
# kdevkit-init: Initialize kdevkit at system or project level

set -euo pipefail

VERSION="0.1.0"
REPO="kusimari/kdevkit"
GITHUB_RAW="https://raw.githubusercontent.com/$REPO/main"
GITHUB_API="https://api.github.com/repos/$REPO"

usage() {
    cat <<EOF
Usage: kdevkit-init <scope> [options]

Scopes:
  system              Initialize system-level kdevkit (~/.kdevkit/)
  project             Initialize project-level kdevkit (./.kdevkit/)

Options:
  --version VERSION   Fetch specific version (default: main)
  --auto              Non-interactive mode (accept all defaults)
  -h, --help          Show this help

Examples:
  kdevkit-init system
  kdevkit-init project --auto
  kdevkit-init system --version v1.2.3
EOF
}

fetch_file() {
    local url="$1"
    local dest="$2"
    
    mkdir -p "$(dirname "$dest")"
    
    if command -v curl &> /dev/null; then
        curl -fsSL "$url" -o "$dest"
    elif command -v wget &> /dev/null; then
        wget -qO "$dest" "$url"
    else
        echo "Error: Neither curl nor wget found" >&2
        exit 1
    fi
}

fetch_latest_commit() {
    local api_url="$GITHUB_API/commits/main"
    
    if command -v curl &> /dev/null; then
        curl -fsSL "$api_url" | grep '"sha"' | head -1 | cut -d'"' -f4
    else
        echo "unknown"
    fi
}

init_system() {
    local kdevkit_dir="$HOME/.kdevkit"
    local auto_mode="$1"
    
    if [ -d "$kdevkit_dir" ]; then
        echo "System kdevkit already exists at $kdevkit_dir"
        if [ "$auto_mode" != "true" ]; then
            read -p "Reinitialize? [y/N] " -n 1 -r
            echo
            [[ ! $REPLY =~ ^[Yy]$ ]] && exit 0
        fi
    fi
    
    echo "Initializing system kdevkit at $kdevkit_dir..."
    
    # Create directory structure
    mkdir -p "$kdevkit_dir"/{upstream,local,memory}
    
    # Fetch upstream files
    echo "Fetching system files from $REPO..."
    local files=(
        "system/README.md"
        "system/git-practices.md"
        "system/coding-standards.md"
        "system/agent-behavior.md"
    )
    
    for file in "${files[@]}"; do
        local basename="${file#system/}"
        echo "  - $basename"
        fetch_file "$GITHUB_RAW/$file" "$kdevkit_dir/upstream/$basename"
    done
    
    # Record upstream reference
    local commit_sha
    commit_sha=$(fetch_latest_commit)
    
    cat > "$kdevkit_dir/upstream/.git-ref" <<EOF
{
  "repo": "$REPO",
  "ref": "main",
  "path": "system",
  "commit": "$commit_sha",
  "fetched_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
    
    # Create default config
    cat > "$kdevkit_dir/config.yaml" <<'EOF'
kdevkit:
  version: 0.1.0
  upstream:
    repo: kusimari/kdevkit
    ref: main
    path: system
  
  local:
    auto_sync: false
    sync_prompt: true
    reconcile_on_exit: true
  
  memory:
    auto_save: true
    max_size_kb: 1024
    retention_days: 90
  
  overrides: []
EOF
    
    # Initialize divergence log
    cat > "$kdevkit_dir/.divergence.log" <<EOF
{
  "tracking_since": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "upstream_ref": "$commit_sha",
  "local_changes": [],
  "memory_stats": {
    "total_files": 0,
    "total_size_kb": 0,
    "oldest_entry": null
  }
}
EOF
    
    echo ""
    echo "✓ System kdevkit initialized at $kdevkit_dir"
    echo ""
    echo "Next steps:"
    echo "  1. Review files in $kdevkit_dir/upstream/"
    echo "  2. Customize by creating overrides in $kdevkit_dir/local/"
    echo "  3. Run 'kdevkit-inspect system' to see status"
}

init_project() {
    local kdevkit_dir=".kdevkit"
    local auto_mode="$1"
    
    if [ ! -d .git ]; then
        echo "Error: Not in a git repository root" >&2
        echo "Run this command from your project's git root directory" >&2
        exit 1
    fi
    
    if [ -d "$kdevkit_dir" ]; then
        echo "Project kdevkit already exists at $kdevkit_dir"
        if [ "$auto_mode" != "true" ]; then
            read -p "Reinitialize? [y/N] " -n 1 -r
            echo
            [[ ! $REPLY =~ ^[Yy]$ ]] && exit 0
        fi
    fi
    
    echo "Initializing project kdevkit at $kdevkit_dir..."
    
    # Create directory structure
    mkdir -p "$kdevkit_dir"/{upstream,local,feature,memory}
    
    # Fetch upstream files
    echo "Fetching project files from $REPO..."
    local files=(
        "project/README.md"
        "project/feature-dev.md"
        "project/agent-dev-loop.md"
        "project/project-template.md"
    )
    
    for file in "${files[@]}"; do
        local basename="${file#project/}"
        echo "  - $basename"
        fetch_file "$GITHUB_RAW/$file" "$kdevkit_dir/upstream/$basename"
    done
    
    # Record upstream reference
    local commit_sha
    commit_sha=$(fetch_latest_commit)
    
    cat > "$kdevkit_dir/upstream/.git-ref" <<EOF
{
  "repo": "$REPO",
  "ref": "main",
  "path": "project",
  "commit": "$commit_sha",
  "fetched_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
    
    # Create project.md from template if it doesn't exist
    if [ ! -f "$kdevkit_dir/local/project.md" ]; then
        cp "$kdevkit_dir/upstream/project-template.md" "$kdevkit_dir/local/project.md"
        echo ""
        echo "Created $kdevkit_dir/local/project.md from template"
        echo "Please edit this file to describe your project"
    fi
    
    # Create default config
    cat > "$kdevkit_dir/config.yaml" <<'EOF'
kdevkit:
  version: 0.1.0
  upstream:
    repo: kusimari/kdevkit
    ref: main
    path: project
  
  local:
    auto_sync: false
    sync_prompt: true
  
  memory:
    auto_save: true
    inherit_system: true
  
  feature:
    auto_persist: true
    template: default
  
  overrides: []
EOF
    
    # Initialize divergence log
    cat > "$kdevkit_dir/.divergence.log" <<EOF
{
  "tracking_since": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "upstream_ref": "$commit_sha",
  "local_changes": [],
  "memory_stats": {
    "total_files": 0,
    "total_size_kb": 0,
    "oldest_entry": null
  }
}
EOF
    
    # Add to .gitignore
    if [ -f .gitignore ]; then
        if ! grep -q "^.kdevkit/memory" .gitignore; then
            echo "" >> .gitignore
            echo "# kdevkit" >> .gitignore
            echo ".kdevkit/memory/" >> .gitignore
            echo ".kdevkit/.divergence.log" >> .gitignore
            echo "Added kdevkit entries to .gitignore"
        fi
    fi
    
    echo ""
    echo "✓ Project kdevkit initialized at $kdevkit_dir"
    echo ""
    echo "Next steps:"
    echo "  1. Edit $kdevkit_dir/local/project.md"
    echo "  2. Run 'kdevkit-inspect project' to see status"
    echo "  3. Start using kdevkit workflows in your coding agent"
}

main() {
    local scope=""
    local version="main"
    local auto_mode="false"
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            system|project)
                scope="$1"
                shift
                ;;
            --version)
                version="$2"
                shift 2
                ;;
            --auto)
                auto_mode="true"
                shift
                ;;
            -h|--help)
                usage
                exit 0
                ;;
            *)
                echo "Error: Unknown argument '$1'" >&2
                usage
                exit 1
                ;;
        esac
    done
    
    if [ -z "$scope" ]; then
        echo "Error: Scope required (system or project)" >&2
        usage
        exit 1
    fi
    
    case $scope in
        system)
            init_system "$auto_mode"
            ;;
        project)
            init_project "$auto_mode"
            ;;
    esac
}

main "$@"
```

**Tasks**:
- [ ] Create `tools/kdevkit-init` script
- [ ] Add execute permissions (`chmod +x`)
- [ ] Test initialization of system kdevkit
- [ ] Test initialization of project kdevkit
- [ ] Add tests in `tests/init-test.sh`
- [ ] Update main README with usage examples

**Deliverable**: PR #2 with kdevkit-init tool

### 2.2 kdevkit-inspect Tool

**File**: `tools/kdevkit-inspect`

```bash
#!/usr/bin/env bash
# kdevkit-inspect: Inspect kdevkit state and suggest improvements

set -euo pipefail

VERSION="0.1.0"

usage() {
    cat <<EOF
Usage: kdevkit-inspect <scope> [command]

Scopes:
  system              Inspect system-level kdevkit (~/.kdevkit/)
  project             Inspect project-level kdevkit (./.kdevkit/)

Commands:
  status              Show current state (default)
  divergence          Show files that have diverged from upstream
  memory              Show memory statistics
  suggest-push        Suggest which changes should be pushed upstream

Options:
  -h, --help          Show this help

Examples:
  kdevkit-inspect system
  kdevkit-inspect project divergence
  kdevkit-inspect system suggest-push
EOF
}

get_file_size_kb() {
    local file="$1"
    if [ -f "$file" ]; then
        local size
        size=$(du -k "$file" | cut -f1)
        echo "$size"
    else
        echo "0"
    fi
}

get_dir_size_kb() {
    local dir="$1"
    if [ -d "$dir" ]; then
        local size
        size=$(du -sk "$dir" | cut -f1)
        echo "$size"
    else
        echo "0"
    fi
}

count_files() {
    local dir="$1"
    if [ -d "$dir" ]; then
        find "$dir" -type f | wc -l
    else
        echo "0"
    fi
}

inspect_system_status() {
    local kdevkit_dir="$HOME/.kdevkit"
    
    if [ ! -d "$kdevkit_dir" ]; then
        echo "System kdevkit not initialized"
        echo "Run: kdevkit-init system"
        exit 1
    fi
    
    echo "kdevkit System Status"
    echo "====================="
    echo ""
    
    # Read upstream ref
    if [ -f "$kdevkit_dir/upstream/.git-ref" ]; then
        local commit
        commit=$(grep '"commit"' "$kdevkit_dir/upstream/.git-ref" | cut -d'"' -f4 | head -c 7)
        local repo
        repo=$(grep '"repo"' "$kdevkit_dir/upstream/.git-ref" | cut -d'"' -f4)
        echo "Upstream: $repo @ $commit"
    else
        echo "Upstream: Unknown"
    fi
    
    echo "Local:    $kdevkit_dir"
    echo ""
    
    # Upstream files
    echo "Upstream Files:"
    if [ -d "$kdevkit_dir/upstream" ]; then
        find "$kdevkit_dir/upstream" -type f -name "*.md" | while read -r file; do
            local basename
            basename=$(basename "$file")
            echo "  ✓ $basename"
        done
    fi
    echo ""
    
    # Local overrides
    echo "Local Overrides:"
    if [ -d "$kdevkit_dir/local" ] && [ -n "$(ls -A "$kdevkit_dir/local" 2>/dev/null)" ]; then
        find "$kdevkit_dir/local" -type f | while read -r file; do
            local basename
            basename=$(basename "$file")
            local size_kb
            size_kb=$(get_file_size_kb "$file")
            echo "  • $basename (${size_kb}KB)"
        done
    else
        echo "  (none)"
    fi
    echo ""
    
    # Memory stats
    echo "Memory:"
    if [ -d "$kdevkit_dir/memory" ]; then
        local count
        count=$(count_files "$kdevkit_dir/memory")
        local size_kb
        size_kb=$(get_dir_size_kb "$kdevkit_dir/memory")
        echo "  Files: $count"
        echo "  Size:  ${size_kb}KB"
    else
        echo "  (empty)"
    fi
}

inspect_project_status() {
    local kdevkit_dir=".kdevkit"
    
    if [ ! -d "$kdevkit_dir" ]; then
        echo "Project kdevkit not initialized"
        echo "Run: kdevkit-init project"
        exit 1
    fi
    
    echo "kdevkit Project Status"
    echo "======================"
    echo ""
    
    # Read upstream ref
    if [ -f "$kdevkit_dir/upstream/.git-ref" ]; then
        local commit
        commit=$(grep '"commit"' "$kdevkit_dir/upstream/.git-ref" | cut -d'"' -f4 | head -c 7)
        local repo
        repo=$(grep '"repo"' "$kdevkit_dir/upstream/.git-ref" | cut -d'"' -f4)
        echo "Upstream: $repo @ $commit"
    else
        echo "Upstream: Unknown"
    fi
    
    echo "Local:    $kdevkit_dir"
    echo ""
    
    # Upstream files
    echo "Upstream Files:"
    if [ -d "$kdevkit_dir/upstream" ]; then
        find "$kdevkit_dir/upstream" -type f -name "*.md" | while read -r file; do
            local basename
            basename=$(basename "$file")
            echo "  ✓ $basename"
        done
    fi
    echo ""
    
    # Local overrides
    echo "Local Files:"
    if [ -d "$kdevkit_dir/local" ] && [ -n "$(ls -A "$kdevkit_dir/local" 2>/dev/null)" ]; then
        find "$kdevkit_dir/local" -type f | while read -r file; do
            local basename
            basename=$(basename "$file")
            local size_kb
            size_kb=$(get_file_size_kb "$file")
            echo "  • $basename (${size_kb}KB)"
        done
    else
        echo "  (none)"
    fi
    echo ""
    
    # Feature files
    echo "Features:"
    if [ -d "$kdevkit_dir/feature" ] && [ -n "$(ls -A "$kdevkit_dir/feature" 2>/dev/null)" ]; then
        find "$kdevkit_dir/feature" -type f -name "*.md" | while read -r file; do
            local basename
            basename=$(basename "$file" .md)
            echo "  • $basename"
        done
    else
        echo "  (none)"
    fi
    echo ""
    
    # Memory stats
    echo "Memory:"
    if [ -d "$kdevkit_dir/memory" ]; then
        local count
        count=$(count_files "$kdevkit_dir/memory")
        local size_kb
        size_kb=$(get_dir_size_kb "$kdevkit_dir/memory")
        echo "  Files: $count"
        echo "  Size:  ${size_kb}KB"
    else
        echo "  (empty)"
    fi
}

inspect_divergence() {
    local scope="$1"
    local kdevkit_dir
    
    if [ "$scope" = "system" ]; then
        kdevkit_dir="$HOME/.kdevkit"
    else
        kdevkit_dir=".kdevkit"
    fi
    
    echo "Divergence Analysis"
    echo "==================="
    echo ""
    
    local found_divergence=false
    
    if [ -d "$kdevkit_dir/local" ]; then
        for local_file in "$kdevkit_dir/local"/*; do
            if [ -f "$local_file" ]; then
                local basename
                basename=$(basename "$local_file")
                local upstream_file="$kdevkit_dir/upstream/$basename"
                
                if [ -f "$upstream_file" ]; then
                    if ! diff -q "$local_file" "$upstream_file" > /dev/null 2>&1; then
                        echo "Modified: $basename"
                        local lines_changed
                        lines_changed=$(diff -u "$upstream_file" "$local_file" | grep -c '^[+-]' || true)
                        echo "  Lines changed: ~$lines_changed"
                        found_divergence=true
                    fi
                else
                    echo "New: $basename"
                    echo "  No upstream equivalent"
                    found_divergence=true
                fi
                echo ""
            fi
        done
    fi
    
    if [ "$found_divergence" = false ]; then
        echo "No divergence detected"
        echo "Local files match upstream"
    fi
}

suggest_push() {
    local scope="$1"
    local kdevkit_dir
    
    if [ "$scope" = "system" ]; then
        kdevkit_dir="$HOME/.kdevkit"
    else
        kdevkit_dir=".kdevkit"
    fi
    
    echo "Push Suggestions"
    echo "================"
    echo ""
    
    local found_suggestions=false
    
    if [ -d "$kdevkit_dir/local" ]; then
        for local_file in "$kdevkit_dir/local"/*; do
            if [ -f "$local_file" ]; then
                local basename
                basename=$(basename "$local_file")
                
                # Skip certain files that should never be pushed
                if [[ "$basename" =~ ^(config\.yaml|.*\.local\..*)$ ]]; then
                    continue
                fi
                
                local upstream_file="$kdevkit_dir/upstream/$basename"
                
                if [ -f "$upstream_file" ]; then
                    if ! diff -q "$local_file" "$upstream_file" > /dev/null 2>&1; then
                        echo "✓ Consider pushing: $basename"
                        echo "  Status: Modified from upstream"
                        echo "  Action: kdevkit-sync push --scope $scope --files $basename"
                        found_suggestions=true
                        echo ""
                    fi
                else
                    echo "✓ Consider pushing: $basename"
                    echo "  Status: New file (no upstream equivalent)"
                    echo "  Action: kdevkit-sync push --scope $scope --files $basename"
                    found_suggestions=true
                    echo ""
                fi
            fi
        done
    fi
    
    if [ "$found_suggestions" = false ]; then
        echo "No files to suggest for push"
        echo "Either no local changes, or all changes are config files"
    fi
}

main() {
    local scope=""
    local command="status"
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            system|project)
                scope="$1"
                shift
                ;;
            status|divergence|memory|suggest-push)
                command="$1"
                shift
                ;;
            -h|--help)
                usage
                exit 0
                ;;
            *)
                echo "Error: Unknown argument '$1'" >&2
                usage
                exit 1
                ;;
        esac
    done
    
    if [ -z "$scope" ]; then
        echo "Error: Scope required (system or project)" >&2
        usage
        exit 1
    fi
    
    case $command in
        status)
            if [ "$scope" = "system" ]; then
                inspect_system_status
            else
                inspect_project_status
            fi
            ;;
        divergence)
            inspect_divergence "$scope"
            ;;
        suggest-push)
            suggest_push "$scope"
            ;;
        memory)
            echo "Memory inspection not yet implemented"
            exit 1
            ;;
    esac
}

main "$@"
```

**Tasks**:
- [ ] Create `tools/kdevkit-inspect` script
- [ ] Add execute permissions
- [ ] Test status command for system and project
- [ ] Test divergence detection
- [ ] Test suggest-push command
- [ ] Add tests in `tests/inspect-test.sh`

**Deliverable**: PR #3 with kdevkit-inspect tool

---

## Phase 3: Sync Mechanism (3-4 PRs)

**Goal**: Implement `kdevkit-sync` for pulling, pushing, and reconciling.

### 3.1 kdevkit-sync pull

**File**: `tools/kdevkit-sync` (partial implementation)

```bash
#!/usr/bin/env bash
# kdevkit-sync: Sync local kdevkit with upstream

set -euo pipefail

VERSION="0.1.0"
REPO="kusimari/kdevkit"
GITHUB_RAW="https://raw.githubusercontent.com/$REPO/main"

# ... (usage, fetch_file functions similar to kdevkit-init)

sync_pull() {
    local scope="$1"
    local kdevkit_dir
    local upstream_path
    
    if [ "$scope" = "system" ]; then
        kdevkit_dir="$HOME/.kdevkit"
        upstream_path="system"
    else
        kdevkit_dir=".kdevkit"
        upstream_path="project"
    fi
    
    if [ ! -d "$kdevkit_dir" ]; then
        echo "Error: kdevkit not initialized for $scope" >&2
        echo "Run: kdevkit-init $scope" >&2
        exit 1
    fi
    
    echo "Pulling upstream changes for $scope kdevkit..."
    echo ""
    
    # Backup current upstream
    local backup_dir="$kdevkit_dir/.backups/$(date +%Y%m%d-%H%M%S)"
    mkdir -p "$backup_dir"
    if [ -d "$kdevkit_dir/upstream" ]; then
        cp -r "$kdevkit_dir/upstream" "$backup_dir/"
        echo "Backed up current upstream to $backup_dir"
    fi
    
    # Fetch new upstream files
    echo "Fetching latest upstream files..."
    
    # Determine files to fetch based on scope
    local files=()
    if [ "$scope" = "system" ]; then
        files=(
            "system/README.md"
            "system/git-practices.md"
            "system/coding-standards.md"
            "system/agent-behavior.md"
        )
    else
        files=(
            "project/README.md"
            "project/feature-dev.md"
            "project/agent-dev-loop.md"
            "project/project-template.md"
        )
    fi
    
    for file in "${files[@]}"; do
        local basename="${file#*/}"
        echo "  - $basename"
        fetch_file "$GITHUB_RAW/$file" "$kdevkit_dir/upstream/$basename"
    done
    
    # Update .git-ref
    local commit_sha
    commit_sha=$(fetch_latest_commit)
    
    cat > "$kdevkit_dir/upstream/.git-ref" <<EOF
{
  "repo": "$REPO",
  "ref": "main",
  "path": "$upstream_path",
  "commit": "$commit_sha",
  "fetched_at": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF
    
    echo ""
    echo "✓ Upstream synced to commit $commit_sha"
    echo ""
    
    # Check for conflicts with local files
    echo "Checking for conflicts with local overrides..."
    local found_conflicts=false
    
    if [ -d "$kdevkit_dir/local" ]; then
        for local_file in "$kdevkit_dir/local"/*; do
            if [ -f "$local_file" ]; then
                local basename
                basename=$(basename "$local_file")
                local upstream_file="$kdevkit_dir/upstream/$basename"
                
                if [ -f "$upstream_file" ]; then
                    if ! diff -q "$local_file" "$upstream_file" > /dev/null 2>&1; then
                        echo "  ⚠ Conflict: $basename (local differs from new upstream)"
                        found_conflicts=true
                    fi
                fi
            fi
        done
    fi
    
    if [ "$found_conflicts" = true ]; then
        echo ""
        echo "Conflicts detected. Run 'kdevkit-sync reconcile' to resolve."
    else
        echo "  No conflicts"
    fi
}

# Additional functions: sync_push, sync_reconcile, sync_inspect...
# (To be implemented in follow-up PRs)

main() {
    local command=""
    local scope=""
    
    # Parse arguments...
    
    case $command in
        pull)
            sync_pull "$scope"
            ;;
        push|reconcile|inspect)
            echo "Command '$command' not yet implemented" >&2
            exit 1
            ;;
        *)
            echo "Error: Unknown command '$command'" >&2
            usage
            exit 1
            ;;
    esac
}

main "$@"
```

**Tasks**:
- [ ] Implement `sync_pull` function
- [ ] Add conflict detection
- [ ] Add backup mechanism
- [ ] Test pulling changes from upstream
- [ ] Add tests in `tests/sync-test.sh`

**Deliverable**: PR #4 with kdevkit-sync pull

### 3.2 kdevkit-sync push

This requires GitHub authentication and PR creation. Two approaches:

**Option A**: Use `gh` CLI (requires user to have gh authenticated)
**Option B**: Provide instructions for manual PR creation

For now, implement **Option B** (provide diff and instructions):

```bash
sync_push() {
    local scope="$1"
    shift
    local files=("$@")
    
    # ... validate files exist in local/ ...
    
    echo "Preparing files for upstream push..."
    echo ""
    
    local temp_dir
    temp_dir=$(mktemp -d)
    
    for file in "${files[@]}"; do
        local local_file="$kdevkit_dir/local/$file"
        local target_file
        
        if [ "$scope" = "system" ]; then
            target_file="system/$file"
        else
            target_file="project/$file"
        fi
        
        mkdir -p "$temp_dir/$(dirname "$target_file")"
        cp "$local_file" "$temp_dir/$target_file"
        
        echo "Prepared: $file → $target_file"
    done
    
    echo ""
    echo "Files prepared in: $temp_dir"
    echo ""
    echo "To push these changes:"
    echo "  1. Fork https://github.com/$REPO"
    echo "  2. Clone your fork"
    echo "  3. Copy files from $temp_dir to your fork"
    echo "  4. Create a branch: git checkout -b sync-$scope-$(date +%Y%m%d)"
    echo "  5. Commit and push"
    echo "  6. Create PR to $REPO"
    echo ""
    echo "Or, if you have 'gh' CLI:"
    echo "  cd $temp_dir"
    echo "  gh repo clone $REPO"
    echo "  cd kdevkit"
    echo "  # copy files, commit, and: gh pr create"
}
```

**Tasks**:
- [ ] Implement `sync_push` with manual instructions
- [ ] Add validation for files
- [ ] Test push workflow
- [ ] Document in README

**Deliverable**: PR #5 with kdevkit-sync push

### 3.3 kdevkit-sync reconcile

Interactive mode for resolving conflicts:

```bash
sync_reconcile() {
    # Show diffs
    # Ask: keep local, take upstream, or merge
    # Update files accordingly
}
```

**Tasks**:
- [ ] Implement interactive reconciliation
- [ ] Add diff visualization
- [ ] Add merge support (or fallback to manual)
- [ ] Test reconciliation workflow

**Deliverable**: PR #6 with kdevkit-sync reconcile

---

## Phase 4: Memory Integration (2-3 PRs)

**Goal**: Define memory structure and inheritance.

### 4.1 Memory Directory Structure

Create template memory files:

**File**: `system/memory-templates/README.md`
```markdown
# Memory Templates

These templates define the structure for agent memory files.

## System Memory (~/.kdevkit/memory/)

- `user.md`: User profile, role, preferences
- `feedback.md`: Agent behavior feedback
- `reference.md`: External system references

## Project Memory (<project>/.kdevkit/memory/)

- `project.md`: Project-specific context
- `feedback.md`: Project-specific feedback
- `architecture.md`: Architecture notes
```

**Tasks**:
- [ ] Create memory templates in `system/memory-templates/`
- [ ] Update kdevkit-init to create memory directories
- [ ] Document memory structure in README
- [ ] Add memory inheritance config

**Deliverable**: PR #7 with memory structure

### 4.2 Memory Pattern Detection

Add to kdevkit-inspect:

```bash
inspect_memory_patterns() {
    # Scan memory files for repeated patterns
    # Suggest converting to explicit rules
}
```

**Tasks**:
- [ ] Implement pattern detection (grep for repeated phrases)
- [ ] Suggest rule creation
- [ ] Add tests

**Deliverable**: PR #8 with memory pattern detection

---

## Phase 5: Agent Integration (2-3 PRs)

**Goal**: Make it easy for agents to load kdevkit context.

### 5.1 kdevkit-load Tool

**File**: `tools/kdevkit-load`

```bash
#!/usr/bin/env bash
# kdevkit-load: Load kdevkit context for coding agents

set -euo pipefail

load_system() {
    local kdevkit_dir="$HOME/.kdevkit"
    
    # Load upstream files
    # Apply local overrides
    # Load memory
    
    # Output merged content to stdout (for agent to consume)
}

load_project() {
    # Similar to load_system, but for project
}

main() {
    local scope="$1"
    
    case $scope in
        system)
            load_system
            ;;
        project)
            load_project
            ;;
        both)
            load_system
            echo "---"
            load_project
            ;;
    esac
}

main "$@"
```

**Tasks**:
- [ ] Implement kdevkit-load tool
- [ ] Handle file merging (upstream + local overrides)
- [ ] Format output for agent consumption
- [ ] Test with Claude Code

**Deliverable**: PR #9 with kdevkit-load tool

### 5.2 CLAUDE.md Integration

Create template CLAUDE.md that references kdevkit:

**File**: `project/templates/CLAUDE.md.template`

```markdown
# Claude Code Integration

## kdevkit Context

At session start:
```
! kdevkit-load both
```

This loads:
- System-level practices (git, coding standards, agent behavior)
- Project-level context (project.md, dev-loop instructions)
- Relevant memory (system and project)

## Project-Specific Instructions

[Add project-specific instructions here]
```

**Tasks**:
- [ ] Create CLAUDE.md template
- [ ] Update feature-dev.md to reference kdevkit-load
- [ ] Test integration with Claude Code CLI
- [ ] Document in README

**Deliverable**: PR #10 with CLAUDE.md template

---

## Phase 6: Nix Integration (1-2 PRs)

**Goal**: Package kdevkit tools for nix and integrate with build-nix.

### 6.1 Nix Packaging

**File**: Create `kdevkit.nix` in your build-nix repo (not in kdevkit)

```nix
{ pkgs, lib, ... }:

let
  kdevkit = pkgs.fetchFromGitHub {
    owner = "kusimari";
    repo = "kdevkit";
    rev = "main";
    sha256 = lib.fakeSha256;  # Replace with actual hash
  };
  
  kdevkit-tools = pkgs.stdenv.mkDerivation {
    name = "kdevkit-tools";
    src = kdevkit;
    
    installPhase = ''
      mkdir -p $out/bin
      cp tools/kdevkit-init $out/bin/
      cp tools/kdevkit-sync $out/bin/
      cp tools/kdevkit-inspect $out/bin/
      cp tools/kdevkit-load $out/bin/
      chmod +x $out/bin/*
    '';
  };
in
{
  home.packages = [ kdevkit-tools ];
  
  home.activation.kdevkit = lib.hm.dag.entryAfter ["writeBoundary"] ''
    if [ ! -d "$HOME/.kdevkit" ]; then
      $DRY_RUN_CMD ${kdevkit-tools}/bin/kdevkit-init system --auto
      echo "Initialized kdevkit system at ~/.kdevkit/"
    fi
  '';
}
```

**Tasks**:
- [ ] Create nix package for kdevkit tools
- [ ] Add activation script to auto-initialize system kdevkit
- [ ] Test on clean nix installation
- [ ] Document in build-nix README

**Deliverable**: PR to your build-nix repo with kdevkit integration

---

## Testing Strategy

### Unit Tests

For each tool:
```bash
tests/
├── init-test.sh          # Test kdevkit-init
├── inspect-test.sh       # Test kdevkit-inspect
├── sync-test.sh          # Test kdevkit-sync
└── load-test.sh          # Test kdevkit-load
```

### Integration Tests

```bash
tests/integration/
├── system-to-project.sh  # Test system → project inheritance
├── sync-workflow.sh      # Test full sync workflow
└── agent-integration.sh  # Test agent loading
```

### Manual Testing Checklist

- [ ] Initialize system kdevkit on fresh machine
- [ ] Initialize project kdevkit in new project
- [ ] Customize local files
- [ ] Detect divergence
- [ ] Pull upstream changes
- [ ] Resolve conflicts
- [ ] Push changes to upstream (manual PR)
- [ ] Load context in Claude Code
- [ ] Verify memory inheritance
- [ ] Test with multiple projects

---

## Rollout Plan

### Week 1-2: Phase 1 (Repository Restructuring)
- Restructure kusimari/kdevkit repo
- Extract system vs. project content
- Update build system
- Test and merge

### Week 3: Phase 2 (Basic Tooling)
- Implement kdevkit-init
- Implement kdevkit-inspect
- Test on your machines
- Document usage

### Week 4: Phase 3 (Sync Mechanism)
- Implement sync pull
- Implement sync push (manual PR)
- Test sync workflow
- Document reconciliation process

### Week 5: Phase 4 (Memory Integration)
- Define memory structure
- Implement memory templates
- Test memory inheritance
- Document memory usage

### Week 6: Phase 5 (Agent Integration)
- Implement kdevkit-load
- Create CLAUDE.md templates
- Test with Claude Code
- Document agent integration

### Week 7: Phase 6 (Nix Integration)
- Package tools for nix
- Integrate with build-nix
- Test on clean installation
- Document nix usage

### Week 8: Polish and Documentation
- Comprehensive README updates
- Usage examples and tutorials
- Video walkthrough (optional)
- Blog post (optional)

---

## Success Metrics

- [ ] Can initialize system kdevkit on any machine with one command
- [ ] Can initialize project kdevkit in any project with one command
- [ ] Local customizations persist and are easily inspected
- [ ] Can reconcile and push changes to upstream
- [ ] Agent automatically loads appropriate context layers
- [ ] Memory grows intelligently without manual maintenance
- [ ] Clear separation between local and upstream content
- [ ] Smooth experience across multiple projects
- [ ] Nix integration works on fresh installations

---

## Risk Mitigation

### Risk: Upstream API changes break tools
**Mitigation**: Pin to specific kdevkit version in config, provide version upgrade path

### Risk: Merge conflicts during sync
**Mitigation**: Robust backup mechanism, clear conflict resolution UX

### Risk: Memory grows too large
**Mitigation**: Configurable size limits, automatic cleanup of old entries

### Risk: Agent doesn't load context correctly
**Mitigation**: Extensive testing with Claude Code, clear error messages

### Risk: Nix activation fails
**Mitigation**: Idempotent init, safe failure modes, manual fallback documented

---

## Future Enhancements (Post-MVP)

- [ ] Web UI for divergence visualization
- [ ] Auto-sync option (periodic pull)
- [ ] Team profiles (org-level kdevkit)
- [ ] kdevkit marketplace (community templates)
- [ ] Analytics (which rules are most effective)
- [ ] Migration tools (from old structure to new)
- [ ] GitHub App for automated PR creation
- [ ] Support for other agents (not just Claude Code)

---

**End of Implementation Plan**
