# kdevkit Multi-Layer Architecture - Revised Specification

**Version**: 0.2.0  
**Date**: 2026-04-16  
**Status**: Draft - Revised based on feedback

---

## Changes from v0.1.0

1. **Feature files are persistent**, not ephemeral - they live in `<project>/.kdevkit/feature/` and can be committed
2. **Agent detection and auto-import** - tools detect which agent (Claude/Gemini/Kiro) and ensure proper imports
3. **Memory can be git-synced** - memory stored in separate repo, can be synced across machines
4. **Python implementation** - all tools in Python (not bash) following tmux script pattern
5. **Token-based GitHub auth** - support for SSH-only terminals using GH_TOKEN

---

## 1. Revised Layer Model

```
┌─────────────────────────────────────────────────────┐
│ Layer 5: Project Feature Files                      │
│ (<project>/.kdevkit/feature/*.md)                   │
│ - Persistent, can be committed to project git       │
│ - Describes specific features being worked on       │
│ - Synced with project repo, not kdevkit repo        │
└─────────────────────────────────────────────────────┘
                        ↓ inherits from
┌─────────────────────────────────────────────────────┐
│ Layer 4: Local Project kdevkit                      │
│ (<project>/.kdevkit/local/)                         │
│ - project.md: project-specific overrides            │
│ - dev-loop customizations                           │
│ - Can diverge from Layer 3                          │
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
│ (~/.kdevkit/local/)                                 │
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

### Key Change: Feature Files are Persistent

Feature files (`<project>/.kdevkit/feature/*.md`) should be:
- **Committed to the project repository** (not gitignored)
- **Shared with team members** working on the same project
- **Documented in project history** (part of feature development)
- **Optional to include** (can be gitignored if team prefers)

This is similar to how `<project>/.kdevkit/project.md` works - it's part of the project, not ephemeral.

---

## 2. Revised Directory Structure

### Project Structure
```
<project>/
├── .kdevkit/
│   ├── upstream/              # Layer 3: from kusimari/kdevkit
│   │   ├── .git-ref
│   │   ├── README.md
│   │   ├── feature-dev.md
│   │   ├── agent-dev-loop.md
│   │   └── project-template.md
│   ├── local/                 # Layer 4: local project divergence
│   │   └── project.md         # Like project.md - committed to project
│   ├── feature/               # Layer 5: feature files - COMMITTED
│   │   ├── auth-system.md
│   │   └── api-refactor.md
│   ├── memory/                # Project memory - GITIGNORED
│   │   ├── project.md
│   │   ├── feedback.md
│   │   └── architecture.md
│   ├── config.yaml            # kdevkit config - GITIGNORED
│   └── .divergence.log        # Divergence tracking - GITIGNORED
├── CLAUDE.md                  # Agent config - COMMITTED
├── GEMINI.md                  # (if using Gemini)
└── .kiro/                     # (if using Kiro)
    └── steering/
        └── workflow.md
```

### .gitignore Recommendations
```gitignore
# kdevkit - memory and logs are local
.kdevkit/memory/
.kdevkit/.divergence.log
.kdevkit/config.yaml

# kdevkit - everything else is committed:
# .kdevkit/upstream/    ← committed (or could be ignored and fetched)
# .kdevkit/local/       ← committed (project-specific)
# .kdevkit/feature/     ← committed (team-shared feature context)
```

---

## 3. Memory Sync Architecture

### 3.1 Memory as Separate Git Repository

Memory should be stored in a **separate git repository** that can be synced across machines.

```
~/.kdevkit-memory/              # Git repository
├── .git/
├── system/                     # System-level memory
│   ├── user.md
│   ├── feedback.md
│   └── reference.md
└── projects/                   # Per-project memory
    ├── project-a/
    │   ├── project.md
    │   ├── feedback.md
    │   └── architecture.md
    ├── project-b/
    │   └── ...
    └── .gitignore              # Ignore sensitive projects

# Symlinked from kdevkit directories:
~/.kdevkit/memory → ~/.kdevkit-memory/system/
<project>/.kdevkit/memory → ~/.kdevkit-memory/projects/<project-name>/
```

### 3.2 Memory Repository Setup

```bash
# Initialize memory repository
cd ~/.kdevkit-memory
git init
git remote add origin git@github.com:yourusername/kdevkit-memory-private.git

# Structure
mkdir -p system projects

# .gitignore for sensitive projects
echo "projects/work-secret-project/" >> .gitignore
echo "projects/client-*/" >> .gitignore

# Commit and push
git add .
git commit -m "init: kdevkit memory repository"
git push -u origin main
```

### 3.3 Memory Sync Commands

```bash
# Push memory to remote
kdevkit-memory sync push

# Pull memory from remote
kdevkit-memory sync pull

# Clone memory to new machine
kdevkit-memory init --repo git@github.com:user/kdevkit-memory-private.git
```

### 3.4 Memory Privacy

- Memory repo should be **private** (contains personal context)
- Per-project memory can be **selectively ignored** via .gitignore
- Memory sync is **opt-in** (disabled by default)
- Can use separate repos for work vs. personal memory

---

## 4. Agent Detection and Auto-Import

### 4.1 Supported Agents

| Agent | Config File | Import Directive |
|-------|-------------|------------------|
| Claude Code | `CLAUDE.md` | Load at session start |
| Gemini CLI | `GEMINI.md` | Load at session start |
| Kiro | `.kiro/steering/workflow.md` | Load at session start |
| OpenAI Codex | `AGENTS.md` | Load at session start |

### 4.2 Agent Detection Logic

When running `kdevkit-init project` or `kdevkit-inspect project`, detect which agent is used:

```python
def detect_agent_config(project_root: Path) -> List[str]:
    """Detect which agent config files exist."""
    agents = []
    
    if (project_root / "CLAUDE.md").exists():
        agents.append("claude")
    if (project_root / "GEMINI.md").exists():
        agents.append("gemini")
    if (project_root / ".kiro" / "steering" / "workflow.md").exists():
        agents.append("kiro")
    if (project_root / "AGENTS.md").exists():
        agents.append("openai")
    
    return agents
```

### 4.3 Auto-Import Injection

**For CLAUDE.md**:
```markdown
<!-- kdevkit:auto-import -->
# kdevkit Integration

At session start, load kdevkit context:
```
! kdevkit-load both
```

This loads:
- System-level: git practices, coding standards, agent behavior
- Project-level: project.md, feature files, dev-loop instructions
- Memory: system and project memory (if synced)
<!-- /kdevkit:auto-import -->

# Your Project-Specific Instructions
...
```

**For GEMINI.md** (similar format):
```markdown
<!-- kdevkit:auto-import -->
# kdevkit Integration
... same as Claude ...
<!-- /kdevkit:auto-import -->
```

**For Kiro (.kiro/steering/workflow.md)**:
```markdown
<!-- kdevkit:auto-import -->
## kdevkit Context Loading

Before any task, load:
- System kdevkit: `~/.kdevkit/`
- Project kdevkit: `.kdevkit/`
- Memory: both system and project

Command: `kdevkit-load both`
<!-- /kdevkit:auto-import -->
```

### 4.4 Idempotent Injection

When running `kdevkit-init project` or `kdevkit-sync`, check if import block exists:

```python
def ensure_kdevkit_import(config_file: Path, agent_type: str) -> bool:
    """Ensure kdevkit import block exists in agent config."""
    content = config_file.read_text()
    
    # Check if already present
    if "<!-- kdevkit:auto-import -->" in content:
        return False  # Already injected
    
    # Inject at top of file
    import_block = get_import_block_for_agent(agent_type)
    new_content = import_block + "\n\n" + content
    
    config_file.write_text(new_content)
    return True  # Injected
```

### 4.5 Keeping Imports in Sync

When kdevkit structure changes, need to update import blocks:

```bash
# Update import blocks in all agent configs
kdevkit-sync update-imports

# Check if imports are out of date
kdevkit-inspect check-imports
```

This command:
1. Detects all agent config files in project
2. Checks if import blocks are up-to-date
3. Updates them if needed (or warns if manual changes detected)

---

## 5. Python Implementation

### 5.1 Project Structure

```
kdevkit/
├── tools/
│   ├── kdevkit                # Main entry point (symlink to kdevkit.py)
│   ├── kdevkit.py             # Main CLI dispatcher
│   ├── kdevkit_init.py        # Init command
│   ├── kdevkit_sync.py        # Sync command
│   ├── kdevkit_inspect.py     # Inspect command
│   ├── kdevkit_load.py        # Load command
│   ├── kdevkit_memory.py      # Memory command
│   ├── kdevkit_common.py      # Shared utilities
│   └── requirements.txt       # Python dependencies (minimal)
```

### 5.2 Example: kdevkit.py (Main Entry Point)

```python
#!/usr/bin/env python3
"""kdevkit: Multi-layer coding agent environment manager."""

import sys
from pathlib import Path
from typing import List, Optional

# Version
__version__ = "0.2.0"


def main():
    """Main CLI entry point."""
    if len(sys.argv) < 2:
        print_usage()
        sys.exit(1)
    
    command = sys.argv[1]
    args = sys.argv[2:]
    
    # Dispatch to subcommands
    if command in ["init", "initialize"]:
        from kdevkit_init import main as init_main
        init_main(args)
    elif command in ["sync"]:
        from kdevkit_sync import main as sync_main
        sync_main(args)
    elif command in ["inspect", "status"]:
        from kdevkit_inspect import main as inspect_main
        inspect_main(args)
    elif command in ["load"]:
        from kdevkit_load import main as load_main
        load_main(args)
    elif command in ["memory"]:
        from kdevkit_memory import main as memory_main
        memory_main(args)
    elif command in ["version", "-v", "--version"]:
        print(f"kdevkit version {__version__}")
    elif command in ["help", "-h", "--help"]:
        print_usage()
    else:
        print(f"Error: Unknown command '{command}'", file=sys.stderr)
        print_usage()
        sys.exit(1)


def print_usage():
    """Print usage information."""
    usage = """
kdevkit - Multi-layer coding agent environment manager

Usage:
  kdevkit <command> [options]

Commands:
  init <scope>           Initialize kdevkit (system or project)
  sync <action>          Sync with upstream or push changes
  inspect <scope>        Inspect kdevkit state
  load <scope>           Load kdevkit context for agents
  memory <action>        Manage agent memory across machines
  version                Show version
  help                   Show this help

Examples:
  kdevkit init system              # Initialize system-level kdevkit
  kdevkit init project             # Initialize project-level kdevkit
  kdevkit sync pull system         # Pull upstream changes
  kdevkit inspect project          # Inspect project kdevkit
  kdevkit load both                # Load system + project context
  kdevkit memory sync push         # Push memory to remote git

For command-specific help:
  kdevkit <command> --help
"""
    print(usage.strip())


if __name__ == "__main__":
    main()
```

### 5.3 Example: kdevkit_init.py

```python
#!/usr/bin/env python3
"""kdevkit init command - initialize system or project kdevkit."""

import sys
import json
from pathlib import Path
from datetime import datetime, timezone
from typing import Optional, List
from dataclasses import dataclass
import urllib.request
import urllib.error


@dataclass
class InitConfig:
    """Configuration for initialization."""
    scope: str  # "system" or "project"
    version: str = "main"
    auto: bool = False
    repo: str = "kusimari/kdevkit"


def main(args: List[str]):
    """Main entry point for init command."""
    config = parse_args(args)
    
    if config.scope == "system":
        init_system(config)
    elif config.scope == "project":
        init_project(config)
    else:
        print(f"Error: Invalid scope '{config.scope}'", file=sys.stderr)
        print_usage()
        sys.exit(1)


def parse_args(args: List[str]) -> InitConfig:
    """Parse command-line arguments."""
    if not args or args[0] in ["-h", "--help"]:
        print_usage()
        sys.exit(0)
    
    scope = args[0]
    version = "main"
    auto = False
    
    i = 1
    while i < len(args):
        if args[i] == "--version" and i + 1 < len(args):
            version = args[i + 1]
            i += 2
        elif args[i] == "--auto":
            auto = True
            i += 1
        else:
            print(f"Error: Unknown argument '{args[i]}'", file=sys.stderr)
            sys.exit(1)
    
    return InitConfig(scope=scope, version=version, auto=auto)


def print_usage():
    """Print usage information."""
    usage = """
Usage: kdevkit init <scope> [options]

Scopes:
  system              Initialize system-level kdevkit (~/.kdevkit/)
  project             Initialize project-level kdevkit (./.kdevkit/)

Options:
  --version VERSION   Fetch specific version (default: main)
  --auto              Non-interactive mode
  -h, --help          Show this help

Examples:
  kdevkit init system
  kdevkit init project --auto
  kdevkit init system --version v1.2.3
"""
    print(usage.strip())


def init_system(config: InitConfig):
    """Initialize system-level kdevkit."""
    kdevkit_dir = Path.home() / ".kdevkit"
    
    if kdevkit_dir.exists():
        print(f"System kdevkit already exists at {kdevkit_dir}")
        if not config.auto:
            response = input("Reinitialize? [y/N] ").strip().lower()
            if response != "y":
                sys.exit(0)
    
    print(f"Initializing system kdevkit at {kdevkit_dir}...")
    
    # Create directory structure
    (kdevkit_dir / "upstream").mkdir(parents=True, exist_ok=True)
    (kdevkit_dir / "local").mkdir(exist_ok=True)
    # Memory directory will be symlinked by kdevkit-memory
    
    # Fetch upstream files
    print("Fetching system files from kusimari/kdevkit...")
    files = [
        "system/README.md",
        "system/git-practices.md",
        "system/coding-standards.md",
        "system/agent-behavior.md",
    ]
    
    github_raw_base = f"https://raw.githubusercontent.com/{config.repo}/{config.version}"
    
    for file in files:
        basename = Path(file).name
        print(f"  - {basename}")
        url = f"{github_raw_base}/{file}"
        dest = kdevkit_dir / "upstream" / basename
        
        try:
            fetch_file(url, dest)
        except Exception as e:
            print(f"Error fetching {file}: {e}", file=sys.stderr)
            sys.exit(1)
    
    # Record upstream reference
    commit_sha = fetch_latest_commit(config.repo, config.version)
    git_ref = {
        "repo": config.repo,
        "ref": config.version,
        "path": "system",
        "commit": commit_sha,
        "fetched_at": datetime.now(timezone.utc).isoformat()
    }
    
    git_ref_file = kdevkit_dir / "upstream" / ".git-ref"
    git_ref_file.write_text(json.dumps(git_ref, indent=2) + "\n")
    
    # Create default config
    config_yaml = """kdevkit:
  version: 0.2.0
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
    sync_enabled: false  # Enable with kdevkit-memory init
  
  overrides: []
"""
    (kdevkit_dir / "config.yaml").write_text(config_yaml)
    
    # Initialize divergence log
    divergence = {
        "tracking_since": datetime.now(timezone.utc).isoformat(),
        "upstream_ref": commit_sha,
        "local_changes": [],
        "memory_stats": {
            "total_files": 0,
            "total_size_kb": 0,
            "oldest_entry": None
        }
    }
    (kdevkit_dir / ".divergence.log").write_text(json.dumps(divergence, indent=2) + "\n")
    
    print()
    print(f"✓ System kdevkit initialized at {kdevkit_dir}")
    print()
    print("Next steps:")
    print(f"  1. Review files in {kdevkit_dir}/upstream/")
    print(f"  2. Customize by creating overrides in {kdevkit_dir}/local/")
    print("  3. Run 'kdevkit inspect system' to see status")
    print("  4. (Optional) Run 'kdevkit memory init' to enable memory sync")


def init_project(config: InitConfig):
    """Initialize project-level kdevkit."""
    kdevkit_dir = Path(".kdevkit")
    project_root = Path.cwd()
    
    # Check if in git repo
    if not (project_root / ".git").exists():
        print("Error: Not in a git repository root", file=sys.stderr)
        print("Run this command from your project's git root directory", file=sys.stderr)
        sys.exit(1)
    
    if kdevkit_dir.exists():
        print(f"Project kdevkit already exists at {kdevkit_dir}")
        if not config.auto:
            response = input("Reinitialize? [y/N] ").strip().lower()
            if response != "y":
                sys.exit(0)
    
    print(f"Initializing project kdevkit at {kdevkit_dir}...")
    
    # Create directory structure
    (kdevkit_dir / "upstream").mkdir(parents=True, exist_ok=True)
    (kdevkit_dir / "local").mkdir(exist_ok=True)
    (kdevkit_dir / "feature").mkdir(exist_ok=True)
    (kdevkit_dir / "memory").mkdir(exist_ok=True)
    
    # Fetch upstream files
    print("Fetching project files from kusimari/kdevkit...")
    files = [
        "project/README.md",
        "project/feature-dev.md",
        "project/agent-dev-loop.md",
        "project/project-template.md",
    ]
    
    github_raw_base = f"https://raw.githubusercontent.com/{config.repo}/{config.version}"
    
    for file in files:
        basename = Path(file).name
        print(f"  - {basename}")
        url = f"{github_raw_base}/{file}"
        dest = kdevkit_dir / "upstream" / basename
        
        try:
            fetch_file(url, dest)
        except Exception as e:
            print(f"Error fetching {file}: {e}", file=sys.stderr)
            sys.exit(1)
    
    # Record upstream reference
    commit_sha = fetch_latest_commit(config.repo, config.version)
    git_ref = {
        "repo": config.repo,
        "ref": config.version,
        "path": "project",
        "commit": commit_sha,
        "fetched_at": datetime.now(timezone.utc).isoformat()
    }
    
    git_ref_file = kdevkit_dir / "upstream" / ".git-ref"
    git_ref_file.write_text(json.dumps(git_ref, indent=2) + "\n")
    
    # Create project.md from template if it doesn't exist
    project_md = kdevkit_dir / "local" / "project.md"
    if not project_md.exists():
        template = kdevkit_dir / "upstream" / "project-template.md"
        if template.exists():
            project_md.write_text(template.read_text())
            print()
            print(f"Created {project_md} from template")
            print("Please edit this file to describe your project")
    
    # Create default config
    config_yaml = """kdevkit:
  version: 0.2.0
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
    sync_enabled: false
  
  feature:
    auto_persist: true
    template: default
  
  overrides: []
"""
    (kdevkit_dir / "config.yaml").write_text(config_yaml)
    
    # Initialize divergence log
    divergence = {
        "tracking_since": datetime.now(timezone.utc).isoformat(),
        "upstream_ref": commit_sha,
        "local_changes": [],
        "memory_stats": {
            "total_files": 0,
            "total_size_kb": 0,
            "oldest_entry": None
        }
    }
    (kdevkit_dir / ".divergence.log").write_text(json.dumps(divergence, indent=2) + "\n")
    
    # Update .gitignore
    gitignore = project_root / ".gitignore"
    update_gitignore(gitignore)
    
    # Detect and update agent configs
    agents = detect_agent_configs(project_root)
    if agents:
        print()
        print(f"Detected agent configs: {', '.join(agents)}")
        if not config.auto:
            response = input("Update agent configs to load kdevkit? [Y/n] ").strip().lower()
            if response != "n":
                update_agent_configs(project_root, agents)
        else:
            update_agent_configs(project_root, agents)
    
    print()
    print(f"✓ Project kdevkit initialized at {kdevkit_dir}")
    print()
    print("Next steps:")
    print(f"  1. Edit {project_md}")
    print("  2. Run 'kdevkit inspect project' to see status")
    print("  3. Start using kdevkit workflows in your coding agent")


def fetch_file(url: str, dest: Path):
    """Fetch file from URL and save to dest."""
    with urllib.request.urlopen(url) as response:
        content = response.read()
    
    dest.parent.mkdir(parents=True, exist_ok=True)
    dest.write_bytes(content)


def fetch_latest_commit(repo: str, ref: str = "main") -> str:
    """Fetch latest commit SHA for a ref."""
    api_url = f"https://api.github.com/repos/{repo}/commits/{ref}"
    
    try:
        with urllib.request.urlopen(api_url) as response:
            data = json.loads(response.read())
            return data["sha"]
    except Exception:
        return "unknown"


def update_gitignore(gitignore: Path):
    """Update .gitignore with kdevkit entries."""
    if gitignore.exists():
        content = gitignore.read_text()
        if ".kdevkit/memory" in content:
            return  # Already has kdevkit entries
        
        # Add kdevkit section
        additions = """
# kdevkit - memory and logs are local
.kdevkit/memory/
.kdevkit/.divergence.log
.kdevkit/config.yaml
"""
        gitignore.write_text(content + additions)
        print("Added kdevkit entries to .gitignore")


def detect_agent_configs(project_root: Path) -> List[str]:
    """Detect which agent config files exist."""
    agents = []
    
    if (project_root / "CLAUDE.md").exists():
        agents.append("claude")
    if (project_root / "GEMINI.md").exists():
        agents.append("gemini")
    if (project_root / ".kiro" / "steering" / "workflow.md").exists():
        agents.append("kiro")
    if (project_root / "AGENTS.md").exists():
        agents.append("openai")
    
    return agents


def update_agent_configs(project_root: Path, agents: List[str]):
    """Update agent config files with kdevkit import."""
    for agent in agents:
        if agent == "claude":
            config_file = project_root / "CLAUDE.md"
        elif agent == "gemini":
            config_file = project_root / "GEMINI.md"
        elif agent == "kiro":
            config_file = project_root / ".kiro" / "steering" / "workflow.md"
        elif agent == "openai":
            config_file = project_root / "AGENTS.md"
        else:
            continue
        
        if inject_kdevkit_import(config_file, agent):
            print(f"  ✓ Updated {config_file.name} with kdevkit import")


def inject_kdevkit_import(config_file: Path, agent_type: str) -> bool:
    """Inject kdevkit import block if not present."""
    content = config_file.read_text()
    
    # Check if already present
    if "<!-- kdevkit:auto-import -->" in content:
        return False  # Already injected
    
    # Import block
    import_block = """<!-- kdevkit:auto-import -->
# kdevkit Integration

At session start, load kdevkit context:
```
! kdevkit load both
```

This loads:
- System-level: git practices, coding standards, agent behavior
- Project-level: project.md, feature files, dev-loop instructions
- Memory: system and project memory (if synced)
<!-- /kdevkit:auto-import -->
"""
    
    # Inject at top
    new_content = import_block + "\n" + content
    config_file.write_text(new_content)
    return True


if __name__ == "__main__":
    main(sys.argv[1:])
```

### 5.4 Dependencies

**requirements.txt**:
```
# No external dependencies needed for basic functionality
# All using Python 3 standard library:
# - urllib for HTTP requests
# - json for JSON parsing
# - pathlib for file operations
# - dataclasses for data structures

# Optional for enhanced features:
# pyyaml  # For YAML config parsing (can use json as fallback)
# rich    # For colored output (can use basic print as fallback)
```

---

## 6. GitHub Authentication for SSH-Only Terminals

### 6.1 Token-Based Authentication

For SSH-only terminals (no browser access), use GitHub Personal Access Token:

```bash
# Create token at: https://github.com/settings/tokens
# Scopes needed: repo, workflow

# Set environment variable
export GH_TOKEN=ghp_xxxxxxxxxxxxxxxxxxxx

# Add to ~/.zshrc or ~/.bashrc
echo 'export GH_TOKEN=ghp_xxxxxxxxxxxxxxxxxxxx' >> ~/.zshrc

# Or use gh CLI with token
echo $GH_TOKEN | gh auth login --with-token
```

### 6.2 kdevkit GitHub Integration

```python
import os
import subprocess
from typing import Optional


def get_github_auth() -> Optional[str]:
    """Get GitHub authentication token."""
    # Try GH_TOKEN environment variable
    token = os.environ.get("GH_TOKEN")
    if token:
        return token
    
    # Try gh CLI
    try:
        result = subprocess.run(
            ["gh", "auth", "token"],
            capture_output=True,
            text=True,
            check=True
        )
        return result.stdout.strip()
    except (subprocess.CalledProcessError, FileNotFoundError):
        return None


def create_pr_via_api(repo: str, title: str, body: str, head: str, base: str = "main"):
    """Create PR using GitHub API (no browser needed)."""
    token = get_github_auth()
    if not token:
        print("Error: GitHub authentication required")
        print("Set GH_TOKEN environment variable or run: gh auth login --with-token")
        sys.exit(1)
    
    import urllib.request
    import json
    
    api_url = f"https://api.github.com/repos/{repo}/pulls"
    data = {
        "title": title,
        "body": body,
        "head": head,
        "base": base
    }
    
    request = urllib.request.Request(
        api_url,
        data=json.dumps(data).encode(),
        headers={
            "Authorization": f"Bearer {token}",
            "Accept": "application/vnd.github.v3+json",
            "Content-Type": "application/json"
        },
        method="POST"
    )
    
    with urllib.request.urlopen(request) as response:
        result = json.loads(response.read())
        return result["html_url"]


def fork_repo_via_api(repo: str) -> str:
    """Fork repository using GitHub API."""
    token = get_github_auth()
    if not token:
        raise Exception("GitHub authentication required")
    
    api_url = f"https://api.github.com/repos/{repo}/forks"
    
    request = urllib.request.Request(
        api_url,
        data=b"",
        headers={
            "Authorization": f"Bearer {token}",
            "Accept": "application/vnd.github.v3+json"
        },
        method="POST"
    )
    
    with urllib.request.urlopen(request) as response:
        result = json.loads(response.read())
        return result["full_name"]  # Returns: "yourusername/kdevkit"
```

### 6.3 Workflow for SSH-Only Terminals

```bash
# 1. Set up token once
export GH_TOKEN=ghp_xxxxxxxxxxxx
echo 'export GH_TOKEN=ghp_xxxxxxxxxxxx' >> ~/.zshrc

# 2. kdevkit can now create PRs without browser
kdevkit sync push --scope system --files git-practices.md

# Behind the scenes:
# - Forks repo via API (if not already forked)
# - Creates branch via API
# - Commits files via API
# - Creates PR via API
# - Returns PR URL
```

---

## 7. Nix Packaging (Python Version)

```nix
{ pkgs, lib, ... }:

let
  kdevkit = pkgs.python3Packages.buildPythonPackage {
    pname = "kdevkit";
    version = "0.2.0";
    
    src = pkgs.fetchFromGitHub {
      owner = "kusimari";
      repo = "kdevkit";
      rev = "main";
      sha256 = lib.fakeSha256;
    };
    
    # No external dependencies (uses stdlib only)
    propagatedBuildInputs = [];
    
    installPhase = ''
      mkdir -p $out/bin
      cp tools/*.py $out/bin/
      
      # Create main entry point
      cat > $out/bin/kdevkit <<EOF
#!/usr/bin/env python3
import sys
sys.path.insert(0, "$out/bin")
from kdevkit import main
main()
EOF
      chmod +x $out/bin/kdevkit
    '';
    
    meta = with lib; {
      description = "Multi-layer coding agent environment manager";
      homepage = "https://github.com/kusimari/kdevkit";
      license = licenses.agpl3;
      platforms = platforms.unix;
    };
  };
in
{
  home.packages = [ kdevkit pkgs.gh ];
  
  home.activation.kdevkit = lib.hm.dag.entryAfter ["writeBoundary"] ''
    if [ ! -d "$HOME/.kdevkit" ]; then
      $DRY_RUN_CMD ${kdevkit}/bin/kdevkit init system --auto
      echo "Initialized kdevkit system at ~/.kdevkit/"
    fi
  '';
}
```

---

## 8. Updated Implementation Phases

### Phase 1: Repository Restructuring + Python Tools Skeleton
- Create system/ and project/ in kdevkit repo
- Create tools/ directory with Python scripts
- Implement kdevkit.py (dispatcher)
- Implement kdevkit_init.py (basic version)
- Add nix packaging

### Phase 2: Init + Inspect + Agent Detection
- Complete kdevkit-init with agent detection
- Implement kdevkit-inspect
- Add agent config injection logic
- Test on Claude/Gemini/Kiro projects

### Phase 3: Sync Mechanism (with API-based PR creation)
- Implement kdevkit-sync pull
- Implement kdevkit-sync push (via GitHub API)
- Add token-based authentication
- Test full sync workflow

### Phase 4: Memory as Separate Repo
- Implement kdevkit-memory init
- Implement kdevkit-memory sync
- Add symlink management
- Test memory across machines

### Phase 5: Load + Integration
- Implement kdevkit-load
- Test with agents (Claude Code)
- Add import block updates
- Documentation

### Phase 6: Polish + Testing
- Comprehensive tests
- Error handling
- Documentation
- Blog post / tutorial

---

## Summary of Key Changes

1. ✅ **Feature files are persistent** - committed to project repo like project.md
2. ✅ **Agent detection** - auto-detect CLAUDE.md/GEMINI.md/Kiro and inject imports
3. ✅ **Memory sync** - separate git repo, can clone to new machines
4. ✅ **Python implementation** - following tmux script pattern
5. ✅ **Token-based GitHub** - works on SSH-only terminals via GH_TOKEN

---

**End of Revised Specification**
