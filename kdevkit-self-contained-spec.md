# kdevkit Self-Contained Architecture

**Version**: 0.4.0  
**Date**: 2026-04-16  
**Status**: Draft - Self-contained with agent adapters

---

## Core Concept

**kdevkit manages itself as a normal project, installable via curl on any system.**

No nix dependency. No system package manager. Just curl and run.

Similar to:
- Oh My Zsh: `curl -fsSL https://install.ohmyz.sh | sh`
- Rustup: `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`

---

## Installation Methods

### System-wide Installation

```bash
# One-liner install
curl -fsSL https://raw.githubusercontent.com/kusimari/kdevkit/main/install.sh | bash

# Or explicit:
curl -fsSL https://raw.githubusercontent.com/kusimari/kdevkit/main/install.sh -o install-kdevkit.sh
bash install-kdevkit.sh
```

**What it does:**
1. Clones kdevkit to `~/.kdevkit-install/` (the tool itself)
2. Creates `~/.kdevkit/` (your system config/templates)
3. Adds `~/.kdevkit-install/bin` to PATH (updates ~/.zshrc, ~/.bashrc)
4. Initializes system templates from `system/`
5. Detects and offers to install agent adapters

**Result:**
- `kdevkit` command available in shell
- System templates ready at `~/.kdevkit/`
- Can now install into projects

### Project Installation

```bash
# From within a project
cd ~/projects/my-app

# Install kdevkit for this project
curl -fsSL https://raw.githubusercontent.com/kusimari/kdevkit/main/install.sh | bash -s -- project

# Or if kdevkit already installed system-wide:
kdevkit init project
```

**What it does:**
1. Checks if system kdevkit is installed (warns if not)
2. Initializes `.kdevkit/` in current project
3. Detects project type and agents (CLAUDE.md, oh-my-openagent, etc.)
4. Installs/configures detected agents
5. Sets up project templates

---

## Repository Structure

```
kusimari/kdevkit/
├── install.sh                    # Curl installer (system and project)
├── bin/                          # CLI tools (added to PATH)
│   ├── kdevkit                   # Main entry point (symlink to lib/cli.py)
│   └── kdevkit-agent             # Agent management CLI
├── lib/                          # Python implementation
│   ├── cli.py                    # Main CLI logic
│   ├── init.py
│   ├── load.py
│   ├── sync.py
│   ├── inspect.py
│   ├── common.py
│   └── agents/                   # Agent adapter framework
│       ├── __init__.py
│       ├── base.py               # Base agent adapter class
│       ├── claude.py             # Claude Code adapter
│       ├── gemini.py             # Gemini CLI adapter
│       ├── openagent.py          # oh-my-openagent adapter
│       ├── aider.py              # Aider adapter
│       └── cline.py              # Cline adapter
├── system/                       # System-level templates
│   ├── git-practices.md
│   ├── coding-standards.md
│   ├── agent-behavior.md
│   └── README.md
├── project/                      # Project-level templates
│   ├── feature-dev.md
│   ├── agent-dev-loop.md
│   ├── project-template.md
│   └── README.md
├── agents/                       # Agent-specific integrations
│   ├── claude/
│   │   ├── CLAUDE.md.template
│   │   └── hooks/
│   ├── openagent/
│   │   ├── .openagent.template/
│   │   └── adapters/
│   └── README.md
├── tests/
│   ├── test_init.py
│   └── test_agents.py
├── README.md
└── LICENSE
```

---

## Installation Script (install.sh)

```bash
#!/bin/bash
# kdevkit installer - works on any Unix-like system

set -e

VERSION="0.4.0"
REPO="kusimari/kdevkit"
INSTALL_DIR="${KDEVKIT_INSTALL_DIR:-$HOME/.kdevkit-install}"
CONFIG_DIR="$HOME/.kdevkit"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

info() {
    echo -e "${GREEN}==>${NC} $1"
}

warn() {
    echo -e "${YELLOW}Warning:${NC} $1"
}

error() {
    echo -e "${RED}Error:${NC} $1"
    exit 1
}

# Detect OS and package manager
detect_system() {
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        OS="linux"
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        OS="macos"
    else
        OS="unknown"
    fi
    
    # Detect shell
    if [ -n "$ZSH_VERSION" ]; then
        SHELL_NAME="zsh"
        SHELL_RC="$HOME/.zshrc"
    elif [ -n "$BASH_VERSION" ]; then
        SHELL_NAME="bash"
        SHELL_RC="$HOME/.bashrc"
    else
        SHELL_NAME="sh"
        SHELL_RC="$HOME/.profile"
    fi
}

# Check prerequisites
check_prerequisites() {
    info "Checking prerequisites..."
    
    # Python 3
    if ! command -v python3 &> /dev/null; then
        error "python3 is required but not installed. Please install Python 3.7+."
    fi
    
    PYTHON_VERSION=$(python3 --version | cut -d' ' -f2 | cut -d'.' -f1,2)
    info "Found Python $PYTHON_VERSION"
    
    # Git
    if ! command -v git &> /dev/null; then
        error "git is required but not installed. Please install git."
    fi
    
    info "Found git $(git --version | cut -d' ' -f3)"
}

# Install system-wide
install_system() {
    info "Installing kdevkit system-wide..."
    
    # Clone or update kdevkit
    if [ -d "$INSTALL_DIR" ]; then
        info "Updating existing installation at $INSTALL_DIR"
        cd "$INSTALL_DIR"
        git pull origin main
    else
        info "Cloning kdevkit to $INSTALL_DIR"
        git clone "https://github.com/$REPO.git" "$INSTALL_DIR"
    fi
    
    # Make CLI executable
    chmod +x "$INSTALL_DIR/bin/kdevkit"
    chmod +x "$INSTALL_DIR/bin/kdevkit-agent"
    
    # Add to PATH if not already there
    if ! echo "$PATH" | grep -q "$INSTALL_DIR/bin"; then
        info "Adding kdevkit to PATH in $SHELL_RC"
        
        echo "" >> "$SHELL_RC"
        echo "# kdevkit" >> "$SHELL_RC"
        echo "export PATH=\"\$HOME/.kdevkit-install/bin:\$PATH\"" >> "$SHELL_RC"
        
        export PATH="$INSTALL_DIR/bin:$PATH"
    fi
    
    # Initialize system config
    if [ ! -d "$CONFIG_DIR" ]; then
        info "Initializing system config at $CONFIG_DIR"
        "$INSTALL_DIR/bin/kdevkit" init system --auto
    else
        info "System config already exists at $CONFIG_DIR"
    fi
    
    # Detect and offer to install agents
    info "Detecting available agents..."
    "$INSTALL_DIR/bin/kdevkit-agent" detect
    
    echo ""
    echo -e "${GREEN}✓ kdevkit installed successfully!${NC}"
    echo ""
    echo "Restart your shell or run:"
    echo "  source $SHELL_RC"
    echo ""
    echo "Then use:"
    echo "  kdevkit --help"
    echo ""
    echo "To add kdevkit to a project:"
    echo "  cd <project>"
    echo "  kdevkit init project"
}

# Install in project
install_project() {
    # Check if system kdevkit is installed
    if [ ! -d "$INSTALL_DIR" ]; then
        warn "System kdevkit not installed. Installing system-wide first..."
        install_system
    fi
    
    info "Initializing kdevkit in current project..."
    
    if [ ! -d .git ]; then
        error "Not in a git repository. Run from project root."
    fi
    
    # Run project init
    "$INSTALL_DIR/bin/kdevkit" init project
    
    echo ""
    echo -e "${GREEN}✓ kdevkit initialized in project!${NC}"
    echo ""
    echo "Files created:"
    echo "  .kdevkit/          - kdevkit structure"
    echo "  CLAUDE.md          - agent config (if using Claude)"
    echo ""
    echo "Next steps:"
    echo "  1. Edit .kdevkit/local/project.md"
    echo "  2. Start your coding agent (e.g., claude-code)"
}

# Main
main() {
    detect_system
    check_prerequisites
    
    MODE="${1:-system}"
    
    case "$MODE" in
        system)
            install_system
            ;;
        project)
            install_project
            ;;
        *)
            error "Unknown mode: $MODE. Use 'system' or 'project'."
            ;;
    esac
}

main "$@"
```

---

## Agent Adapter Framework

### Base Adapter Class

```python
# lib/agents/base.py

from pathlib import Path
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Optional, List, Dict


@dataclass
class AgentConfig:
    """Configuration for an agent."""
    name: str
    display_name: str
    config_file: str
    install_url: Optional[str] = None
    requires_system: List[str] = None  # System dependencies


class AgentAdapter(ABC):
    """Base class for agent adapters."""
    
    def __init__(self, project_root: Path):
        self.project_root = project_root
    
    @property
    @abstractmethod
    def config(self) -> AgentConfig:
        """Return agent configuration."""
        pass
    
    @abstractmethod
    def detect(self) -> bool:
        """Detect if this agent is used in the project."""
        pass
    
    @abstractmethod
    def is_installed(self) -> bool:
        """Check if agent is installed on the system."""
        pass
    
    @abstractmethod
    def install(self) -> bool:
        """Install the agent (if possible)."""
        pass
    
    @abstractmethod
    def configure_kdevkit(self, system_path: Path, project_path: Path) -> bool:
        """Configure agent to use kdevkit context."""
        pass
    
    @abstractmethod
    def load_context(self, scope: str) -> str:
        """Load kdevkit context in agent-specific format."""
        pass
    
    def inject_import_block(self, config_file: Path, block: str) -> bool:
        """Inject import block into agent config if not present."""
        if not config_file.exists():
            return False
        
        content = config_file.read_text()
        
        # Check if already present
        if "<!-- kdevkit:auto-import -->" in content:
            return False
        
        # Inject at top
        new_content = block + "\n\n" + content
        config_file.write_text(new_content)
        return True
```

### Claude Code Adapter

```python
# lib/agents/claude.py

from pathlib import Path
from .base import AgentAdapter, AgentConfig


class ClaudeAdapter(AgentAdapter):
    """Adapter for Claude Code."""
    
    @property
    def config(self) -> AgentConfig:
        return AgentConfig(
            name="claude",
            display_name="Claude Code",
            config_file="CLAUDE.md",
            install_url="https://claude.ai/code",
            requires_system=["python3"]
        )
    
    def detect(self) -> bool:
        """Detect if Claude Code is used."""
        return (self.project_root / "CLAUDE.md").exists()
    
    def is_installed(self) -> bool:
        """Check if Claude Code CLI is available."""
        import shutil
        return shutil.which("claude-code") is not None
    
    def install(self) -> bool:
        """Claude Code requires manual installation."""
        print(f"Please install Claude Code from: {self.config.install_url}")
        return False
    
    def configure_kdevkit(self, system_path: Path, project_path: Path) -> bool:
        """Configure Claude to use kdevkit."""
        claude_md = self.project_root / "CLAUDE.md"
        
        # Create if doesn't exist
        if not claude_md.exists():
            template = (
                Path(__file__).parent.parent.parent / 
                "agents" / "claude" / "CLAUDE.md.template"
            )
            if template.exists():
                claude_md.write_text(template.read_text())
        
        # Inject import block
        import_block = """<!-- kdevkit:auto-import -->
# kdevkit Context Loading

At session start, load kdevkit context:
```
! kdevkit load both
```

This loads system and project kdevkit context.
<!-- /kdevkit:auto-import -->"""
        
        return self.inject_import_block(claude_md, import_block)
    
    def load_context(self, scope: str) -> str:
        """Load context in markdown format."""
        # Claude Code expects markdown
        # This is handled by the main kdevkit load command
        return ""
```

### oh-my-openagent Adapter

```python
# lib/agents/openagent.py

import json
import shutil
from pathlib import Path
from .base import AgentAdapter, AgentConfig


class OpenAgentAdapter(AgentAdapter):
    """Adapter for oh-my-openagent.
    
    References: https://github.com/code-yeongyu/oh-my-openagent
    """
    
    @property
    def config(self) -> AgentConfig:
        return AgentConfig(
            name="openagent",
            display_name="oh-my-openagent",
            config_file=".openagent/config.json",
            install_url="https://github.com/code-yeongyu/oh-my-openagent",
            requires_system=["python3", "pip"]
        )
    
    def detect(self) -> bool:
        """Detect if oh-my-openagent is used."""
        openagent_dir = self.project_root / ".openagent"
        return openagent_dir.exists() or self._has_openagent_import()
    
    def _has_openagent_import(self) -> bool:
        """Check if project imports openagent."""
        # Look for requirements.txt or pyproject.toml with openagent
        req_file = self.project_root / "requirements.txt"
        if req_file.exists():
            content = req_file.read_text()
            if "openagent" in content or "oh-my-openagent" in content:
                return True
        
        pyproject = self.project_root / "pyproject.toml"
        if pyproject.exists():
            content = pyproject.read_text()
            if "openagent" in content or "oh-my-openagent" in content:
                return True
        
        return False
    
    def is_installed(self) -> bool:
        """Check if oh-my-openagent is installed."""
        try:
            import openagent
            return True
        except ImportError:
            return False
    
    def install(self) -> bool:
        """Install oh-my-openagent via pip."""
        print("Installing oh-my-openagent...")
        import subprocess
        try:
            subprocess.run(
                ["pip", "install", "oh-my-openagent"],
                check=True,
                capture_output=True
            )
            print("✓ oh-my-openagent installed")
            return True
        except subprocess.CalledProcessError as e:
            print(f"Failed to install: {e.stderr.decode()}")
            return False
    
    def configure_kdevkit(self, system_path: Path, project_path: Path) -> bool:
        """Configure oh-my-openagent to use kdevkit."""
        openagent_dir = self.project_root / ".openagent"
        openagent_dir.mkdir(exist_ok=True)
        
        # Create config.json with kdevkit hooks
        config_file = openagent_dir / "config.json"
        
        if config_file.exists():
            config = json.loads(config_file.read_text())
        else:
            config = {}
        
        # Add kdevkit hooks to openagent config
        config["hooks"] = config.get("hooks", {})
        config["hooks"]["pre_session"] = [
            {
                "type": "shell",
                "command": "kdevkit load both",
                "description": "Load kdevkit system and project context"
            }
        ]
        
        # Add kdevkit context paths
        config["context_paths"] = config.get("context_paths", [])
        context_paths = [
            str(system_path / "upstream"),
            str(system_path / "local"),
            str(project_path / "upstream"),
            str(project_path / "local"),
        ]
        
        for path in context_paths:
            if path not in config["context_paths"]:
                config["context_paths"].append(path)
        
        # Write config
        config_file.write_text(json.dumps(config, indent=2))
        
        print(f"✓ Configured {config_file}")
        return True
    
    def load_context(self, scope: str) -> str:
        """Load context for openagent.
        
        oh-my-openagent uses a different format, so we adapt it.
        """
        # This would translate kdevkit markdown to openagent's expected format
        # For now, return markdown (openagent can handle it)
        return ""
```

### Aider Adapter

```python
# lib/agents/aider.py

from pathlib import Path
from .base import AgentAdapter, AgentConfig


class AiderAdapter(AgentAdapter):
    """Adapter for Aider (https://github.com/paul-gauthier/aider)."""
    
    @property
    def config(self) -> AgentConfig:
        return AgentConfig(
            name="aider",
            display_name="Aider",
            config_file=".aider.conf.yml",
            install_url="https://github.com/paul-gauthier/aider",
            requires_system=["python3", "pip"]
        )
    
    def detect(self) -> bool:
        """Detect if Aider is used."""
        return (
            (self.project_root / ".aider.conf.yml").exists() or
            (self.project_root / ".aider").exists()
        )
    
    def is_installed(self) -> bool:
        """Check if aider is installed."""
        import shutil
        return shutil.which("aider") is not None
    
    def install(self) -> bool:
        """Install aider."""
        print("Installing aider...")
        import subprocess
        try:
            subprocess.run(
                ["pip", "install", "aider-chat"],
                check=True,
                capture_output=True
            )
            print("✓ aider installed")
            return True
        except subprocess.CalledProcessError:
            return False
    
    def configure_kdevkit(self, system_path: Path, project_path: Path) -> bool:
        """Configure aider to use kdevkit."""
        conf_file = self.project_root / ".aider.conf.yml"
        
        # Create or update .aider.conf.yml
        if conf_file.exists():
            content = conf_file.read_text()
        else:
            content = ""
        
        # Add read-only files (kdevkit contexts)
        kdevkit_section = """
# kdevkit integration
read:
  - .kdevkit/local/*.md
  - .kdevkit/feature/*.md

# Load system context
env-file: .kdevkit/.env
"""
        
        if "# kdevkit integration" not in content:
            content += kdevkit_section
            conf_file.write_text(content)
            print(f"✓ Configured {conf_file}")
            return True
        
        return False
    
    def load_context(self, scope: str) -> str:
        """Aider loads files directly, no special format needed."""
        return ""
```

---

## Agent Management CLI

```python
# bin/kdevkit-agent (symlink to lib/cli_agent.py)

#!/usr/bin/env python3
"""kdevkit-agent: Manage coding agents."""

import sys
from pathlib import Path

# Add lib to path
sys.path.insert(0, str(Path(__file__).parent.parent / "lib"))

from agents.claude import ClaudeAdapter
from agents.gemini import GeminiAdapter
from agents.openagent import OpenAgentAdapter
from agents.aider import AiderAdapter


ADAPTERS = [
    ClaudeAdapter,
    GeminiAdapter,
    OpenAgentAdapter,
    AiderAdapter,
]


def detect_agents(project_root: Path):
    """Detect all agents used in project."""
    detected = []
    
    for adapter_class in ADAPTERS:
        adapter = adapter_class(project_root)
        if adapter.detect():
            detected.append(adapter)
    
    return detected


def list_agents():
    """List all supported agents."""
    print("Supported agents:")
    for adapter_class in ADAPTERS:
        adapter = adapter_class(Path.cwd())
        config = adapter.config
        print(f"  - {config.display_name} ({config.name})")
        print(f"    Config: {config.config_file}")
        if config.install_url:
            print(f"    Install: {config.install_url}")
        print()


def detect_and_configure():
    """Detect agents and offer to configure."""
    project_root = Path.cwd()
    
    print("Detecting agents in current project...")
    detected = detect_agents(project_root)
    
    if not detected:
        print("No agents detected.")
        print("\nSupported agents:")
        list_agents()
        return
    
    print(f"Found {len(detected)} agent(s):")
    for adapter in detected:
        print(f"  ✓ {adapter.config.display_name}")
    
    print()
    
    # Check installation and configure
    system_path = Path.home() / ".kdevkit"
    project_path = project_root / ".kdevkit"
    
    for adapter in detected:
        print(f"Configuring {adapter.config.display_name}...")
        
        # Check if installed
        if not adapter.is_installed():
            print(f"  ⚠ {adapter.config.display_name} not installed")
            response = input(f"  Install now? [y/N] ").strip().lower()
            if response == "y":
                adapter.install()
        
        # Configure kdevkit
        if adapter.configure_kdevkit(system_path, project_path):
            print(f"  ✓ Configured")
        else:
            print(f"  → Already configured")
    
    print()
    print("✓ Agent configuration complete")


def main():
    """Main entry point."""
    if len(sys.argv) < 2:
        print("Usage: kdevkit-agent <command>")
        print("\nCommands:")
        print("  detect      Detect and configure agents in current project")
        print("  list        List all supported agents")
        sys.exit(1)
    
    command = sys.argv[1]
    
    if command == "detect":
        detect_and_configure()
    elif command == "list":
        list_agents()
    else:
        print(f"Unknown command: {command}")
        sys.exit(1)


if __name__ == "__main__":
    main()
```

---

## Usage Examples

### Installing kdevkit

```bash
# Install system-wide (one-time per machine)
curl -fsSL https://raw.githubusercontent.com/kusimari/kdevkit/main/install.sh | bash

# Restart shell
source ~/.zshrc

# Verify
kdevkit --version
# Output: kdevkit 0.4.0
```

### Adding kdevkit to a Project

```bash
# Go to your project
cd ~/projects/my-app

# Initialize kdevkit
kdevkit init project

# Output:
# Initializing project kdevkit at .kdevkit/...
# Fetching project files from kusimari/kdevkit...
#   - feature-dev.md
#   - agent-dev-loop.md
#   - project-template.md
# 
# Created .kdevkit/local/project.md from template
# Please edit this file to describe your project
# 
# Detecting agents in current project...
# Found 2 agent(s):
#   ✓ Claude Code
#   ✓ oh-my-openagent
# 
# Configuring Claude Code...
#   ✓ Updated CLAUDE.md with kdevkit import
# 
# Configuring oh-my-openagent...
#   ⚠ oh-my-openagent not installed
#   Install now? [y/N] y
#   Installing oh-my-openagent...
#   ✓ oh-my-openagent installed
#   ✓ Configured .openagent/config.json
# 
# ✓ Project kdevkit initialized!
```

### Working with oh-my-openagent Project

```bash
# Initialize project with oh-my-openagent
cd my-openagent-project

# oh-my-openagent is already configured in project
kdevkit init project

# kdevkit detects .openagent/ and configures it
# Now when you run oh-my-openagent:
openagent run

# It automatically:
# 1. Runs kdevkit load both (via pre_session hook)
# 2. Loads system git practices, coding standards
# 3. Loads project context from .kdevkit/
# 4. Has full kdevkit context available
```

### Managing Agents

```bash
# List all supported agents
kdevkit-agent list

# Output:
# Supported agents:
#   - Claude Code (claude)
#     Config: CLAUDE.md
#     Install: https://claude.ai/code
# 
#   - oh-my-openagent (openagent)
#     Config: .openagent/config.json
#     Install: https://github.com/code-yeongyu/oh-my-openagent
# 
#   - Aider (aider)
#     Config: .aider.conf.yml
#     Install: https://github.com/paul-gauthier/aider

# Detect and configure agents in current project
kdevkit-agent detect
```

---

## Benefits of Self-Contained Design

### 1. **No System Package Manager Dependency**
- Works on any system with Python 3 and Git
- No need for nix, apt, brew, etc.
- Users without admin access can install to ~/ only

### 2. **kdevkit Manages Itself**
- Update kdevkit: `cd ~/.kdevkit-install && git pull`
- Or: `kdevkit update` (runs git pull internally)
- Self-contained in one directory

### 3. **Easy Distribution**
- One curl command to install
- No pre-built binaries needed
- Works on Linux, macOS, WSL

### 4. **Agent Agnostic**
- Adapters for any agent (Claude, Gemini, oh-my-openagent, Aider)
- Easy to add new adapters
- Each adapter knows how to configure its agent

### 5. **Version Control Friendly**
- `.kdevkit-install/` is separate from project
- Projects only have `.kdevkit/` (local config)
- Can pin kdevkit version per project if needed

---

## How oh-my-openagent Integration Works

### Scenario: Using kdevkit with oh-my-openagent

```bash
# 1. Install kdevkit system-wide
curl -fsSL https://raw.githubusercontent.com/kusimari/kdevkit/main/install.sh | bash

# 2. Create project using oh-my-openagent
mkdir my-agent-project
cd my-agent-project
git init

# 3. Install oh-my-openagent (or kdevkit can do it)
pip install oh-my-openagent

# 4. Initialize oh-my-openagent project
openagent init
# Creates .openagent/ directory

# 5. Initialize kdevkit
kdevkit init project
# Detects .openagent/ and configures integration

# 6. Edit project description
vim .kdevkit/local/project.md

# 7. Run oh-my-openagent with kdevkit context
openagent run "Build an API server"

# Behind the scenes:
# - openagent runs pre_session hook: kdevkit load both
# - kdevkit outputs merged context (system + project)
# - openagent loads this context
# - openagent now knows:
#   - Your git practices
#   - Your coding standards
#   - This project's context
#   - Past memory and learnings
```

### What Gets Configured

**Before kdevkit**:
```json
// .openagent/config.json
{
  "model": "claude-3-opus",
  "temperature": 0.7
}
```

**After `kdevkit init project`**:
```json
// .openagent/config.json
{
  "model": "claude-3-opus",
  "temperature": 0.7,
  "hooks": {
    "pre_session": [
      {
        "type": "shell",
        "command": "kdevkit load both",
        "description": "Load kdevkit system and project context"
      }
    ]
  },
  "context_paths": [
    "/home/user/.kdevkit/upstream",
    "/home/user/.kdevkit/local",
    "/home/user/my-project/.kdevkit/upstream",
    "/home/user/my-project/.kdevkit/local"
  ]
}
```

---

## Updating kdevkit

```bash
# Update kdevkit itself
kdevkit update

# Or manually:
cd ~/.kdevkit-install
git pull origin main

# Update system templates
kdevkit sync pull system

# Update project templates
cd ~/projects/my-app
kdevkit sync pull project
```

---

## Nix Integration (Optional)

For users who DO use nix, kdevkit can still integrate:

```nix
# In your build-nix or home-manager config
{ pkgs, ... }:
{
  home.activation.kdevkit = lib.hm.dag.entryAfter ["writeBoundary"] ''
    # Install kdevkit via curl if not present
    if [ ! -d "$HOME/.kdevkit-install" ]; then
      $DRY_RUN_CMD curl -fsSL https://raw.githubusercontent.com/kusimari/kdevkit/main/install.sh | bash
    fi
  '';
  
  # Or package it properly if you prefer
  home.packages = with pkgs; [
    # ... other packages
  ];
  
  programs.zsh.shellAliases = {
    kdk = "kdevkit";
  };
}
```

But kdevkit **doesn't require** nix. It's self-contained.

---

## Summary

**Self-contained kdevkit**:
- ✅ Installs via curl (like rustup, oh-my-zsh)
- ✅ No system package manager needed
- ✅ Manages itself as a git repo
- ✅ Agent adapters for Claude, Gemini, oh-my-openagent, Aider, etc.
- ✅ Works on any Unix-like system
- ✅ Can optionally integrate with nix

**Installation**:
```bash
# System-wide
curl -fsSL https://raw.githubusercontent.com/kusimari/kdevkit/main/install.sh | bash

# Per-project
cd <project>
kdevkit init project
```

**Agent integration**:
- Detects agents automatically
- Configures them to load kdevkit context
- Supports oh-my-openagent, Claude Code, Aider, etc.

---

**End of Self-Contained Specification**
