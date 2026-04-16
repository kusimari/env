# kdevkit Multi-Layer Architecture - Design Session

**Date**: 2026-04-16  
**Session**: Planning and design for multi-layer kdevkit architecture  
**Goal**: Create a self-contained, curl-installable coding agent environment system

---

## Table of Contents

1. [Overview](#overview)
2. [Evolution of Design](#evolution-of-design)
3. [Final Design: Self-Contained Architecture](#final-design-self-contained-architecture)
4. [Key Questions and Answers](#key-questions-and-answers)
5. [Implementation Roadmap](#implementation-roadmap)
6. [Next Steps](#next-steps)

---

## Overview

### Problem Statement

Currently, kdevkit exists as a git repository with reusable instruction sets that users curl into their coding agents. However, it lacks:

1. **System-wide consistency** - No way to have git conventions and coding practices consistent across all projects
2. **Project-specific customization** - Can't inherit from system defaults while allowing project overrides
3. **Local divergence management** - No way to customize locally and reconcile with upstream
4. **Memory persistence** - Agent memory is per-conversation, not persistent across sessions
5. **Agent integration** - No automatic detection and configuration of different agents

### Desired Architecture

A multi-layer system where:
- **System-level kdevkit** provides global defaults (git practices, coding standards)
- **Project-level kdevkit** inherits and overrides for specific projects
- **Local customizations** can diverge and be reconciled with upstream
- **Memory** persists across sessions and machines
- **Agents** (Claude, Gemini, oh-my-openagent, etc.) automatically integrate

---

## Evolution of Design

### v0.1.0: Initial Multi-Layer Spec

**Layers:**
1. Upstream System kdevkit (git)
2. Local System kdevkit (~/.kdevkit/)
3. Upstream Project kdevkit (git)
4. Local Project kdevkit (<project>/.kdevkit/)
5. Project Feature Files (ephemeral)

**Issues identified:**
- Feature files should be persistent (like project.md)
- Memory structure was overcomplicated (separate git repo with symlinks)
- Config.yaml had too many unnecessary fields
- Agent integration was manual

### v0.2.0: Revised with Clarifications

**Changes:**
- Feature files are **persistent and committed** (not ephemeral)
- Agent detection and auto-injection of import blocks
- Memory can be synced via separate git repo
- Python implementation (not bash)
- Token-based GitHub auth for SSH-only terminals

**Key questions answered:**
1. **What goes in upstream/?** - Verbatim copy of GitHub kdevkit for offline use
2. **How to teach agents?** - Inject import block at init, agents run `kdevkit load both`
3. **Why separate memory repo?** - Actually, don't need to - just make it a directory
4. **What's in config.yaml?** - Most things auto-detected, config optional

### v0.3.0: Simplified

**Changes:**
- Removed config.yaml (not needed - auto-detect everything)
- Memory is just directories (use git directly if you want to sync)
- Clearer file purposes and what gets committed vs gitignored
- More explicit about inheritance and overrides

### v0.4.0: Self-Contained Design (Final)

**Major pivot:**
- Make kdevkit **self-contained** (not vended through nix)
- **Curl-installable** like rustup, oh-my-zsh
- kdevkit **manages itself** as a normal project
- **Agent adapter framework** for detecting and configuring any agent

**Key insight:** kdevkit should work standalone on any system, not require nix or other package managers.

---

## Final Design: Self-Contained Architecture

### Core Concept

```bash
# One-liner installation
curl -fsSL https://raw.githubusercontent.com/kusimari/kdevkit/main/install.sh | bash

# Installs to ~/.kdevkit-install/ (the tool)
# Initializes ~/.kdevkit/ (your system config)
# Adds to PATH
# Ready to use
```

### Directory Structure

#### System-wide Installation

```
~/.kdevkit-install/          # The kdevkit tool itself (git repo)
├── bin/
│   ├── kdevkit             # Main CLI
│   └── kdevkit-agent       # Agent management
├── lib/
│   ├── cli.py
│   ├── init.py
│   ├── load.py
│   ├── sync.py
│   └── agents/             # Agent adapters
│       ├── base.py
│       ├── claude.py
│       ├── gemini.py
│       ├── openagent.py
│       └── aider.py
├── system/                 # System templates
│   ├── git-practices.md
│   ├── coding-standards.md
│   └── agent-behavior.md
├── project/                # Project templates
│   ├── feature-dev.md
│   ├── agent-dev-loop.md
│   └── project-template.md
└── install.sh              # Curl installer

~/.kdevkit/                 # Your system config
├── upstream/               # Cached from GitHub
│   ├── .git-ref           # Tracks upstream commit
│   ├── git-practices.md
│   ├── coding-standards.md
│   └── agent-behavior.md
├── local/                  # Your customizations
│   └── git-practices.md   # (optional override)
└── memory/                 # Your agent memory
    ├── .git/              # (optional - make it a git repo)
    ├── user.md
    └── feedback.md
```

#### Project Installation

```
<project>/
├── .kdevkit/
│   ├── upstream/           # Cached from GitHub
│   │   ├── .git-ref
│   │   ├── feature-dev.md
│   │   └── agent-dev-loop.md
│   ├── local/             # Project customizations (COMMITTED)
│   │   └── project.md
│   ├── feature/           # Feature context (COMMITTED)
│   │   ├── auth-system.md
│   │   └── api-refactor.md
│   └── memory/            # Project memory (your choice: commit or gitignore)
│       ├── project.md
│       └── architecture.md
├── CLAUDE.md              # Auto-configured with kdevkit import
└── .openagent/            # If using oh-my-openagent
    └── config.json        # Auto-configured with kdevkit hooks
```

### Agent Adapter Framework

**Base adapter class:**
```python
class AgentAdapter(ABC):
    @abstractmethod
    def detect(self) -> bool:
        """Detect if this agent is used."""
    
    @abstractmethod
    def configure_kdevkit(self, system_path, project_path) -> bool:
        """Configure agent to use kdevkit context."""
```

**Built-in adapters:**
- **ClaudeAdapter** - Detects CLAUDE.md, injects import block
- **OpenAgentAdapter** - Detects .openagent/, configures hooks and context_paths
- **AiderAdapter** - Detects .aider.conf.yml, adds kdevkit files to read list
- **GeminiAdapter** - Detects GEMINI.md, injects import block

**How it works:**
```bash
# When you run:
kdevkit init project

# It:
# 1. Detects agents (checks for CLAUDE.md, .openagent/, etc.)
# 2. Configures each detected agent to load kdevkit context
# 3. For oh-my-openagent: adds pre_session hook to run "kdevkit load both"
# 4. For Claude: injects import block into CLAUDE.md
```

### How Agent Loading Works

**1. Init time (once):**
```bash
cd <project>
kdevkit init project
```

Updates CLAUDE.md:
```markdown
<!-- kdevkit:auto-import -->
# kdevkit Context Loading

At session start, run:
```
! kdevkit load both
```
<!-- /kdevkit:auto-import -->

# Your Project Instructions
...
```

**2. Session start (every time):**
```bash
claude-code
```

Claude:
1. Reads CLAUDE.md
2. Sees `! kdevkit load both`
3. Executes via Bash tool
4. Receives merged markdown with all kdevkit context
5. Loads into context, ready to work

**3. What `kdevkit load both` outputs:**
```markdown
# kdevkit System Context
## Git Practices
[content from ~/.kdevkit/local/git-practices.md OR upstream/]
## Coding Standards
[content from ~/.kdevkit/upstream/coding-standards.md]
...

# kdevkit Project Context
## Project Description
[content from .kdevkit/local/project.md]
## Feature Development Workflow
[content from .kdevkit/upstream/feature-dev.md]
...

# Memory
[content from memory files]
```

### Commands

```bash
# System installation
curl -fsSL https://raw.githubusercontent.com/kusimari/kdevkit/main/install.sh | bash

# Project initialization
kdevkit init project

# Load context (used by agents)
kdevkit load both

# Sync with upstream
kdevkit sync pull system
kdevkit sync pull project

# Push changes back to upstream (creates PR)
kdevkit sync push system git-practices.md

# Inspect status
kdevkit inspect system
kdevkit inspect project

# Manage agents
kdevkit-agent detect
kdevkit-agent list

# Update kdevkit itself
kdevkit update
```

---

## Key Questions and Answers

### Q1: What goes into `upstream/`?

**A:** Verbatim copy of GitHub kdevkit content, cached locally for offline use and fast divergence detection.

- Synced when you run `kdevkit sync pull`
- Your customizations go in `local/`, never touch `upstream/`
- Enables: offline work, fast diffs, no constant GitHub API calls

### Q2: How do we teach Claude/Gemini/Kiro the structure?

**A:** Two-stage process:

**Stage 1 (init):** `kdevkit init project` detects agent configs and injects import blocks
**Stage 2 (session):** Agent reads config, runs `kdevkit load both`, gets merged context

### Q3: Why can't memory be part of .kdevkit?

**A:** It IS! Previous designs were overengineered with separate git repos and symlinks.

**Simple approach:**
- System memory: `~/.kdevkit/memory/` - optionally `git init` to sync across machines
- Project memory: `.kdevkit/memory/` - gitignore (personal) or commit (team-shared)
- No special tooling - just use git directly

### Q4: What goes into config.yaml?

**A:** Nothing by default! Most things are auto-detected:
- Upstream repo: read from `.git-ref`
- Overrides: any file in `local/`
- Agent type: detect CLAUDE.md, .openagent/, etc.
- Divergence: compare on the fly

Optional config.yaml only if you need custom upstream repo or behavior toggles.

### Q5: Why not vend through nix?

**A:** To make kdevkit universally accessible:
- Works on any system with Python 3 + Git
- No package manager dependency
- Users without admin access can install
- Curl-installable like rustup, oh-my-zsh
- Can still integrate with nix if desired (optional)

### Q6: How does oh-my-openagent integration work?

**A:** When `kdevkit init project` detects `.openagent/`, it configures:

```json
// .openagent/config.json
{
  "hooks": {
    "pre_session": [
      {
        "command": "kdevkit load both",
        "description": "Load kdevkit system and project context"
      }
    ]
  },
  "context_paths": [
    "/home/user/.kdevkit/upstream",
    "/home/user/.kdevkit/local",
    "/home/user/project/.kdevkit/upstream",
    "/home/user/project/.kdevkit/local"
  ]
}
```

Now when you run `openagent run`, it automatically loads all kdevkit context via the pre_session hook.

---

## Implementation Roadmap

### Phase 1: Repository Structure
- [ ] Create `system/`, `project/`, `agents/` directories in kdevkit git
- [ ] Move existing files (feature-dev.md, etc.) to appropriate locations
- [ ] Extract system-level content (git-practices.md, coding-standards.md)
- [ ] Create agent template directories (agents/claude/, agents/openagent/)

### Phase 2: Core CLI Tools
- [ ] Implement `install.sh` (curl installer)
- [ ] Implement `lib/cli.py` (main dispatcher)
- [ ] Implement `lib/init.py` (init command)
- [ ] Implement `lib/load.py` (load command)
- [ ] Test installation on clean system

### Phase 3: Agent Adapter Framework
- [ ] Implement `lib/agents/base.py` (base adapter class)
- [ ] Implement `lib/agents/claude.py` (Claude Code adapter)
- [ ] Implement `lib/agents/openagent.py` (oh-my-openagent adapter)
- [ ] Implement `lib/agents/aider.py` (Aider adapter)
- [ ] Implement `bin/kdevkit-agent` (agent management CLI)
- [ ] Test with each agent type

### Phase 4: Sync Mechanism
- [ ] Implement `lib/sync.py` (sync command)
- [ ] Add `kdevkit sync pull` (fetch upstream updates)
- [ ] Add `kdevkit sync push` (create PR via GitHub API)
- [ ] Add divergence detection
- [ ] Test sync workflow

### Phase 5: Inspect and Update
- [ ] Implement `lib/inspect.py` (inspect command)
- [ ] Add status reports
- [ ] Add divergence visualization
- [ ] Implement `kdevkit update` (self-update)

### Phase 6: Documentation and Polish
- [ ] Comprehensive README
- [ ] Usage examples for each agent type
- [ ] Video walkthrough (optional)
- [ ] Blog post (optional)

---

## Next Steps

### Immediate Actions

1. **Create branch for this work** ✅
   ```bash
   git checkout -b feature/kdevkit-multi-layer-design
   ```

2. **Check in design documents**
   - This session summary
   - All spec versions (for reference)

3. **Push to remote**
   ```bash
   git add .
   git commit -m "docs: add kdevkit multi-layer architecture design"
   git push origin feature/kdevkit-multi-layer-design
   ```

### Future Work

**When resuming:**
1. Start with install.sh implementation
2. Test installation on clean system
3. Implement core CLI (init, load)
4. Build agent adapters
5. Test with real projects

**Testing plan:**
- Test on clean Linux system (not nix-managed)
- Test on macOS
- Test with Claude Code project
- Test with oh-my-openagent project
- Test with Aider project
- Test sync workflow (pull, push, divergence)

---

## Design Principles Summary

1. **Self-contained** - No external dependencies beyond Python 3 and Git
2. **Curl-installable** - One command to install, works anywhere
3. **Auto-detecting** - Detects agents, project types, divergence automatically
4. **Agent-agnostic** - Adapter framework supports any agent
5. **Git-native** - Use git directly for syncing (no custom tools)
6. **Minimal config** - Auto-detect everything, config only when needed
7. **Offline-capable** - Cached upstream enables offline work
8. **Team-friendly** - Share project configs, features, optionally memory

---

## File Purposes Reference

| Item | Location | Committed? | Purpose |
|------|----------|-----------|---------|
| kdevkit tool | `~/.kdevkit-install/` | N/A (git repo) | The kdevkit CLI itself |
| System templates | `~/.kdevkit/upstream/` | NO | Cached from GitHub |
| System customizations | `~/.kdevkit/local/` | Optional | Your overrides |
| System memory | `~/.kdevkit/memory/` | Optional | Your agent memory |
| Project templates | `.kdevkit/upstream/` | NO | Cached from GitHub |
| Project customizations | `.kdevkit/local/` | YES | Project-specific |
| Feature files | `.kdevkit/feature/` | YES | Feature context |
| Project memory | `.kdevkit/memory/` | Your choice | Project memory |
| Agent configs | `CLAUDE.md`, etc. | YES | Agent instructions |
| Git ref | `.git-ref` | NO | Tracks upstream |

---

## Related Documents

This session produced multiple specification documents as the design evolved:

1. **kdevkit-multilayer-spec.md** (v0.1.0) - Initial design with 5 layers
2. **kdevkit-revised-spec.md** (v0.2.0) - Clarified feature files, memory, agent detection
3. **kdevkit-simplified-spec.md** (v0.3.0) - Removed config.yaml, simplified memory
4. **kdevkit-self-contained-spec.md** (v0.4.0) - Final design: curl-installable, agent adapters
5. **kdevkit-implementation-plan.md** - Original phased implementation plan (for v0.1.0)
6. **kdevkit-design-session.md** (this file) - Summary of entire design process

**Recommended reading order:**
1. This file (kdevkit-design-session.md) - overview and evolution
2. kdevkit-self-contained-spec.md - detailed final design
3. Other specs as reference for how we got here

---

## Session Metadata

**Participants:** User, Claude Code  
**Duration:** ~2 hours  
**Key decisions made:** 5 major design iterations  
**Outcome:** Self-contained, curl-installable kdevkit with agent adapter framework

**Key insights:**
- Feature files should be committed (like project.md), not ephemeral
- Memory doesn't need special tooling - just use git directly
- Config.yaml not needed - auto-detect everything
- Make it self-contained (curl-installable) rather than nix-dependent
- Agent adapter framework enables any agent to integrate

**Next session:** Implementation starts with install.sh and core CLI

---

**End of Design Session Summary**
