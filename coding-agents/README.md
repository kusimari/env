# AI Coding Helper Scripts

This directory contains helper scripts and prompts for managing AI-aided coding sessions.

## Files

### `session.sh`
A generic wrapper script to start an AI coding agent (such as `gemini` or `claude`) initialized with a specific session context file.

**Features:**
- Verifies the agent executable is in the PATH.
- Validates the existence of the session file.
- Displays a brief summary of the session context.
- Initializes the agent with a prompt to review the provided session file.

**Usage:**
```bash
./session.sh <agent_executable> <path_to_session_file.md>
```

**Example:**
```bash
~/env/coding-agents/session.sh gemini ~/env/coding-agents-aided-features/new-feature.md
```

### `feature-session-manager-prompt.md`
A prompt template used to bootstrap new feature sessions.

**Usage:**
Load this file into your AI agent to interactively create a new structured markdown session file (like the ones used by `session.sh`). It includes templates for feature briefs, requirements, and implementation plans.