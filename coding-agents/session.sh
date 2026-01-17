#!/bin/bash

# Generic AI Coding Agent launcher with session file context

AGENT="$1"
SESSION_FILE="$2"

if [ -z "$AGENT" ] || [ -z "$SESSION_FILE" ]; then
    echo "Usage: $0 <agent-executable> <session-file.md>"
    echo "Example: $0 gemini coding-agents-aided-features/gemini-cli-configure.md"
    echo "Example: $0 claude coding-agents-aided-features/emacs-modern-completion.md"
    exit 1
fi

# Check if the agent is in the PATH
if ! command -v "$AGENT" &> /dev/null; then
    echo "Error: Agent '$AGENT' not found in PATH."
    exit 1
fi

# Make path absolute if it's relative
if [[ "$SESSION_FILE" != /* ]]; then
    SESSION_FILE="$(pwd)/$SESSION_FILE"
fi

if [ ! -f "$SESSION_FILE" ]; then
    echo "Error: Session file not found: $SESSION_FILE"
    exit 1
fi

echo "Starting $AGENT with session context from: $SESSION_FILE"
echo ""

# Extract key information from the session file
echo "=== Session Context ==="
grep "^# " "$SESSION_FILE" | head -1
grep "^\*\*Status:\*\*" "$SESSION_FILE" || true
echo "Session file: $SESSION_FILE"
echo "======================"
echo ""

# Create the session loading prompt
PROMPT="Please load and review the session file at $SESSION_FILE to understand the current feature I'm working on. This file contains:
- The feature description and requirements
- Implementation plan and progress
- Key decisions and context
- Commands and code snippets from our previous sessions

After reviewing the file, briefly acknowledge what feature we're working on and the current status."

# Handle different agents differently based on their stdin support
if [[ "$(basename "$AGENT")" == "gemini"* ]]; then
    # Gemini supports -i/--prompt-interactive to start with an initial prompt
    echo "Starting Gemini with session context..."
    exec "$AGENT" -i "$PROMPT"
else
    # Claude and other agents work fine with stdin input
    echo "Starting $AGENT with session context..."

    # Start the agent with an initial message that loads the session context
    exec "$AGENT" << EOF
$PROMPT
EOF
fi
