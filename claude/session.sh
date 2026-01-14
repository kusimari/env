#!/bin/bash

# Simple Claude Code launcher with session file context

SESSION_FILE="$1"

if [ -z "$SESSION_FILE" ]; then
    echo "Usage: $0 <session-file.md>"
    echo "Example: $0 ai-aided-features/emacs-modern-completion.md"
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

echo "Starting Claude Code with session context from: $SESSION_FILE"
echo ""

# Extract key information from the session file
echo "=== Session Context ==="
grep "^# " "$SESSION_FILE" | head -1
grep "^\*\*Status:\*\*" "$SESSION_FILE" || true
echo "Session file: $SESSION_FILE"
echo "======================"
echo ""

# Start Claude Code with an initial message that loads the session context
claude << EOF
Please load and review the session file at $SESSION_FILE to understand the current feature I'm working on. This file contains:
- The feature description and requirements
- Implementation plan and progress
- Key decisions and context
- Commands and code snippets from our previous sessions

After reviewing the file, briefly acknowledge what feature we're working on and the current status.
EOF
