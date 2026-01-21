#!/bin/bash

# Generic AI Coding Agent launcher with session file context

AGENT="$1"
shift # Shift past the agent argument

# Initialize variables
SYSTEM_PROMPT_FILE=""
SESSION_FILE=""

# Parse named arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        -s|--system-prompt) SYSTEM_PROMPT_FILE="$2"; shift ;;
        -f|--feature-file) SESSION_FILE="$2"; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

# --- Validation ---
if [ -z "$AGENT" ]; then
    echo "Usage: $0 <agent-executable> --system-prompt <path> --feature-file <path>"
    echo "Example: $0 gemini --system-prompt coding-agents/session-system-prompt.md --feature-file coding-agents-aided-features/gemini-cli-configure.md"
    exit 1
fi

if ! command -v "$AGENT" &> /dev/null; then
    echo "Error: Agent '$AGENT' not found in PATH."
    exit 1
fi

if [ -z "$SYSTEM_PROMPT_FILE" ]; then
    echo "Error: --system-prompt is a required argument."
    echo "Usage: $0 <agent-executable> --system-prompt <path> --feature-file <path>"
    exit 1
fi

if [ -z "$SESSION_FILE" ]; then
    echo "Error: --feature-file is a required argument."
    echo "Usage: $0 <agent-executable> --system-prompt <path> --feature-file <path>"
    exit 1
fi

# Make paths absolute if they are relative
if [[ "$SYSTEM_PROMPT_FILE" != /* ]]; then
    SYSTEM_PROMPT_FILE="$(pwd)/$SYSTEM_PROMPT_FILE"
fi
if [[ "$SESSION_FILE" != /* ]]; then
    SESSION_FILE="$(pwd)/$SESSION_FILE"
fi

if [ ! -f "$SYSTEM_PROMPT_FILE" ]; then
    echo "Error: System prompt file not found: $SYSTEM_PROMPT_FILE"
    exit 1
fi
if [ ! -f "$SESSION_FILE" ]; then
    echo "Error: Session file not found: $SESSION_FILE"
    exit 1
fi


# --- Execution ---
echo "Starting $AGENT..."
echo "  System Prompt: $SYSTEM_PROMPT_FILE"
echo "  Feature File:  $SESSION_FILE"
echo ""

# Create the session loading prompt
PROMPT="Please refer to $SYSTEM_PROMPT_FILE for overall session instructions and preferences.

Then, please load and review the session file at $SESSION_FILE to understand the current feature I'm working on. This file contains:
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
