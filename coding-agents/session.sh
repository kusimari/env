#!/bin/bash

# Generic AI Coding Agent launcher with session file context

AGENT="$1"
shift # Shift past the agent argument

# Initialize variables
SYSTEM_PROMPT_FILE=""
SESSION_FILE=""
AGENT_ARGS=()

# Parse named arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
        -s|--system-prompt) SYSTEM_PROMPT_FILE="$2"; shift ;;
        -f|--feature) SESSION_FILE="$2"; shift ;;
        --) shift; AGENT_ARGS=("$@"); break ;;  # Everything after -- goes to agent
        --*) AGENT_ARGS+=("$1" "$2"); shift ;;  # Pass through agent flags with values
        -*) AGENT_ARGS+=("$1"); ;;               # Pass through agent flags without values
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done

# --- Validation ---
if [ -z "$AGENT" ]; then
    echo "Usage: $0 <agent-executable> --system-prompt <path> [--feature <path>] [agent-options...]"
    echo "       $0 <agent-executable> --system-prompt <path> [--feature <path>] -- [agent-options...]"
    echo ""
    echo "Examples:"
    echo "  $0 claude-code --system-prompt coding-agents/session-system-prompt.md --feature coding-agents-aided-features/gittree-treemacs.md --model opus"
    echo "  $0 claude-code --system-prompt coding-agents/session-system-prompt.md --model sonnet"
    echo "  $0 gemini --system-prompt coding-agents/session-system-prompt.md --feature coding-agents-aided-features/gemini-cli-configure.md"
    exit 1
fi

if ! command -v "$AGENT" &> /dev/null; then
    echo "Error: Agent '$AGENT' not found in PATH."
    exit 1
fi

if [ -z "$SYSTEM_PROMPT_FILE" ]; then
    echo "Error: --system-prompt is a required argument."
    echo "Usage: $0 <agent-executable> --system-prompt <path> [--feature <path>] [agent-options...]"
    exit 1
fi


# Make paths absolute if they are relative
if [[ "$SYSTEM_PROMPT_FILE" != /* ]]; then
    SYSTEM_PROMPT_FILE="$(pwd)/$SYSTEM_PROMPT_FILE"
fi
if [[ -n "$SESSION_FILE" && "$SESSION_FILE" != /* ]]; then
    SESSION_FILE="$(pwd)/$SESSION_FILE"
fi

if [ ! -f "$SYSTEM_PROMPT_FILE" ]; then
    echo "Error: System prompt file not found: $SYSTEM_PROMPT_FILE"
    exit 1
fi
if [[ -n "$SESSION_FILE" && ! -f "$SESSION_FILE" ]]; then
    echo "Error: Feature file not found: $SESSION_FILE"
    exit 1
fi


# --- Execution ---
echo "Starting $AGENT..."
echo "  System Prompt: $SYSTEM_PROMPT_FILE"

# Create the session loading prompt and handle feature file presence
PROMPT="Please refer to $SYSTEM_PROMPT_FILE for overall session instructions and preferences."

if [[ -n "$SESSION_FILE" ]]; then
	# Feature file provided - full session management mode
	echo "  Feature File:  $SESSION_FILE"

	PROMPT="$PROMPT

Then, please load and review the session file at $SESSION_FILE to understand the current feature context.

After reviewing both files, follow the session management instructions from the system prompt."

else
	# No feature file - general coding mode

	PROMPT="$PROMPT

No session/feature file has been provided for this session.

After reviewing the system prompt, follow the appropriate workflow for sessions without a feature file."

fi

echo ""

# Handle different agents differently based on their stdin support
if [[ "$(basename "$AGENT")" == "kiro-cli" ]]; then
    # kiro-cli requires manual prompt entry - display prompt and wait for user
    echo "=== PROMPT FOR KIRO-CLI ==="
    echo "Copy the following prompt, then press any key to launch kiro-cli:"
    echo ""
    echo "----------------------------------------"
    echo "$PROMPT"
    echo "----------------------------------------"
    echo ""
    echo -n "Press any key to launch kiro-cli..."
    read -n 1 -s
    echo ""
    echo "Starting kiro-cli with args: ${AGENT_ARGS[*]}"
    exec "$AGENT" "${AGENT_ARGS[@]}"
elif [[ "$(basename "$AGENT")" == "gemini"* ]]; then
    # Gemini supports -i/--prompt-interactive to start with an initial prompt
    echo "Starting Gemini with session context and args: ${AGENT_ARGS[*]}"
    exec "$AGENT" "${AGENT_ARGS[@]}" -i "$PROMPT"
else
    # Claude and other agents work fine with stdin input
    echo "Starting $AGENT with session context and args: ${AGENT_ARGS[*]}"

    # Start the agent with an initial message that loads the session context
    exec "$AGENT" "${AGENT_ARGS[@]}" << EOF
$PROMPT
EOF
fi
