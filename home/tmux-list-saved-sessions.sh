#!/usr/bin/env bash
# View saved tmux sessions in a human-readable format

RESURRECT_DIR="${HOME}/.tmux/resurrect"
SAVE_FILE="${RESURRECT_DIR}/last"

if [[ ! -f "$SAVE_FILE" ]]; then
    echo "No saved tmux sessions found at: $SAVE_FILE"
    exit 1
fi

echo "=== Saved Tmux Sessions (from: $(basename $(readlink -f "$SAVE_FILE"))) ==="
echo

# First pass: collect all data into associative arrays
declare -A sessions
declare -A windows
declare -A panes
declare -A window_names
declare -A window_active
declare -A pane_details

while IFS=$'\t' read -r type rest; do
    case "$type" in
        pane)
            # Format: pane session win_idx win_active_marker active_win_marker pane_idx title path pane_active program pane_pid command
            IFS=$'\t' read -r session win_idx win_active_marker active_win_marker pane_idx title path pane_active program pane_pid command <<< "$rest"

            # Store pane data
            pane_key="${session}:${win_idx}:${pane_idx}"
            pane_details[$pane_key]="$path|$program|$command|$pane_active"

            # Track which windows exist per session
            sessions[$session]=1
            windows["${session}:${win_idx}"]=1
            ;;
        window)
            # Format: window session win_idx name active_count active_marker layout rest
            IFS=$'\t' read -r session win_idx name active_count active_marker layout rest <<< "$rest"
            window_names["${session}:${win_idx}"]="$name"
            window_active["${session}:${win_idx}"]="$active_marker"
            ;;
    esac
done < "$SAVE_FILE"

# Second pass: display in hierarchy
for session in $(printf '%s\n' "${!sessions[@]}" | sort); do
    echo -e "📦 Session: \033[1;36m$session\033[0m"

    # Get all windows for this session
    for win_key in $(printf '%s\n' "${!windows[@]}" | grep "^${session}:" | sort -t: -k2 -n); do
        win_idx="${win_key#*:}"
        win_name="${window_names[$win_key]}"
        win_active_marker="${window_active[$win_key]}"

        active_marker=""
        [[ "$win_active_marker" == ":*" ]] && active_marker=" \033[1;32m●\033[0m"

        echo -e "  ├─ Window ${win_idx}: ${win_name}${active_marker}"

        # Get all panes for this window
        for pane_key in $(printf '%s\n' "${!pane_details[@]}" | grep "^${session}:${win_idx}:" | sort -t: -k3 -n); do
            pane_idx="${pane_key##*:}"
            IFS='|' read -r path program command pane_active <<< "${pane_details[$pane_key]}"

            pane_active_marker=""
            [[ "$pane_active" == "1" ]] && pane_active_marker=" \033[1;33m✳\033[0m"

            # Clean up the command for display
            cmd_display="$program"
            if [[ -n "$command" && "$command" != ":" ]]; then
                command="${command#:}"
                cmd_display="$command"
            fi

            # Truncate long commands
            if [[ ${#cmd_display} -gt 70 ]]; then
                cmd_display="${cmd_display:0:67}..."
            fi

            echo -e "  │  ├─ Pane ${pane_idx}${pane_active_marker}: \033[0;90m${path}\033[0m"
            echo -e "  │  │  └─ \033[0;35m${cmd_display}\033[0m"
        done
    done
    echo
done

echo -e "\033[0;90m💾 Sessions saved to: $RESURRECT_DIR\033[0m"
echo -e "\033[0;90m📁 Total files: $(ls -1 "$RESURRECT_DIR"/*.txt 2>/dev/null | wc -l)\033[0m"
