#!/usr/bin/env python3
"""View saved tmux sessions in hierarchical format."""

import sys
from pathlib import Path
from dataclasses import dataclass, field
from typing import List, Dict
from collections import defaultdict


@dataclass
class Pane:
    index: int
    path: str
    program: str
    command: str
    is_active: bool


@dataclass
class Window:
    index: int
    name: str
    is_active: bool
    panes: List[Pane] = field(default_factory=list)


@dataclass
class Session:
    name: str
    windows: List[Window] = field(default_factory=list)


def parse_save_file(file_path: Path) -> List[Session]:
    """Parse tmux-resurrect save file into structured data."""
    # Collect data from file
    panes_data = defaultdict(lambda: defaultdict(list))  # session -> window_idx -> [panes]
    windows_data = {}  # (session, window_idx) -> (name, is_active)
    sessions_set = set()

    with open(file_path, 'r') as f:
        for line in f:
            parts = line.strip().split('\t')
            if not parts:
                continue

            record_type = parts[0]

            if record_type == 'pane':
                # Format varies based on whether pane has a title:
                # With title: pane session win_idx ? ? pane_idx title :path pane_active program :command
                # No title:   pane session win_idx ? ? pane_idx :path pane_active program pid :command
                # Key: field 7 starts with ':' means no title, otherwise it has a title
                if len(parts) < 11:
                    continue

                session = parts[1]
                win_idx = int(parts[2])
                pane_idx = int(parts[5])

                # Determine if there's a title by checking if field 7 starts with ':'
                if parts[6].startswith(':'):
                    # No title format
                    path_field = parts[6]
                    pane_active = parts[7]
                    program = parts[8]
                    # parts[9] is PID, skip it
                    command = parts[10] if len(parts) > 10 else ':'
                else:
                    # Has title format
                    # parts[6] is title, skip it
                    path_field = parts[7]
                    pane_active = parts[8]
                    program = parts[9]
                    command = parts[10] if len(parts) > 10 else ':'

                # Remove leading colon from path
                path = path_field[1:] if path_field.startswith(':') else path_field

                # Clean up command
                if command == ':':
                    cmd_display = program
                else:
                    cmd_display = command[1:] if command.startswith(':') else command

                # Truncate long commands
                if len(cmd_display) > 70:
                    cmd_display = cmd_display[:67] + '...'

                pane = Pane(
                    index=pane_idx,
                    path=path,
                    program=program,
                    command=cmd_display,
                    is_active=(pane_active == '1')
                )

                panes_data[session][win_idx].append(pane)
                sessions_set.add(session)

            elif record_type == 'window':
                # window	session	win_idx	name	active_count	active_marker	layout	rest
                if len(parts) < 6:
                    continue
                _, session, win_idx, name, _, active_marker = parts[:6]
                windows_data[(session, int(win_idx))] = (name, active_marker == ':*')
                sessions_set.add(session)

    # Build session objects
    sessions = []
    for session_name in sorted(sessions_set):
        session = Session(name=session_name)

        # Get all window indices for this session
        window_indices = sorted(set(
            win_idx for (sess, win_idx) in windows_data.keys() if sess == session_name
        ))

        for win_idx in window_indices:
            win_name, is_active = windows_data.get((session_name, win_idx), (f'window-{win_idx}', False))
            window = Window(index=win_idx, name=win_name, is_active=is_active)

            # Add panes to window
            if win_idx in panes_data[session_name]:
                window.panes = sorted(panes_data[session_name][win_idx], key=lambda p: p.index)

            session.windows.append(window)

        sessions.append(session)

    return sessions


def format_hierarchy(sessions: List[Session]) -> str:
    """Format sessions as hierarchical tree with box-drawing chars."""
    lines = []

    for session in sessions:
        # Session header
        lines.append(f"📦 Session: \033[1;36m{session.name}\033[0m")

        for window in session.windows:
            # Window line
            active_marker = " \033[1;32m●\033[0m" if window.is_active else ""
            lines.append(f"  ├─ Window {window.index}: {window.name}{active_marker}")

            # Panes
            for pane in window.panes:
                pane_active_marker = " \033[1;33m✳\033[0m" if pane.is_active else ""
                lines.append(f"  │  ├─ Pane {pane.index}{pane_active_marker}: \033[0;90m{pane.path}\033[0m")
                lines.append(f"  │  │  └─ \033[0;35m{pane.command}\033[0m")

        lines.append("")  # Blank line between sessions

    return '\n'.join(lines)


def main():
    resurrect_dir = Path.home() / ".tmux" / "resurrect"
    save_file = resurrect_dir / "last"

    if not save_file.exists():
        print(f"No saved tmux sessions found at: {save_file}", file=sys.stderr)
        sys.exit(1)

    # Read symlink to get actual filename
    actual_file = save_file.resolve()

    try:
        sessions = parse_save_file(save_file)
        output = format_hierarchy(sessions)

        print(f"=== Saved Tmux Sessions (from: {actual_file.name}) ===\n")
        print(output)
        print(f"\033[0;90m💾 Sessions saved to: {resurrect_dir}\033[0m")

        # Count save files
        save_files = list(resurrect_dir.glob('*.txt'))
        print(f"\033[0;90m📁 Total files: {len(save_files)}\033[0m")

    except Exception as e:
        print(f"Error parsing save file: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
