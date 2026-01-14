# Claude Code Session Management

Simple session management for Claude Code using markdown feature files.

## Setup

Add this alias to your shell configuration (`~/.bashrc`, `~/.zshrc`, etc.):

```bash
alias claude-session='/Users/gorantls/env/claude/session.sh'
```

Then reload your shell or run:
```bash
source ~/.bashrc  # or ~/.zshrc
```

## Usage

Start Claude Code with a session file:

```bash
# From anywhere, with relative or absolute path
claude-session ai-aided-features/emacs-modern-completion.md

# Or with absolute path
claude-session /Users/gorantls/env/ai-aided-features/emacs-modern-completion.md
```

## How it works

1. The script validates the session file exists
2. Shows a brief summary of the session context
3. Starts Claude Code with an initial prompt to load and review the session file
4. Claude will acknowledge the feature and current status, maintaining context from your session file

## Session File Format

Your session markdown files should include:
- Feature description and requirements
- Implementation plan and progress
- Key decisions and context
- Commands and code snippets from previous sessions
- Progress log with dated entries

See `ai-aided-features/emacs-modern-completion.md` for an example format.