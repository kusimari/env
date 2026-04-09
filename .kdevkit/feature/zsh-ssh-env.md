# Feature: zsh-ssh-env

Source `.zshrc` for SSH sessions that skip it (Tailscale and other SSH clients).

## Problem

When SSHing in (via Tailscale or any client), the shell starts as a **non-interactive login shell**. Zsh's startup file order for that case is:

```
.zshenv → .zprofile → .zlogin   (no .zshrc)
```

`.zshrc` — which holds all of home-manager's program integrations (oh-my-zsh, direnv, fzf, zoxide, atuin, completions, etc.) — is only sourced for **interactive** shells. Non-interactive login shells skip it, leaving the session bare.

Affects all targets: macOS, Ubuntu, AL2, Cloud NixOS.

## Solution

Add `programs.zsh.profileExtra` to `home/home.nix`. `.zprofile` is sourced for every login shell. Guard with `[[ ! -o interactive ]]` to avoid double-sourcing `.zshrc` in normal interactive terminal sessions.

## Acceptance Criteria

- SSH session (Tailscale or standard) has the same environment as an interactive terminal.
- Interactive terminal sessions are unaffected (no double-sourcing).
- `./build-nix/test.sh` passes (flake evaluation).

## Implementation

### File: `home/home.nix`

Add `profileExtra` to the existing `programs.zsh` block:

```nix
profileExtra = ''
  # Source .zshrc for non-interactive login shells (e.g., SSH via Tailscale)
  [[ ! -o interactive ]] && [[ -f "''${ZDOTDIR:-$HOME}/.zshrc" ]] && source "''${ZDOTDIR:-$HOME}/.zshrc"
'';
```

No other files need to change.

## Testing Strategy

1. `./build-nix/test.sh` — flake evaluation must pass.
2. Manual: SSH into an affected machine and confirm `which git`, `which rg`, `direnv`, `fzf`, etc. are available.
3. Manual: Open a normal interactive terminal and confirm it still works without double-init errors.

## Notes

- `.zshrc` is safe to source non-interactively for this config: oh-my-zsh, atuin, and fzf all carry their own interactive guards.
- `${ZDOTDIR:-$HOME}` is used rather than `$HOME` directly, following zsh convention.
- The Nix string literal uses `''${...}` to escape interpolation in Nix strings.
