# Project: env

Personal development environment managed with Nix flakes and home-manager.

## Targets
- macOS (aarch64-darwin) via nix-darwin
- Ubuntu x86_64-linux via home-manager standalone
- Amazon Linux 2 (kelasa-al2) via home-manager standalone

## Stack
- Nix flakes (`flake.nix`)
- home-manager for user-level config (`home/home.nix`)
- nix-darwin for macOS system-level config
- Programs: zsh + oh-my-zsh, tmux, alacritty, emacs, direnv, fzf, bat, eza, zoxide, atuin, rclone, gh

## Conventions
- Branch naming: `type/description`
- Conventional commits
- Local scope only; PR gate requires passing builds
- Feature files in `.kdevkit/feature/<name>.md`
