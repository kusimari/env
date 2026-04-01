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

## Development Commands

### Build & Test Commands
- **Test flake evaluation**: `./build-nix/test.sh`
- **Darwin build**: `./build-nix/darwin.sh`
- **Ubuntu build**: `./build-nix/ubuntu.sh`
- **Amazon Linux 2**: First run `./build-nix/al2-fix-ssl.sh`, then `./build-nix/al2.sh`

### New Project Setup
```bash
mkdir <folder> && cd <folder>
nix-init
# Use language-specific builders or nix-templates
```

## Quality Gates

### Build Requirements
- Nix flake evaluation must pass (`./build-nix/test.sh`)
- All target platform builds must succeed
- No syntax errors in Nix expressions
- Configuration references must be valid

### Testing Strategy
- Verify configuration changes work across all target platforms
- Test new packages and programs installation
- Validate home-manager configuration evaluation
- Check for breaking changes and regressions

### Code Review Thresholds
- **Quality Score**: ≥ 80/100
- **High Severity Issues**: Must be 0 (blocking)
- **Medium Severity Issues**: Should be addressed for maintainability
- **Low Severity Issues**: Optional improvements

## Architecture

### Project Structure
- `flake.nix`: Main configuration defining all platforms
- `home/home.nix`: User-level configuration shared across platforms
- `home/user-host.nix`: User and hostname definitions
- `build-nix/_common.sh`: Shared build utilities

### Platform-Specific Considerations
- **Darwin-specific**: Homebrew casks, TouchID authentication, system settings
- **Linux-specific**: GUI applications via rofi, nixGL overlays, custom .desktop files
- **AL2-specific**: PATH fixes, locale configuration, single-user Nix installation
- **Cross-platform**: Nix packages, home-manager modules, common configuration

### Key Overlays
- alacritty-theme.nix
- nix-vscode-extensions
- claude-code-nix
- nixGL (Linux only)

## Conventions
- Branch naming: `type/description`
- Conventional commits
- Local scope only; PR gate requires passing builds
- Feature files in `.kdevkit/feature/<name>.md`
