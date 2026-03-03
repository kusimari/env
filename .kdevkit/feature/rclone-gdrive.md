# Feature: rclone-env

## Status: implemented — pending Mac test

## Summary
Create a self-contained `rclone-env/` directory with a home-manager Nix module that provides a `rclone-env` CLI tool for mounting and unmounting rclone remotes (gdrive, SSH, etc.) with zsh autocompletion. Follows the gittree module pattern.

## Requirements
- Single `rclone-env` command with subcommands: `mount`, `umount`, `status`, `list`, `add`, `remove`
- Works with any rclone remote type (gdrive, sftp, s3, etc.)
- User-level config file at `~/.config/rclone-env/remotes` mapping rclone remote paths to local mount points
- `add` subcommand invokes `rclone config` interactively then saves the mount mapping
- Zsh autocompletion: subcommands, `mount` completes unmounted remotes, `umount` completes mounted ones, `remove` completes all configured remotes
- Packaged as a home-manager module (`programs.rclone-env.enable`) imported into `home/home.nix`
- FUSE managed in `flake.nix` alongside other platform-specific packages: `fuse3` in `linuxConfiguration`, `macfuse` cask in `darwinConfiguration` — both commented as required for rclone-env
- Module itself has no platform conditionals

## Reference: gittree pattern
The `gittree/gittree-module.nix` pattern to follow:
- Module in its own directory, imported via `../gittree/gittree-module.nix` in `home/home.nix`
- Uses `pkgs.writeShellScriptBin` to create the command
- Large scripts kept in a separate file read via `builtins.readFile ./script.sh`
- Config/data files kept alongside the module and referenced with relative paths

## Directory structure
```
rclone-env/
  rclone-env-module.nix   # home-manager module
  rclone-env.sh           # main script (loaded via builtins.readFile)
  _rclone-env             # zsh completion function file
```

## Config file (~/.config/rclone-env/remotes)
Plain text, one entry per line, `#` comments ignored:
```
# rclone-remote     mount-point
gdrive:             ~/mnt/gdrive
myserver:photos     ~/mnt/photos
```
Fields are whitespace-separated. Mount point may use `~`.

## Subcommands
| Command | Description |
|---|---|
| `list` | Show all configured remotes with mounted/unmounted status |
| `status` | Alias for `list` |
| `mount [remote]` | Mount remote; interactive `select` prompt if no arg |
| `umount [remote]` | Unmount remote; interactive prompt if no arg |
| `add` | Run `rclone config` interactively, then prompt for mount point, append to remotes config |
| `remove <remote>` | Remove entry from remotes config (does not modify rclone.conf) |

## rclone-env-module.nix design
No platform conditionals — FUSE is handled at the flake level (see below).

```nix
{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.programs.rclone-env;
in {
  options.programs.rclone-env = {
    enable = mkEnableOption "rclone-env - mount/unmount rclone remotes";
  };

  config = mkIf cfg.enable {
    home.packages = [
      (pkgs.writeShellScriptBin "rclone-env" (builtins.readFile ./rclone-env.sh))
    ];

    # Zsh completion: place _rclone-env in a dir on $fpath
    home.file.".zfunc/_rclone-env".source = ./_rclone-env;

    # Ensure ~/.zfunc is on fpath (prepend in zsh initExtra)
    programs.zsh.initExtra = ''
      fpath=(~/.zfunc $fpath)
    '';
  };
}
```

## flake.nix changes (FUSE — platform mirrors of each other)
FUSE dependencies live in the flake alongside other platform-specific packages, not in the module. Both entries get a comment pointing to rclone-env.

**linuxConfiguration** — add `pkgs.fuse3` to `home.packages`:
```nix
# fuse3: required for rclone-env mount functionality
home.packages = [ pkgs.google-chrome pkgs.fuse3 ];
```

**darwinConfiguration** — add `"macfuse"` to `homebrew.casks`:
```nix
casks = [
  "raycast"
  "google-chrome"
  "porting-kit"
  "macfuse"  # required for rclone-env mount functionality
];
```

## rclone-env.sh design
- Reads config from `~/.config/rclone-env/remotes`; creates it if missing
- Resolves `~` in mount paths
- Mount: `mkdir -p <mount-point>` then `rclone mount <remote> <mount-point> --daemon`
- Unmount: `fusermount -u <mount-point>` on Linux, `umount <mount-point>` on macOS
- Mount detection: check via `mount | grep <mount-point>` (or `findmnt`)
- `add` flow: call `rclone config`, then `read` the remote name and desired mount point, append line to remotes file

## _rclone-env zsh completion design
- Completes subcommands on first arg
- `mount`: sources remotes config, filters to unmounted ones
- `umount`: sources remotes config, filters to currently-mounted ones
- `remove`/`status`: all configured remotes

## home/home.nix changes
```nix
imports = [
  ../gittree/gittree-module.nix
  ../rclone-env/rclone-env-module.nix   # add this
];

# add:
programs.rclone-env.enable = true;
```

## Implementation Steps
1. `rclone-env/rclone-env.sh` — main script ✓
2. `rclone-env/_rclone-env` — zsh completion ✓
3. `rclone-env/rclone-env-module.nix` — Nix module ✓
4. `home/home.nix` — add import and enable ✓

## Testing
- `home-manager build` passes (no eval errors)
- `rclone-env list` works with empty config
- `rclone-env add` invokes `rclone config` and appends entry
- Tab-completion works for subcommands and remote names after `home-manager switch`

## Open questions / risks
- `programs.zsh.initExtra` may conflict if already set elsewhere — check before writing
- `fuse3` vs `fuse` package name on older nixpkgs — verify during impl
- macOS `rclone mount` requires macFUSE; script should detect and warn if unavailable

## Git / worktree workflow
- Branch: `feat/rclone-env`
- Worktree created as a sibling to `main/`, e.g. `/home/kusimari/env/feat-rclone-env`
  - Create with: `git -C /home/kusimari/env/main worktree add ../feat-rclone-env -b feat/rclone-env`
  - **Do NOT create worktree inside `main/`**
- After implementation: push `feat/rclone-env` to remote so Mac can pull and test before merging
- Testing on Mac: `home-manager switch --flake .#darwin` — verify macfuse cask added, `rclone-env` command available, tab completion works
- Merge via PR after Mac verification passes

## Session pickup instructions
To resume implementation in a new session:
1. Open session in `/home/kusimari/env/feat-rclone-env`
2. Load kdevkit-dev guide from: https://raw.githubusercontent.com/kusimari/kdevkit/main/build/kdevkit-dev.md
3. Feature file is at `.kdevkit/feature/rclone-gdrive.md` — all context is there
4. Status is `implemented — pending Mac test` — run `home-manager build` then test `rclone-env` command
