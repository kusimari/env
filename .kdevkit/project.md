# Project: env

Personal development environment for Darwin, Ubuntu, and Amazon Linux
(2 / 2023), managed through one Nix flake with per-platform home-manager
configs. Designed so a fresh machine reaches a nixified shell in a
fixed, debuggable sequence, and day-2 rebuilds are a single command.

Read this file first — it is the map. Specific files only as needed.

## envKinds

`envKind` is the single switch that distinguishes targets. It is passed
into home-manager via `extraSpecialArgs` in `flake.nix` and consumed by
`home/home.nix` and its imports.

| envKind value | Target name(s) | Where defined | Notes |
|---|---|---|---|
| `"mane"` | `ubuntu-mane` | `flake.nix` `homeConfigurations.ubuntu-mane` | Home Ubuntu, graphical (rofi, chrome, desktop files). |
| `"kelasa"` | `darwin-kelasa`, `al2-kelasa`, `al2023-kelasa` | `flake.nix` `darwinConfigurations.darwin-kelasa` + `homeConfigurations.al2*-kelasa` | Work machines. AL2/AL2023 are headless SSH; darwin is macOS. |

The envKind string is `"mane"` or `"kelasa"` — *not* the full target
name. Target names (`al2-kelasa` etc.) only appear in `flake.nix`
attribute keys; code elsewhere tests `envKind`.

## Four-layer build system

Each layer is one script, run independently. No chaining — different
change rates and different trust domains. See `README.md` for the full
discussion; the table below is the operational summary.

| Layer | Script | Repo | Runs when | Purpose |
|---|---|---|---|---|
| 1 | `bootstrap-<envKind>.sh` | `env` (public) or kelasa-specific env repo | New machine | Native OS prep: auth, certs, sudoers, package mirrors. Makes machine nix-ready. |
| 2 | `build-nix/bootstrap-common.sh` | `env` | New machine | Clones `env` + `mAId` into `~/env-workplace/`, pins git identity. |
| 3 | `build-nix/<envKind>.sh` | `env` | Every rebuild | `home-manager switch` / `nix-darwin switch` from the cloned source. |
| 4 | `post-nix-kelasa.sh` | kelasa-specific env repo | After L3 on kelasa | Installs non-nixable corporate tooling (Amazon `toolbox`), writes `~/.post-nix-rc`. |

**Shell hook bridge.** Layers 1 and 4 are not nix-managed, but they can
inject shell state into the nix-managed zsh by writing to
`~/.pre-nix-rc` (L1) and `~/.post-nix-rc` (L4). Both are sourced from
`programs.zsh.envExtra` in `home/home.nix` (lines 107-120). These files
are the only supported contract between non-nixable work and the
nixified shell. Writers must be idempotent (diff-check, overwrite on
mismatch).

## Tiered package model

Three tiers, documented at the top of `flake.nix` and enforced by
`env-verify.nix`. Every package addition must pick a tier.

| Tier | Where it lives | Install path | Verifier covers? |
|---|---|---|---|
| 1 — always installed via nix | `home/home.nix` unconditional `home.packages` list, or `programs.*.enable` | nix on every envKind | yes |
| 2 — wanted everywhere, nix on some envs only | `home/home.nix`, wrapped in `++ lib.optionals (<envKind-predicate>) [...]` | nix where predicate admits; external tooling (e.g. Layer 4 toolbox install) elsewhere | yes — checks binary is on PATH regardless of source |
| 3 — per-env differences | `home/envKind-<name>.nix` (user-level) or a `<envKind>Configuration` attrset in `flake.nix` (system-level) | only envs that opt in | no |

**env-verify** (`env-verify.nix`, run via `nix run .#env-verify`) does
not hand-maintain an invariant list. It evaluates the mane + kelasa
home-manager configs, takes the union of `home.packages`, subtracts
tier-3 contributions, maps each package to `meta.mainProgram`, and
checks `command -v` for each resulting binary name. Adding a package to
tier 1 or tier 2 automatically extends coverage; adding to tier 3 does
not.

Implication for tier-2 packages provided by external tooling: the
external tool must drop a binary onto PATH whose name matches the nix
package's `meta.mainProgram` (often the package name, but not always —
e.g. `claude-code` exposes `claude`).

## flake.nix structure

Outputs are composed by **stacking module lists** — every target pulls
`commonConfiguration` plus platform-specific modules plus
`./home/home.nix`. The same `home.nix` file serves all targets; envKind
branching happens inside it.

Key attrsets:
- `commonConfiguration` — overlays (claude-code, alacritty-theme,
  vscode-extensions, direnv-no-check), flake experimental-features,
  allowUnfree. Shared by every target.
- `darwinConfiguration` — darwin-only: homebrew casks, touchID sudo,
  `programs.zsh.enable` (a system option on darwin).
- `linuxBaseConfiguration` — nixGL overlay, explicit `nix.package`.
  Applied to every Linux target.
- `linuxGraphicalConfiguration` — chrome, rofi, desktop files. Applied
  only to `ubuntu-mane`.
- `al2KelasaConfiguration` + `al2KelasaModules` — shared AL2/AL2023
  bundle: username, home dir, `sessionPath` for single-user Nix, locale
  packages + env vars. Fork into two lists if the kelasa Linux targets
  diverge.

Target declarations (bottom of file):
- `darwinConfigurations.darwin-kelasa` — `extraSpecialArgs.envKind = "kelasa"`.
- `homeConfigurations.ubuntu-mane` — `extraSpecialArgs.envKind = "mane"`.
- `homeConfigurations.al2-kelasa`, `.al2023-kelasa` — both
  `extraSpecialArgs.envKind = "kelasa"`, both use `al2KelasaModules`.

`apps` output points at `env-verify.nix` (kept separate so install and
verification evaluate independently).

## home/home.nix structure

One file, all envKinds. Order of top-to-bottom concerns:
1. `imports` — pulls in `gittree/gittree-module.nix`, `tmux/tmux.nix`,
   `home/envKind-${envKind}.nix` (tier-3 hook), `home/ssh-setup.nix`,
   `home/emacs.nix`.
2. `home.packages` — tier 1 and tier 2 packages (see table above).
3. `programs.alacritty` — with nixGL wrapper on Linux (via
   `symlinkJoin` so app launchers still find the `.desktop` file).
4. `programs.zsh` — oh-my-zsh + plugins, `envExtra` hook that sources
   `~/.pre-nix-rc` and `~/.post-nix-rc`. This is the bridge to
   non-nixable layers.
5. `programs.{direnv,fzf,bat,eza,zoxide,atuin,vscode,git,gittree}` —
   home-manager modules. `vscode.enable = false` today.
6. `home.file.".zfunc/*"` — generated zsh completions placed in the
   fpath that `initContent` sets up.

## Directory map

```
env/
├── flake.nix              # entry point; tier model documented at top
├── env-verify.nix         # nix run .#env-verify — tier-1/2 PATH check
├── flake.lock
├── README.md              # layer system, envKinds, install commands
├── setup-notes.md         # operator cheat-sheet, post-install tasks
│
├── build-nix/             # Layer 2 + Layer 3 scripts
│   ├── bootstrap-common.sh        # L2 — clone env + mAId
│   ├── bootstrap-ubuntu-mane.sh   # L1 (public envKind)
│   ├── ubuntu-mane.sh             # L3 for ubuntu-mane
│   ├── al2-kelasa.sh              # L3 for AL2
│   ├── al2023-kelasa.sh           # L3 for AL2023
│   ├── darwin-kelasa.sh           # L3 for darwin
│   ├── al2-fix-ssl.sh             # L1 helper for AL2 SSL quirks
│   ├── _common.sh                 # shared bash helpers across L3 scripts
│   └── test.sh                    # flake eval without building
│
├── home/                  # home-manager user-level config
│   ├── home.nix                   # single entry consumed by every envKind
│   ├── envKind-mane.nix           # tier-3 for mane (graphical home)
│   ├── envKind-kelasa.nix         # tier-3 for kelasa (work)
│   ├── user-host.nix              # user + hostname inputs
│   ├── emacs.nix                  # programs.emacs module (pairs with emacs/)
│   ├── ssh-setup.nix              # SSH config module
│   ├── ssh-setup.sh               # non-nix side of SSH setup
│   └── kusimari.bashrc            # bash legacy (where applicable)
│
├── emacs/                 # emacs core + language modes (loaded by home/emacs.nix)
│   ├── core.el, core-gittree.el, core-alternates-backup.el
│   └── <lang>-mode.el             # python, rust, scala, java, nix, haskell, js, latex
│
├── tmux/                  # tmux config + home-manager glue
│   ├── tmux.nix                   # programs.tmux module
│   ├── tmux.conf
│   └── tmux-sessions-view.py      # session picker helper
│
├── gittree/               # lazygit wrapper exposed as `lg`
│   ├── gittree-module.nix         # programs.gittree home-manager module
│   └── lazygit-config.yml
│
├── rclone-env/            # `rclone-env` bash script packaged as writeShellScriptBin
│   ├── rclone-env.sh              # read by home.nix into a derivation
│   └── _rclone-env                # zsh completion, installed to ~/.zfunc/
│
├── nix-init/              # `nix-init` — bootstrap a flake + direnv in a fresh project
│   └── nix-init.sh                # read by home.nix into a derivation
│
├── rofi-desktop/          # .desktop files for rofi (applied only to ubuntu-mane)
│   └── system-{reboot,shutdown,sleep}.desktop
│
├── workspace-tools/       # design docs + capture/setup instructions for AI workspaces
│   ├── DESIGN.md
│   └── workspace-{capture,setup}-instruction.md
│
├── features/              # feature-level design docs (not flake-consumed)
│   └── ssh-setup.md
│
└── .kdevkit/              # project-level dev-agent state
    ├── project.md                 # this file
    └── feature/                   # one markdown per in-flight or past feature
        └── wip/                   # active specs
```

## Non-obvious invariants

- **envKind branching happens inside `home/home.nix`**, not by
  swapping entry files. Keep it that way — the single-entry design is
  what makes env-verify's union-of-configs approach work.
- **Tier 2 requires an external provider on excluded envs.** Adding a
  tier-2 package without also wiring Layer 4 (or Layer 1) to install
  its binary breaks `nix run .#env-verify` on that envKind.
- **Binary name ≠ package name.** env-verify looks up
  `meta.mainProgram`. Before assuming a tier-2 handoff works, confirm
  the package's main program name matches what the external installer
  produces on PATH.
- **`~/.pre-nix-rc` and `~/.post-nix-rc` are the only non-nix → nix
  shell bridge.** Any other shell-init hack will drift.
- **`build-nix/test.sh`** evaluates the flake without building — use
  it as the first check after any flake edit.

## Conventions

- Branch naming: short, purpose-first (e.g. `feature-build-layers`,
  `claude-code-internalize`). L1 and L2 scripts accept `--branch` /
  `--env-branch` / `--maid-branch` so feature branches can be
  bootstrapped end-to-end.
- Conventional-commit style messages.
- Feature design docs in `.kdevkit/feature/<name>.md`; active ones in
  `.kdevkit/feature/wip/`.
- Day-2 rebuild flow: edit → `build-nix/test.sh` → `build-nix/<envKind>.sh`.
