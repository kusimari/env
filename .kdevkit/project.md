# Project: env

Personal development environment for Darwin, Ubuntu, and Amazon Linux
(2 / 2023), managed through one Nix flake with per-platform home-manager
configs. Designed so a fresh machine reaches a nixified shell in a
fixed, debuggable sequence, and day-2 rebuilds are a single command.

Read this file first — it is the map. Specific files only as needed.

## Naming contract — envKind, platform, target

A machine is one **identity class** running on one **OS**. Two
orthogonal axes name it; layer scripts and flake keys derive from them:

| Term | Values | Meaning |
|---|---|---|
| **envKind** | `mane` · `kelasa` | machine class — personal (`mane`) vs work (`kelasa`). Drives git identity, what's installed, the L4 post-nix split. |
| **platform** | `ubuntu` · `al2` · `al2023` · `darwin` | the operating system. |
| **target** | `<platform>-<envKind>` | the full machine key = the `flake.nix` attribute name. Fully specifies a machine. `envKind` is the suffix after the last `-`. |

| target | envKind | platform | Where defined | Notes |
|---|---|---|---|---|
| `ubuntu-mane` | `mane` | `ubuntu` | `flake.nix` `homeConfigurations.ubuntu-mane` | Home Ubuntu, graphical (rofi, chrome, desktop files). |
| `al2-kelasa` | `kelasa` | `al2` | `flake.nix` `homeConfigurations.al2-kelasa` | Work Amazon Linux 2 (headless SSH). |
| `al2023-kelasa` | `kelasa` | `al2023` | `flake.nix` `homeConfigurations.al2023-kelasa` | Work Amazon Linux 2023 (headless SSH). |
| `darwin-kelasa` | `kelasa` | `darwin` | `flake.nix` `darwinConfigurations.darwin-kelasa` | Work macOS. |

`flake.nix` passes `envKind` (the short string) into home-manager via
`extraSpecialArgs`; code elsewhere tests `envKind`. The full `target`
appears as the flake attribute key and as the suffix on per-machine
layer scripts.

**Which axis each layer varies on** is encoded in its filename suffix —
a tight contract, so the suffix tells you the axis:

| Suffix shape | Varies by | Layers | Example |
|---|---|---|---|
| `layer-N-<target>.sh` | platform **and** envKind | L1, L3 | `layer-3-al2023-kelasa.sh` |
| `layer-N-<envKind>.sh` | envKind only (shared across that class's OSes) | L4 | `layer-4-kelasa.sh` |
| `layer-N.sh` | nothing (envKind-agnostic) | L5, L6 | `layer-5.sh` |

The `layer-run` driver (below) takes `--target` and derives `envKind`
from it, so it can resolve every layer's filename from one input.

## Layer build system (L0–L7)

Each layer is one script, run independently. No chaining — different
change rates and different trust domains. **L0** is the pre-clone curl
bootstrap (gets the repos onto a fresh machine; on kelasa runs L1 prep
before `env` exists). **L1–L5** are the always-installed base env +
tooling fetch (run in order on a fresh machine, individually on day-2).
**L6** builds the tools and is separable — a bare rebuild stops at L5.
**L7** is per-project, on-demand — never run as part of bootstrap. The
`layer-run` driver runs **L1–L6** in one command (see its row); L0 and
L7 are owned by other entrypoints (curl bootstrap; `workplace-setup.sh`).
See `README.md` for the full discussion; the table below is the
operational summary. Per-machine scripts use the naming contract above
(`<target>` for L1/L3, `<envKind>` for L4).

| Layer | Script | Repo | Runs when | Purpose |
|---|---|---|---|---|
| 0 | curl bootstrap (clone-only `env-setup.sh`) | `<kelasa-specific env repo>` | Brand-new machine | Pre-clone: gets the env repos onto the machine; on kelasa runs L1 prep. Not run by `layer-run`. |
| 1 | `layer-1-<target>.sh` | `env` (public) or `<kelasa-specific env repo>` | New machine | Native OS prep: auth, certs, sudoers, package mirrors. Makes machine nix-ready. |
| 2 | `layers/layer-2.sh` | `env` | New machine | Clones `env` into `~/env-workplace/`, pins git identity. |
| 3 | `layers/layer-3-<target>.sh` → sources `layers/layer-3-common.sh`, which tails `layers/layer-3-post-nix-common.sh` | `env` | Every rebuild | `home-manager switch` / `nix-darwin switch`, then envKind-agnostic post-nix tail. |
| 4 | `layer-4-kelasa.sh` | `<kelasa-specific env repo>` | After L3 on kelasa, or any day-2 change to envKind-specific post-nix content | envKind-specific non-nixable post-install. Writes `~/.post-nix-rc`. |
| 5 | `layers/layer-5.sh` (public) + `desktop-layers/layer-5.sh` (private) | `env` + `<kelasa-specific env repo>` | New machine | **Get only.** Runs one inline `{ ... }` block per workspace and store: clone/fetch. Workspaces clone under `~/tool-workplace/<name>/<repo>/`; stores clone flat under `~/dabba/<repo>/`. Also `mkdir -p ~/workplace/` (Layer 7 populates per-project). Does **not** run any cloned repo's install — that is Layer 6. On kelasa run the private `layer-5.sh`; it chains the public one first. |
| 6 | `layers/layer-6.sh` (driver) | `env` (public) + `<kelasa-specific env repo>` (private) | On demand | **Build tools.** Thin wrapper: walks the tool workplaces L5 cloned and runs each one's own root `install`/`setup` entry-point. **Not part of env setup** — a bare rebuild stops at L5. The normal fast path is running a tool workspace's entry-point from inside it; L6 is the run-them-all convenience. |
| 7 | `projects/workplace-setup.sh` (driver) + `projects/<project>/` (recipes) | `<envKind repo with project recipes>` | On demand, per project | **Projects, bidirectional.** *Hydrate:* replay a project recipe inside `~/workplace/<project>/` (symlinks, `.envrc`, optional per-project `bootstrap.sh`). *Capture:* start tracking an untracked workspace by writing its recipe back under `projects/<project>/`. **Not** part of bootstrap; never mutates the environment. |

**`layer-run` — one-run driver for L1–L6.**
`layer-run --target <target> [--repo <path>] [--layer 1,2,3] [--dry-run]`
runs the selected layers in ascending order; default (no `--layer`) runs
L1–L6. It takes the full `target` (deriving `envKind` for L4) and an
optional `--repo` path to the envKind-specific companion (its private
layers are skipped when `--repo` is absent). L0 and L7 are **not**
runnable through it — they appear in `layer-run --help` for orientation
but are owned by the curl bootstrap (L0) and `workplace-setup.sh` (L7).
For L5/L6 with `--repo` set it invokes the private half, which chains
the public half first. Relies on every layer being idempotent.

**L3 vs L4 post-nix split.** L3's `layer-3-post-nix-common.sh` is
envKind-agnostic; L4 is envKind-specific. If a post-nix step is
useful on every envKind, it belongs in L3. If it's meaningful only
on one envKind, it belongs in L4.

**L5 framework — three roots, get-only.** L5 is not for nix-managed
content. It exists as a staging area for things that change faster
than the base env — workspace repos with their own install flows.
**L5 only clones/fetches; it never runs a cloned repo's install (that
is L6) and never builds content.** Three roots, distinct semantics:

- `~/tool-workplace/` — env-tooling under active churn. Backed up
  via git remotes only. Workspace blocks clone here.
- `~/dabba/` — stores. Cross-machine state that must be backed up
  off the local disk (git-backed repos and, in future, rclone
  mounts). Store blocks clone flat here.
- `~/workplace/` — per-project workspaces. L5 only `mkdir -p`s the
  root; Layer 7 populates entries on demand.

Each workspace or store owns its own `install`/`setup` entry-point;
L6 runs it. Graduation (into L3 or L4) is a deliberate decision once
a workspace stabilizes.

**L6 framework — build the tools, separable.** L6 is a thin wrapper
with no registry of its own: it walks the tool workplaces L5 cloned
under `~/tool-workplace/` and runs each workspace's own root
`install`/`setup` entry-point (preferring `setup` when both exist).
The content repo owns its install; L6 only invokes it.

L6 is **not part of env setup.** A bare rebuild (L1–L5) leaves the
tools un-built; L6 is an explicit, separate step. The normal fast
path is running a tool workspace's entry-point from inside it (the
fast iteration loop); L6 is the run-them-all convenience and the
layer the rebuild driver targets when tooling should be rebuilt.

**L7 framework — projects, bidirectional, on demand.** L7 owns the
`~/workplace/<project>/` tree. Each project has a recipe checked
into the envKind repo under `projects/<project>/`: a `workspace.md`
(natural-language setup steps), an optional Nix flake, an optional
`bootstrap.sh` (project-specific setup the driver runs after
symlinks/`.envrc`), optional tool configs. The driver
`projects/workplace-setup.sh` is bidirectional:

- **Hydrate (replay)** runs every time the project is needed on a
  machine. The driver writes the recipe symlink, generates `.envrc`,
  runs any `bootstrap.sh`, invokes direnv. Idempotent — safe to
  re-run.
- **Capture** runs when a machine has a workspace not yet tracked.
  The developer (or a coding agent — see
  `env/project-workspace-tools/workspace-capture-instruction.md`) writes
  the recipe back to `projects/<project>/` so future machines can
  hydrate it.

L7 is **not** part of bootstrap and never mutates the environment
(no nix, no rc files, no PATH) — only the `~/workplace/<project>/`
tree. Project workspaces are recreated only when a developer wants to
work on a specific project on this machine. The base env (L1–L4) is
always-installed; tooling (L5 get + L6 build) runs on every machine
but L6 is separable; L7 is per-project and on-demand. Declarations
live in the envKind repos — discovered across whichever are present,
not a single hardcoded path.

**Shell hook bridge.** Layers 1 and 4 are not nix-managed, but they can
inject shell state into the nix-managed zsh by writing to
`~/.pre-nix-rc` (L1) and `~/.post-nix-rc` (L4). These files are the
only supported contract between non-nixable work and the nixified
shell. Writers must be idempotent (diff-check, overwrite on mismatch),
and any PATH manipulation inside them must be idempotent move-to-front
prepend (strip any existing entry, then prepend) so repeated sourcing
across the three hooks is a no-op and ordering is preserved.

`~/.post-nix-rc` is sourced from **all three** home-manager zsh hooks
in `home/home.nix`: `envExtra` (`.zshenv`, every shell), `loginExtra`
(`.zlogin`, login shells), and `initContent` (`.zshrc`, interactive
shells). The three-hook pattern preserves our PATH overlay across the
OS's own shell-init layering — the OS may write its own `.zprofile`
(or other files that fire between `.zshenv` and `.zshrc`) which can
prepend PATH entries after `.zshenv` has run. Re-sourcing
`~/.post-nix-rc` from later hooks with idempotent move-to-front
prepends inside it claims top of PATH in every shell form regardless
of what the OS does in between. See the policy comment above the hook
block in `home/home.nix` for the full ordering table.

`~/.pre-nix-rc` is currently a stub reserved for forward-compat L1-time
shell state; the active toolbox PATH wiring lives in `~/.post-nix-rc`,
which is owned by the kelasa env repo's L4 (a private, envKind-specific
companion to this public repo).

## Tiered package model

Three tiers, documented at the top of `flake.nix` and enforced by
`env-verify.nix`. Every package addition must pick a tier.

| Tier | Where it lives | Install path | Verifier covers? |
|---|---|---|---|
| 1 — always installed via nix | `home/home.nix` unconditional `home.packages` list, or `programs.*.enable` | nix on every envKind | yes |
| 2 — wanted everywhere, nix on some envs only | `home/home.nix`, wrapped in `++ lib.optionals (<envKind-predicate>) [...]` | nix where predicate admits; external tooling (e.g. Layer 4 post-install) elsewhere | yes — checks binary is on PATH regardless of source |
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
├── layers/                # Every layer script for the public side: L1 (ubuntu-mane) + L2 + L3 + L5 + L6, plus L1/L3 helpers
│   ├── layer-1-ubuntu-mane.sh        # L1 for the ubuntu-mane target (public; kelasa L1 is private)
│   ├── layer-2.sh                    # L2 — clone env
│   ├── layer-3-ubuntu-mane.sh        # L3 for ubuntu-mane
│   ├── layer-3-al2-kelasa.sh         # L3 for AL2
│   ├── layer-3-al2023-kelasa.sh      # L3 for AL2023
│   ├── layer-3-darwin-kelasa.sh      # L3 for darwin
│   ├── layer-3-common.sh             # shared body sourced by every L3 envKind script
│   ├── layer-3-post-nix-common.sh    # L3 tail — universal non-nixable post-nix nudges
│   ├── layer-5.sh                    # L5 (public): get — inline workspace + store clone/fetch blocks
│   ├── layer-6.sh                    # L6 (public): build — fd-discovers + runs each tool's setup/install
│   └── test-flake.sh                 # flake eval without building (tooling, not a layer)
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
├── project-workspace-tools/  # L7 design docs — capture/replay instructions for project workspaces
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

## Testing

This repo builds an environment on top of a machine, so validation
splits into two tracks: things that can be evaluated against the
checkout itself, and things that can only be observed on a machine
that has actually been activated. The first track is the kdevkit
Test Gate; the second is operator-driven and lives next to the
day-2 update flows in Conventions.

**Auto-runnable on a clean checkout** (the Test Gate). Anything
evaluable against the repo without changing machine state goes
here. From `env/`:

- Type-check / schema check the flake with `nix flake check`. Cheap
  evaluation pass that catches obvious schema or import breakage
  before a build is attempted.
- Build the flake without activating with `bash layers/test-flake.sh`.
  Builds `homeConfigurations.ubuntu-mane.activationPackage` and
  `homeConfigurations.al2-kelasa.activationPackage` to a Nix store
  path; no `home-manager switch`, no symlink swap. This is the real
  type check for `home/home.nix` and its imports.
- Bash parser-only check with `bash -n layers/*.sh`. Catches
  syntax errors without running anything.
- Bash lint with `shellcheck layers/*.sh`. Catches L1-L5 driver
  bugs that pass `bash -n`. `shellcheck` is in tier-1 `home.packages`,
  so it lands on PATH on every activated envKind and is therefore
  also picked up as an env-verify invariant.
- L5 dry-run with `bash layers/layer-5.sh --dry-run`. Walks the
  inline workspace + store blocks without cloning or fetching;
  verifies the arg parser and that each block evaluates. (L5 is
  get-only — it never invokes entry-points; that is L6.)
- L6 dry-run with `bash layers/layer-6.sh --dry-run`. Lists the
  tool entry-points `fd` discovers under `~/tool-workplace/` without
  running them; verifies discovery + the `setup`-over-`install` pick.

Together these are the format / lint / type-check / test commands
the kdevkit loop reads out of this section. There is no separate
formatter — Nix and bash are written by hand in this repo.

**Live-only** (operator-driven, not in the Test Gate). The rest of
validation requires an activated machine, so it stays manual:

- `nix run .#env-verify` is the canonical post-activation check.
  It enumerates tier-1 + tier-2 invariants and asserts each binary
  is on PATH. On a vanilla checkout most invariants will report
  MISS because they were never installed there — that is by design;
  the check is meaningful only after L3 has switched the
  home-manager generation. Run it after every day-2 rebuild.
- L1 machine prep (`layer-1-<target>.sh`) only matters on a fresh
  OS install and mutates system state — out of the loop entirely.
- L3 / L4 activation (`layer-3-<target>.sh`, then `layer-4-<envKind>.sh`
  on kelasa) actually swap the home-manager generation and write
  `~/.post-nix-rc` — verifiable only by re-running them and
  observing shell behaviour after the next login.
- L5 real clone (`bash layers/layer-5.sh` without `--dry-run`)
  clones into `~/tool-workplace/` and `~/dabba/` (get-only).
  Verified by inspecting those trees.
- L6 real build (`bash layers/layer-6.sh` without `--dry-run`) runs
  each discovered tool's `setup`/`install`. Separable from the base
  env. Verified by the tool's own post-install state.
- `~/.pre-nix-rc` / `~/.post-nix-rc` shell behaviour requires an
  interactive shell on an activated machine — there is no
  evaluation-time check for it.

The per-layer rerun chain for live-only validation is the same one
documented under Conventions §"Day-2 update flows"; chase the
layer that owns what changed rather than re-running everything.

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
- **`layers/test-flake.sh`** evaluates the flake without building — use
  it as the first check after any flake edit.

## Conventions

- Branch naming: short, purpose-first (e.g. `feature-build-layers`,
  `claude-code-internalize`). L1 and L2 scripts accept `--branch`
  / `--env-branch` so feature branches can be bootstrapped
  end-to-end. L5 pins workspaces and stores to default branches via
  inline `{ ... }` blocks in `layers/layer-5.sh` (and
  `desktop-layers/layer-5.sh` in the private repo) — one block per
  workspace or store, hand-edited when adding a new entry.
- Conventional-commit style messages.
- Feature design docs in `.kdevkit/feature/<name>.md`; active ones in
  `.kdevkit/feature/wip/`.
- Day-2 update flows (each layer re-runs independently; chase the
  layer that owns what changed):
  - `env` flake / `home.nix` edits: `layers/test-flake.sh` →
    `layers/layer-3-<target>.sh`.
  - kelasa-specific env-repo post-nix content (site-managed tools,
    aliases, `~/.post-nix-rc`):
    `desktop-layers/layer-4-<envKind>.sh` (in the kelasa env repo).
  - L5 workspace / store changes:
    `desktop-layers/layer-5.sh` on kelasa, else `layers/layer-5.sh`.
  - A specific project workspace recipe (L7, on demand only):
    `cd ~/workplace/<project> && projects/workplace-setup.sh`
    from inside the envKind repo with project recipes.
  Multi-area changes: pull the relevant repos with `git pull --ff-only`,
  then re-run L3 → L4 → L5 → L6 in that order.
