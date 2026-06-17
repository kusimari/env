# env

Nix-managed home environment spanning Darwin, Ubuntu, and Amazon
Linux (2 / 2023). One flake, several envKinds.

---

## Layer design

Seven layers, one job each. No chaining. Layers 1–5 run on a fresh
machine in sequence to bring the base env online and fetch the
fast-moving tooling; Layer 6 builds that tooling and is separable
(a bare rebuild can stop at L5); Layer 7 is per-project and only
runs when a specific project workspace is needed on this machine.

### The philosophy

- **Layer 1 — make the machine nix-ready, and clone non-nixable
  envKind repos.** Runs before nix exists, so everything here is
  bash + the OS's native package manager. Site-specific auth,
  sudoers, cert installs, and clones of the envKind-specific repos
  (for kelasa envKinds) live here. Public envKinds get their L1
  from `env`; private envKinds get it from their envKind repo.

- **Layer 2 — pull the nix-managed environment source.** Generic
  across all envKinds: no machine prep, no envKind assumptions.
  Clones `env` into `~/env-workplace/`, confirms GitHub SSH, pins
  commit identity, exits.

- **Layer 3 — build the nix environment, then run universal
  post-nix tail.** `home-manager switch` or `nix-darwin switch`
  from the cloned source, followed by `layers/layer-3-post-nix-common.sh`.
  The tail is envKind-agnostic; anything envKind-specific belongs
  in Layer 4. Layer 3 also exposes two extension points —
  `~/.pre-nix-rc` and `~/.post-nix-rc` — so Layer 1 and Layer 4 can
  inject shell state that doesn't belong in the flake. See
  [Shell-hook extension points](#shell-hook-extension-points--how-layer-3-hooks-layer-1-and-layer-4)
  below.

- **Layer 4 — envKind-specific non-nixable post-install.**
  Site-specific tool installs (via whatever vendor tooling the
  site requires, not nix), one-time setup commands, shell aliases
  keyed to non-nix binaries. Lives in the envKind's own repo.
  Writes `~/.post-nix-rc`; never builds nix artifacts.

- **Layer 5 — get the fast-moving tooling and stores.** Three roots
  and a small set of inline `{ ... }` blocks, one per known
  workspace and store. L5 drivers (`layers/layer-5a.sh` in `env`,
  `desktop-layers/layer-5b.sh` in the envKind repo) walk their
  blocks:
    - **workspaces** — clone into
      `~/tool-workplace/<name>/<repo>/` (env-tooling under active
      churn).
    - **stores** — clone flat into `~/dabba/<repo>/` (cross-machine,
      backed-up content).
  Both drivers also `mkdir -p ~/workplace/`, which Layer 7 (below)
  populates per-project on demand.
  For each block the driver pins git identity and **stops at
  clone/fetch — L5 is get-only.** It does not run any cloned repo's
  install; that is Layer 6. The driver never builds content — each
  workspace or store owns its own install.
  L5 is the home for things that change faster than the base env
  and aren't (yet) worth nix-managing. When a workspace hardens
  enough, it can graduate into L3 (nix-managed) or L4 (non-nix).
  Adding a workspace or store: copy an existing `{ ... }` block in
  the relevant driver and edit the name/url.

- **Layer 6 — build the tools, separable.** A thin driver
  (`layers/layer-6.sh`) with no registry of its own: it walks the
  tool workplaces L5 cloned under `~/tool-workplace/` and runs each
  workspace's own root `install`/`setup` entry-point (preferring
  `setup` when both exist). The content repo owns its install; L6
  only invokes it.
  L6 is **not part of env setup** — a bare rebuild (L1–L5) leaves
  the tools un-built. The normal fast path is running a tool
  workspace's entry-point from inside it (the fast iteration loop);
  L6 is the run-them-all convenience and the layer the rebuild
  driver targets when tooling should be rebuilt.

- **Layer 7 — per-project workplace recreation, on demand.** Project
  workspaces under `~/workplace/<project>/` are **not** bulk-installed
  during machine bootstrap. Each project owns a recipe (a
  `workspace.md` plus optional Nix flake, optional `bootstrap.sh`,
  and tool configs) checked into the envKind repo under
  `projects/<project>/`. A small driver
  (`projects/workplace-setup.sh`, sibling to the recipes) is
  bidirectional:
    - **Hydrate (replay)** — every time that project is needed on a
      machine, run the driver from inside `~/workplace/<project>/`:
      it writes symlinks back to the recipe directory, generates
      `.envrc` for the project's Nix flake, runs any project
      `bootstrap.sh`, and hands the shell to direnv.
    - **Capture** — when a machine has a workspace not yet tracked,
      the developer (or a coding agent following the capture
      instructions in `env/workspace-tools/`) writes the recipe back
      under `projects/<project>/` so future machines can hydrate it.
  L7 is **not** part of fresh-machine bootstrap and never mutates the
  environment (no nix, no rc files, no PATH) — only the
  `~/workplace/<project>/` tree. Project workspaces come and go —
  they get recreated only when a developer decides to work on that
  project on this machine.

Why distinct scripts, no chaining? L1 and L2 run rarely (new machine,
major env refresh). L3 runs often. L4 is out-of-band and not always
needed. L5 fetches fast-moving workspaces independently of the base
env. L6 builds them and is separable. L7 runs only when a developer
wants to work on a specific project on this machine. Grouping them
into an orchestrator would bundle different change rates and risks.
Separate scripts keep each layer's scope obvious and debuggable
alone.

### At a glance

| Layer | Script | Repo | Curl-able | Purpose |
|---|---|---|---|---|
| 1 | `layers/layer-1-<envKind>.sh` (`env`) or `desktop-layers/layer-1-<envKind>-kelasa.sh` (private) | `env` (public envKinds) or a `<kelasa-specific env repo>` | yes | Machine ready for nix |
| 2 | `layers/layer-2.sh` | `env` | yes | env cloned |
| 3 | `layers/layer-3-<envKind>.sh` → `layers/layer-3-common.sh` → `layers/layer-3-post-nix-common.sh` | `env` | no | nix build + universal post-nix nudges |
| 4 | `desktop-layers/layer-4-kelasa.sh` | `<kelasa-specific env repo>` | no | envKind-specific non-nixable post-install |
| 5a | `layers/layer-5a.sh` | `env` (public) | no | **Get only.** Workspaces → `~/tool-workplace/`, stores → `~/dabba/`, mkdir `~/workplace/`. Clone/fetch; no install (that is L6). |
| 5b | `desktop-layers/layer-5b.sh` | `<kelasa-specific env repo>` (private) | no | Chains 5a, then clones/fetches private workspace + store blocks under the same three roots. Get-only. |
| 6 | `layers/layer-6.sh` | `env` (public) + `<kelasa-specific env repo>` (private) | no | **Build tools.** Walks `~/tool-workplace/` and runs each workspace's own `install`/`setup`. Separable — not part of env setup. |
| 7 | `projects/workplace-setup.sh` (driver) + `projects/<project>/` (recipes) | `<envKind repo with project recipes>` | no | On demand, per-project. *Hydrate:* replay a recipe inside `~/workplace/<project>/` (symlinks, `.envrc`, `bootstrap.sh`). *Capture:* track an untracked workspace. Never mutates the env. |

### Day-2 update flows

After initial setup, layers re-run independently. Pick based on
what changed.

| What changed | Run |
|---|---|
| `env` flake / `home.nix` / nix-managed config | L3: `~/env-workplace/env/layers/layer-3-<envKind>.sh` |
| envKind-specific post-nix content (site-managed tools, aliases, `~/.post-nix-rc`) | L4: `~/env-workplace/<kelasa-specific env repo>/desktop-layers/layer-4-<envKind>.sh` |
| L5 workspace block, store block, or store content (clone/fetch only) | L5b on kelasa machines: `~/env-workplace/<kelasa-specific env repo>/desktop-layers/layer-5b.sh` (chains 5a). L5a on public-only machines: `~/env-workplace/env/layers/layer-5a.sh`. |
| A tool workspace's own `install`/`setup` (rebuild the tooling) | Run it from inside the workspace (fast path), or L6 to run them all: `~/env-workplace/env/layers/layer-6.sh` |
| A specific project's workspace recipe | L7, on demand: `mkdir -p ~/workplace/<project> && cd ~/workplace/<project> && ~/env-workplace/<envKind repo with project recipes>/projects/workplace-setup.sh` |
| Multiple of the above | L3 → L4 → L5 → L6 → L7 in that order |

Pulling new upstream commits before re-running a layer:

```bash
# To pick up new env commits before L3:
git -C ~/env-workplace/env pull --ff-only
~/env-workplace/env/layers/layer-3-<envKind>.sh

# To pick up new commits in the kelasa-specific env repo before
# L4 / L5b:
git -C ~/env-workplace/<kelasa-specific env repo> pull --ff-only
~/env-workplace/<kelasa-specific env repo>/desktop-layers/layer-4-<envKind>.sh
~/env-workplace/<kelasa-specific env repo>/desktop-layers/layer-5b.sh
```

A `git pull` alone is not enough — re-run the matching layer after.

---

## envKinds

envKind names are the source of truth in `flake.nix`.

| envKind | L1 location | L4 location | Notes |
|---|---|---|---|
| `ubuntu-mane` | `env` (public) | — | Home Ubuntu machine (graphical). |
| `darwin-kelasa` | `<kelasa-specific env repo>` | `<kelasa-specific env repo>` | Work macOS. |
| `al2-kelasa` | `<kelasa-specific env repo>` | `<kelasa-specific env repo>` | Work Amazon Linux 2 (headless). |
| `al2023-kelasa` | `<kelasa-specific env repo>` | `<kelasa-specific env repo>` | Work Amazon Linux 2023 (headless). |

Layer 1 for `*-kelasa` envKinds readies the machine for nix —
site-specific auth, package mirrors, sudoers tweaks. It lives in a
`<kelasa-specific env repo>` alongside `env`, **not** in `env`,
because that prep isn't nix-managed (it predates nix). That's the
whole point of Layer 1.

Fresh-machine commands:

```bash
# Layer 1 — curl your envKind's bootstrap script
curl <L1-url>/layer-1-<envKind>.sh | bash

# Layer 2 — curl layer-2.sh from env
curl -fsSL https://raw.githubusercontent.com/kusimari/env/main/layers/layer-2.sh | bash

# Layer 3 — now that env is on disk, run the build directly
~/env-workplace/env/layers/layer-3-<envKind>.sh

# Layer 4 (if your envKind has one) — standalone post-nix install
~/env-workplace/<kelasa-specific env repo>/desktop-layers/layer-4-kelasa.sh

# Layer 5 — clone/fetch workspace + store repos (get-only, no install).
# Use the private driver on kelasa machines; it runs the public
# driver first.
~/env-workplace/env/layers/layer-5a.sh                               # public-only
~/env-workplace/<kelasa-specific env repo>/desktop-layers/layer-5b.sh # private; chains public first

# Layer 6 — build the tools L5 fetched. Separable; not part of the
# base env. Or run a single workspace's own install/setup from inside
# it (the fast iteration path).
~/env-workplace/env/layers/layer-6.sh

# Layer 7 — recreate a specific project workspace on demand. NOT a
# bulk install; run only when you actually want to work on that
# project on this machine. Repeat per project.
mkdir -p ~/workplace/<project>
cd ~/workplace/<project>
~/env-workplace/<envKind repo with project recipes>/projects/workplace-setup.sh
```

### Cloning or switching to a feature branch

L1 and L2 accept branch flags. On initial clone, the clone targets
that branch. On re-run, the scripts switch to the named branch if
the working tree is clean — they refuse to clobber uncommitted
changes, and prompt you to commit or stash first.

```bash
# L1 — pass --branch to check out a feature branch of a kelasa-
# specific env repo. The curl URL (and any auth it needs) is
# whatever your private hosting provider uses; see that repo's
# own README.
curl <L1-url-from-your-kelasa-env-repo> \
  | bash -s -- --branch feature-build-layers

# L2 — pass --env-branch to pick a non-main env branch
curl -fsSL https://raw.githubusercontent.com/kusimari/env/feature-build-layers/layers/layer-2.sh \
  | bash -s -- --env-branch feature-build-layers
```

L5 pins workspaces and stores to their default branches via inline
`{ ... }` blocks in `layers/layer-5a.sh` (and
`desktop-layers/layer-5b.sh` on private machines). To test a
workspace or store feature branch, edit the relevant block in a
local checkout before running L5.

To check the flake without building: `./layers/test-flake.sh`.

---

## Shell-hook extension points — how Layer 3 hooks Layer 1 and Layer 4

Layer 3 is nix-managed zsh. Layers 1 and 4 are **not** nix-managed.
Two RC files bridge them: each is sourced by the nix-managed
zshenv (see `programs.zsh.envExtra` in `home/home.nix`), so Layer 1
and Layer 4 can inject shell state — PATH entries for externally-
installed binaries, aliases, env vars — into the Layer-3
environment without putting non-nixable content in the flake.

| File | Written by | Sourced | Use for |
|---|---|---|---|
| `~/.pre-nix-rc` | Layer 1 | `.zshenv` (early) | Reserved stub for future L1-time shell state. Empty today. |
| `~/.post-nix-rc` | Layer 4 | `.zshenv` + `.zlogin` + `.zshrc` | PATH / aliases that depend on Layer-3 nix artifacts. Re-sourced from later hooks so the OS-managed `.zprofile` cannot shadow earlier prepends. |

Writers must be idempotent — diff-check the intended content,
overwrite only on mismatch. PATH manipulation must use idempotent
prepend (case-guarded or move-to-front) so repeat sourcing across
the three hooks is a no-op. These files are the contract between
non-nixable work and the nixified shell. Don't put shell
initialization anywhere else.

---

## Tiered package model

Three tiers, read before adding packages. Full detail at the top of
`flake.nix` and in `env-verify.nix`.

| Tier | Where | How |
|---|---|---|
| **1** — nix-managed, every env | `home/home.nix` (packages list, `programs.*.enable`) | `pkgs.<name>` in the list, or a home-manager module. |
| **2** — every env, nix on some, external bootstrap on others | Same files as Tier 1, wrapped in `lib.optionals (<envKind-predicate>) [...]` | Predicate-admitted envs get the nix install; excluded envs must provide the same binary on PATH via their own post-install tooling. `nix run .#env-verify` checks. |
| **3** — per-env differences | `home/envKind-<name>.nix` (user-level) or `<envKind>Configuration` attrset in `flake.nix` (system-level) | Only the env(s) that want it see it. No verifier coverage. |

---

## Further reading

- `flake.nix` — entry point; tier-layering explained at the top.
- `env-verify.nix` — on-demand PATH check for tier-1 + tier-2
  invariants.
- `setup-notes.md` — operator cheat-sheet and post-install steps.
- `.kdevkit/feature/wip/` — active feature specs and design docs.

## Adding a new project folder

```bash
mkdir <folder> && cd <folder>
nix-init
# then a language-specific builder, or one of the nix-templates
```
