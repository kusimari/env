# env

Nix-managed home environment spanning Darwin, Ubuntu, and Amazon
Linux (2 / 2023). One flake, several envKinds.

---

## Layer design

Four layers, one job each. No chaining. Run in sequence on a fresh
machine; run only Layer 3 for day-2 rebuilds.

### The philosophy

- **Layer 1 — make the machine nix-ready, and clone non-nixable
  envKind repos.** Runs before nix exists, so everything here is
  bash + the OS's native package manager. Site-specific auth,
  sudoers, cert installs, and clones of the envKind-specific repos
  (for kelasa envKinds) live here. Public envKinds get their L1
  from `env`; private envKinds get it from their envKind repo.

- **Layer 2 — pull the nix-managed environment source.** Generic
  across all envKinds: no machine prep, no envKind assumptions.
  Clones `env` and `mAId` into `~/env-workplace/`, confirms GitHub
  SSH, pins commit identity, exits.

- **Layer 3 — build the nix environment.** `home-manager switch`
  or `nix-darwin switch` from the cloned source. Nixifies
  everything that belongs in nix. Exposes two extension points —
  `~/.pre-nix-rc` and `~/.post-nix-rc` — so Layer 1 and Layer 4
  can inject shell state that doesn't belong in the flake. See
  [Shell-hook extension points](#shell-hook-extension-points--how-layer-3-hooks-layer-1-and-layer-4)
  below.

- **Layer 4 — non-nixable post-install via the envKind repo.**
  Site-specific tool installs (via vendor tooling like `toolbox`,
  not nix), one-time setup commands, shell aliases keyed to
  non-nix binaries. Writes `~/.post-nix-rc`; never builds nix
  artifacts.

Why four distinct scripts, no chaining? L1 and L2 run rarely (new
machine, major env refresh). L3 runs often. L4 is out-of-band and
not always needed. Grouping them into an orchestrator would bundle
different change rates and risks. Separate scripts keep each
layer's scope obvious and debuggable alone.

### At a glance

| Layer | Script | Repo | Curl-able | Purpose |
|---|---|---|---|---|
| 1 | `bootstrap-<envKind>.sh` | `env` (public envKinds) or a `<kelasa-specific env repo>` | yes | Machine ready for nix |
| 2 | `build-nix/bootstrap-common.sh` | `env` | yes | env + mAId cloned |
| 3 | `build-nix/<envKind>.sh` | `env` | no | nix build |
| 4 | `post-nix-kelasa.sh` | `<kelasa-specific env repo>` | no | non-nixable post-install |

Day-2 rebuild: just run Layer 3.

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
curl <L1-url>/bootstrap-<envKind>.sh | bash

# Layer 2 — curl bootstrap-common.sh from env
curl -fsSL https://raw.githubusercontent.com/kusimari/env/main/build-nix/bootstrap-common.sh | bash

# Layer 3 — now that env is on disk, run the build directly
~/env-workplace/env/build-nix/<envKind>.sh

# Layer 4 (if your envKind has one) — standalone post-nix install
~/env-workplace/<kelasa-specific env repo>/desktop/post-nix-kelasa.sh
```

### Cloning or switching to a feature branch

Both L1 (kelasa) and L2 accept branch flags. On initial clone, the
clone targets that branch. On re-run, the scripts switch to the
named branch if the working tree is clean — they refuse to clobber
uncommitted changes, and prompt you to commit or stash first.

```bash
# L1 — pass --branch to check out a Gorantls-env feature branch
curl -fsSL -b ~/.midway/cookie \
  'https://code.amazon.com/packages/Gorantls-env/blobs/heads/feature-build-layers/--/desktop/bootstrap-<envKind>.sh?raw=1' \
  | bash -s -- --branch feature-build-layers

# L2 — pass --env-branch / --maid-branch to pick non-main branches
curl -fsSL https://raw.githubusercontent.com/kusimari/env/feature-build-layers/build-nix/bootstrap-common.sh \
  | bash -s -- --env-branch feature-build-layers --maid-branch feature-build-layers
```

To check the flake without building: `./build-nix/test.sh`.

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
| `~/.pre-nix-rc` | Layer 1 | `.zshenv` (early) | PATH / env needed before interactive shells fully start |
| `~/.post-nix-rc` | Layer 4 | `.zshenv` (after pre-nix-rc) | PATH / aliases that depend on Layer-3 nix artifacts |

Writers must be idempotent — diff-check the intended content,
overwrite only on mismatch. These files are the contract between
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
