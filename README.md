# env

Nix-managed home environment spanning Darwin, Ubuntu, and Amazon
Linux (2 / 2023). One flake, several envKinds.

---

## Layer design

Five layers, one job each. No chaining. Run in sequence on a fresh
machine; run only Layer 3 for day-2 rebuilds of the base env, and
Layer 5 when AI tooling needs refreshing.

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
  from the cloned source, followed by `build-nix/post-nix-common.sh`.
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

- **Layer 5 — fast-moving updates on top of the base env.** A small
  workspace-registry framework. L5 drivers (`layer-5/run` in `env`
  and the envKind repo) iterate a registry of `{ name, repo-url,
  entry-point }` entries. For each entry they clone/fetch the repo
  into `~/workplace/<name>/<repo>/`, pin git identity, and hand
  off to the repo's own `install` entry-point. The driver itself
  never builds content — each workspace repo owns its own install.
  L5 is the home for things that change faster than the base env
  and aren't (yet) worth nix-managing. When a workspace hardens
  enough, it can graduate into L3 (nix-managed) or L4 (non-nix).

Why five distinct scripts, no chaining? L1 and L2 run rarely (new
machine, major env refresh). L3 runs often. L4 is out-of-band and
not always needed. L5 runs whenever fast-moving workspaces need a
refresh, independently of the base env. Grouping them into an
orchestrator would bundle different change rates and risks.
Separate scripts keep each layer's scope obvious and debuggable
alone.

### At a glance

| Layer | Script | Repo | Curl-able | Purpose |
|---|---|---|---|---|
| 1 | `bootstrap-<envKind>.sh` | `env` (public envKinds) or a `<kelasa-specific env repo>` | yes | Machine ready for nix |
| 2 | `build-nix/bootstrap-common.sh` | `env` | yes | env cloned |
| 3 | `build-nix/<envKind>.sh` → `build-nix/post-nix-common.sh` | `env` | no | nix build + universal post-nix nudges |
| 4 | `post-nix-kelasa.sh` | `<kelasa-specific env repo>` | no | envKind-specific non-nixable post-install |
| 5 | `layer-5/run` | `env` (public) + `<kelasa-specific env repo>` (private) | no | Clone workspace repos; hand off to each repo's own `install` |

Day-2 rebuild of the base env: just run Layer 3.
Day-2 refresh of AI tooling (or other workspaces): just run Layer 5.

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

# Layer 5 — clone workspace repos and let them self-install.
# Use the private driver on kelasa machines; it runs the public
# driver first.
~/env-workplace/env/layer-5/run                               # public-only
~/env-workplace/<kelasa-specific env repo>/layer-5/run        # private; chains public first
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
curl -fsSL https://raw.githubusercontent.com/kusimari/env/feature-build-layers/build-nix/bootstrap-common.sh \
  | bash -s -- --env-branch feature-build-layers
```

L5 pins workspace repos to their default branches from the
registry embedded in `layer-5/run`. To test a workspace feature
branch, edit the registry in a local checkout before running L5.

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
