# feature-build-layers — Iteration 1 (landed)

## Status

✅ **Iteration 1 complete.** Four-layer bootstrap design shipped
across `env` (public) and a private kelasa env repo (private
envKinds). Iter 2 (`mAId`) continues in
`.kdevkit/feature/wip/feature-build-layers.md` — that doc
references this one for the layered context it now builds on.

This file is the durable record of what Iteration 1 delivered. It
doesn't re-derive design; see §2 for the final layer model and §5
for the session log covering the evolution.

---

## 1 · What Iter 1 delivered

Four distinct, single-purpose scripts, no chaining between them.
User runs them in sequence on a fresh machine, or just Layer 3 for
day-2 rebuilds.

### Layer 1 — machine prep (envKind-specific)

Makes the machine nix-ready, and clones any non-nixable envKind
repos that later layers reference. Runs before nix exists, so
bash-only, using the OS's native package manager.

- `env/build-nix/bootstrap-ubuntu-mane.sh` — apt prereqs +
  Determinate Nix installer. Public envKind.
- `<kelasa-env-repo>/desktop/bootstrap-al2-kelasa.sh` — corp SSH +
  auth check, self-clone the kelasa env repo, vendor-tool install,
  single-user Nix install (auth-sync-safe), `/etc/nix/nix.conf`,
  chsh to zsh, idempotent `~/.pre-nix-rc` writer.
- `<kelasa-env-repo>/desktop/bootstrap-al2023-kelasa.sh` — same
  pattern as al2-kelasa; AL2023-specific SSL cert path, sudoers
  NOPASSWD for `/nix` creation, no chsh step (AL2023 ships with
  zsh).
- `<kelasa-env-repo>/desktop/bootstrap-darwin-kelasa.sh` — minimal:
  require_nix, self-clone the kelasa env repo, pin git identity.
  brew is NOT required here (nix-darwin manages it via
  `homebrew.enable`).

Every L1 script:
- Is curl-able and clone-runnable.
- Accepts `--dry-run` (mutation-free) and `--help`.
- Accepts `--branch NAME` and `--branch=NAME` on kelasa L1s to
  clone or switch the kelasa env repo checkout to a feature branch;
  refuses to clobber uncommitted changes.
- Is step-level idempotent (`ensure_*` helpers verify-and-skip).
- Pins commit identity on the clone it makes — kelasa L1s set a
  corp identity on the kelasa env repo; L2 sets
  `kusimari <kusimari@gmail.com>` on env + mAId. Local config
  only; global git config is never touched.

### Layer 2 — generic sync

Pulls the nix-managed environment source. Generic across all
envKinds: no machine prep, no envKind assumptions.

- `env/build-nix/bootstrap-common.sh` — creates `~/env-workplace/`,
  checks GitHub SSH reachability (generates key + prints "add to
  GitHub" instruction if unreachable), clones or fetches `env`
  and `mAId`, pins git identity on both, exits. Curl-mode and
  clone-mode auto-detected via `$ENV_CLONE/.git` presence.
- `--env-branch NAME` / `--maid-branch NAME` + `=VALUE` variants
  switch (or clone at) a specific branch with the same uncommitted-
  changes guard as L1.
- `--dry-run`, `--help`.

### Layer 3 — nix build

`home-manager switch` or `nix-darwin switch`. Nixifies everything
that belongs in nix.

- `env/build-nix/ubuntu-mane.sh`
- `env/build-nix/darwin-kelasa.sh`
- `env/build-nix/al2-kelasa.sh`
- `env/build-nix/al2023-kelasa.sh`
- `env/build-nix/_common.sh` — shared body: placeholder swap →
  `eval NIX_COMMAND` → cleanup stray `.#*` template dirs →
  restore placeholders → print `setup-notes.md`. ~55 lines after
  the pre/post-nix `$1`/`$2` plumbing was removed (Iter-1 design
  had that, the decoupled final design doesn't need it).

Each `<envKind>.sh` is a thin wrapper that exports
`NIX_COMMAND` + `NIX_ECHO_MESSAGE` (+ `SED_INPLACE_FLAG` on
darwin) and sources `_common.sh`. No args.

L3 exposes two shell-hook extension points
(`~/.pre-nix-rc`, `~/.post-nix-rc`); see §3.

### Layer 4 — non-nixable post-install (envKind-specific)

Runs any time after nix exists. Installs tools via whatever vendor
tooling the kelasa env needs; writes `~/.post-nix-rc` with PATH /
aliases that depend on L3 artifacts.

- `<kelasa-env-repo>/desktop/post-nix-kelasa.sh` — plain bash script
  (no nix flake, no `nix run` indirection). Checks the vendor tool
  is present, conflict-checks each corp-tool entry against non-
  vendor installs (resolving symlinks), installs missing tools via
  the vendor tool, runs its setup hook, writes `~/.post-nix-rc`.
- Hard-fails on any error. No silent `|| echo warning` swallowing.
- Shared across all kelasa envKinds (AL2 / AL2023 / darwin —
  the vendor tool abstracts OS differences).

---

## 2 · The layer model

Source of truth for envKind names: `flake.nix`.
(`ubuntu-mane` / `darwin-kelasa` / `al2-kelasa` / `al2023-kelasa`.)

| Layer | Script | Repo | Curl-able | Purpose |
|---|---|---|---|---|
| 1 | `bootstrap-<envKind>.sh` | `env` (public envKinds) or `<kelasa-specific env repo>` | yes | Machine ready for nix |
| 2 | `build-nix/bootstrap-common.sh` | `env` | yes | env + mAId cloned |
| 3 | `build-nix/<envKind>.sh` | `env` | no | nix build |
| 4 | `post-nix-kelasa.sh` | `<kelasa-specific env repo>` | no | non-nixable post-install |

Why separate scripts, no orchestrator? L1 and L2 run rarely (new
machine, major env refresh). L3 runs often. L4 is out-of-band and
not always needed. Grouping them into a runner would bundle
different change rates and risks. Separate scripts keep each
layer's scope obvious and debuggable alone.

The env `README.md` carries the user-facing version of this
philosophy paragraph plus the envKind table.

---

## 3 · Shell-hook extension points

Layer 3 is nix-managed zsh. Layers 1 and 4 are not. Two RC files
bridge them, sourced by `programs.zsh.envExtra` in
`env/home/home.nix`:

| File | Written by | When | Use for |
|---|---|---|---|
| `~/.pre-nix-rc` | Layer 1 | Before L3 runs | PATH / env needed by later layers or interactive shells |
| `~/.post-nix-rc` | Layer 4 | After L3 artifacts exist | PATH / aliases that depend on L3-built binaries |

Writers must be idempotent (diff-check intended content,
overwrite on mismatch). These files are the contract between
non-nixable setup and the nixified shell. Layer 3 doesn't
know what's in them — it just sources them.

---

## 4 · Files delivered

**env** (`feature-build-layers` branch, merged-to-main pending)
- `build-nix/bootstrap-common.sh` (new)
- `build-nix/bootstrap-ubuntu-mane.sh` (new)
- `build-nix/ubuntu-mane.sh` (renamed from `ubuntu.sh`)
- `build-nix/darwin-kelasa.sh` (renamed from `darwin.sh`)
- `build-nix/al2-kelasa.sh` (renamed from `al2.sh`)
- `build-nix/al2023-kelasa.sh` (renamed from `al2023.sh`)
- `build-nix/_common.sh` (trimmed: pre/post-nix plumbing removed)
- `build-nix/al2-fix-ssl.sh` (unchanged — generic utility)
- `build-nix/test.sh` (unchanged — references the new names
  already worked)
- `README.md` (rewritten: philosophy-first layer design,
  envKinds, branch flags, shell-hook extension points, tiered
  package model)
- `setup-notes.md` (back to short list of truly-manual items)
- `flake.nix` (added `doCheck = false` overlay for direnv after
  aarch64-darwin test-suite hang)

**Kelasa env repo** (`feature-build-layers` branch, merged-to-
mainline pending; site-specific prep lives here, not in this
public repo)
- `desktop/bootstrap-al2-kelasa.sh` (renamed + refactored)
- `desktop/bootstrap-al2023-kelasa.sh` (renamed + refactored)
- `desktop/bootstrap-darwin-kelasa.sh` (new)
- `desktop/post-nix-kelasa.sh` (new, bash)
- `desktop/pre-nix.sh` (deleted — job absorbed into L1 scripts)
- `desktop/post-nix/` directory (deleted — flake + wrapper both
  gone)
- `desktop/post-nix-run.sh` (deleted — the `nix run` wrapper)

---

## 5 · Guardrails

- **NFR5 — public-repo scrub.** No site / employer / internal-tool
  identifiers in `env`. Only abstract references ("kelasa-specific
  env repo"), OS names ("Amazon Linux"), and repo owners
  (`kusimari` for GitHub URLs) — all acceptable.
- **Idempotency.** Every bootstrap script is step-level verify-
  and-skip. Re-runs are no-ops on a provisioned machine.
- **Shellcheck-clean.** Every script passes `shellcheck -x`.
- **Dry-run is mutation-free.** L1, L2, L4 all support `--dry-run`
  that logs planned actions without touching disk.
- **Hard-fail on real errors.** L4 stopped swallowing errors with
  `|| echo warning` — genuine failures now surface.
- **Commit identity pinning.** L1 pins a corp identity on the
  kelasa env repo; L2 pins `kusimari <kusimari@gmail.com>` on
  env + mAId. Local git config only. Prevents dev-desktop
  hostname leaks into commit metadata of public repos.

---

## 6 · Session log (Iter 1)

<!-- Newest at top. Iter-2 log lives in the WIP doc. -->

### 2026-05-07 - L4 de-nixified; README philosophy-first
- The L4 flake in the kelasa env repo (`desktop/post-nix/flake.nix`)
  was removed along with its `post-nix-run.sh` wrapper. Replaced
  by a plain bash script `desktop/post-nix-kelasa.sh`. Rationale:
  the flake had zero nix dependencies at runtime — it shells out
  to the vendor tool for every install, and its `installScript`
  body was already pure bash templated through `writeShellScript`.
  The `forAllSystems` wrapping and `packages.default` buildEnv
  were ceremony nothing consumed. Collapsing to bash loses zero
  functionality and removes the nixpkgs-unstable input dependency.
- New script: same corp-tool list, same `~/.post-nix-rc` content,
  same conflict-check with `readlink -f` trick. Key behavior
  change per user direction: **hard-fail instead of warn-and-
  continue** on vendor install / setup errors. The flake swallowed
  errors with `|| echo warning`; that's gone. `set -euo pipefail`
  + each command under `run` surfaces genuine failures. Shared
  across all kelasa envKinds for now (AL2/AL2023/darwin all use
  the same vendor tool, no OS branches needed in the body); split
  later if they diverge.
- `env/README.md` Layer-design section rewritten to lead with the
  philosophy instead of the mechanical script list. Four short
  paragraphs describe L1/L2/L3/L4's *responsibility and why*, plus
  a "why separate scripts" rationale about different change rates
  (L1/L2 rare, L3 frequent, L4 out-of-band). The Layer table stays,
  now under "At a glance" heading. Other README sections
  (envKinds, branch flags, shell-hook extension points, tiered
  packages, further reading) unchanged.
- Verification on this AL2023 host: shellcheck clean, `--help`
  and `--dry-run` both render correctly, unknown args error
  cleanly.

### 2026-05-06 - Iter 1 refinement: decouple the four layers
- **No chaining.** Each layer is a distinct, single-purpose script
  that exits when its job is done. User runs four commands in
  sequence on a fresh machine, or just Layer 3 for day-2 rebuilds.
  Runner/orchestrator deferred until a concrete need appears.
- Trimmed `env/build-nix/bootstrap-common.sh` to L2-only: removed
  `--envKind`, `--pre-nix`, `--post-nix`, `--post-clone`,
  `invoke_layer_3`, `print_next_steps`, and the curl-mode re-exec
  into Layer 3. Still curl-able and clone-runnable; auto-detects
  both modes. Keeps `--dry-run`, `--help`, mode detection,
  `ensure_workspace`, `ensure_github_ssh`, `clone_or_fetch`.
- Trimmed all four Layer-1 scripts (`bootstrap-ubuntu-mane.sh`,
  `bootstrap-al2-kelasa.sh`, `bootstrap-al2023-kelasa.sh`,
  `bootstrap-darwin-kelasa.sh`): removed `ensure_envkind_default`,
  `hand_off_to_layer_2`, and their invocations. Each script ends
  cleanly after its last `ensure_*` step with a "Layer 1 done"
  log.
- Trimmed `env/build-nix/_common.sh`: removed the pre-nix/post-nix
  `$1`/`$2` source-around-nix plumbing. From 98 → ~55 lines; flow
  is now: replace placeholders → cd flake → eval `NIX_COMMAND` →
  clean stray template dirs → restore placeholders → print
  setup-notes. Refactored the two placeholder-sed blocks into a
  single `replace_placeholders` helper.
- Dropped `"$@"` from each Layer-3 script's
  `source _common.sh` line. L3 scripts now take no args.
- Rewrote `env/README.md` around decoupled layers.
- Added `--branch` / `--env-branch` / `--maid-branch` flags to L1
  (kelasa) and L2. On initial clone, cloned at that branch. On
  re-run, switches to it if the working tree is clean; refuses
  to clobber uncommitted changes (forces commit or stash first).

### 2026-05-06 - Iter 1 refactor: envKind-aligned naming + kelasa split
- Source of truth for names is `flake.nix`. All layers renamed to
  the full envKind:
  - Layer 3: `build-nix/{ubuntu,darwin,al2,al2023}.sh` →
    `build-nix/{ubuntu-mane,darwin-kelasa,al2-kelasa,al2023-kelasa}.sh`
    (via `git mv`; content unchanged).
  - Layer 1 public: `build-nix/bootstrap-ubuntu.sh` →
    `build-nix/bootstrap-ubuntu-mane.sh`.
- Kelasa Layer 1 moved to the kelasa env repo:
  - `desktop/bootstrap-al2.sh` → `bootstrap-al2-kelasa.sh`.
  - `desktop/bootstrap-al2023.sh` → `bootstrap-al2023-kelasa.sh`.
  - New `desktop/bootstrap-darwin-kelasa.sh` (minimal).
  - Each: dropped generic steps (workspace, GitHub SSH, env clone)
    now owned by Layer 2; kept kelasa-specific (corp SSH/auth,
    self-clone, vendor tool install, sudoers [al2023], single-
    user Nix, nix.conf, zsh/chsh [al2], `~/.pre-nix-rc` writer).
  - Deleted `desktop/pre-nix.sh` — its job (write `~/.pre-nix-rc`
    with vendor-tool PATH) now lives in each AL bootstrap as an
    idempotent `ensure_pre_nix_rc` step.
- Code review (two passes). Fixed: `post-nix-run.sh` used `exec`
  inside a script that gets `source`d from `_common.sh:69` —
  would've killed the caller. Now plain `nix run` (since removed).
  AL2 vendor-tool install: `-fsS` on curl so HTML error pages
  don't become the Authorization header; explicit cleanup in
  error branches instead of RETURN trap. Wrapped `mkdir`/`chmod`
  on `~/.ssh` and `~/.config/nix/` in `run` so `--dry-run` is
  truly mutation-free.

### 2026-05-06 - Iteration 1 landed: four-layer bootstrap
- Created `env/build-nix/bootstrap-common.sh` (Layer 2). Initial
  version carried `--envKind` dispatch and `--pre-nix`/`--post-nix`
  hook forwarding (dropped in the decoupling refinement later
  that day).
- Created `env/build-nix/bootstrap-ubuntu.sh` (Layer 1 Ubuntu;
  renamed to `bootstrap-ubuntu-mane.sh` the next day).
- Idempotent dry-run verified: two runs against a staging HOME
  produce byte-identical output; the HOME stays untouched (only
  tmpdir remains).

### 2026-05-05 - Spec/design/impl/test plan locked
- Restructured the original WIP doc into spec / design / skills /
  implementation / test plan. Locked design decisions: Deno
  runtime for mAId (not nix for transforms), minimal flake
  wrapper, link-vs-build mode via frontmatter, three source types
  (original / external / patch), central registry, intent routing
  lives in top-level `CLAUDE.md` (not as a skill), `inbox/`
  committed, darwin Layer-1 skipped (at the time — later
  superseded by `bootstrap-darwin-kelasa.sh`).

### 2026-04-30 - Scrub site-specific references
- Removed employer-specific names, internal tool names, and
  specific org/user handles from the WIP doc. Replaced with
  abstract references ("kelasa-specific env repo",
  "site-specific auth"). Added NFR5 to make the policy explicit.
  Concrete clone URLs stay in the bootstrap scripts, not in
  design docs.

### 2026-04-30 - Spun out from misc-updates
- Split bootstrap + mAId work out of `misc-updates` into this
  feature doc. The mAId sibling repo was already cloned empty at
  `~/env-workplace/mAId`; work began on `feature-build-layers`.

---

## 7 · Follow-ups for Iter 2 (mAId)

Continues in `.kdevkit/feature/wip/feature-build-layers.md`.
Iter 2 adds the `maid` CLI that maps generic skill/agent/command/
MCP sources to per-tool paths under `$HOME` via symlinks. Layer 2
already clones `mAId` alongside `env`, so the sync machinery is in
place — Iter 2 only fills in the CLI and the source tree.

Four open items flagged at Iter-1 close, to decide during Iter 2:
intent keyword list seeding, symlink relative-vs-absolute, inbox
session-log lifecycle, writing-style.md inline draft timing.
