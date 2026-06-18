# Feature: env-setup-layer-selection

## Git Setup

- Branch: `feat/env-setup-layer-selection`
- Base: `main`

Part of initiative: [[env-rebuild-separation]] (spec in the
envKind-specific private companion repo's `.kdevkit/initiative/`).

## Feature Brief

A single `layer-run` driver that rebuilds a machine's environment in one
run, with layer selection. Today the layers (L1–L7) are run by hand, one
script at a time, in the right order — easy to misorder or forget which
number does what. `layer-run --layer 1,2,3` runs the chosen layers in
order; with no `--layer`, it runs the always-on base + tooling (L1–L6).
It resolves the public and private layer scripts from two inputs the
caller supplies — the target (full machine key; envKind derived from it)
and an optional envKind-specific repo path — so the public driver names
no private repo yet drives its layers when present.

**Scope boundary — `layer-run` drives L1–L6; L0 and L7 are bookends.** Layer 0 is the
curl bootstrapper: the pre-clone, curl-able entrypoint that brings the
repos onto a brand-new machine (and on kelasa runs the L1 machine prep
before `env` exists). The existing clone-only `env-setup.sh` already
plays that role. `layer-run` only exists *after* L0 has put it on disk,
so it owns L1–L6 (day-2 rebuilds and post-bootstrap first runs); it does
not own L0 or first-run-L1-on-kelasa. This is the clean split for the
three planning open questions.

## Requirements

The user runs one command instead of a hand-ordered sequence of layer
scripts. Observable surface:

- **`layer-run --target <target> [--repo <path>] [--layer <list>] [--dry-run]`**
  runs the selected layers in ascending order.
- **`--target <target>`** (e.g. `al2023-kelasa`, `ubuntu-mane`) is
  **required and explicit** — no auto-detection. It is the full machine
  key; `envKind` is derived as the suffix after the last `-`. Resolves
  the per-machine layer scripts (L1/L3 by target, L4 by envKind).
- **`--repo <path>`** points at the envKind-specific companion repo that
  carries the private layers (L1-kelasa, L4, the private L5/L6, L7).
  Optional — when omitted/null, only the public layers run, and any
  selected layer that lives only in the private repo is reported as
  skipped (not an error).
- **`--layer <list>`** is a comma-list of layer numbers, e.g.
  `--layer 1,2,3` or `--layer 5`. No range shorthand. Valid values are
  **1–6**. Omitted → **all of L1–L6** (the always-on base env + tooling).
  **L0 and L7 are not runnable values** — L0 is the pre-clone curl
  bootstrap and L7 is per-project (owned by `project-workspace-tools`);
  both appear in the `--help` legend for orientation but are rejected
  with a pointer to the right tool if passed to `--layer`.
- **For a layer present in both repos (L5, L6), the env/public half runs
  before the envkind/private half.** When `--repo` is set, `layer-run`
  invokes the private half, which already chains the public half first
  (from Stream 1) — so env-before-envkind holds and the public half runs
  once. No `--repo` → the public half runs directly.
- **`--dry-run`** prints what each selected layer would do without
  changing anything. Forwarded to layers that honor `--dry-run`; layers
  that don't (L3, the nix switch) are **announced and skipped** under
  `--dry-run`, never run for real.
- **`--help`** prints a **layer-number → purpose** legend plus usage, so
  the caller never has to remember the numbering:
  - L0 = curl bootstrap *(external — pre-clone, not run here)*
  - L1 = make nix-ready · L2 = clone env gits · L3 = setup nix ·
    L4 = setup non-nix · L5 = get tools/stores · L6 = build tools
  - L7 = project workspaces *(per-project — use `project-workspace-tools`,
    not this driver)*
  L0 and L7 show in the legend for orientation but are rejected as
  `--layer` values, each with a one-line pointer to the right tool.
- Re-running any selection is safe: every layer is idempotent, so the
  driver does not track state or guard against re-execution.
- Output names each layer as it runs (number + purpose + which script),
  and a final summary of run / skipped / failed.

## Test Strategy

Per `project.md` Testing (bash Test Gate):

### Static (every change)
- `bash -n` on the new driver; `shellcheck` clean.

### Functional (dry-run / arg-parsing, user-observable)
- `--help` prints the full layer legend (L0–L7, with L0/L7 marked
  not-runnable) and usage.
- `--layer 2,3 --target ubuntu-mane --dry-run` runs exactly L2 then L3, in
  order, invoking neither L1 nor others.
- Bad input rejected: unknown layer number, malformed list, missing
  `--target`, non-`<platform>-<envKind>` target → clear error, non-zero
  exit.
- **`--layer 0` and `--layer 7` rejected** with a pointer to the right
  tool (L0 = curl bootstrap; L7 = `project-workspace-tools`), non-zero
  exit — not silently skipped.
- `--repo` omitted + a private-only layer selected (e.g. `--layer 4`) →
  reported skipped, exit stays 0 (or a documented "nothing to run" code).
- For L5/L6 with `--repo` set, the private half is invoked and the run
  shows public-then-private ordering (the S1 chain).
- Default (no `--layer`) → plan shows L1–L6 in order (never L0 or L7).
- Layer ordering: `--layer 6,2,5` executes as 2,5,6.

### Live-only (manual)
- Real run of a safe subset (e.g. `--layer 5` on a machine with the
  companion repo) clones get-only; re-run is a no-op fetch.

## Design

**Rationale / what's reused.** The hard work already lives in the layer
scripts; this driver is pure orchestration — resolve which scripts to
run from (envKind, repo, layer-list), then exec them in order, forwarding
`--dry-run`. It leans on the **idempotency contract** every layer already
honors (re-running is safe), so the driver holds no state and needs no
re-run guard. It mirrors the existing argument-parsing idiom in the layer
scripts (`while/case`, `--help` via the `END-USAGE` awk banner, a `run`
wrapper for dry-run) rather than introducing a new style.

**Two inputs, no hardcoded private name.** The driver takes `--target`
and `--repo`. The public layers resolve relative to the driver's own
location (it ships at the `env/` root); the private layers resolve under
`--repo`. With no `--repo`, private layers are skipped. This is the
"convention, not hardcoded names" contract from the initiative — the
public source names no private repo; the caller supplies the path.

**Layer registry.** A small in-script table maps each layer number to:
its purpose string, scope (public / private / both), and whether it
honors `--dry-run`. `--help` renders this table as the legend.
Resolution per selected layer:

- Public-only (L2, L3): under the driver's repo (`layers/…`). L3
  substitutes `<target>`.
- Private-only (L4): under `--repo`; skipped if no `--repo`.
- Both (L1, L5, L6): L1 is public for the mane target, private for kelasa
  envKinds — resolved by which file exists for the target. (On a *fresh*
  kelasa machine, L1 is run by L0 pre-clone; `layer-run` driving L1 is a
  `mane` / day-2 path. L1 is idempotent, so a day-2 re-run via the driver
  is safe.) For L5/L6: when `--repo` is set, run the **private half**
  (it already chains the public half first — verified S1); else run the
  **public half** directly.
- Not driven: L0 (pre-clone bootstrap) and L7 (per-project; reached only
  through `project-workspace-tools` / `workplace-setup.sh`, which takes
  the project from cwd — there is no "run all projects" step by design).

**Public-before-private — reuse S1's chain, no S1 change.** The rule
"env half before envkind half" is already guaranteed by Stream 1: the
private L5/L6 self-chain the public half (private invocation runs
public-then-private). So `layer-run` honors the rule for free — for a
both-repos layer with `--repo` set, it invokes only the private half and
the chain orders it correctly; the public half runs exactly once. No
edit to the S1-shipped private scripts. (Verified by dry-run: private L5
logs "Chaining public driver" → "Layer 5 (public)" → "Layer 5
(private)".)

**Ordering.** Selected numbers are sorted ascending before execution,
so `--layer 6,2` runs 2 then 6. Within a layer that has public+private
halves, the private half already chains the public (per Stream 1), so the
driver invokes one entry-point per layer, not two.

**Failure handling.** A layer exiting non-zero stops the run by default
(later layers depend on earlier ones) and the summary marks it failed;
`--dry-run` never fails on layer work. (Whether to add a `--keep-going`
is deferred — see open questions.)

### Resolved design questions (were open at planning)

- **Driver name + location** → **`layer-run`**, shipping in the public
  `env` repo (alongside `layers/`, exact path settled in slice 1). It
  does **not** converge with the existing clone-only `env-setup.sh` —
  that script is **L0** (see below), a different layer.
- **envKind** → **always explicit** (`--envkind` required), no
  hostname/uname auto-detection.
- **L0 = the curl bootstrapper.** The pre-clone, curl-able entrypoint
  (today's clone-only `env-setup.sh`) becomes Layer 0: it gets the repos
  onto a fresh machine and, on kelasa, runs L1 machine prep before `env`
  exists. `layer-run` drives L1–L6 only; it never owns L0, L7, or first-run
  L1-on-kelasa. Clean split — `layer-run` is a day-2 + post-bootstrap
  driver, L0 is the from-nothing bootstrap. (L0 formalization beyond
  naming it in the legend is out of scope for this feature; tracked as a
  follow-up if the existing script needs renaming/moving.)

## Implementation Plan

- [x] Naming-contract tightening (slice 0, folded in per user): define
      envKind / platform / **target** in `project.md`; fix L1/L3 docs to
      `<target>`, L4 stays `<envKind>`; reframe table as L0–L7. Files
      already complied — docs were the loose part.
- [x] Scaffold `layer-run` driver: arg parser (`--target`, `--repo`,
      `--layer`, `--dry-run`, `--help`) in the layer-script idiom.
      (Dropped `--envkind` for `--target`; dropped reserved `--yes`.)
- [x] Layer registry table (number → purpose, scope, dry-run capability)
      + `--help` legend rendering (L0/L7 shown as not-runnable).
- [x] Resolution logic: public vs private vs both; `<target>` for L1/L3,
      `<envKind>` for L4; null-repo skip path; reject `--layer 0` and
      `--layer 7` with pointers; for L5/L6 invoke the private half when
      `--repo` set (reuses S1 chain).
- [x] Ordered execution with `--dry-run` forwarding (gated on per-layer
      capability — L3 announced-not-run) + run/skip/fail summary.
- [x] Static gate (`bash -n`, `shellcheck` clean) + functional dry-run
      tests per Test Strategy (all pass).
- [x] Update `README.md` + `.kdevkit/project.md`: document `layer-run`
      as the one-run rebuild entrypoint (Day-2 flows table + fresh-
      machine commands), and name L0 as the curl bootstrap step.
      Both done; README reframed to L0–L7, target/envKind contract,
      `layer-run` in At-a-glance + Day-2 + fresh-machine.

- *Risk note:* public-repo hygiene — the driver and its `--help` text
  must name no private repo; refer to it only via `--repo`/placeholders.
- *Risk note:* `layer-run` owns L1–L6 only; L0 (curl bootstrap) and
  first-run L1-on-kelasa are out of scope by design.
- *Risk note:* this is S2; it builds on S1's clean L5/L6 split, already
  on `main`. No cross-stream rebase needed (S1 shipped).

## Session Log

<!-- append: date · what was done · decisions made -->

- 2026-06-18 · Planning. S2 of env-rebuild-separation, started after S1
  shipped to `main`. Decided with user: driver takes `--envkind` + a
  nullable `--repo` (no scanning / no marker files); `--layer` is a
  comma-list only (no ranges); default = L1–L6; `--help` must show the
  layer-number→purpose legend; rely on layer idempotency rather than
  re-run guards.
- 2026-06-18 · Planning round 2 (user resolved the 3 open questions):
  (1) driver name = **`layer-run`**; (2) `--envkind` **always explicit**,
  no auto-detect; (3) introduce **Layer 0** = the curl bootstrapper
  (today's clone-only `env-setup.sh`). `layer-run` drives L1–L6 only; L0 and L7
  owns from-nothing bootstrap + first-run L1-on-kelasa. This is a model
  refinement — propagate L0 into the seven-layer docs at closure (the
  model is now L0–L7).
- 2026-06-18 · Dev. Built `env/layer-run` (registry + resolution +
  ordered exec). Two findings during build, both folded in: (a) layer
  scripts name by **two axes** — L1/L3 by full `target`, L4 by
  `envKind` — so the flag became `--target` (envKind derived as suffix);
  added the naming contract to `project.md` (slice 0). (b) **L3 has no
  `--dry-run`** and would run a real nix switch if forwarded — added a
  per-layer `LAYER_DRYRUN` capability map; non-capable layers are
  announced-not-run under `--dry-run`. Quality + Test gates green
  (shellcheck clean; ordering, rejection, null-repo skip, repo-resolve,
  idempotency all verified). Note: L1-mane path can't be fully exercised
  from a kelasa box (its platform guard exits — correct behavior).
- 2026-06-18 · Planning round 3 (user): (1) L7 is **not** a runnable
  layer — `layer-run` drives L1–L6; L7 only via `project-workspace-tools`
  (no "run all projects"). (2) For both-repos layers, env runs before
  envkind — confirmed this already holds via S1's private→public chain,
  so no S1 edit (checked: `--repo`-set kelasa path runs public-then-
  private). Driver now owns the middle (L1–L6); L0 and L7 are bookends.

## Decision Log

<!-- append: decision · rationale · alternatives rejected -->

- **Private layers resolved via an explicit `--repo` path, not
  discovery.** Rationale (user): caller passes envKind + the
  envKind-specific repo path (nullable); simpler and more predictable
  than scanning `~/env-workplace/*` or a marker-file convention, and
  still hardcodes no private name in the public source. Alternatives
  rejected: sibling-dir scan (implicit, surprising); marker manifest
  (new convention to maintain).
- **`--layer` is a comma-list only; default L1–L6.** Rationale (user):
  simpler parser; ranges add little for seven layers. L7 excluded from
  default because it is per-project / on-demand (initiative contract).
- **Driver relies on layer idempotency, holds no state.** Rationale
  (user): every layer is already idempotent, so re-running a selection
  is safe; a stateful driver would duplicate that guarantee. `--help`
  carries the layer legend because the numbering isn't memorable.
- **Driver name = `layer-run`** (planning round 2, user). Plain verb on
  the noun; no collision with the L0 `env-setup.sh` bootstrapper.
- **`--envkind` always explicit** (user). No hostname/uname detection —
  avoids a wrong-envKind rebuild from a misfiring detector; the caller
  always states intent. Alternative rejected: auto-detect with override.
- **Layer 0 = the curl bootstrapper** (user). Formalizes the pre-clone,
  curl-able step (today's clone-only `env-setup.sh`) as L0, distinct
  from `layer-run`'s L1–L7. Resolves the driver-convergence and
  first-run-L1-on-kelasa questions by *not* merging them: L0 bootstraps
  from nothing (incl. kelasa L1 machine prep), `layer-run` takes over
  once on disk. The seven-layer model becomes **L0–L7**; doc propagation
  happens at closure (§8.2 touches Layout/Testing across both repos).
- **`layer-run` drives L1–L6 only; L7 is not runnable** (planning round
  3, user). L7 is per-project and selective — a "run L7" with no project
  argument could only mean "set up all projects," which is the
  bulk-project-install anti-pattern the model forbids. L7 is reached only
  through `project-workspace-tools` / `workplace-setup.sh` (project from
  cwd). L0 and L7 are legend-only in `--help`; passing either to
  `--layer` is a hard error with a pointer. (So the driver owns the
  middle of the stack; L0 and L7 are the bookends owned by other tools.)
- **env-before-envkind reuses S1's chain — no S1 edit** (planning round
  3, user). The rule holds automatically: the private L5/L6 already
  chain the public half first (verified by dry-run on the shipped S1
  scripts), so `layer-run` invokes only the private half when `--repo`
  is set. Considered de-chaining the private scripts so the runner owns
  ordering explicitly; rejected — it would churn just-shipped S1 code
  and change stand-alone private-script behavior for no real gain.
- **Flag is `--target`, not `--envkind`** (build finding). Layer scripts
  name by two axes — L1/L3 by full target (`al2023-kelasa`), L4 by
  envKind (`kelasa`). `--envkind` alone can't resolve L1/L3, so the
  driver takes the full `target` and derives `envKind` as the suffix
  after the last `-`. Codified the envKind/platform/target naming
  contract in `project.md`. Alternative rejected: two flags
  (`--target` + `--envkind`) — redundant, target implies envKind.
- **Per-layer `--dry-run` capability map** (build finding). L3 (the nix
  switch) ignores args and has no `--dry-run`; blindly forwarding it
  would run a real `home-manager`/`nix-darwin switch` under a dry-run.
  The driver carries a `LAYER_DRYRUN` map; non-capable layers are
  announced (and skipped) under `--dry-run`, never executed. Keeps
  `layer-run --dry-run` actually safe.
- **Dropped `--yes`** (reserved-but-unused) — YAGNI; add when an
  interactive prompt actually exists to gate. shellcheck SC2034 too.
