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
caller supplies — the envKind and an optional envKind-specific repo path
— so the public driver names no private repo yet drives its layers when
present.

**Scope boundary — `layer-run` drives L1–L7, not L0.** Layer 0 is the
curl bootstrapper: the pre-clone, curl-able entrypoint that brings the
repos onto a brand-new machine (and on kelasa runs the L1 machine prep
before `env` exists). The existing clone-only `env-setup.sh` already
plays that role. `layer-run` only exists *after* L0 has put it on disk,
so it owns L1–L7 (day-2 rebuilds and post-bootstrap first runs); it does
not own L0 or first-run-L1-on-kelasa. This is the clean split for the
three planning open questions.

## Requirements

The user runs one command instead of a hand-ordered sequence of layer
scripts. Observable surface:

- **`layer-run --envkind <kind> [--repo <path>] [--layer <list>] [--dry-run] [--yes]`**
  runs the selected layers in ascending order.
- **`--envkind <kind>`** (e.g. `mane`, `kelasa`) is **required and
  explicit** — no auto-detection. Selects which per-envKind layer
  scripts run (L1, L3, L4 are envKind-specific).
- **`--repo <path>`** points at the envKind-specific companion repo that
  carries the private layers (L1-kelasa, L4, the private L5/L6, L7).
  Optional — when omitted/null, only the public layers run, and any
  selected layer that lives only in the private repo is reported as
  skipped (not an error).
- **`--layer <list>`** is a comma-list of layer numbers, e.g.
  `--layer 1,2,3` or `--layer 5`. No range shorthand. Omitted →
  **L1–L6** (the always-on base env + tooling). L7 never runs unless
  explicitly listed — it is per-project and on-demand.
- **`--dry-run`** prints what each selected layer would do without
  changing anything (forwarded to each layer script, which already
  supports it).
- **`--help`** prints a **layer-number → purpose** legend (L0 = curl
  bootstrap [external — not run by this driver], L1 = make nix-ready,
  L2 = clone env gits, … L7 = project workspaces) plus usage, so the
  caller never has to remember the numbering. L0 appears in the legend
  for orientation but is rejected as a `--layer` value (it predates the
  driver).
- Re-running any selection is safe: every layer is idempotent, so the
  driver does not track state or guard against re-execution.
- Output names each layer as it runs (number + purpose + which script),
  and a final summary of run / skipped / failed.

## Test Strategy

Per `project.md` Testing (bash Test Gate):

### Static (every change)
- `bash -n` on the new driver; `shellcheck` clean.

### Functional (dry-run / arg-parsing, user-observable)
- `--help` prints the full layer legend (L1–L7) and usage.
- `--layer 2,3 --envkind mane --dry-run` runs exactly L2 then L3, in
  order, invoking neither L1 nor others.
- Bad input rejected: unknown layer number, malformed list, missing
  `--envkind` → clear error, non-zero exit.
- `--repo` omitted + a private-only layer selected (e.g. `--layer 4`) →
  reported skipped, exit stays 0 (or a documented "nothing to run" code).
- Default (no `--layer`) → plan shows L1–L6 in order, not L7.
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

**Two inputs, no hardcoded private name.** The driver takes `--envkind`
and `--repo`. The public layers resolve relative to the driver's own
location (it ships in `env/`); the private layers resolve under
`--repo`. With no `--repo`, private layers are skipped. This is the
"convention, not hardcoded names" contract from the initiative — the
public source names no private repo; the caller supplies the path.

**Layer registry.** A small in-script table maps each layer number to:
its purpose string, whether it is public / private / both, and the
script path template (with `<envKind>` substituted). `--help` renders
this table as the legend. Resolution per selected layer:

- Public-only (L2, L3): under the driver's repo (`layers/…`). L3
  substitutes `<envKind>`.
- Private-only (L4, L7): under `--repo`; skipped if no `--repo`.
- Both (L1, L5, L6): L1 is public for `mane`, private for kelasa
  envKinds — resolved by which file exists for the envKind. (On a *fresh*
  kelasa machine, L1 is run by L0 pre-clone; `layer-run` driving L1 is a
  `mane` / day-2 path. L1 is idempotent, so a day-2 re-run via the driver
  is safe.) L5/L6 have a public and a private half; when `--repo` is set,
  run the private half (it chains the public), else run the public half
  directly.

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
  exists. `layer-run` drives L1–L7 only; it never owns L0 or first-run
  L1-on-kelasa. Clean split — `layer-run` is a day-2 + post-bootstrap
  driver, L0 is the from-nothing bootstrap. (L0 formalization beyond
  naming it in the legend is out of scope for this feature; tracked as a
  follow-up if the existing script needs renaming/moving.)

## Implementation Plan

- [ ] Scaffold `layer-run` driver: arg parser (`--envkind`, `--repo`,
      `--layer`, `--dry-run`, `--yes`, `--help`) in the layer-script
      idiom.
- [ ] Layer registry table (number → purpose, scope, path template) +
      `--help` legend rendering (includes L0 as external/not-runnable).
- [ ] Resolution logic: public vs private vs both; `<envKind>`
      substitution; null-repo skip path; reject `--layer 0`.
- [ ] Ordered execution with `--dry-run` forwarding + run/skip/fail
      summary.
- [ ] Static gate (`bash -n`, `shellcheck`) + functional dry-run tests
      per Test Strategy.
- [ ] Update `README.md` + `.kdevkit/project.md`: document `layer-run`
      as the one-run rebuild entrypoint (Day-2 flows table + fresh-
      machine commands), and name L0 as the curl bootstrap step.

- *Risk note:* public-repo hygiene — the driver and its `--help` text
  must name no private repo; refer to it only via `--repo`/placeholders.
- *Risk note:* `layer-run` owns L1–L7 only; L0 (curl bootstrap) and
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
  (today's clone-only `env-setup.sh`). `layer-run` drives L1–L7 only; L0
  owns from-nothing bootstrap + first-run L1-on-kelasa. This is a model
  refinement — propagate L0 into the seven-layer docs at closure (the
  model is now L0–L7).

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
