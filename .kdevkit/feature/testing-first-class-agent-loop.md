# Feature: testing-first-class-agent-loop

## Git Setup

- Branch: `feat/testing-first-class-agent-loop`
- Base: `main` (post-merge of the L5-inline-registries change)

## Feature Brief

The kdevkit methodology defines a Quality → Test → Push loop (§8 of
`SKILL.md`) that reads test commands out of `project.md`'s Testing
section. `env/.kdevkit/project.md` does not currently carry a Testing
section, so the agentic Test Gate has nothing to run and "what gets
checked in is always tested" relies on operator discipline rather
than automation.

This feature adds an explicit Testing section to
`env/.kdevkit/project.md` that splits validation into two clearly
labelled tracks:

- **Auto-runnable** — commands the kdevkit loop can run on a clean
  Linux dev-desktop checkout without activating the home-manager
  configuration. These become the Test Gate.
- **Live-only** — manual procedures that mutate the machine (full
  L1-L5 activation, post-nix shell behavior, `nix run .#env-verify`
  on an activated host). These get a written recipe but stay out of
  the Test Gate.

This repo builds an environment on top of a machine, so the live
half of validation is irreducible. The point is not to chase 100%
automation; it is to make the auto-testable surface explicit and to
document the live recipe so neither half is forgotten.

## Requirements

- A new `## Testing` section in `env/.kdevkit/project.md`, placed
  near `## Conventions` so it sits next to the existing "Day-2
  update flows" prose.
- The section is written as prose (per kdevkit §2 first-time
  detection rule: "Write the Testing section as prose — describe the
  test layers and mention commands inline where natural. Do not
  invent a structured 'Toolchain' block; §8 reads commands out of
  the prose at run time.").
- Auto-runnable commands enumerated below must run successfully on a
  clean checkout on a Linux dev-desktop.
- Live-only validation must reference the existing Day-2 update flow
  chain in Conventions rather than duplicating it.
- No change to `Gorantls-env` in this feature — that repo gets its
  own kdevkit bootstrap as a separate feature.

### Out of scope

- Pre-push hook enforcement of the Test Gate. The kdevkit loop
  remains advisory (operator-driven) for now.
- A new bash test harness (bats / shunit2 / etc.). We only document
  and wire commands that already exist plus a `shellcheck` pass.
- Auto-migration of `.kdevkit/` to `specs/`. kdevkit forbids it.
- Anything outside `env/`.

## Design

### Auto vs. live split

| Validation | Auto-runnable on clean checkout? | Command | Notes |
|---|---|---|---|
| Flake schema/eval | yes | `nix flake check` | catches obvious schema/import breakage |
| Flake build (homeConfigs) | yes | `bash layers/test-flake.sh` | builds `ubuntu-mane` and `al2-kelasa` activation packages without activating |
| Bash syntax | yes | `bash -n layers/*.sh` | cheap parser-only check |
| Bash lint | yes | `shellcheck layers/*.sh` | catches L1-L5 driver bugs |
| L5 dry-run | yes | `bash layers/layer-5a.sh --dry-run` | side-effect-free walk; verifies arg parser + entry blocks |
| Invariant PATH check | live-only | `nix run .#env-verify` | passes only on a fully activated envKind; running on a vanilla checkout reports MISS for tier-1 binaries that were never installed there |
| L1 machine prep | live-only | n/a | only meaningful on a fresh OS install |
| L3/L4 activation | live-only | per-envKind L3/L4 scripts | mutates home-manager state |
| L5 real clone+install | live-only | `bash layers/layer-5a.sh` | side-effects on `~/tool-workplace`, `~/dabba` |
| `~/.pre-nix-rc` / `~/.post-nix-rc` shell behavior | live-only | interactive shell | needs an activated machine |

The rule the section will state explicitly: **anything evaluable
against the repo without changing machine state goes in the Test
Gate; anything that mutates the machine stays manual.**

### Tooling availability

`shellcheck` is required by the auto track. Verification step in the
implementation plan checks whether it is already present (typically
shipped via Nix on `$PATH` after activation; see `home/home.nix`). If
not, add it to `home.packages` in the same change. `bash -n` and
`nix` are already invariants.

### Optional `## Agent Development` block

kdevkit allows an optional `## Agent Development` section under
`project.md` that may carry per-skill overrides (e.g. score
threshold, retry budget for the Quality Gate). Default kdevkit
values (threshold 70, retry budget 2) are appropriate for this repo;
the implementation will **not** add an `## Agent Development` block
unless we discover a reason to override.

### Section placement and shape

Insert `## Testing` between `## Directory map` and `## Non-obvious
invariants`. The prose orders auto-track first, then live-only, then
points at Conventions §"Day-2 update flows" for the per-layer rerun
chain.

## Test Strategy

- **Self-validation.** Every auto-track command listed in the new
  section must succeed on a clean checkout before the feature is
  declared done. The implementation runs them as a final pass.
- **Doc accuracy.** The live-only callout must match the Day-2
  update flow chain already documented in Conventions; if the two
  drift the section is wrong.
- **No new test code.** This feature only documents and wires
  existing tests plus `shellcheck`. There is no new harness to test.

## Implementation Plan

1. **Confirm `shellcheck` on `$PATH`.** If absent, add to
   `home/home.nix` `home.packages` in the same change. (Note: this
   may pull `shellcheck` into the env-verify invariant set
   automatically — confirm `meta.mainProgram` resolves to
   `shellcheck` before assuming.)
2. **Draft the `## Testing` section** in `env/.kdevkit/project.md`,
   placed between `## Directory map` and `## Non-obvious invariants`.
   Write as prose per the kdevkit detection rule.
3. **Run each auto-track command** on a clean checkout. Fix any
   that fail or change them in the doc to match what actually works.
4. **Self-review** against kdevkit's §8 inputs paragraph: the loop
   must be able to read clear `format` (none — bash/Nix), `lint`
   (`shellcheck`), `type-check` (`bash -n` + `nix flake check`), and
   `test` (`test-flake.sh` + `layer-5a.sh --dry-run`) commands out
   of the new section.
5. **Land as two commits in one PR**, squash on merge. Order:
   1. `fix(env-verify): mirror commonConfiguration overlays and pass
      lib/config to tier-3 imports` — env-verify.nix only.
   2. `docs(kdevkit): add Testing section for agentic Test Gate` —
      `home/home.nix` (shellcheck addition), `.kdevkit/project.md`
      (Testing section), and this spec's logs.

   Two commits because the env-verify fixes are correctness changes
   to evaluation, not docs; bisectable on their own. One PR because
   the auto track is meaningless until both land together — the doc
   would otherwise advertise a Test Gate that fails on day one.
6. **Promote** the spec from `feature/wip/` to `feature/` once
   merged. `git mv` only — kdevkit forbids auto-migration of trees.

### Risk notes

- `nix run .#env-verify` is tempting to put in the auto track, but
  it fails on an unactivated checkout because the tier-1 binaries it
  expects are not on `$PATH` until home-manager has activated. Keep
  it live-only and document it as the canonical post-activation
  check.
- Adding `shellcheck` as tier-1 expands env-verify's invariant list.
  The build will start asserting `shellcheck` is on `$PATH` on every
  envKind. That is the correct behavior, but it means every machine
  must re-activate after the merge before env-verify will be green.
  Call this out in the commit body so the reader knows.

## Session Log

- 2026-05-26 · Spec authored. No implementation yet. Decision logged
  to keep `nix run .#env-verify` live-only and to add `shellcheck`
  to `home.packages` if missing.
- 2026-05-27 · Implementation. Added `shellcheck` to tier-1
  `home.packages` (was missing). Drafted prose `## Testing` section
  in `project.md` between Directory map and Non-obvious invariants.
  Auto-track validation surfaced two pre-existing latent bugs in
  `env-verify.nix` that `nix flake check` had never exercised
  before because no one had run it on a clean checkout — fixed
  both (see Decision Log). Final auto-track sweep: `nix flake
  check`, `bash layers/test-flake.sh`, `bash -n layers/*.sh`,
  `shellcheck layers/*.sh` (via `nix-shell -p`), and `bash
  layers/layer-5a.sh --dry-run` all pass on this checkout.
- 2026-05-27 · Audit pass before commit. Plan agent reviewed the
  spec end-to-end against kdevkit SKILL.md §§2/6/8. Confirmed: this
  is the right feature (agent loop genuinely has nothing to read
  without the section); auto/live split is correct; the env-verify
  fixes belong in this feature, not a precursor. Skipped audit
  suggestions to (a) tighten the auto-track criterion in prose, and
  (b) add overlay-parity / tier-3-argset maintenance contracts to
  `project.md`'s Testing prose — instead landed those two coupling
  warnings as one-line code comments inside `env-verify.nix` next
  to the `mkPkgs` and `tier3` definitions, where the next person to
  edit those locations will actually see them. Skipped the
  suggestion to add a `§8 readability artifact` paragraph to the
  spec's Test Strategy as gold-plating; the new Testing prose itself
  is the artifact.

## Decision Log

- 2026-05-26 · **Test Gate stays advisory** (no pre-push hook in
  this feature). Rationale: minimum viable wiring of the kdevkit
  loop first; enforcement is a separate decision once the loop has
  been used in practice. Alternative rejected: ship advisory + hook
  together — too much for one PR; harder to revert the hook half.
- 2026-05-26 · **`nix run .#env-verify` lives in the live-only
  track.** Rationale: it depends on activated machine state and
  fails by design on a clean checkout. Alternative rejected: gate it
  on `$IN_NIX_SHELL` or similar — adds complexity for no real
  benefit; the Day-2 update flow already calls it out.
- 2026-05-26 · **Prose, not a structured Toolchain block.** Per
  kdevkit §2's explicit first-time-detection rule.
- 2026-05-27 · **Fix env-verify overlays inline with this feature**
  rather than splitting into a separate PR. Rationale: making
  `nix flake check` part of the Test Gate is the whole point of
  the feature, and the gate is meaningless if it does not pass on
  a clean checkout. Two latent bugs surfaced — (a) `mkPkgs` did
  not apply the `antigravity-cli` overlay, so any consumer of
  `apps.x86_64-linux.env-verify` failed with `undefined variable
  'antigravity-cli'`; (b) `tier3` imported `envKind-*.nix` files
  with only `pkgs`, but `envKind-kelasa.nix` uses `lib.mkIf` and
  expects `{ config, lib, pkgs, ... }`. Both fixed in
  `env-verify.nix`. Alternative rejected: ship the doc and split
  the env-verify fix into a separate commit — would land a Test
  Gate that is broken from day one, which contradicts the spec's
  self-validation requirement.
- 2026-05-27 · **Document env-verify coupling as code comments,
  not as `project.md` prose.** The audit recommended adding two
  maintenance-contract bullets to the new `## Testing` section
  ("overlay parity between `commonConfiguration` and `mkPkgs`"
  and "tier-3 argset must match `env-verify.nix`'s `tier3`
  import"). Decided instead to put one-line warnings inside
  `env-verify.nix` next to `mkPkgs` and `tier3`. Rationale: the
  bug recurs in the file where the change is made, and a comment
  in that file is what the next reader will actually see. The
  Testing section stays short and focused on what to run.
  Alternative rejected: write the same coupling in both places —
  duplicated source of truth.
- 2026-05-27 · **Two commits, one PR, squash on merge.** Spec
  originally said one commit (`docs(kdevkit): ...`). Revised to:
  (1) `fix(env-verify): ...`, (2) `docs(kdevkit): ...`. Rationale:
  the env-verify changes are correctness fixes to flake
  evaluation, bisectable independently of the docs. Squash-merge
  keeps the public history at one logical change ("make the agent
  loop's auto track green and document it"). Alternative
  rejected: one commit conflating fix and docs — diff is harder
  to review, and the next person git-blaming `mkPkgs` for an
  unrelated reason would land on a "docs:" commit.
- 2026-05-28 · **No top-level `## Agent Development` block.**
  Per kdevkit §2's optional-section guidance, the block is for
  per-skill overrides; the env repo runs cleanly on the defaults
  (score threshold 70, retry budget 2). Adding the block to
  document defaults would create a parallel source of truth that
  drifts as kdevkit's defaults evolve. Keep `project.md` minimal
  and rely on the skill's defaults until a real override
  emerges. Originally raised as an Open Question; resolving here
  during the §9 close-out.
