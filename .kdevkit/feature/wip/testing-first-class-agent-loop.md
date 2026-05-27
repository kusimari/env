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
5. **Land as one commit** on `feat/testing-first-class-agent-loop`.
   Conventional-commit subject: `docs(kdevkit): add Testing section
   for agentic Test Gate`.
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

## Open questions for the next session

- Should we add a top-level `## Agent Development` block now to
  document the kdevkit defaults explicitly (score 70, retries 2),
  even if we are not overriding them? Default answer in this spec:
  no — only document overrides.
- Do we want the Test Gate to also assert that
  `env/.kdevkit/feature/wip/` does not contain any spec older than N
  days as a hygiene check? Probably out of scope for this feature;
  noted as a future idea.
- Do we want a follow-up feature that bootstraps
  `Gorantls-env/.kdevkit/project.md` with the same auto/live split,
  adapted for the kelasa-side surface (no flake; mostly shellcheck +
  layer-5b dry-run)? Recommendation: yes, but as a separate feature
  in that repo's spec tree once we decide whether to introduce
  kdevkit there at all.

## Session Log

- 2026-05-26 · Spec authored. No implementation yet. Decision logged
  to keep `nix run .#env-verify` live-only and to add `shellcheck`
  to `home.packages` if missing.

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
