# l6-tool-install-entry

## What / Why
`layer-run` cloned mAId in L6 but never installed it: mAId had moved
its install to `just install` and no longer shipped a root
`install`/`setup`, so the L6 walk found nothing and said nothing.
mAId now ships `./install` (runs `just install`); L6 picks it up as is.
This side records that contract, and L6 now fast-forwards what it
fetched so a rerun builds the latest upstream.

## Requirements
- [x] R1 L6 runs mAId's root `./install`, with no mAId-specific code here.
- [x] R2 After the fetch, L6 fast-forwards the checked-out branch to its
  upstream; when it can't (local commits, no upstream), it builds the
  checkout as is. Never fails the layer.
- [x] R3 `project.md` states that every L6 tool repo must ship a root
  `install`/`setup`.

## Design
`fast_forward` in `layer-common.sh`: `git merge --ff-only @{u}`, warn on
failure, return 0. Env knows only the declared clone; how that repo is
worked on (branches, worktrees) stays out of env. The private L6 carries
a copy, like its other helpers. L5 does not call it.

## Test Strategy
- `fast_forward` on temp repos: behind → fast-forwarded; local commit →
  warn, rc 0, left as is; feature branch with no upstream → warn, rc 0;
  `DRY_RUN=1` → prints, changes nothing.
- `bash layers/layer-6.sh --dry-run` lists mAId's `./install`.

## Implementation Plan
- [x] `fast_forward` in `layer-common.sh`, called from the L6 mAId block.
- [x] Update the mAId block comment.
- [x] Update `project.md` (L6 row, L6 framework, directory map, Testing).

## Handoff

- **Stage:** review
- **Ready for:** merge, after the mAId `./install` change lands
- **Carry forward:** none
- **Deliberately left:** L5 stores stay fetch-only — not asked for.

### Crossings
<!-- Append one line per crossing. Never edit or delete a line. -->
- dev → review · EXCEPTION · skipping: planning phase · why: user
  asked for a small change without a full plan
- review → dev · RETURN · fault: requirements · issue: L6 must build
  only the latest main/mainline · fix: add R4, fast-forward the default
  branch after fetch · done when: the temp-repo tests pass
- dev → review
- review → dev · RETURN · fault: design · issue: worktree exclude and
  branch switching put repo workflow into env · fix: drop both; plain
  fast-forward to upstream, else use as is · done when: temp-repo tests
  pass
- dev → review
