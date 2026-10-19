# l6-tool-install-entry

## What / Why
`layer-run` cloned mAId in L6 but never installed it: mAId had moved
its install to `just install` and no longer shipped a root
`install`/`setup`, so the L6 walk found nothing and said nothing.
mAId now ships `./install` (runs `just install`); L6 picks it up as is.
This side records that contract and keeps the walk off worktrees.

## Requirements
- [x] R1 L6 runs mAId's root `./install`, with no mAId-specific code here.
- [x] R2 The walk never runs an entry-point under a `<repo>-worktrees/`
  dir (a branch in progress would install over the main checkout).
- [x] R3 `project.md` states that every L6 tool repo must ship a root
  `install`/`setup`.
- [x] R4 L6 builds the latest default branch: after the fetch, switch
  the checkout to `origin/HEAD`'s branch (`main`/`mainline`) and
  fast-forward it; a dirty checkout fails that workspace's get instead.
  L5 stores stay fetch-only.

## Design
`fd --exclude '*-worktrees'` on the discovery walk. At today's
`ai-workspace/<repo>-worktrees/<wt>/install` nesting the entry-point
is depth 4, already past `MAX_DEPTH=3`; the exclude covers a workspace
placed directly under `~/tool-workplace/` (depth 3) and any later
depth bump.

`update_default_branch` lives in `layer-common.sh` (L6-only caller);
the private L6 carries a copy, like its other helpers.

## Test Strategy
- `bash layers/layer-6.sh --dry-run` lists mAId's `./install`.
- fd against a temp tree with `repo/install` and
  `repo-worktrees/wt/install` at depth 3: only `repo/install` listed.
- `update_default_branch` on temp repos: feature branch behind origin →
  on `main`, 0 behind; `mainline` remote → `mainline`; dirty → rc 1,
  branch untouched; `DRY_RUN=1` → prints, changes nothing.

## Implementation Plan
- [x] Exclude `*-worktrees` in `layers/layer-6.sh`.
- [x] Update the mAId block comment.
- [x] Update `project.md` (L6 framework, Testing).
- [x] `update_default_branch` in `layer-common.sh`, called from L6.

## Handoff

- **Stage:** review
- **Ready for:** merge, after the mAId `./install` change lands
- **Carry forward:** a real L6 run switches mAId's checkout to `main`;
  until the mAId `./install` change is on `main` there is nothing to run.
- **Deliberately left:** L5 stores stay fetch-only (local work).

### Crossings
<!-- Append one line per crossing. Never edit or delete a line. -->
- dev → review · EXCEPTION · skipping: planning phase · why: user
  asked for a small change without a full plan
- review → dev · RETURN · fault: requirements · issue: L6 must build
  only the latest main/mainline · fix: add R4, fast-forward the default
  branch after fetch · done when: the temp-repo tests pass
- dev → review
