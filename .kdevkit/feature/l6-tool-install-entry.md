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

## Design
`fd --exclude '*-worktrees'` on the discovery walk. At today's
`ai-workspace/<repo>-worktrees/<wt>/install` nesting the entry-point
is depth 4, already past `MAX_DEPTH=3`; the exclude covers a workspace
placed directly under `~/tool-workplace/` (depth 3) and any later
depth bump.

## Test Strategy
- `bash layers/layer-6.sh --dry-run` lists mAId's `./install`.
- fd against a temp tree with `repo/install` and
  `repo-worktrees/wt/install` at depth 3: only `repo/install` listed.

## Implementation Plan
- [x] Exclude `*-worktrees` in `layers/layer-6.sh`.
- [x] Update the mAId block comment.
- [x] Update `project.md` (L6 framework, Testing).

## Handoff

- **Stage:** review
- **Ready for:** merge, after the mAId `./install` change lands
- **Carry forward:** `clone_or_fetch` only fetches, never merges, so a
  rerun does not move the working tree forward.
- **Deliberately left:** fast-forward on fetch — separate change.

### Crossings
<!-- Append one line per crossing. Never edit or delete a line. -->
- dev → review · EXCEPTION · skipping: planning phase · why: user
  asked for a small change without a full plan
