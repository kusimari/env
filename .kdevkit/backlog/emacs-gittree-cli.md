# Backlog: emacs-gittree-cli

## What

Two conveniences on top of the working `gittree-mode`, both built once
and reverted:

1. **`emacs-gittree` CLI** — `emacs-gittree [ref-a [ref-b [file]]]`
   launches emacs in gittree-mode, optionally seeded with a diff
   between two refs. Zero args behaves like today's alias (just turns
   the mode on).
2. **lazygit `E` binding** — from inside lazygit, press `E` on a file,
   a commit, or a commit file to pop into `emacs-gittree` with the
   appropriate refs pre-filled.

Both must preserve the gittree layout invariant: **left panel = tree
(treemacs), right panel = file view / vdiff**. The treemacs side is
what makes gittree distinctive; a launch path that bypasses it is not
gittree-mode.

The mode itself is not in scope — `emacs/core-gittree.el` works and is
in daily use via the `emacs-gittree` shell alias
(`home/emacs.nix`). This item is only the CLI + lazygit entry points.

## Why

Iteration 1 shipped on the `misc-updates` branch and was reverted on
2026-04-30 after field testing surfaced a selection regression. The
problem was structural rather than a local bug, so it needed a fresh
requirements/design pass rather than a patch — and that pass never
happened.

Demoted from an in-flight feature spec to backlog on 2026-07-29: idle
~3 months, nothing depends on it, and lazygit's own `autoRefresh`
plus the working mode cover the day-to-day. The want is real but weak;
the accumulated design knowledge below is the part worth keeping.

## The reverted code is gone — these notes are the only record

**`git show <sha>` will not work.** The `misc-updates` branch was
deleted, the six iteration-1 commits
(`09ad090`, `887e01b`, `3ff963e`, `593dcf7`, `f4b1165`, `236a377`)
are unreachable, and none survives as a dangling object. Verified
2026-07-29. The hermetic test harness (`emacs/tests/*`) went with
them and exists on no surviving ref.

So anything below must be **re-derived from the description**, not
retrieved. The fixes are individually small; it is the diagnosis that
was expensive, and that is what this file preserves.

## What worked in iteration 1 (re-derive these)

- A `gittree--ref-has-path-p` probe (via `git cat-file -e`) so
  added/deleted files render as empty buffers instead of leaking git
  error text into vdiff panes.
- `call-process` instead of `call-process-shell-command`, so zsh/atuin
  init stops running on every git read (this was the source of the
  "could not create dir /nix/.../atuin" noise).
- Splitting from the treemacs window explicitly in
  `gittree-cleanup-panels`
  (`with-selected-window treemacs-win (split-window-right)`).
- A `gittree--get-ref-diff-status` so the tree highlights files
  changed *between the two launch refs*, not only working-tree
  changes.

## What broke (the reason for the revert)

- **Selection regression.** After selecting a file unchanged between
  the launch refs and then switching to a modified file, the right
  panel kept showing the older file's content.
- **Buffer reuse.** Once a `*file@ref*` buffer existed, later visits
  didn't reliably refresh it (`read-only-mode` plus stale data).
- The cleanup / refocus / buffer-create order under launch mode
  differs subtly from the normal click flow; overrides were threaded
  through both paths but the paths were never unified.

### Root-cause hypothesis

The override defvars (`gittree-launch-left-ref`,
`gittree-launch-right-ref`) thread through `gittree-visit-node`, but
the buffer lifecycle never cleanly separates "launch session" from
"normal session". Clicking around in launch mode mixes status-derived
refs (from working-tree `git status`) with launch-mode refs depending
on which branch of `gittree-visit-node` was hit.

Candidate directions:

- Model a launch-mode click as a first-class alternative to
  `gittree-visit-node`'s normal path, not an override inside it.
- Make buffer refresh idempotent — reusing a `*file@ref*` buffer
  should always re-run `git show`, or skip work only when a
  ref+path hash matches.
- Reconsider vdiff vs ediff when neither side is editable (both
  commits).

### Audit findings still open

- `gittree--status-cache` is keyed by basename → collisions for
  same-name files in different directories.
- No ref validation in `gittree-launch` — invalid refs fail silently
  at `git show` time inside emacs.
- Status refresh timing race (0.5s idle timer vs. immediate click).

## Open questions for a fresh requirements pass

- What "two committish" means precisely, in terms of tree content,
  click behavior, and vdiff editability.
- What `emacs-gittree <sha>` (single arg) should mean — `<sha>` vs
  working tree? vs `HEAD`? Needs to be documented explicitly, since
  ambiguity here contributed to the ref-mixing bug.
- Which lazygit contexts should bind `E` — files / commits /
  commitFiles / stash / reflog? Iteration 1 bound only `files`.
- Whether launch mode and normal mode should share code at all.
- Whether to rebuild the hermetic test harness first. It caught real
  bugs in iteration 1, and it is gone; given that iteration 1 failed
  on a structural bug that a harness is well suited to catch,
  rebuilding it before re-attempting the feature is probably the
  cheaper order.

## Related

- `emacs/core-gittree.el` — the working mode; unchanged by the revert.
- `home/emacs.nix` — carries the `emacs-gittree` shell alias that
  stands in for the reverted CLI.
- `gittree/lazygit-config.yml` — lazygit config; where an `E` binding
  would land.
