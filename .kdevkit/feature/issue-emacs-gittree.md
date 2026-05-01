# issue-emacs-gittree (WIP)

## Status
🚧 **Work in progress.** The initial implementation was built on the
`misc-updates` branch and reverted after field testing surfaced a
selection regression. Requirements and design will be re-done from
scratch before the next implementation attempt.

A separate feature branch (`issue-emacs-gittree`, to be created) will
carry the rework. Do not land gittree CLI changes on `misc-updates`.

## Feature Brief
Two related goals:

1. **`emacs-gittree` CLI**: `emacs-gittree [ref-a [ref-b [file]]]` —
   launch emacs in gittree-mode, optionally seeded with a diff between
   two refs. With zero args, behaves like the original alias (just
   turns on gittree-mode).
2. **lazygit `E` binding**: from inside lazygit, press `E` on a file,
   a commit, or a commit file to pop into emacs-gittree with the
   appropriate refs pre-filled.

Both must preserve the gittree layout invariant: **left panel = tree
(treemacs), right panel = file view / vdiff**. The treemacs side is
what makes gittree distinctive; any launch path that bypasses it is
not gittree-mode.

## Known issues (from iteration 1 on misc-updates)

The code from iteration 1 has been reverted from `misc-updates`. A
summary of what was tried and what broke lives here so the next
iteration doesn't repeat the same mistakes.

### What worked
- `gittree--ref-has-path-p` probe (via `git cat-file -e`) so
  added/deleted files render as empty buffers instead of leaking git
  error text into vdiff panes.
- Switching `call-process-shell-command` → `call-process` to stop
  zsh/atuin init from running on every git read (fixed the "could not
  create dir /nix/.../atuin" noise).
- `gittree-cleanup-panels` split-from-treemacs fix
  (`with-selected-window treemacs-win (split-window-right)`).
- `gittree--get-ref-diff-status` so the tree highlights files changed
  between the two launch refs rather than only working-tree changes.

### What broke
- **Selection regression**: after selecting a file that was unchanged
  between the launch refs and then switching to a modified file, the
  right panel kept showing the older file's content.
- Buffer reuse: once a `*file@ref*` buffer exists, subsequent visits
  didn't reliably refresh its contents (`read-only-mode` + stale data).
- The cleanup / refocus / buffer-create order under launch mode is
  subtly different from the normal click flow; the overrides were
  threaded through but the two paths were not unified.

### Likely root cause (hypothesis for the rework)
The override defvars (`gittree-launch-left-ref`,
`gittree-launch-right-ref`) thread through `gittree-visit-node` but
the buffer lifecycle doesn't cleanly separate "launch session" from
"normal session". When the user clicks around in launch mode, the code
mixes status-derived refs (from working-tree `git status`) with
launch-mode refs depending on which branch of `gittree-visit-node` it
hit. Candidate fix directions for the rework:

- Model "launch-mode click" as a first-class alternative to
  `gittree-visit-node`'s normal path, not as an override inside it.
- Make buffer refresh idempotent — reusing a `*file@ref*` buffer
  should always re-run `git show` (or skip work when the ref+path
  hash matches).
- Consider whether vdiff vs ediff is the right choice when neither
  side is editable (both are commits).

### Audit findings still open
From the earlier code audit:
- `gittree--status-cache` keyed by basename → collisions for same-name
  files in different dirs.
- No ref validation in `gittree-launch` — invalid refs fail silently
  at `git show` time inside emacs.
- Status refresh timing race (0.5s idle timer vs. immediate click).

## Next actions (when picked up)

1. New branch `issue-emacs-gittree` off `main` (or off the merged
   `misc-updates`, whichever is current).
2. Fresh requirements pass — specifically nail down:
   - What exactly "two committish" means in terms of tree content,
     click behavior, and vdiff editability.
   - What `emacs-gittree <sha>` (single arg) should mean — `<sha>` vs
     working? vs HEAD? Document explicitly.
   - lazygit contexts that should bind `E` — files / commits /
     commitFiles / stash / reflog? Today only `files` has it.
3. Fresh design pass covering: buffer lifecycle, window split
   invariants, how launch mode and normal mode interact (or whether
   they should share code at all).
4. Port over the hermetic test harness (`emacs/tests/*`) from the
   reverted `misc-updates` history — the harness itself was solid
   and caught real bugs.
5. Re-apply the individually-good fixes (ref-has-path probe,
   call-process-not-shell, cleanup-panels split).

## Pointers to reverted code (for archaeology)

The reverted commits are still reachable via their SHAs on the
`misc-updates` branch history before the revert merge:
- `09ad090` feat(gittree): emacs-gittree CLI accepts two committish args
- `887e01b` fix(gittree): accept arbitrary refs and diff-status the tree
- `3ff963e` fix(gittree): use call-process directly (no shell)
- `593dcf7` fix(gittree): render missing-at-ref as empty buffer
- `f4b1165` test(emacs): add hermetic gittree test harness
- `236a377` feat(lazygit): route E binding through emacs-gittree CLI

`git show <sha>` or `git checkout <sha> -- <path>` to retrieve.

## Out of scope
- markdown-mode (already on `misc-updates`).
- exec-path-from-shell unconditional init (already on `misc-updates`).
- elpa wipe activation hook (already on `misc-updates`).
- Bootstrap / mAId content (on `feature-build-layers`).

## Session Log
<!-- Newest at top -->

### 2026-04-30 - Spun out from misc-updates
- Reverted all gittree CLI and lazygit E-binding changes from
  `misc-updates` because of the selection regression described above.
- Recorded known issues and pointers to the reverted commits here so
  the rework doesn't start from scratch on research.
- Next step is a fresh requirements/design pass on a new feature
  branch.
