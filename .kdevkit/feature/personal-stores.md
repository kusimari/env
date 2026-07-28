# Feature: personal-stores

Add a personal notes store and clarify that **cloud file storage is
set up manually**, not by layer scripts. Two parts:

- **`kusimari-dabba`** — a private GitHub notes store (plain Markdown)
  — wired as an L5 git-clone store block in the **public** L5
  (`layers/layer-5.sh`), mirroring the existing store-block pattern.
- **`rclone-env mount`** — add mount/umount/status subcommands to the
  existing `rclone-env` wrapper, so cloud remotes (e.g. Google Drive)
  can be mounted on demand under `~/dabba/`.

Plus a documentation/UX change: **layer scripts never set up cloud file
storage** (no symlink, no mount, no naming assumptions). They only
fetch git-backed stores. A manual-setup notes file, echoed at the end
of every `layer-run`, reminds the operator to wire cloud storage under
`~/dabba/` by hand.

Supersedes the stale `.kdevkit/feature/rclone-gdrive.md` (deleted when
this was filed), which claimed a mount-capable `rclone-env` was already
implemented (it is not — the shipped wrapper has no mount subcommand)
and specified a heavier design (separate nix module, macFUSE cask,
`fuse3` in the flake, config-file mount mappings) than this adopts.

## Backlog status & pickup

**Status: deferred — fully planned, not yet implemented.** Planning is
complete and every design decision below is settled; nothing here needs
re-litigating. Promote with
`git mv .kdevkit/backlog/personal-stores.md .kdevkit/feature/` and start
the dev loop straight from the Implementation Plan.

**Pre-work already done outside this repo** (no action needed):

- The private notes repo `kusimari/kusimari-dabba` is created (verified
  owner-only), cloned to `~/dabba/kusimari-dabba`, and pruned to a
  plain-Markdown `README.md` + `inbox/`.
- A Google Drive `rclone` remote (`google-drive:`) is added via
  `rclone-env add`, and `rclone nfsmount` is verified working on macOS.

**Where to pick this up:** any machine — the code is envKind-agnostic
(the `kusimari-dabba` git block is in the *public* L5 so it comes up
everywhere; the `rclone-env` subcommands are just tooling). A **kelasa
Linux** machine is a fine place to implement it: this repo is public and
present there, and the `mount` path is exercised via `rclone mount`
(Linux) rather than `nfsmount` (macOS). Note the *runtime scoping* —
Google Drive is wired manually only on `mane` machines — is a use-time
choice, not an implement-time constraint.

**Companion change:** a matching L5 edit in the private kelasa-side env
repo (removing its OneDrive symlink block) is tracked as its own backlog
item there — see that repo's `.kdevkit/backlog/`. On a kelasa machine
both repos are checked out under `~/env-workplace/`, so a coding agent
can work both from the two backlog files.

## Requirements

- After L5 runs on any machine, `~/dabba/kusimari-dabba` is a clone of
  the private `kusimari/kusimari-dabba` repo, pinned to the public
  (`kusimari`) git identity — self-healing and fetch-on-rerun, like the
  other git-backed L5 stores. It lives in the **public** L5 so it comes
  up on every machine including personal Ubuntu.
- `kusimari-dabba` is plain Markdown, portable to any editor — no
  editor-specific coupling committed into the store.
- `rclone-env mount <remote:path> <mount-point>` mounts a configured
  remote; running it again when already mounted is a no-op (no error,
  no double-mount). `umount` cleanly unmounts; `status` shows each
  configured remote's mounted state.
- Mounting works with **only nix-provided `rclone` (already tier-1) plus
  each OS's own mount helper** — no extra nix dependency and no macFUSE:
  macOS uses the built-in NFS client (`nfsmount`), Linux uses its stock
  `fusermount` (`mount`). No `flake.nix` edit is required.
- **Layer scripts do nothing for cloud file storage** — no symlink, no
  mount, no assumption about names. Any cloud store (OneDrive, Google
  Drive, or another) is wired manually, under any name, in any envKind.
- A manual-setup notes file documents the manual steps and is **echoed
  at the end of every `layer-run`**, so the reminder prints no matter
  which layer ran.

## Design

### kusimari-dabba store block (public L5)

Add one `{ ... } || { warn ...; FAILED=1; }` block to
`layers/layer-5.sh`, mirroring the existing store-block pattern but for
a public GitHub repo pinned to the public identity:

- `url="git@github.com:kusimari/kusimari-dabba.git"`
- `clone_or_fetch` into `$DABBA_ROOT/kusimari-dabba`
- `ensure_git_identity … "$PUBLIC_USER_NAME" "$PUBLIC_USER_EMAIL"`

The public L5 already defines `PUBLIC_USER_NAME`/`PUBLIC_USER_EMAIL`,
`clone_or_fetch`, `ensure_git_identity`, and `repo_basename` — reuse
them. It's the store-shaped variant (flat into `$DABBA_ROOT`) of the
existing workspace block. The placeholder store comment at the bottom
of L5 is where it goes.

### rclone-env mount/umount/status subcommands

Extend the existing `rclone-env/rclone-env.sh` wrapper (do **not**
introduce a separate nix module — the wrapper is already packaged via
`writeShellScriptBin` in `home/home.nix`). Add three subcommands
alongside the existing `ls/copy/sync/…`:

- `mount <remote:path> <mount-point>` — idempotent: if already mounted
  (checked via `mount | grep`), log and return 0; else `mkdir -p` the
  mount point and mount.
- `umount <mount-point>` — `umount` on macOS (falls back to
  `diskutil unmount force`), `fusermount -u` on Linux.
- `status` — list configured remotes and whether each is mounted.

**Mount mechanism is platform-split, chosen at runtime by `uname`:**

- **macOS** → `rclone nfsmount` (built-in NFS client; needs **no**
  macFUSE). Verified working on this machine against a Drive remote.
- **Linux** → `rclone mount` (stock `fusermount`, standard on Ubuntu).

Both run with `--daemon --vfs-cache-mode full --dir-cache-time 24h`.

**No `flake.nix` FUSE edit.** `rclone` is already tier-1. macOS's NFS
client is built in; Ubuntu ships `fusermount`. Adding nix `fuse`/`fuse3`
would need a setuid `fusermount` (awkward off NixOS) and buys nothing —
so the flake is left untouched, contrary to the stale spec.

The remote is not hardcoded; the operator runs e.g.
`rclone-env mount google-drive: ~/dabba/gdrive-personal`. Persisting
remote→mountpoint mappings and an auto-mount-at-login service are
explicitly **out of scope** (see backlog).

### What `~/dabba/` holds — two kinds of store

`~/dabba/` is the stores root, and it holds exactly two kinds of entry,
by two different owners:

1. **Git-backed stores — added explicitly by L5.** One inline
   `{ ... }` block per repo in `layers/layer-5.sh`, personal
   (`kusimari` identity, public repos) *and* work-related (private
   identity, in the companion L5). L5 clones/fetches these and pins
   identity. This is the *only* thing the layers do for `~/dabba/`.
2. **Cloud file storage — added manually by the operator.** Symlinks
   into `~/dabba/` pointing at cloud mounts: an app-owned mount like
   OneDrive (`ln -s ~/Library/CloudStorage/OneDrive-… ~/dabba/<name>`),
   or an `rclone-env mount` point for Google Drive / another remote.
   Any name, any envKind, wired by hand — layers never create these.

This two-owner model (L5 = git repos; operator = cloud-storage links)
is the durable invariant this feature establishes; **at closure it
bubbles into `project.md`** (the L5-framework / `~/dabba/` description),
so the split is documented where the layer model lives.

### Cloud storage is manual — layers stay out of it

Layer scripts fetch git-backed stores only. Cloud file storage
(OneDrive via its desktop app + a manual symlink; Google Drive via
`rclone-env add` then `rclone-env mount`; or any other) is wired by the
operator, under any name, on any envKind. This keeps the public L5 and
the private companion L5 **identical in what they do for cloud storage:
nothing** — the only per-envKind difference stays in the git-backed
stores each side fetches.

Rename `setup-notes.md` → `setup-manual-notes.md` (clearer intent), keep
it terse and to-the-point, and have `layer-run` echo a one-line pointer
to it at the very end of every run so it surfaces regardless of which
layers ran.

### Zsh completion

Extend `rclone-env/_rclone-env`: add `mount`/`umount`/`status` to the
subcommand list; `mount` completes remotes, `umount` completes mount
points.

### Why not a login service / macFUSE

Considered a launchd/systemd keep-alive so a Drive mount auto-mounts at
login and survives crashes. Rejected: the operator asked for the simpler
manual parity — cloud mounts are on-demand. macFUSE was also rejected
(GUI kext + sudo install breaks "just run the scripts"); nfsmount needs
zero extra deps. Both noted in the backlog as possible future upgrades.

## Test Strategy

Maps to `project.md`'s Testing section (bash track):

- `bash -n layers/layer-5.sh rclone-env/rclone-env.sh layer-run` — parse.
- `shellcheck layers/layer-5.sh rclone-env/rclone-env.sh layer-run` — lint.
- `bash layers/layer-5.sh --dry-run` — new store block evaluates and
  prints its planned clone/fetch without touching disk.
- `bash layers/test-flake.sh` — flake still builds (home.nix reads the
  edited `rclone-env.sh` into a derivation).
- `bash layer-run --target darwin-kelasa --dry-run` (or any target) —
  the manual-notes pointer echoes at the end.
- Live (operator): `rclone-env mount google-drive: ~/dabba/gdrive-personal`
  mounts + lists; re-running is a no-op; `status` shows mounted;
  `umount` unmounts. (Already smoke-tested manually.)

## Implementation Plan

- [x] Add `kusimari-dabba` store block to `layers/layer-5.sh`.
- [x] Add `mount`/`umount`/`status` subcommands to
      `rclone-env/rclone-env.sh` (platform-split nfsmount vs mount).
- [x] Extend `rclone-env/_rclone-env` completion.
- [x] `git mv setup-notes.md setup-manual-notes.md`; prune to terse
      manual steps (cloud storage under `~/dabba/`, any name).
- [x] `layer-run` echoes a one-line pointer to the notes file at the end.
- [x] At closure, bubble into `project.md`: (a) the two-owner `~/dabba/`
      model — L5 adds git repos (personal + work); operator adds
      cloud-storage symlinks; (b) the renamed manual-notes file + the
      `layer-run` cat-at-end; (c) the "layers don't touch cloud storage"
      invariant (Non-obvious invariants). Done in the L5-framework
      `~/dabba/` bullet + a new Manual-setup-notes paragraph + a new
      Non-obvious invariant.
- [x] `git rm` the stale `.kdevkit/feature/rclone-gdrive.md` at closure.
      (Already absent — removed when the backlog item was filed.)
- [x] Prune the `kusimari-dabba` repo to README + `inbox/` (separate
      repo; done outside this branch — see Backlog status pre-work).
- [x] Run the bash Test Gate (parse, shellcheck, L5 + layer-run dry-run,
      test-flake).

## Companion change (separate repo)

The private kelasa-side companion repo carries a matching L5 whose
mac-only OneDrive symlink block will be **removed** so its L5 does
nothing for cloud storage either — keeping both L5s identical on that
axis. That edit and its own spec live in that repo (kdevkit §10
multi-repo); this feature only records the dependency.

## Session Log

<!-- Newest at top. -->

- Post-ship follow-up (PR #43, operator-found on macOS). Two reported
  symptoms + one latent bug:
  (1) `mount`/`umount` didn't tab-complete while `browse` did — **not a
  code bug**: the mac was on a pre-merge home-manager generation, so its
  `_rclone-env` symlink predated the mount subcommands. Reproduced by
  running the old file. No manual cache step is needed to pick up a new
  one: `compinit` re-reads a completion's body from disk each shell, and
  oh-my-zsh (which owns `compinit` here) deletes its own zcompdump when
  `$fpath` changes — so L3 + a new shell suffices. An earlier suggestion
  to `rm ~/.zcompdump*` was wrong and retracted.
  (2) `mount` froze the terminal for seconds — `--daemon` makes rclone
  wait for readiness, and on macOS/BSD that wait is a *constant sleep*
  defaulting to 1m. Fixed with `--daemon-wait 5s` + a message printed
  before the pause instead of after.
  (3) Latent: `is_mounted` interpolated the mount point into a grep
  regex, so `.`/`*` in a real path matched a different mount and the
  idempotency guard falsely reported "Already mounted". Fixed with
  `grep -qxF` over an extracted mount-point field (handles both the
  Linux `type <fs>` and macOS table formats) plus path resolution for
  symlinked mount points.
  Added `rclone-env/test-rclone-env.sh` (mocks rclone/mount/uname/
  fusermount; 16 cases; both platform paths from any machine) and
  registered it in project.md's Test Gate. Confirmed the suite fails
  against the pre-fix code, so it isn't vacuous. Lesson: the mount
  surface had no automated coverage at all — dry-runs never exercise it,
  which is why all three slipped past the original Test Gate.
- Closure (§8): PR #42 approved. Two minor review comments (drop the
  explanatory comments on `layer-3-common.sh`'s tail and `layer-run`'s
  `banner()`) applied in `3630d83`. Bubbled into `project.md`: the
  two-owner `~/dabba/` model (L5 = git stores; operator = cloud-storage
  links), the `setup-manual-notes.md` + cat-at-end-of-`layer-run`
  behaviour, and a new Non-obvious invariant "layers never set up cloud
  file storage." Ticked the remaining Implementation-Plan boxes
  (project.md bubble-ups, kusimari-dabba prune done out-of-branch).
  Squash-merged to `main`.
- Thorough follow-up (operator-requested) to the notes-file miss: moved
  the manual-notes print out of L3 entirely (`layer-3-common.sh` no
  longer cats the file) to the end of `layer-run`, which now cats the
  file in full — real content, not a "see …" pointer — after all
  requested layers run, guarded by `[[ -f ]]` so a missing file warns
  rather than fails. Added a `banner()` helper (full-width heavy rules +
  bold title, colour only on a TTY / honours NO_COLOR, width from
  COLUMNS/tput) and a banner per layer + SUMMARY + MANUAL SETUP NOTES,
  so it's obvious which layer is running in a long scrollback. Notes now
  print on every run regardless of which layers ran (they used to only
  appear when L3 ran, buried mid-output). Parse + shellcheck clean;
  dry-run verified banners + end-of-run cat.
- Bug (operator-found, real L3 run): the `setup-notes.md` →
  `setup-manual-notes.md` rename missed three live references —
  `layers/layer-3-common.sh` (`cat "$FLAKE_DIR/setup-notes.md"`, which
  broke L3 with a missing-file error), `README.md`, and the
  `project.md` directory map. Fixed all three. Root cause of the miss:
  the Test Gate's L3 is *announced, not executed* under `layer-run
  --dry-run` (L3 does a real nix switch), so the `cat` line was never
  exercised until a real L1-5 run. Lesson: `git grep` the old name
  across the whole repo on any rename, not just docs + home.nix.
- Code Review Gate: score 86/100 (threshold 70 — pass). Applied two
  machine-relevant findings: `status` matched bare `nfs` (would catch
  corporate NFS home dirs on kelasa work machines) → narrowed to
  rclone's own mounts (`fuse.rclone` on Linux, localhost NFS on macOS);
  Linux `umount` gained a lazy `fusermount -uz` fallback mirroring the
  macOS `diskutil ... force` path. Left as nits: unbounded VFS cache
  (`--vfs-cache-max-size` unset) and `is_mounted` regex-escaping (paths
  are operator-controlled). Re-ran parse/shellcheck/test-flake — green.
- Implemented (dev loop): `kusimari-dabba` store block added to
  `layers/layer-5.sh` (flat into `$DABBA_ROOT`, public identity);
  `mount`/`umount`/`status` added to `rclone-env.sh` (platform-split
  `nfsmount` vs `mount`, `--daemon --vfs-cache-mode full
  --dir-cache-time 24h`, idempotent via `is_mounted`); `_rclone-env`
  completion extended (`mount` → remote then dir, `umount` → dir);
  `setup-notes.md` → `setup-manual-notes.md` pruned to terse cloud +
  remote steps; `layer-run` echoes the notes pointer at the very end of
  every run. `status` reports configured remotes + active
  rclone/fuse/nfs mounts rather than claiming a per-remote mapping
  (`mount` doesn't reliably name the backing remote, esp. macOS NFS to
  localhost). Test Gate green: `bash -n`, `shellcheck`, L5 dry-run,
  `layer-run --layer 5 --dry-run` (pointer echoes), `test-flake.sh`
  (rclone-env.drv rebuilt).
- Public-repo hygiene: first draft of this spec named internal stores
  directly; rewritten to abstract them (generic "OneDrive"/"the private
  companion repo") per project.md's `<kelasa-specific env repo>`
  convention. `kelasa`/`mane` are public envKind terms, kept.
- Grounding: env repo is public (`kusimari/env`); L5 is get-only inline
  blocks; `~/dabba/` already anticipates rclone mounts (project.md).
  macFUSE NOT installed; `rclone nfsmount` verified mounting a Drive
  remote at `~/dabba/gdrive-personal`, then cleanly force-unmounted.
  OneDrive desktop client is NOT nix-managed on either side — the
  companion L5's symlink block only works if the app was installed
  manually, which is why cloud setup is being made fully manual.
  `kusimari-dabba` private repo already created (verified owner-only),
  cloned, and pushed.

## Decision Log

<!-- Newest at top. -->

- `~/dabba/` has a two-owner model, to be documented in `project.md` at
  closure: L5 explicitly adds git-backed stores (personal + work);
  the operator manually adds cloud-storage symlinks (OneDrive-style
  app mounts or `rclone-env mount` points). The two never overlap.
- Cloud storage fully manual; layers do nothing for it. Makes public and
  companion L5 identical on the cloud-storage axis; the companion's
  OneDrive symlink block is removed (its repo). Rationale: the real
  setup (client install + auth) is manual and outside nix anyway.
- Notes store is plain Markdown, README + `inbox/` only — no
  editor-specific coupling committed. (Obsidian is still provided as a
  nix cask on mac; it just opens the plain files.)
- Chose on-demand mount over a login service, and nfsmount over macFUSE
  — per the "keep it simple" steer. See Design › "Why not…".
- Extend the existing `rclone-env.sh` wrapper rather than the stale
  spec's separate nix module — the wrapper already owns this surface.
