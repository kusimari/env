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

Supersedes the stale `.kdevkit/feature/rclone-gdrive.md`, which claimed
a mount-capable `rclone-env` was already implemented (it is not — the
shipped wrapper has no mount subcommand) and specified a heavier design
(separate nix module, macFUSE cask, `fuse3` in the flake, config-file
mount mappings) than this feature adopts.

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

- [ ] Add `kusimari-dabba` store block to `layers/layer-5.sh`.
- [ ] Add `mount`/`umount`/`status` subcommands to
      `rclone-env/rclone-env.sh` (platform-split nfsmount vs mount).
- [ ] Extend `rclone-env/_rclone-env` completion.
- [ ] `git mv setup-notes.md setup-manual-notes.md`; prune to terse
      manual steps (cloud storage under `~/dabba/`, any name).
- [ ] `layer-run` echoes a one-line pointer to the notes file at the end.
- [ ] Update `project.md` references to the renamed notes file + the
      "layers don't touch cloud storage" invariant.
- [ ] `git rm` the stale `.kdevkit/feature/rclone-gdrive.md` at closure.
- [ ] Prune the `kusimari-dabba` repo to README + `inbox/` (separate
      repo; done outside this branch).
- [ ] Run the bash Test Gate (parse, shellcheck, L5 + layer-run dry-run,
      test-flake).

## Companion change (separate repo)

The private kelasa-side companion repo carries a matching L5 whose
mac-only OneDrive symlink block will be **removed** so its L5 does
nothing for cloud storage either — keeping both L5s identical on that
axis. That edit and its own spec live in that repo (kdevkit §10
multi-repo); this feature only records the dependency.

## Session Log

<!-- Newest at top. -->

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
