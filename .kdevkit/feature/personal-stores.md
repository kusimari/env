# Feature: personal-stores

Wire two personal (`mane`/`kusimari`-identity) stores into `~/dabba/`,
mirroring how the existing work stores are wired:

- **`kusimari-dabba`** — a private GitHub notes/Obsidian vault — as an
  L5 git-clone store block (mirrors the `Gorantls-store` block in the
  private L5).
- **Google Drive** — as an on-demand `rclone-env mount` (mirrors how
  OneDrive is a mount the app owns, not an L5 git clone).

Supersedes the stale `.kdevkit/feature/rclone-gdrive.md`, which claimed
a mount-capable `rclone-env` was already implemented (it is not — the
shipped `rclone-env` has no mount subcommand) and specified a heavier
design (separate nix module, macFUSE cask, config-file mount mappings)
than this feature adopts.

## Requirements

- After L5 runs on any machine, `~/dabba/kusimari-dabba` is a clone of
  the private `kusimari/kusimari-dabba` repo, pinned to the public
  (`kusimari`) git identity — self-healing and fetch-on-rerun, exactly
  like the other L5 stores.
- The store block lives in the **public** L5 (`layers/layer-5.sh`) so
  it comes up on every machine including personal Ubuntu, not just the
  kelasa Mac.
- `rclone-env mount` mounts a configured remote at a local path under
  `~/dabba/`; running it again when already mounted is a no-op (no
  error, no double-mount).
- `rclone-env umount` cleanly unmounts; `rclone-env status` (and
  `list`) show each configured remote's mounted/unmounted state.
- Google Drive specifically mounts at `~/dabba/gdrive-personal`.
- Mounting works with **only nix-provided `rclone` (already tier-1) plus
  each OS's own mount helper** — no extra nix dependency and no macFUSE:
  macOS uses the built-in NFS client (`nfsmount`), Ubuntu uses its stock
  `fusermount` (`mount`). No `flake.nix` edit is required.
- Google Drive is scoped to `mane` machines only (mac + personal
  Ubuntu) to start — symmetric with OneDrive, which is absent on
  `kelasa`. `kelasa` Linux is out of scope for now.
- If the Google Drive remote is not yet configured, `rclone-env mount`
  fails soft with a clear pointer to `rclone-env add` — it never leaves
  a half-mounted state.
- The manual, per-machine steps (add the rclone remote, then mount)
  stay documented in `setup-notes.md` — the layers don't automate
  interactive auth.

## Design

### kusimari-dabba store block (public L5)

Add one `{ ... } || { warn ...; FAILED=1; }` block to
`layers/layer-5.sh`, mirroring the private L5's `Gorantls-store` block
but for a public GitHub repo pinned to the public identity:

- `url="git@github.com:kusimari/kusimari-dabba.git"`
- `clone_or_fetch` into `$DABBA_ROOT/kusimari-dabba`
- `ensure_git_identity … "$PUBLIC_USER_NAME" "$PUBLIC_USER_EMAIL"`

The public L5 already defines `PUBLIC_USER_NAME`/`PUBLIC_USER_EMAIL`,
`clone_or_fetch`, `ensure_git_identity`, and `repo_basename` — reuse
them; the block is a near-copy of the existing `ai-workspace/mAId`
workspace block, but flat into `$DABBA_ROOT` (store shape, not
workspace shape). The placeholder store comment at the bottom of L5 is
where it goes.

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

- **macOS** → `rclone nfsmount` (uses the OS's built-in NFS client;
  needs **no** macFUSE). Verified working on this machine against
  `google-drive:`.
- **Linux** → `rclone mount` (stock FUSE, standard on Ubuntu).

Both run with `--daemon --vfs-cache-mode full --dir-cache-time 24h`.

**No `flake.nix` FUSE edit.** `rclone` is already tier-1 in
`home/home.nix`. macOS's NFS client is built in; Ubuntu ships
`fusermount`. Adding nix `fuse`/`fuse3` would need a setuid
`fusermount` (awkward off NixOS) and buys nothing here — so the flake
is left untouched, contrary to the stale spec's `fuse3`/`macfuse`
additions.

Google Drive is not hardcoded in the wrapper; the user runs
`rclone-env mount google-drive: ~/dabba/gdrive-personal`. (A future
enhancement could persist remote→mountpoint mappings, but that is
explicitly out of scope for this simpler design.)

### Zsh completion

Extend `rclone-env/_rclone-env`: add `mount`/`umount`/`status` to the
subcommand list; `mount` completes remotes, `umount` completes mount
points.

### Why not a login service / macFUSE

Considered a launchd/systemd keep-alive service so Drive auto-mounts at
login and survives crashes (OneDrive-like). Rejected for this feature:
the user explicitly asked for the simpler OneDrive/git parity — Drive is
an on-demand mount, not an always-on service. macFUSE was also rejected
(GUI kext + sudo install breaks "just run the layer scripts"); nfsmount
achieves the mount with zero extra deps. Both are noted as possible
future upgrades in the backlog rather than built now.

## Test Strategy

Maps to `project.md`'s Testing section (bash track):

- `bash -n layers/layer-5.sh rclone-env/rclone-env.sh` — parse check.
- `shellcheck layers/layer-5.sh rclone-env/rclone-env.sh` — lint.
- `bash layers/layer-5.sh --dry-run` — the new store block evaluates
  and prints its planned clone/fetch without touching disk.
- `bash layers/test-flake.sh` — flake still builds (home.nix reads the
  edited `rclone-env.sh` into a derivation).
- Live (operator): `rclone-env mount google-drive: ~/dabba/gdrive-personal`
  mounts and lists files; re-running is a no-op; `rclone-env status`
  shows mounted; `rclone-env umount ~/dabba/gdrive-personal` unmounts.
  (Already smoke-tested manually before writing this spec.)

## Implementation Plan

- [ ] Add `kusimari-dabba` store block to `layers/layer-5.sh`.
- [ ] Add `mount`/`umount`/`status` subcommands to
      `rclone-env/rclone-env.sh` (platform-split nfsmount vs mount).
- [ ] Extend `rclone-env/_rclone-env` completion.
- [ ] Update `setup-notes.md` mount step; note gdrive mount point.
- [ ] `git rm` the stale `.kdevkit/feature/rclone-gdrive.md` at closure.
- [ ] Run the bash Test Gate (parse, shellcheck, L5 dry-run, test-flake).

## Session Log

<!-- Newest at top. -->

- Grounding: env repo is public (`kusimari/env`); L5 is get-only inline
  blocks; `~/dabba/` already anticipates rclone mounts (project.md L96).
  macFUSE NOT installed on this Mac; `rclone nfsmount google-drive:`
  verified mounting `~/dabba/gdrive-personal` and listing files, then
  cleanly force-unmounted. `google-drive:` remote already added via
  `rclone-env add`. `kusimari-dabba` private repo already created,
  cloned, seeded (obsidian config + folder skeleton) and pushed.

## Decision Log

<!-- Newest at top. -->

- Chose on-demand mount over a login service, and nfsmount over macFUSE
  — per user's "keep it simple like git+onedrive" steer. Rationale in
  Design › "Why not a login service / macFUSE."
- Extend the existing `rclone-env.sh` wrapper rather than the stale
  spec's separate `rclone-env-module.nix` — the wrapper is already the
  home-manager-packaged owner of this surface; a second module would
  duplicate it.
