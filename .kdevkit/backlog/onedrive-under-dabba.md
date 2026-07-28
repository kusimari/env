# Backlog: onedrive-under-dabba

## What

Give OneDrive content a live, path-addressable home under `~/dabba/`,
the way Google Drive already has one. **Deferred 2026-07-28** — no
workable mount path exists today, so OneDrive is worked with through
a sanctioned first-party MCP server instead (verified working). This
item records what was tried, what it cost, and what would have to change
for a mount to become viable, so none of it is re-derived.

## Why (and why not now)

`~/dabba/` holds three entries today, in the two-owner model
`project.md` documents (L5 clones git stores; the operator wires cloud
storage by hand):

| Entry | Owner | Kind |
|---|---|---|
| `<kelasa store>` | private L5 | git store |
| `kusimari-dabba` | public L5 | git store |
| `google-drive` | operator | `rclone mount` (live, `fuse.rclone`) |

OneDrive is the gap. The value of closing it is **path addressability**:
a mount is usable by `grep`, `rg`, editors, and scripts. The MCP is
tool-call-based, so it covers "let an agent read/write my OneDrive
files" but not "let local tooling see them as a directory."

Not now because every route to a mount is either blocked or carries a
standing cost that outweighs the benefit (below).

## What was tried

**1. `rclone` directly against OneDrive — blocked.** Not a technical
failure; third-party client access is not permitted. Treated as a real
control, not an obstacle to route around.

**2. `rclone` SFTP mount of a mac folder (a symlink to the local
OneDrive CloudStorage dir) — mechanism proven, transport impractical.**

Proven on this Linux desktop against a loopback SFTP target, and again
through a reverse tunnel. Both times: live FUSE mount, read, write-back,
and new-file visibility all worked.

- `--sftp-ssh` is **required**. rclone's internal SSH library could not
  authenticate against this host (cert-based auth via
  a vendor-managed keys directory, not `~/.ssh/authorized_keys`).
  `--sftp-ssh` delegates to the real `ssh`
  binary so certs/agents/bastions work.
- **`--dir-cache-time 24h` (the shipped `MOUNT_FLAGS` default) breaks
  "live."** A file created at the source after mounting stayed invisible;
  remounting with `10s` surfaced it immediately. The shipped default is
  tuned for Google Drive API quota, and is wrong for an SFTP mount whose
  whole point is liveness. **Any mount feature here needs per-remote
  flag profiles in `rclone-env`, not one global `MOUNT_FLAGS`.**
- `--links` is needed to traverse the mac-side symlink, and
  `--sftp-set-modtime=false` avoids modtime errors against a
  cloud-backed FS. Neither was verifiable locally (no CloudStorage
  symlink on Linux).

**Why the transport is impractical:** this is a remote cloud desktop;
the mac is behind corporate NAT with no inbound route, so
desktop→mac cannot work. The only direction that connects is a reverse
tunnel initiated *from* the mac (`ssh -R 2222:localhost:22 <desktop>`),
which requires:

- macOS Remote Login enabled, and the mac kept awake;
- a manual tunnel re-established after every sleep/network change —
  precisely the "remember to run something" design this env avoids;
- `ssh-copy-id` appending the desktop's public key to the mac's
  `~/.ssh/authorized_keys`, i.e. a standing grant of passwordless
  laptop access from a cloud desktop. If ever done, use a **dedicated**
  key (`ssh-keygen -t ed25519 -f ~/.ssh/mac_sftp`), never the corporate
  SSO identity key, so it is revocable by deleting one line.

Also note `[localhost]:2222` is a host-key collision magnet — the same
name fronting different hosts yields `Host key verification failed`.
Use a distinct `~/.ssh/config` alias per target if revisited.

## Current answer: MCP

A sanctioned first-party MCP server (already installed on kelasa
machines and wired into a workspace agent alongside the other internal
MCP servers) reaches the corporate file store through its supported,
authenticated API. Verified working. Exact server and config paths are
kelasa-side; see the private companion repo.

This is the right default regardless of the mount question: it needs no
tunnel, no sshd on the laptop, no `authorized_keys` grant, no FUSE — and
it uses the supported door rather than working around the control that
blocked rclone. Bind it per-session with
the agent tooling's generated `.mcp.json` via `--mcp-config` (add
`--strict-mcp-config` to bound the tool set). Sessions resume with
history intact across an MCP rebind — the transcript is on disk and MCP
servers resolve at startup — so `--resume` (or `--fork-session
--resume`) is safe.

Its limit stays the reason this item exists: tool calls, not a path.

## What would unblock a mount

Any one of these changes the calculus:

- **A stable network identity for the mac** (Tailscale/ZeroTier) —
  removes the tunnel entirely; the mac gets a stable IP and
  `rclone-env mount` over SFTP works directly, no manual step. This is
  the most likely path to viability.
- **A sanctioned OneDrive rclone/WebDAV path**, if third-party client
  access is ever permitted — removes the mac hop altogether.
- **A FUSE filesystem over the MCP** — would give path addressability
  with the sanctioned transport, but is a build, not a config.

## If picked up

- Confirm the policy position first. rclone→OneDrive was blocked; a
  mount that lands the same content on a cloud dev desktop by another
  route reaches the same end state. Clear it before building. (ARCC MCP
  was unavailable in the exploring session, so it was never queried.)
- Add per-remote flag profiles to `rclone-env` (the `--dir-cache-time`
  conflict above) — likely a prerequisite for any second mount, and
  useful on its own.
- Extend `rclone-env/test-rclone-env.sh`; it already mocks
  `rclone`/`mount`/`uname`/`fusermount`, so a profile-selection test
  fits the existing harness.
- Keep the `project.md` invariant intact: **layers never set up cloud
  file storage.** Whatever lands here stays operator-wired, manual, and
  documented in `setup-manual-notes.md`.

## Related

- `.kdevkit/feature/personal-stores.md` — established the two-owner
  `~/dabba/` model, the `rclone-env mount` surface, and the
  manual-cloud-storage invariant.
- `setup-manual-notes.md` — where the manual steps live; already
  describes both cloud-storage options generically.
