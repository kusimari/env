# Manual setup notes

Steps the layer scripts (L0–L7) deliberately do **not** automate —
interactive auth, per-machine secrets, one-off remotes, cloud file
storage. The layered rebuild itself (L1–L6 via `layer-run`, L7 via
`workplace-setup.sh`) is documented in `README.md`.

## Cloud file storage — wire it manually under `~/dabba/`

Layer scripts never set up cloud file storage (no symlink, no mount,
no naming assumptions). Wire any store — Google Drive, OneDrive, SSHFS,
or anything else — by hand under `~/dabba/`, under any name. Two options:

- **rclone** (when the backend supports it): `rclone-env add`
  (interactive auth), then `rclone-env mount <remote>: ~/dabba/<name>`.
- **client installable + symlink**: install/sign in to the vendor
  client, then `ln -s <client-mount-path> ~/dabba/<name>` (e.g. macOS
  OneDrive under `~/Library/CloudStorage/OneDrive-<tenant>`).

Some corporate backends permit neither — third-party clients are
blocked and the machine has no route to one that is signed in. Those
stores get **no `~/dabba/` entry** and are reached through whatever
sanctioned API tooling exists instead. That means no local path, so
`grep`/editors/scripts can't see them; it is a real limitation, not an
oversight. Don't add a layer script to work around it.
