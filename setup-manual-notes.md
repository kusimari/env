# Manual setup notes

Steps the layer scripts (L0–L7) deliberately do **not** automate —
interactive auth, per-machine secrets, one-off remotes, cloud file
storage. The layered rebuild itself (L1–L6 via `layer-run`, L7 via
`workplace-setup.sh`) is documented in `README.md`.

## Cloud file storage — wire it manually under `~/dabba/`

Layer scripts never set up cloud file storage (no symlink, no mount,
no naming assumptions). Wire any cloud store by hand, under any name:

- **Google Drive (or any rclone remote):** `rclone-env add`
  (interactive auth), then `rclone-env mount google-drive: ~/dabba/<name>`.
- **OneDrive (macOS):** install + sign in to the OneDrive app, then
  `ln -s ~/Library/CloudStorage/OneDrive-<tenant> ~/dabba/<name>`.

## Other one-off remotes

- Add ssh remote for desktop-aka: `rclone-env add` (interactive auth).
