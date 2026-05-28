# Post-install setup notes

Things to do manually — outside the layer scripts.

- Add google drive remote: `rclone-env add`
- Add ssh remote for desktop-aka: `rclone-env add`
- Run Layer 5 to clone AI-tooling workspaces + stores and let them
  self-install: `~/env-workplace/env/layers/layer-5a.sh` (public) or
  `~/env-workplace/Gorantls-env/desktop-layers/layer-5b.sh` (private;
  also runs the public driver first). Workspaces land under
  `~/tool-workplace/`, stores under `~/dabba/`,
  `~/workplace/` is mkdir-only (humans populate).
