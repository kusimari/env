# Post-install setup notes

Things to do manually — outside the layer scripts.

- Add google drive remote: `rclone-env add`
- Add ssh remote for desktop-aka: `rclone-env add`
- Run Layer 5 to clone (get-only) AI-tooling workspaces + stores:
  `~/env-workplace/env/layers/layer-5.sh` (public) or
  `~/env-workplace/<kelasa-specific env repo>/desktop-layers/layer-5.sh`
  (private; also runs the public driver first). Workspaces land under
  `~/tool-workplace/`, stores under `~/dabba/`,
  `~/workplace/` is mkdir-only (humans populate).
- Run Layer 6 to build the tools (runs each tool workspace's own
  `setup`/`install`): `~/env-workplace/env/layers/layer-6.sh`. Separable
  from the base env — a bare rebuild can stop at L5.
