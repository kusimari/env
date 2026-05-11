# Post-install setup notes

Things to do manually — outside `build-nix` and `post-nix`.

- Add google drive remote: `rclone-env add`
- Add ssh remote for desktop-aka: `rclone-env add`
- Configure claude to use kdevkit
- Run Layer 5 to clone AI-tooling workspaces and let them
  self-install: `~/env-workplace/env/layer-5/run` (public) or
  `~/env-workplace/Gorantls-env/layer-5/run` (private; also runs
  the public driver first).
- If an older `~/env-workplace/mAId/` clone exists from before the
  Layer-5 refactor, it's no longer used. Remove it at your
  convenience: `rm -rf ~/env-workplace/mAId`.
