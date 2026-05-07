# Post-install setup notes

Things to do manually — outside `build-nix` and `post-nix`.

- Add google drive remote: `rclone-env add`
- Add ssh remote for desktop-aka: `rclone-env add`
- Configure claude to use kdevkit
- `cd ~/env-workplace/mAId && nix profile install .` (once mAId
  ships on this machine) to put `maid` on `$PATH`, then `maid
  deploy` to activate the shared agentic config.
