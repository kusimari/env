# Post-install setup notes

Manual steps that are **not** part of the layer scripts (L0–L7) —
things the layers can't or shouldn't automate (interactive auth,
per-machine secrets, one-off remotes). The layered rebuild itself
(L1–L6 via `layer-run`, L7 via `workplace-setup.sh`) is documented in
`README.md`; don't duplicate it here.

- Add google drive remote: `rclone-env add` (interactive auth).
- Add ssh remote for desktop-aka: `rclone-env add` (interactive auth).
