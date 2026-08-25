# Backlog: claude-desktop-on-ubuntu-mane

## What

Install and wire Claude Desktop on the `ubuntu-mane` target, as the
personal-machine counterpart to the `darwin-kelasa` work tracked as an
initiative in the `<kelasa-specific env repo>`. Personal mode signs in
with a vendor account instead of routing through a third-party inference
provider, so the credential wiring that dominates the kelasa side
reduces to a sign-in — but the install itself, and the AI-resource
plugin wiring, still need a home.

Note there is currently **no `layer-4-mane.sh`** anywhere: L4 exists only
for kelasa. This would be the first `mane` L4, or would need another home.

## Why

That initiative was deliberately scoped to `darwin-kelasa`, the machine
it was designed on, because `mane` work cannot be verified from a kelasa
machine. Deferring rather than shipping-untested keeps it honest.

Linux support is also weaker: Claude Desktop on Linux is **beta**,
supports Ubuntu/Debian (x64 and arm64), and while Chat, Code, and Cowork
all work there, Computer Use is unavailable, dictation is missing, and
the Quick Entry hotkey needs the desktop's GlobalShortcuts portal on
native Wayland. Cowork also requires working hardware virtualization.

## Open questions

- Does `ubuntu-mane` meet the Cowork device requirements? Run the
  published Linux readiness check on that machine first.
- Is there a Linux package for the desktop app, and does it belong in
  `linuxGraphicalConfiguration.home.packages` next to `pkgs.google-chrome`?
  On the Darwin side the install is declarative — the app is a Homebrew
  cask, so it joins `darwinConfiguration.homebrew.casks` alongside the
  other self-updating GUI apps. There is no `nixpkgs` derivation for the
  desktop app on `aarch64-darwin` (the CLI does have one), so confirm what
  exists for Linux before assuming symmetry. If nothing packaged exists,
  this becomes an L4 job — and note there is no `layer-4-mane.sh` yet.
- Does a paid personal plan exist on that account? Cowork and Code
  require a paid tier; Chat alone is available on the free tier.
- Can the plugin mechanism chosen on the kelasa side be reused as-is?
  Personal mode uses a different application-data root than third-party
  mode, and the Linux root differs again from macOS.
