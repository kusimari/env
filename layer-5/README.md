# env/layer-5

Layer 5 is the "fast-moving updates" layer on top of the nix-managed
base env. Its job is to clone content-bearing workspaces and hand
off to their own install entry-points. L5 does not build, does not
symlink, does not know what any workspace actually installs.

See `env/README.md` for the full layer model.

## Entry point

```
env/layer-5/run
```

Runs the public workspace registry. Flags:

| Flag | Effect |
|---|---|
| `--dry-run` | Log planned actions; make no changes. |
| `--skip-install` | Clone/fetch only; do not invoke any entry-point. Useful when a new workspace's entry-point doesn't exist yet. |
| `--help`, `-h` | Show the script header and exit. |

The driver is idempotent. Running it twice does no harm: existing
clones are fetched, identities are verified-and-skipped.

## Registry format

The registry lives inside `run` itself — a tab-separated heredoc
around the top of the file:

```
# Format: <workspace-name>\t<repo-url>\t<entry-point>
ai-workspace	git@github.com:kusimari/mAId.git	install
```

Columns:

1. **workspace-name** — directory under `~/workplace/`. The driver
   creates `~/workplace/<workspace-name>/` and clones the repo
   inside it.
2. **repo-url** — anything `git clone` accepts.
3. **entry-point** — path (relative to the clone) of an executable
   the driver runs after clone/fetch. Convention is `install` at
   the repo root.

Blank lines and `#`-comments are ignored.

## Adding a workspace

Edit `run`, add a row to the `WORKSPACES` heredoc, commit. That's
it. No other files need updating on this side.

## Cross-repo contract

If a workspace repo renames its entry-point (the third column in
the registry), update `run` here. Otherwise the driver will log a
`entry point not found` warning and skip the install step.

The public registry is public-only — do not add private repos or
corporate URLs. Private workspaces live in the private layer-5
registry, on a separate repo.

## Testing the driver without a real entry-point

When building out a new workspace whose `install` script doesn't
exist yet, invoke with `--skip-install` to exercise only the
clone/fetch and identity-pin steps:

```
env/layer-5/run --dry-run --skip-install   # preview
env/layer-5/run --skip-install             # for real
```
