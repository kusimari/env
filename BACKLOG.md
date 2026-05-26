# Backlog — env

Post-merge follow-ups. Each entry stands alone; pick one and land it
as its own PR. Cross-references to `Gorantls-env/BACKLOG.md` mark
items that need both repos changed together.

## Inline the L5 driver registries

**Status:** queued; do after the layer-restructure PR (PR #26) merges.

**Pairs with:** `Gorantls-env/BACKLOG.md` — the same change applies
to `desktop-layers/layer-5b.sh`. Land both together.

### What

Drop the registry framework in `layers/layer-5a.sh`. The
WORKSPACES/STORES heredocs + `iterate_registry` + `process_workspace`
+ `process_store` + per-row failure isolation are framework for
N-many entries; today the public driver has exactly one workspace
entry (`ai-workspace` → `git@github.com:kusimari/mAId.git`) and an
empty STORES heredoc. About 80 lines of indirection support a TSV
format with one row.

Inline the per-entry work directly. Keep the small reusable
helpers; drop the framework.

### Why

Surface area in a 1-entry registry doesn't justify the indirection.
Adding a new workspace becomes copy/paste-a-block instead of
appending a TSV row — both are equally easy at this scale, and the
block is more readable than the TSV + iterator combination.

### What to keep

These are real reuse, called per-entry regardless of whether the
caller is registry-driven or inline:

- `clone_or_fetch`
- `ensure_git_identity`
- `run_entry_point`
- The three-roots loop (`mkdir -p` for `~/tool-workplace`, `~/dabba`,
  `~/workplace`)
- The arg parser (`--dry-run`, `--skip-install`, `--help`)
- The `repo_basename` helper

### What to drop

- `WORKSPACES` heredoc + `STORES` heredoc
- `iterate_registry` function
- `process_workspace` + `process_store` wrappers
- `FAILURES` array and end-of-script summary loop
- `set -euo pipefail` → keep `set -uo pipefail` only. Replace `set
  -e` with explicit `|| warn "<entry>: failed"` after each entry
  block so a single failure doesn't abort the rest of the script
  (preserves today's fail-soft behavior the cheap way).

### Sketch (illustrative — final wording goes into the script)

```bash
# Roots — always-on, idempotent.
for r in "$TOOL_WORKPLACE_ROOT" "$DABBA_ROOT" "$WORKPLACE_ROOT"; do
    [[ -d "$r" ]] || run mkdir -p "$r"
done

# Workspace: ai-workspace/mAId
{
    name="ai-workspace"
    url="git@github.com:kusimari/mAId.git"
    clone="$TOOL_WORKPLACE_ROOT/$name/mAId"
    log "Workspace: $name/mAId"
    [[ -d "$TOOL_WORKPLACE_ROOT/$name" ]] || run mkdir -p "$TOOL_WORKPLACE_ROOT/$name"
    clone_or_fetch "$name/mAId" "$url" "$clone"
    ensure_git_identity "$clone" "$PUBLIC_USER_NAME" "$PUBLIC_USER_EMAIL"
    run_entry_point "$clone" install "$name/mAId"
} || warn "ai-workspace/mAId: failed (continuing)"

# Stores: none today. Add a block here when a public store appears.
```

### Files to edit

- `layers/layer-5a.sh` — body rewrite. Net ~60-80 lines shorter.
- `.kdevkit/project.md` — under "L5 framework — three roots, two
  registries", drop "two registries" / `iterate_registry` language;
  describe as "fixed list of workspace + store entries, one block
  per entry, hand-edit when adding".
- `README.md` — Layer-5 paragraph: drop "iterates two registries";
  switch to "for each known workspace + store, clones/fetches and
  hands off to the entry's own install". Adding a workspace section
  in feature-branch test instructions: "edit the entry block in a
  local checkout before running L5".

### Files to leave alone

- The three roots and their semantics (`~/tool-workplace/`,
  `~/dabba/`, `~/workplace/`) — unchanged.
- L1-L4 + L6 — unchanged.
- Helpers (`clone_or_fetch`, `ensure_git_identity`,
  `run_entry_point`, `repo_basename`) — unchanged.

### Verification

```bash
# Dry-run still mkdirs the three roots and walks the entry.
~/env-workplace/env/layers/layer-5a.sh --dry-run

# Live run is idempotent on the second invocation:
~/env-workplace/env/layers/layer-5a.sh
~/env-workplace/env/layers/layer-5a.sh   # second run: only "fetching updates"

# A failed entry doesn't abort subsequent entries — temporarily
# break the URL and confirm the script continues + exits non-zero.
```

### Resumption notes for a fresh session

- Read `layers/layer-5a.sh` end-to-end first; the helpers it keeps
  are short, the framework it drops is the bulky middle.
- Mirror the change in `Gorantls-env/desktop-layers/layer-5b.sh`
  before testing — `layer-5b.sh` `PUBLIC_DRIVER` invokes 5a, so
  testing one without the other still works, but the shape of 5b
  should match.
- Two-commit cadence (one per repo) keeps the change reviewable; or
  a single CR on Gorantls-env after the env PR merges, since the
  Gorantls-env side stands alone once 5a is merged.
