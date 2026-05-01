# feature-build-layers

## Status
📋 **Not started.** This doc captures the plan that was drafted on
`misc-updates` but deferred so gittree issues could be isolated. A new
branch `feature-build-layers` will carry the implementation.

## Feature Brief
Two related pieces of work that make the env repo easy to bootstrap
onto a fresh machine and easy to extend with global AI tooling:

1. **Four-layer build model** — a clean separation between machine-
   specific prep, generic source sync, the nix build itself, and
   machine-specific post-nix steps.
2. **`mAId` sibling repo integration** — a sibling repo at
   `~/env-workplace/mAId` that holds global Claude / Kiro / Gemini
   configs, MCP settings, and reusable skills/agents/prompts. env
   clones it during bootstrap; activation into `$HOME` is driven by
   mAId's own `flake.nix` (`nix profile install`-style).

## Scope guardrails

**This repo is public; this document stays site-agnostic.** Avoid
naming specific employers, internal tools, private repos, auth
systems, or clone URLs. Sibling repositories that carry machine-
specific or site-specific concerns are referred to abstractly as
*private sibling env repo(s)*. Concrete URLs and credentials live in
the bootstrap scripts themselves (where they belong), not in design
docs.

## Why the two pieces are bundled
mAId lives next to env. The cleanest way to have both cloned and
present for bootstrap is to let the new `bootstrap-common.sh` clone
both as part of Layer 2. Doing the bootstrap refactor without mAId
leaves an awkward "now also clone this other repo" step dangling.

## Requirements

### FR-four-layer (bootstrap)
- **FR-B1**: Every Layer-3 build (`build-nix/<platform>.sh`) must be
  preceded by a Layer-1 script. Layer-1 may be empty but must exist so
  the flow is uniform.
- **FR-B2**: `env/build-nix/bootstrap-common.sh` (Layer 2) is
  curl-able AND runnable from a clone; the same script auto-detects
  its mode.
- **FR-B3**: `bootstrap-common.sh` scope: create `~/env-workplace`,
  check GitHub SSH reachability, clone/fetch `env`, clone/fetch
  `mAId`. Accept `--pre-nix <script>` / `--post-nix <script>` hooks so
  Layer 1 and Layer 4 can inject without modifying env.
- **FR-B4**: Platform-specific prep that is machine- or site-specific
  (e.g. credential enrollment, private auth flows, internal tool
  installation, sudoers rules for Nix directory creation) lives in a
  *private sibling env repo*, outside this public repo. Those
  Layer-1 scripts delegate to `bootstrap-common.sh` at the end. The
  public repo does not need to know they exist.
- **FR-B5**: `env/build-nix/bootstrap-ubuntu.sh` is a new Layer-1
  script for Ubuntu. Plain Ubuntu setup is generic (no site-specific
  auth), so its Layer 1 lives in env.
- **FR-B6**: Layer-3 scripts (`build-nix/<platform>.sh`) stay pure:
  nix invocation only; no prep.
- **FR-B7**: Layer 4 is a convention, not scripts-on-this-branch. The
  `--post-nix` hook path in Layer 2 is the extension point.
- **FR-B8**: All bootstrap scripts idempotent.

### FR-mAId
- **FR-M1**: Repo is a sibling of env on the same hosting platform.
  Concrete org/owner and URL captured in `bootstrap-common.sh` — not
  in this doc.
- **FR-M2**: Initial layout:
  ```
  mAId/
  ├── flake.nix              # packages.<system>.default = activator
  ├── CLAUDE.md              # minimal pointer to skills/
  ├── GEMINI.md              # minimal pointer
  ├── KIRO.md                # minimal pointer
  ├── skills/
  │   ├── git.md             # authored interactively
  │   └── development.md     # authored interactively
  ├── agents/                # reserved, empty
  ├── commands/              # reserved, empty
  └── mcp/                   # reserved, empty
  ```
- **FR-M3**: `nix profile install .` from inside mAId creates symlinks
  in `$HOME` pointing into the mAId checkout (so edits are live):
  `~/CLAUDE.md`, `~/.claude/{skills,agents,commands,mcp}`,
  `~/GEMINI.md`, `~/.gemini/...`, `~/KIRO.md`, `~/.kiro/...`.
- **FR-M4**: env does NOT take mAId as a flake input on this branch.
  `bootstrap-common.sh` clones it; user runs `nix profile install .`
  once. Future branch may wire in a `mAId.nix` flake input.
- **FR-M5**: Root markdown files (`CLAUDE.md` etc.) are minimal
  pointers. Content lives in `skills/*.md`.
- **FR-M6**: Accept potential conflict with home-manager file
  management — mAId only touches paths env doesn't manage. Fallback to
  a plain `activate.sh` if `nix profile install` collides in practice.

### Non-functional
- **NFR1**: `./build-nix/test.sh` stays green.
- **NFR2**: Bootstrap surfaces clear error messages for manual-action
  cases (e.g., "add this key to GitHub").
- **NFR3**: No new flake inputs in env (mAId via flake input is a
  later branch).
- **NFR4**: Bootstrap scripts shellcheck-clean.
- **NFR5**: No site-specific or employer-specific identifiers in
  scripts or docs that live in this public repo. If such coupling is
  needed, it belongs in a private sibling env repo.

### Success criteria
- Fresh Linux VM → `curl ... bootstrap-<platform>.sh | bash` →
  bootstrap completes → `./build-nix/<platform>.sh` succeeds.
- `cd ~/env-workplace/mAId && nix profile install .` creates working
  symlinks pointing into the checkout (`ls -l ~/CLAUDE.md` resolves
  into the mAId clone).
- Re-running the bootstrap on the same machine is a no-op.

## Design (sketch)

### Four-layer diagram
```
Layer 1: machine prep                     Layer 2: generic sync
  Private sibling env repo                  env/build-nix/
    bootstrap-<platform>.sh                   bootstrap-common.sh
    (site-specific auth,                      (env + mAId clones,
     internal tooling, etc.)                   GitHub SSH,
  env/build-nix/                               --pre-nix / --post-nix hooks)
    bootstrap-ubuntu.sh
    (generic Ubuntu prep)

                    │
                    ▼
Layer 3: nix build                        Layer 4: post-nix machine-specific
  env/build-nix/                            (convention; hook in Layer 2)
    al2.sh / al2023.sh /
    darwin.sh / ubuntu.sh
```

### D-bootstrap-common
- Detect curl-vs-clone via `readlink -f "${BASH_SOURCE[0]}"`. If inside
  `~/env-workplace/env/`, assume clone mode. Else curl mode: clone env
  first, re-exec from the clone with a `--post-clone` sentinel to
  prevent re-exec loops.
- Clone/fetch both env and mAId from their GitHub URLs (URLs hard-
  coded in the script, not in this doc).
- Run optional `--pre-nix` and `--post-nix` scripts if provided.

### D-mAId flake
- `packages.<system>.default` exposes an activator that, when
  installed, places the symlinks in `$HOME`. Either:
  - `nix profile install .` → adds an `maid-activate` binary to
    `~/.nix-profile/bin`, user runs it once, or
  - bundle the symlinks directly into the profile derivation so
    installation itself creates them.
- First approach is cleaner (user controls when to activate); second
  is more automatic. Decide during implementation.
- Fallback plan: if `nix profile install` conflicts with home-manager
  paths in practice, ship a plain `activate.sh` and document it in
  env's `setup-notes.md`.

### D-skills content
- `skills/git.md` and `skills/development.md` authored interactively
  with the user during implementation. Format: rule + **Why:** +
  **How to apply:** (matches the feedback-memory convention).

## Automated testing

1. `shellcheck -x` over env's `build-nix/*.sh`.
2. `bootstrap-common.sh --dry-run` twice against a staging `HOME` to
   verify idempotency.
3. Manual: one real Linux VM run before merge.
4. Manual: `nix profile install` in mAId on a test host, verify
   `ls -l ~/CLAUDE.md` resolves into the checkout.

## Out of scope
- gittree CLI / lazygit rework (on `issue-emacs-gittree`).
- Test harness for emacs (part of `issue-emacs-gittree`).
- Anything specific to a particular employer's infrastructure —
  belongs in private sibling repos.

## Session Log
<!-- Newest at top -->

### 2026-04-30 - Scrub site-specific references
- Removed employer-specific names, internal tool names, and specific
  org/user handles from the doc. Replaced with abstract references
  ("private sibling env repo", "site-specific auth"). Added NFR5 to
  make the policy explicit. Concrete clone URLs stay in the
  bootstrap scripts, not in this design doc.

### 2026-04-30 - Spun out from misc-updates
- Split bootstrap + mAId work out of `misc-updates` into this new
  feature doc. The mAId sibling repo is already cloned empty at
  `~/env-workplace/mAId` with local branch `misc-updates`; when work
  begins, rename / branch off to `feature-build-layers`.
- No code written yet. First implementation task is the requirements
  pass with the user (interactive skills content, nail down activator
  mechanism, decide on build-script unification question).
