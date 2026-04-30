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

## Why the two are bundled
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
  Layer-1 and Layer-4 can inject without modifying env.
- **FR-B4**: `Gorantls-env/desktop/bootstrap-al2.sh` and
  `bootstrap-al2023.sh` (Layer 1) are pared to Amazon-only work
  (midway, Amazon SSH, toolbox, Nix install, sudoers, SSL cert). They
  delegate to `bootstrap-common.sh` at the end.
- **FR-B5**: `env/build-nix/bootstrap-ubuntu.sh` is a new Layer-1
  script for Ubuntu. Ubuntu isn't Amazon-specific so Layer 1 lives in
  env.
- **FR-B6**: Layer-3 scripts (`build-nix/<platform>.sh`) stay pure:
  nix invocation only; no prep.
- **FR-B7**: Layer 4 is a convention, not scripts-on-this-branch. The
  `--post-nix` hook path in Layer 2 is the extension point.
- **FR-B8**: All bootstrap scripts idempotent.

### FR-mAId
- **FR-M1**: Repo `kusimari/mAId` (already created, empty). Branch
  `misc-updates` already checked out as a sibling. Work on a new
  `feature-build-layers` branch in mAId; initial content committed
  there.
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

### Success criteria
- Fresh AL2023 VM → `curl ... bootstrap-al2023.sh | bash` → bootstrap
  completes → `./build-nix/al2023.sh` succeeds.
- `cd ~/env-workplace/mAId && nix profile install .` creates working
  symlinks pointing into the checkout (`ls -l ~/CLAUDE.md` resolves
  into the mAId clone).
- Re-running the bootstrap on the same machine is a no-op.

## Design (sketch)

### Four-layer diagram
```
Layer 1: machine prep                     Layer 2: generic sync
  Gorantls-env/desktop/                     env/build-nix/
    bootstrap-al2.sh                          bootstrap-common.sh
    bootstrap-al2023.sh                       (env + mAId clones,
  env/build-nix/                               GitHub SSH,
    bootstrap-ubuntu.sh                        --pre-nix / --post-nix hooks)

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
- Clone/fetch both `env` (`git@github.com:kusimari/env.git`) and
  `mAId` (`git@github.com:kusimari/mAId.git`).
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

1. `shellcheck -x` over `build-nix/*.sh` and (when mAId is cloned)
   `Gorantls-env/desktop/bootstrap-*.sh`.
2. `bootstrap-common.sh --dry-run` twice against a staging `HOME` to
   verify idempotency.
3. Manual: one real AL2023 VM run before merge.
4. Manual: `nix profile install` in mAId on a test host, verify
   `ls -l ~/CLAUDE.md` resolves into the checkout.

## Out of scope
- gittree CLI / lazygit rework (on `issue-emacs-gittree`).
- Test harness for emacs (part of `issue-emacs-gittree`).

## Session Log
<!-- Newest at top -->

### 2026-04-30 - Spun out from misc-updates
- Split bootstrap + mAId work out of `misc-updates` into this new
  feature doc. mAId sibling repo is already cloned empty at
  `~/env-workplace/mAId` with local branch `misc-updates`; when work
  begins, rename / branch off to `feature-build-layers`.
- No code written yet. First implementation task is the requirements
  pass with the user (interactive skills content, nail down activator
  mechanism, decide on build-script unification question).
