# misc-updates

## How to use this session file
This session file contains comprehensive specifications generated through structured interviews. Each section serves dual purposes:
1. **Decision Record**: Captures user thoughts, preferences, and rationale for future reference
2. **Agent Instructions**: Provides clear, actionable guidance for coding agents to implement the feature

Coding agents should read the entire specification before beginning work, paying particular attention to the Requirements, Technical Design, and Test Strategy sections. This file intentionally omits an implementation task list (per user request); implementation proceeds from the spec directly.

## Feature Brief

Branch: `misc-updates` (both in `env` and in the new sibling `mAId` repo).

Six grouped updates bundled on one branch:

1. **gittree CLI**: extend `emacs-gittree` to accept two committish args + optional file. Preserve gittree layout — left = tree, right = file (diff if changed).
2. **Bootstrap refactor (four-layer model)**: Layer 1 machine-specific non-nix prep → Layer 2 generic `bootstrap-common.sh` (fetch nix sources, pre/post hooks) → Layer 3 `nix build` → Layer 4 machine-specific post-nix (hook reserved).
3. **Headless Linux targets**: split `linuxConfiguration` so AL2/AL2023 kelasa don't install chrome/rofi.
4. **`mAId` sibling repo**: new repo at `~/env-workplace/mAId` for global Claude/Kiro/Gemini config. Ships its own `flake.nix` so `nix profile install .` creates `$HOME` symlinks. env does NOT import mAId yet.
5. **lazygit integration**: swap `E` binding to new `emacs-gittree` CLI if one-liner.
6. **Markdown mode in core emacs**: promote `emacs/markdown-mode.el` from opt-in module to default in `core.el`.

## Status
📋 Not Started → starting with Groups 1 & 6 in parallel.

**Status Legend:** 📋 Not Started | ⏳ In Progress | ✅ Complete

Per-group status:
- Group 1 (gittree CLI): ✅ (bug fixes for arbitrary refs + diff-based tree status added 2026-04-30)
- Group 3 (headless linux): ✅
- Group 5 (lazygit integration): ✅ — E binding rewritten for files / commits / commitFiles contexts
- Group 6 (markdown-mode): ✅ (tar/gzip runtime deps added 2026-04-30)
- Group 4 (mAId repo): 📋 — repo cloned empty at `~/env-workplace/mAId`, branch `misc-updates`
- Group 2 (bootstrap four-layer): 📋

**Reordered landing** (reflecting emacs-wave bundling): 1 & 6 & 3 & 5 (all emacs/diff related, landing together) → 4 (mAId) → 2 (bootstrap).

## Requirements Specification

### Functional Requirements

#### FR-gittree (Group 1)
- **FR-G1**: `emacs-gittree <committish-a> <committish-b> [file]` must preserve the **gittree layout**: left = treemacs file tree, right = file view (with or without diff based on changes).
  - With `[file]`: right panel opens that file in the dual-panel view (diff if the two refs differ; plain view if identical).
  - Without `[file]`: emacs opens in `gittree-mode` with tree active; user picks a file; right panel then shows the two-committish diff.
  - Refs may be `working` (on-disk), `:0` (staged), `HEAD`, or any git ref. Non-literal tokens → `git show <ref>:<path>`.
- **FR-G2**: Zero args → today's behavior preserved (`emacs --eval "(gittree-mode 1)"`).
- **FR-G3**: One arg → `<ref> vs working`, still in gittree layout.
- **FR-G4**: Underlying diff display reuses existing elisp (`gittree-show-dual-panel` / `gittree--create-buffer`). Only new elisp is the thin launcher that keeps the treemacs side panel present.

#### FR-bootstrap (Group 2)
- **FR-B1**: `env/build-nix/bootstrap-common.sh` must be both curl-able and runnable from a local clone; same script detects its mode.
- **FR-B2**: `bootstrap-common.sh` scope: create `~/env-workplace`; ensure GitHub SSH works (else print pubkey and exit 1); clone/fetch `env`; clone/fetch `mAId`. Accepts `--pre-nix` / `--post-nix` hook flags so Layer 1 / Layer 4 can inject without modifying env. **Not in scope**: Nix install, Amazon SSH, midway, toolbox, sudo.
- **FR-B3**: `Gorantls-env/desktop/bootstrap-al2.sh` / `bootstrap-al2023.sh` (Layer 1) pared to Amazon-only responsibilities; delegate to Layer 2 `bootstrap-common.sh` at the end.
- **FR-B4**: Audit `env/build-nix/{darwin,ubuntu,al2,al2023}.sh`. Move pre-nix prep that is Layer-1 work into `Gorantls-env/desktop/bootstrap-<platform>.sh` (Amazon platforms). Ubuntu Layer-1 lives in env. Layer-3 scripts become pure nix invocations.
- **FR-B5**: All bootstrap scripts idempotent; safe to re-run after partial failure.
- **FR-B6**: Layer 4 is a new concept reserved for machine-specific non-nixable setup that runs *after* `nix build` but still interacts with nix output. No Layer-4 scripts ship on this branch; the hook point in `bootstrap-common.sh` (a post-build callback) enables future Layer-4 additions without touching env build logic.

#### FR-headless (Group 3)
- **FR-H1**: al2-kelasa and al2023-kelasa must not install `google-chrome`, enable `programs.rofi`, or populate rofi desktop files.
- **FR-H2**: ubuntu-mane unchanged.
- **FR-H3**: Split driven by `envKind` at the **module-list level** (not inside modules).
- **FR-H4**: `linuxConfiguration` becomes pure shared base (nixGL overlay, `nix.package`). Graphical pieces move to new `linuxGraphicalConfiguration`.

#### FR-mAId (Group 4)
- **FR-M1**: Public GitHub repo `kusimari/mAId` (already created, empty). Work on `misc-updates` branch for initial commits.
- **FR-M2**: Initial layout:
  ```
  mAId/
  ├── flake.nix
  ├── CLAUDE.md            # minimal → points to skills/
  ├── GEMINI.md            # minimal
  ├── KIRO.md              # minimal
  ├── skills/
  │   ├── git.md
  │   └── development.md
  ├── agents/              # empty
  ├── commands/            # empty
  └── mcp/                 # empty
  ```
- **FR-M3**: `mAId/flake.nix` exposes a package such that `nix profile install .` produces `$HOME` symlinks pointing **into the mAId checkout** (so edits are live): `~/CLAUDE.md`, `~/.claude/{skills,agents,commands,mcp}`, `~/GEMINI.md`, `~/.gemini/*`, `~/KIRO.md`, `~/.kiro/*`.
- **FR-M4**: env does NOT take mAId as a flake input on this branch. `bootstrap-common.sh` clones mAId only. Follow-up: `mAId.nix` flake input in env.
- **FR-M5**: Root markdown files are minimal pointers; content lives in `skills/*.md`.
- **FR-M6**: Assumed: `nix build` (env/home-manager) and `nix profile install` (mAId) don't conflict. Fallback: plain `activate.sh` in mAId if they do.

#### FR-lazygit (Group 5)
- **FR-L1**: Existing `E` custom command keeps working after Group 1.
- **FR-L2**: If new CLI replaces `(gittree-compare-working ...)` cleanly as a one-liner, rewrite. Else defer.
- **FR-L3**: No new keybindings on this branch.

#### FR-markdown (Group 6)
- **FR-MD1**: `markdown-mode` must auto-activate for `.md` files in every emacs session, without setting `EMACS_MODULES`.
- **FR-MD2**: Promote the package to `core.el`'s default ELPA pattern (`my-use-package markdown-mode` with `:ensure t`).
- **FR-MD3**: `.md` files open via `auto-mode-alist` → `markdown-mode`. yasnippet `text-mode-hook` and global `visual-line-mode` continue to apply.
- **FR-MD4**: Remove `emacs/markdown-mode.el` — module-local variant redundant once in core.

### Non-Functional Requirements
- **NFR1**: `./build-nix/test.sh` passes for all four platforms.
- **NFR2**: Bootstrap surfaces clear error messages for manual-action cases.
- **NFR3**: No new flake inputs on this branch (mAId via flake input is follow-up).
- **NFR4**: No backwards-compat shims for removed graphical packages.
- **NFR5**: Markdown mode loads unconditionally (no opt-in env var).

### Success Criteria
- Fresh AL2023 bootstrap works end-to-end via `bootstrap-al2023.sh`.
- `nix flake check` + `./build-nix/test.sh` green on all four targets.
- kelasa `nix-store` has no chrome/rofi.
- `emacs-gittree HEAD~1 HEAD README.md` shows tree-left + diffed-file-right.
- `emacs-gittree` (no args) → gittree-mode (unchanged).
- `cd ~/env-workplace/mAId && nix profile install .` produces live-editable `$HOME` symlinks.
- `emacs README.md` → major mode is `markdown-mode`.

## Technical Design Specification

### D-gittree (Group 1)
- Replace `home.shellAliases.emacs-gittree` in `home/home.nix` with a `writeShellScriptBin "emacs-gittree"` (aliases can't dispatch on arg count).
- Script emits a single `emacs --eval "(...)"` call:
  - 0 args → `(gittree-mode 1)`.
  - 1+ args → new `gittree-launch` elisp wrapper that (a) activates `gittree-mode` so treemacs is present, (b) calls dual-panel logic for the right-hand window with the given refs/file.
- **New elisp**: `gittree-launch (left-ref right-ref &optional file)` in `emacs/core-gittree.el` — thin wrapper over `gittree-mode` + `gittree-show-dual-panel`.
- Arg convention: positional only. Literals `working`, `:0`, `empty` handled specially; any other token is passed to `git show <ref>:<path>` (so HEAD, HEAD~N, SHA, branch, tag all work).
- **Bug fix (2026-04-30)**: `gittree--create-buffer` previously errored on non-literal refs ("Unknown ref: HEAD~1"). Default branch now routes arbitrary refs through `gittree--create-git-buffer`.
- **Tree status under two refs (2026-04-30)**: when `gittree-launch-left-ref` and `gittree-launch-right-ref` are both set (and neither is `working`/`:0`), `gittree-refresh-status` sources file-change status from `git diff --name-status <left> <right>` instead of `git status --porcelain`. This makes the tree highlight files that changed *between the two commits* rather than the working-tree state. Added helper `gittree--get-ref-diff-status`.
- **Reuse**: `gittree-show-dual-panel` (`core-gittree.el:256`), `gittree--create-buffer` (`core-gittree.el:243`), `gittree-mode` (`core-gittree.el:486`).

### D-bootstrap (Group 2) — four-layer model

```
Layer 1: machine-specific non-nix prep     ─►  Layer 2: generic sync
  Gorantls-env/desktop/                         env/build-nix/
    bootstrap-al2.sh                              bootstrap-common.sh
    bootstrap-al2023.sh                           (env + mAId clones,
  env/build-nix/                                   GitHub SSH,
    bootstrap-ubuntu.sh   (ubuntu not Amazon)     --pre-nix / --post-nix hooks)
  [darwin: manual steps, then bootstrap-common.sh]

                        │
                        ▼
Layer 3: nix build                          ─►  Layer 4: post-nix machine-specific
  env/build-nix/                                (reserved — no scripts ship;
    al2.sh / al2023.sh /                         --post-nix hook in Layer 2
    darwin.sh / ubuntu.sh                        enables Layer-4 additions)
    (pure nix switch via _common.sh)
```

- **Layer 1 invariant**: every Layer-3 build has a Layer-1 precursor (possibly empty). Layer-1 ends with `exec ~/env-workplace/env/build-nix/bootstrap-common.sh "$ENV_BRANCH"`.
- **Layer 2** is self-bootstrapping: `readlink -f "${BASH_SOURCE[0]}"` detects curl vs. clone mode. Curl mode: clone env, re-exec from clone with `--post-clone` sentinel (prevents loop). `--pre-nix <script>` / `--post-nix <script>` flags let Layer 1 / Layer 4 inject hooks.
- **Layer 3**: pure `nix run` / `home-manager switch` via existing `_common.sh` sed dance. No prep.
- **Layer 4**: a convention, not scripts. Hook exists in Layer 2's `--post-nix` flag.
- **Build-script unification**: keep four per-platform scripts (trivial cost, clearer). Revisit after bootstrap extraction stabilizes.

### D-headless (Group 3)

```nix
linuxBaseConfiguration = { pkgs, ... }: {
  nix.package = pkgs.nix;
  nixpkgs.overlays = [ inputs.nixgl.overlays.default ];
};

linuxGraphicalConfiguration = { pkgs, lib, ... }: {
  home.packages = [ pkgs.google-chrome ];
  home.file = lib.mapAttrs' (...) (builtins.readDir ./rofi-desktop);
  programs.rofi = { ... };
};
```

Module lists:
- `ubuntu-mane`: `[common, linuxBase, linuxGraphical, {user, home}, ./home/home.nix]`
- `al2-kelasa`: `[common, linuxBase, al2KelasaConfiguration, ./home/home.nix]`
- `al2023-kelasa`: `[common, linuxBase, al2KelasaConfiguration, ./home/home.nix]`

`envKind` stays in `extraSpecialArgs` untouched; headless selection is module-list composition. `rofi-desktop/` directory retained (referenced only from `linuxGraphicalConfiguration`).

### D-mAId (Group 4)
- **Repo URL**: `git@github.com:kusimari/mAId.git` (confirmed). Empty on creation — initial content committed on `misc-updates` branch.
- **`mAId/flake.nix`**: exposes `packages.<system>.default`. Package produces `$out/bin/maid-activate` that creates `$HOME` symlinks into the checkout at runtime. `nix profile install .` puts the activator on `PATH`. Fallback: plain `activate.sh` at repo root if `nix profile` clashes with home-manager paths.
- **Minimal root files**: `CLAUDE.md` = `See skills/ for reusable practices.` Same pattern for `GEMINI.md` / `KIRO.md`.
- **`skills/git.md` + `skills/development.md`**: authored interactively with user. Structure: rule + **Why:** + **How to apply:** (matches feedback-memory convention).
- **env-side**: `bootstrap-common.sh` clones mAId; nothing else. `setup-notes.md` gains an "activate" line.
- **Future**: `mAId.nix` in env imports mAId as flake input.

### D-lazygit (Group 5)
Replaced the single `E` binding with three context-specific ones, all routing through the new `emacs-gittree` CLI (which means the tree on the left highlights changed files and the right panel vdiffs the selected file):
- **files** context: `emacs-gittree {{.SelectedCommit.Sha}} working {{.SelectedFile.Name}}` — working tree vs a commit.
- **commits** context: `emacs-gittree {{.SelectedLocalCommit.Sha}}^ {{.SelectedLocalCommit.Sha}}` — diff a commit against its parent; pick any changed file from the tree.
- **commitFiles** context: `emacs-gittree {{.SelectedLocalCommit.Sha}}^ {{.SelectedLocalCommit.Sha}} {{.SelectedCommitFile.Name}}` — jump directly to a file's diff within a commit.

### D-markdown (Group 6)
Add to `emacs/core.el` after the General Programming Support section:
```elisp
(defun activate-markdown ()
  (my-use-package markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode)))
(activate-markdown)
```
Remove `emacs/markdown-mode.el`. `visual-line-mode` is already global (`core.el:195`); yasnippet's `text-mode-hook` (`core.el:352`) fires because `markdown-mode` derives from `text-mode`.

**Runtime deps (2026-04-30)**: `package.el` uses `tar xf` to extract MELPA tarballs. On minimal images (e.g., headless AL2023) the system `tar`/`gzip` may not be visible to emacs's subprocess PATH, producing "Failed to install markdown-mode: tar not found". Added `pkgs.gnutar` and `pkgs.gzip` to `home/emacs.nix`'s `home.packages` so they're on the nix profile PATH. No new flake inputs.

## Test Strategy Specification

### Test layers
1. **Flake evaluation** (`./build-nix/test.sh`): extend to cover `al2023-kelasa`; build all four `.activationPackage`s with `--no-link`.
2. **Headless regression** (new `build-nix/test-headless.sh`): `nix eval .#homeConfigurations.<cfg>.activationPackage.drvPath` → `nix derivation show` → JSON grep for `google-chrome`/`rofi`. al2/al2023: 0 hits. ubuntu-mane: ≥1 hit each.
3. **Bootstrap tests**:
   - `shellcheck -x` over `build-nix/*.sh` and Gorantls-env bootstrap scripts (when sibling present).
   - `--dry-run` flag on `bootstrap-common.sh`.
   - Idempotency smoke: twice against staging `HOME`; diff.
   - Full integration on fresh AL2023 VM once before merge (manual).
4. **gittree CLI test** (new `emacs/test-gittree-cli.sh`): `mktemp -d` git repo, two commits on fixture, invoke `emacs-gittree HEAD~1 HEAD <file>` in batch, assert diff buffers exist AND `gittree-mode` is active. Zero-arg: assert `gittree-mode` on.
5. **markdown-mode test**: `emacs --batch` on a `.md` fixture, eval `major-mode`, assert `markdown-mode`.
6. **mAId symlink test**: CI in mAId repo (not env). `nix profile install .` in container, assert symlinks resolve into checkout.

### Not automated
Emacs UI / vdiff visual layout; lazygit keypress; darwin bootstrap.

### Harness changes
`build-nix/test.sh` grows independently-reporting sections: flake eval + al2023, headless grep, shellcheck, gittree CLI, markdown-mode. Uses `nix shell nixpkgs#shellcheck -c` and `nix shell nixpkgs#emacs -c` — no new flake inputs.

## Dependencies & Suggested Landing Order
- Group 1 (gittree CLI) — independent.
- Group 6 (markdown-mode) — independent.
- Group 3 (headless) — independent.
- Group 4 (mAId) — independent of env.
- Group 2 (bootstrap) — references mAId URL.
- Group 5 (lazygit) — depends on Group 1 merged.

**Order: 1 & 6 → 3 → 4 → 2 → 5.**

## Risks
- **Gittree layout preservation**: `gittree-show-dual-panel` may destroy treemacs on window split. Mitigation: `gittree-launch` guards the treemacs window.
- **`nix profile` / home-manager clash**: mAId targets paths env doesn't manage — low risk. Fall back to `activate.sh`.
- **Bootstrap re-exec loop**: `--post-clone` sentinel guards.
- **Kelasa users losing chrome/rofi**: intended cleanup; `setup-notes.md` updated.
- **Amazon bootstrap drift**: Layer-1 is thin; cross-repo coordination cost low.
- **Markdown-mode major-mode conflict**: unlikely; `.dir-locals.el` overrides are correct behavior if present.

## Session Log
<!-- Instructions: Newest at top -->

### 2026-04-30 - Session Focus: emacs-wave iteration 2 (D-1..D-4)
Context: after iteration 1 landed, field testing on AL2023 surfaced
two real bugs (bad refs showed `fatal:` text in vdiff; cleanup-panels
squeezed treemacs) plus the workflow gap of no automated way to
exercise the elisp. Plan file: `we-use-env-for-elegant-blum.md` on the
claude plans dir.

- **D-3 (test harness)** ✅ first — created `env/emacs/tests/{run.sh,stubs.el,helpers.el,test-gittree.el}`. Builds a throwaway two-commit fixture repo (`alpha.md` modified, `beta.md` deleted, `gamma.md` added). Runs `emacs --batch` with stubs.el providing `my-use-package` + treemacs/vdiff no-ops, so `core-gittree.el` loads cleanly. 6 test cases covering zero-arg mode toggle, added-file empty-left, deleted-file empty-right, modified-file both-populated, `gittree--get-ref-diff-status` A/M/D shape, and short-SHA equivalence. First run against pre-fix code correctly reproduced the `"fatal: path 'gamma.md' exists on disk, but not in 'HEAD~1'"` error the user reported.
- **D-1 (empty-on-missing)** ✅ added `gittree--ref-has-path-p` using `git cat-file -e`; `gittree--create-git-buffer` now routes "path not at ref" to a labeled empty buffer (`<file>@<ref> (missing)`). Same refactor applied to latent callsites `gittree-compare-commits` + `gittree-compare-working`. Also fixed the buffer-reuse case (buffers are read-only after first creation) with `inhibit-read-only` + `read-only-mode -1` inside the buffer context.
- **D-2 (cleanup-panels split)** ✅ wrapped the empty-content-windows split in `with-selected-window treemacs-win (split-window-right)` so a fresh gittree session doesn't squeeze treemacs itself.
- **D-4 (elpa wipe on source change)** ✅ added `home.activation.emacsElpaRefresh` in `home/emacs.nix`. Hashes `~/.config/emacs` (`find -print0 | sort -z | sha256sum`), stores in `~/.emacs.d/.emacs-source-hash`. Wipes `~/.emacs.d/elpa` only when the hash differs, so routine switches stay fast but stale-byte-compile recoveries are automatic.
- Hooked `emacs/tests/run.sh` into `build-nix/test.sh` — failing tests now fail the top-level nix test script.
- Result: 6/6 gittree tests green. `./build-nix/test.sh` exits 0 end-to-end.

### 2026-04-30 - Session Focus: emacs wave fixes (Groups 1 + 6) and Group 5
- **Group 1 fix**: `emacs-gittree <ref-a> <ref-b> [file]` was silently unusable:
  1. `gittree--create-buffer` had `(t (error "Unknown ref: %s" ref))` — any non-literal ref (HEAD~1, SHAs, branches) threw, aborting the dual-panel. Fixed: default branch now delegates to `gittree--create-git-buffer` so arbitrary refs pass through to `git show`. Docstring rewritten to match.
  2. Tree didn't highlight files changed between the two refs (status cache sourced from working-tree `git status`). Added `gittree--get-ref-diff-status` (wraps `git diff --name-status ref-a ref-b`) and a conditional in `gittree-refresh-status` that uses it when launch overrides are active.
  3. Added a confirmation `message` inside `gittree-launch` so the user can see it took effect.
- **Group 6 fix**: MELPA install of markdown-mode failed with "tar not found". Added `pkgs.gnutar` + `pkgs.gzip` to `home/emacs.nix` `home.packages` so emacs's subprocess PATH sees them on minimal images.
- **Group 5 (lazygit)** ✅: replaced the single `E` binding with three context-specific bindings, all via `emacs-gittree`:
  - `context: files` → `emacs-gittree <commit-sha> working <file>`
  - `context: commits` → `emacs-gittree <sha>^ <sha>` (diff a commit vs its parent)
  - `context: commitFiles` → `emacs-gittree <sha>^ <sha> <file>`
- **Wave regrouping**: Groups 1 + 3 + 5 + 6 all touch emacs/diff UX and landed together. Landing order revised: emacs wave → Group 4 (mAId) → Group 2 (bootstrap).
- Sanity: `./build-nix/test.sh` green on ubuntu-mane + al2-kelasa.

### 2026-04-27 - Session Focus: Groups 1, 3, 6 implementation
- **Group 1 (gittree CLI)** ✅
  - Added `gittree-launch-left-ref` / `gittree-launch-right-ref` override defvars and new `gittree-launch` entry point in `emacs/core-gittree.el`. The override threads through `gittree-visit-node` so the tree-left / file-right layout is preserved: user-supplied refs replace the status-derived ones when set, and vdiff is forced on. Mode disable clears the overrides.
  - Added shell script `gittree/emacs-gittree.sh` handling 0/1/2 args + optional file; packaged via `writeShellScriptBin "emacs-gittree"` in `home/home.nix`. Removed the old `home.shellAliases.emacs-gittree`.
- **Group 3 (headless linux)** ✅
  - Split `linuxConfiguration` in `flake.nix` into `linuxBaseConfiguration` (nixGL overlay, `nix.package`) and `linuxGraphicalConfiguration` (chrome, rofi, rofi-desktop). Module lists: ubuntu-mane gets both; al2/al2023-kelasa get only base.
  - Verified: `nix-store -qR` on al2-kelasa closure shows 0 hits for chrome/rofi; ubuntu-mane shows both.
- **Group 6 (markdown-mode)** ✅
  - Added `activate-markdown` section to `emacs/core.el` (standard `my-use-package` path). Removed `emacs/markdown-mode.el` (opt-in module redundant).
- Sanity: `nix flake check` and `./build-nix/test.sh` (ubuntu-mane + al2-kelasa) both green.
- `flake.lock` now committed (previously gitignored/untracked); nix was warning `Git tree is dirty` before staging it.
- Next: stop here for user review of emacs + flake changes before proceeding to Group 4 (mAId content) and Group 2 (bootstrap).

### 2026-04-27 - Session Focus: Spec & branch setup
- Interviewed user; finalized six-group scope. Key design inputs from user:
  - Gittree launch must **preserve the left-tree/right-file layout**, not just show two diff buffers.
  - Bootstrap is a **four-layer** model (machine-prep → common sync → nix build → machine-specific post-nix). Ubuntu bootstrap stays in env; Amazon in Gorantls-env.
  - Headless split driven by `envKind` (kelasa = headless).
  - mAId = sibling repo; ships its own `flake.nix` for `nix profile install`. env does NOT import yet.
  - Markdown mode added to core emacs build (previously opt-in module, never loaded).
- Cloned empty `kusimari/mAId` to `~/env-workplace/mAId`; created local `misc-updates` branch there.
- Wrote this feature spec at `.kdevkit/feature/misc-updates.md`. No implementation steps included (per user).
- Next session: start Groups 1 & 6 in parallel (smallest & fully independent).

## Useful References
- `env/.kdevkit/project.md` — repo conventions.
- `env/.kdevkit/feature/refactor-darwin-ubuntu.md` — format model for this spec.
- `env/emacs/core.el:369-370` — gittree load point; new markdown section goes nearby.
- `env/emacs/core.el:342-360` — `activate-general-code` pattern to mirror for markdown.
- `env/emacs/core-gittree.el:256` — `gittree-show-dual-panel` (Group 1 reuse).
- `env/emacs/core-gittree.el:486` — `gittree-mode` (Group 1 layout).
- `env/emacs/markdown-mode.el` — to be removed (Group 6).
- `env/flake.nix:78-102` — current `linuxConfiguration` (Group 3 split target).
- `env/build-nix/_common.sh` — sed placeholder dance (preserved).
- `Gorantls-env/desktop/bootstrap-al2023.sh:187-214` — env-clone logic to move.
- `env/gittree/lazygit-config.yml:43-49` — existing `E` binding.

## Decision Log

### 2026-04-27 - Four-layer bootstrap model
**Context**: Three-layer proposal didn't capture machine-specific post-nix work the user has in mind.
**Decision**: Four layers — (1) machine-prep, (2) common sync with pre/post hooks, (3) nix build, (4) post-nix machine-specific. No Layer-4 scripts ship yet; hook point in Layer 2 enables future additions.
**Rationale**: Lets Gorantls-env (and future private repos) extend the flow without modifying env.
**Alternatives**: Three layers; platform-specific post-hooks hard-coded in env.
**Impact**: env becomes a cleaner public build substrate; Amazon-specific logic stays in Gorantls-env.

### 2026-04-27 - Gittree layout preservation
**Context**: Initial design called `gittree-show-dual-panel` directly, losing the treemacs side panel.
**Decision**: Introduce a thin `gittree-launch` elisp wrapper that ensures `gittree-mode` is active before splitting the right window.
**Rationale**: Gittree's value is the coordinated tree+file view; a CLI that bypasses that degrades the UX.
**Alternatives**: Require users to launch gittree-mode first, then invoke the diff command interactively.
**Impact**: Small new elisp function; no change to existing functions.

### 2026-04-27 - mAId as sibling with own flake, not env flake input
**Context**: mAId content changes much more often than env (prompts/skills edited constantly).
**Decision**: Sibling checkout + mAId's own `flake.nix` for activation via `nix profile install`. env does not import mAId yet.
**Rationale**: Avoids forcing a home-manager switch on every mAId edit. Symlinks point into the checkout, so edits are live.
**Alternatives**: mAId as flake input pinned to a commit; plain shell-script symlinks.
**Impact**: Two independent `nix` invocations per machine (env `home-manager switch` + mAId `nix profile install .`). Acceptable assuming no store conflicts; fallback is `activate.sh`.

### 2026-04-27 - Markdown mode moved to core
**Context**: `emacs/markdown-mode.el` module exists but is opt-in (via `EMACS_MODULES=markdown-mode`) and `activate-markdown` is commented out. In practice `.md` files open in `fundamental-mode`.
**Decision**: Promote to default `core.el` section; delete the module file.
**Rationale**: Markdown is universal; opt-in gate has no benefit.
**Alternatives**: Uncomment the opt-in but leave it module-local; document the env var.
**Impact**: One section added in `core.el`; one file removed; new auto-mode-alist entry.
