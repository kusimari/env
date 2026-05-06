# feature-build-layers — spec, design, implementation, tests

## Status

🚧 **Spec locked; implementation not started.** This doc is the
review artifact: spec, design, implementation order, and test plan
for the full mAId system plus the four-layer bootstrap. Edit in
place. The implementation loop reads this file as its instruction
set.

> **Scope on branch `feature-build-layers`:** iterations 1 + 2 only.
> Iterations 3–6 are captured here so the design is evaluated whole,
> even though this branch doesn't implement them.

## Scope guardrails

**This repo is public; this document stays site-agnostic.** Avoid
naming specific employers, internal tools, private repos, auth
systems, or clone URLs. Sibling repositories that carry machine-
specific or site-specific concerns are referred to abstractly as
*private sibling env repo(s)*. Concrete URLs and credentials live in
the bootstrap scripts themselves, not in design docs. (NFR5.)

---

## 0 · Context

Two bundled pieces of work:

1. **Four-layer bootstrap** — separate machine prep (Layer 1) from
   generic sync (Layer 2, new `bootstrap-common.sh`) from nix build
   (Layer 3, unchanged) from post-nix machine-specific steps
   (Layer 4, convention via hook). Lets env bootstrap identically on
   any machine, with site-specific prep delegated to a *private
   sibling env repo*.

2. **mAId sibling repo** — a tool-agnostic source of truth for
   agentic resources (skills, agents, MCPs, commands) that compiles
   to whatever AI tool is in use (Claude Code, Gemini CLI, Kiro,
   Claude Desktop, AWS Q, claude.ai, future tools). Lives at
   `~/env-workplace/mAId`, cloned by Layer 2.

The two ship together because Layer 2 is the natural place to clone
mAId; splitting them leaves a dangling "now also clone this other
repo" step.

---

## 1 · Spec

### 1.1 What mAId is

mAId holds generic, tool-agnostic authorship of agentic resources:
**skills** (prose guidance), **agents** (invocable specialists),
**commands** (slash commands), **MCPs** (server definitions + deps).
The `maid` CLI turns that source tree into per-tool views under
`$HOME` via symlinks, either directly (`mode: link`) or after a
transform (`mode: build`).

### 1.2 Core properties

| Property | Mechanism |
|---|---|
| Generic source, per-tool output | Central `registry.ts` maps `(tool, kind) → (path, transform)`; sources declare only `kind` |
| Composition, not reinvention | Three source types — `original` (curated), `external` (verbatim pointer), `patch` (named delta) — pick the right one per skill |
| Intent-gated activation | Not all skills load every session. A top-level instruction in `CLAUDE.md`/`GEMINI.md`/`KIRO.md` tells the AI to classify session intent and surface only matching skills; `maid intent <msg>` is the reference classifier |
| Self-mutating | Skills that notice patterns edit themselves (`mode: link` = live). Structural changes go to `inbox/issues/` for deliberate follow-up |
| Home stays empty | `$HOME/CLAUDE.md`, `$HOME/.claude/skills`, etc. are all symlinks into the mAId checkout. Single versioned source |
| Nix is OS-wiring only | mAId is a Deno/TypeScript project. `flake.nix` is a minimal wrapper that puts `maid` on `$PATH` |

### 1.3 Functional requirements (bootstrap)

- **FR-B1** Every Layer-3 build (`build-nix/<platform>.sh`) must be
  preceded by a Layer-1 script. Layer-1 may be empty but must exist
  so the flow is uniform.
- **FR-B2** `env/build-nix/bootstrap-common.sh` (Layer 2) is
  curl-able AND runnable from a clone; the same script auto-detects
  its mode.
- **FR-B3** `bootstrap-common.sh` scope: create `~/env-workplace`,
  check GitHub SSH reachability, clone/fetch `env`, clone/fetch
  `mAId`. Accept `--pre-nix <script>` / `--post-nix <script>` hooks
  so Layer 1 and Layer 4 can inject without modifying env.
- **FR-B4** Platform-specific prep that is machine- or site-specific
  (e.g. credential enrollment, private auth flows, internal tool
  installation, sudoers rules for Nix directory creation) lives in
  a *private sibling env repo*, outside this public repo. Those
  Layer-1 scripts delegate to `bootstrap-common.sh` at the end. The
  public repo does not need to know they exist.
- **FR-B5** `env/build-nix/bootstrap-ubuntu.sh` is a new Layer-1
  script for Ubuntu. Plain Ubuntu setup is generic (no site-specific
  auth), so its Layer 1 lives in env.
- **FR-B6** Layer-3 scripts (`build-nix/<platform>.sh`) stay pure:
  nix invocation only; no prep.
- **FR-B7** Layer 4 is a convention, not scripts-on-this-branch. The
  `--post-nix` hook path in Layer 2 is the extension point.
- **FR-B8** All bootstrap scripts idempotent.

### 1.4 Functional requirements (mAId)

- **FR-M1** `maid deploy` creates symlinks from tool-native paths in
  `$HOME` to the mAId checkout. Idempotent; re-running is a no-op.
- **FR-M2** `maid deploy` NEVER overwrites existing non-symlink
  files in `$HOME`. On collision: print a warning, skip the file,
  continue. User resolves manually.
- **FR-M3** `maid deploy` NEVER touches files outside the registered
  set (`~/.claude/settings.json`, `~/.claude/projects/`,
  `~/.claude/sessions/`, `~/.claude/plans/` are all off-limits).
- **FR-M4** `maid build` resolves `source: external` and `source:
  patch` files into `build/<tool>/<name>.md`. `build/` is
  gitignored. `maid deploy` symlinks into `build/` for those files.
- **FR-M5** `maid intent <first-user-message>` returns JSON with
  `{ classified_intent, selected_skills, ambiguous_with }`. Used
  both by AI tools (via the top-level CLAUDE.md instruction) and by
  humans for debugging the router.
- **FR-M6** `maid inbox add <title> [--body <text>]` appends a
  markdown file under `inbox/issues/<YYYY-MM-DD>-<slug>.md`. AI
  tools follow the same protocol when asked to file a change
  request.
- **FR-M7** Every source file has YAML frontmatter with at least
  `kind:`, `source:`, and `intent:`. `mode:` defaults from `source:`
  (original → link, external/patch → build). Missing mandatory
  fields = hard error on build/deploy.
- **FR-M8** Frontmatter is validated against a schema (one place:
  `maid/schema.ts`). Schema violations print which field, which
  file, which line.
- **FR-M9** `maid` exit code is non-zero on any failure. Output is
  human-readable by default; `--json` flag switches to JSON for
  tooling.
- **FR-M10** mAId's own flake exports a `packages.<system>.default`
  that wraps `deno run --allow-read --allow-write --allow-run`
  pointing at `maid/main.ts` with the mAId checkout embedded as a
  runtime path. No TS compilation at build time — source is live.

### 1.5 Non-functional requirements

- **NFR1** `./build-nix/test.sh` stays green.
- **NFR2** Bootstrap surfaces clear error messages for manual-
  action cases (e.g., "add this key to GitHub").
- **NFR3** No new flake inputs in env (mAId via flake input is a
  later branch).
- **NFR4** Bootstrap scripts shellcheck-clean.
- **NFR5** No site-specific or employer-specific identifiers in
  scripts or docs that live in this public repo. If such coupling
  is needed, it belongs in a private sibling env repo.
- **NFR-M1** `maid deploy` completes under 2 seconds on warm cache
  (read sources, create symlinks).
- **NFR-M2** `maid` has zero runtime deps outside Deno stdlib
  (no npm, no node_modules).
- **NFR-M3** All schema violations fail fast with a pointer to the
  offending line (`path/to/file.md:12: missing 'kind:'`).
- **NFR-M4** Deterministic output order — deploying on two machines
  from the same commit produces byte-identical link targets.
- **NFR-M5** Tests (`deno test`) run in under 10 seconds total on
  an empty deno cache.

### 1.6 Out of scope for this branch

- Multi-tool transforms (gemini-cli, kiro, claude-desktop) — iter 3+
- Agents implementation — iter 4
- MCP dependency system — iter 5
- Meeting-prep scheduler — iter 6
- `maid reload` helper — not needed until build-mode exists
- `gittree` CLI / lazygit rework (separate branch)
- Anything specific to a particular employer's infrastructure —
  belongs in private sibling repos

### 1.7 Success criteria

- Fresh Linux VM → `curl ... bootstrap-<platform>.sh | bash` →
  bootstrap completes → `./build-nix/<platform>.sh` succeeds.
- `cd ~/env-workplace/mAId && nix profile install .` creates
  working symlinks pointing into the checkout
  (`ls -l ~/CLAUDE.md` resolves into the mAId clone).
- Re-running the bootstrap on the same machine is a no-op.

---

## 2 · Design

### 2.1 Four-layer diagram

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

Darwin Layer-1 is intentionally skipped on this branch.
`env/build-nix/darwin.sh:3-7` already carries inline setup notes
(brew/toolbox/Determinate installer); no new TODO required.

### 2.2 `bootstrap-common.sh` behavior

- Detect curl-vs-clone via `readlink -f "${BASH_SOURCE[0]}"`. If
  inside `~/env-workplace/env/`, assume clone mode. Else curl mode:
  clone env first, re-exec from the clone with a `--post-clone`
  sentinel to prevent re-exec loops.
- Clone/fetch both env and mAId from their GitHub URLs (URLs hard-
  coded in the script, not in this doc).
- Run optional `--pre-nix` and `--post-nix` scripts if provided.

### 2.3 mAId layout

```
mAId/
├── flake.nix                # minimal — wraps `deno run maid/main.ts`
├── deno.json                # imports map, task runner
├── .gitignore               # build/, .deno_cache/, etc.
├── README.md                # quickstart; points at .kdevkit/
│
├── CLAUDE.md                # TOP-LEVEL instruction file — see §2.9
├── GEMINI.md                # same instructions, Gemini flavor
├── KIRO.md                  # same instructions, Kiro flavor
│
├── maid/                    # the CLI
│   ├── main.ts              # argv parser + dispatcher
│   ├── schema.ts            # frontmatter schema + validator
│   ├── registry.ts          # (tool × kind) → (path, transform, mode)
│   ├── sources.ts           # read + parse sources/ tree
│   ├── deploy.ts            # symlink manager
│   ├── build.ts             # transform runner (stub in iter 2)
│   ├── intent.ts            # session-intent classifier
│   └── inbox.ts             # issue + session-log writer
│
├── sources/                 # authored truth
│   ├── skills/
│   │   ├── git.md
│   │   ├── development.md
│   │   └── writing-style.md
│   ├── agents/              # empty; iter 4
│   ├── commands/            # empty
│   └── mcp/                 # empty; iter 5
│
├── build/                   # gitignored; written by `maid build`
│
├── inbox/                   # committed self-mutation audit trail
│   ├── README.md
│   ├── issues/              # AI-filed or user-filed change requests
│   └── session-log/         # write-back observations (e.g. writing-style)
│
├── tests/                   # deno test suite (see §5)
│   ├── schema_test.ts
│   ├── deploy_test.ts
│   ├── intent_test.ts
│   └── fixtures/            # sample source trees, expected outputs
│
└── .kdevkit/                # feature docs on the mAId side
    └── feature/
        └── mAId-bootstrap.md   # pointer to this doc
```

### 2.4 Frontmatter schema

Single source of truth for what's valid:

```yaml
---
# Required
name:    <string>          # defaults to basename(file)
kind:    skill | agent | command | mcp
source:  original | external | patch
intent:  [tag, tag, …]     # at least one tag

# Recommended
activates-with: [skill-name, …]   # sibling skills loaded together
mode:    link | build             # defaults: original=link, else=build
self-updates: true | false        # default false
targets: [tool-name, …]           # default: all registered tools

# Source-type-specific (exactly one group required)

# Type A: original
inspired-by:
  - name: <label>
    url:  <https://…>

# Type B: external
fetch: <https://…>
cache: sources/<kind>/_external/<name>.md

# Type C: patch
base:
  fetch: <https://…>
---
```

Validation lives in `maid/schema.ts`. Invalid files fail fast with a
file+line error message.

### 2.5 Source types (provenance, not mode)

| Type | `source:` | Authoring | Runtime | When to use |
|---|---|---|---|---|
| **A** | `original` | Full markdown in mAId | `link` | Your own method, even if inspired by others |
| **B** | `external` | Just frontmatter; content fetched | `build` (fetch + cache) | Public skill you use verbatim; upstream stable |
| **C** | `patch` | Frontmatter + delta in body | `build` (fetch base, apply delta) | Public thing is mostly right, need named override |

On this branch, all skills are Type A. Type B/C land in iter 3+ when
`build` transforms work.

### 2.6 Link mode vs. build mode

**`mode: link`**
- `maid deploy` creates `~/<tool-path>/<name>.md` as a symlink
  pointing at `sources/<kind>/<name>.md`.
- Edits are live across every tool that reads the same shape.
- Required for `source: original` on this branch (we have no
  transforms yet, so no build output).

**`mode: build`**
- `maid build` writes `build/<tool>/<kind>/<name>.md` by running
  the registered transform.
- `maid deploy` then symlinks `~/<tool-path>/<name>.md` to the
  build output.
- Stubbed in iter 2 (`maid build` prints "no build-mode sources
  yet" and exits 0 if no `source: external|patch` files exist).

**Rule for AI sessions:**
AI can freely edit `link`-mode files and anything under `inbox/`.
Changes that affect build-mode sources, the registry, or `maid/`
source code must be filed as inbox issues — they require a
deliberate rebuild.

### 2.7 Registry (the one place tool-shape knowledge lives)

Conceptual shape (actual code not included here):

```
registry[tool][kind] = {
  path:       string_template,     # e.g. "{HOME}/.claude/skills/{name}.md"
  transform:  function | null,     # null = link mode; non-null = build
  merge:      boolean,             # true if multiple sources merge
  supported:  boolean,             # false = tool doesn't support this kind
}
```

Iter 2 registry content:

| tool | kind | path | transform | notes |
|---|---|---|---|---|
| claude-code | skill | `~/.claude/skills/{name}.md` | null | link |
| claude-code | command | `~/.claude/commands/{name}.md` | null | link; no commands yet |
| claude-code | agent | `~/.claude/agents/{name}.md` | (stub) | iter 4 |
| claude-code | mcp | `~/.claude.json` | (stub) | iter 5; merge=true |
| gemini-cli | * | * | — | **commented stub**, iter 3 |
| kiro | * | * | — | commented stub |

Top-level symlinks (outside the per-kind grid, because they're the
root pointer files):
- `~/CLAUDE.md` → `mAId/CLAUDE.md`
- `~/GEMINI.md` → `mAId/GEMINI.md`
- `~/KIRO.md`   → `mAId/KIRO.md`

### 2.8 Intent routing

**Decision locked: the router lives in top-level `CLAUDE.md`, not
as a skill.** Rationale: always-on instructions that *decide what
skills load* shouldn't themselves be intent-gated.

Flow at session start:

1. AI reads `~/CLAUDE.md` (symlinked to `mAId/CLAUDE.md`) —
   contains the routing instructions.
2. On first user message, AI either:
   - calls `maid intent "<first-user-message>"` and uses its
     output, OR
   - does the classification itself following the rules in
     `CLAUDE.md`.
3. Plays back to the user: *"Session intent: coding. Using skills:
   development, git."*
4. If ambiguous: asks one disambiguating question; otherwise
   proceeds.

`maid intent` is a pure function over the sources tree — matches
intent tags from the user's message against each skill's `intent:`
frontmatter, applies `activates-with:` transitive closure, returns
JSON.

Implementation of the classifier itself is rules-based in iter 2
(keyword matching against intent tags), not LLM-powered. Keeps it
deterministic and testable. An LLM-powered upgrade can come later.

### 2.9 Top-level CLAUDE.md / GEMINI.md / KIRO.md

These are short instruction sheets, identical in intent across
tools. Shape:

```
# Always-on routing

On a new session:
1. Before acting on the user's first message, determine session
   intent. Intent tags you'll encounter: coding, refactor, bug-fix,
   review, narrative, notes, meeting-prep, tool-config.
2. Surface matching skills from ~/.claude/skills (or equivalent
   per tool). Use `maid intent <msg>` if available, else classify
   by matching keywords to each skill's intent frontmatter.
3. Play back what was selected: "Session intent: X. Using skills:
   A, B."
4. If intent is ambiguous, ask exactly one disambiguation question.
5. Only then proceed with the task.

# Self-update protocol

- Markdown skill edits are allowed mid-session for `mode: link`
  files. Commit the change before session ends.
- Structural changes (new MCP, new skill, registry edit, maid/
  code) go to inbox/issues/ via `maid inbox add`. Do not modify
  build-mode sources or maid/ source files mid-session.

# Skills, agents, commands, MCPs

See ~/.claude/skills, ~/.claude/agents, ~/.claude/commands.
```

Same content, different filename, for GEMINI.md and KIRO.md.

### 2.10 Self-update loop — what each case looks like

| Scenario | Mechanism | File written |
|---|---|---|
| writing-style spots new pattern | Direct edit (link mode) | `sources/skills/writing-style.md` |
| User asks to add a git rule | Direct edit (link mode) | `sources/skills/git.md` |
| User asks for a whole new skill | New file (link mode) | `sources/skills/<name>.md` |
| User asks to change an MCP | Inbox issue | `inbox/issues/<date>-<slug>.md` |
| User asks to wire a new tool | Inbox issue | `inbox/issues/<date>-<slug>.md` |
| `maid` bug | Inbox issue | `inbox/issues/<date>-<slug>.md` |

`session-log/` is for skills to dump observations during a session
without cluttering the skill file. Example: writing-style appends
raw observations during a narrative session; at `wrap up`, the AI
promotes the useful ones into `writing-style.md` and removes the
log.

### 2.11 Inbox protocol

Issue file shape:

```markdown
---
id: 2026-05-01-intent-keyword-list-is-too-rigid
filed-by: claude-code
filed-during: <session-id or short context>
kind: enhancement | bug | question
affects: maid/intent.ts, sources/skills/
---
# Short title

## What I noticed
...

## Suggested change
...

## Repro / example
...
```

`maid inbox add <title>` scaffolds this file. `maid inbox list`
lists open issues. `maid inbox resolve <id>` moves the file to
`inbox/issues/_resolved/`.

### 2.12 Flake.nix shape

Minimal. Pseudocode of what it exposes:

```
outputs = { self, nixpkgs }: {
  packages.<system>.default = pkgs.writeShellApplication {
    name = "maid";
    runtimeInputs = [ pkgs.deno ];
    text = ''
      exec deno run --allow-read --allow-write --allow-run --allow-env \
        --allow-net=raw.githubusercontent.com \
        ${self}/maid/main.ts "$@"
    '';
  };
};
```

Two install paths the user can pick later:
- `nix profile install path:./mAId` in the checkout.
- Add mAId as a flake input to env and install via home-manager.

On this branch we don't wire it into env's flake yet; user runs
`nix profile install .` once in mAId after bootstrap.

### 2.13 Deno choice — scope of permissions

`maid` runs with explicit `--allow-read --allow-write --allow-run
--allow-env`. Net access (`--allow-net`) is only needed for iter 3
(fetching Type B/C externals); on this branch it's omitted until
the fetch step lands.

---

## 3 · Skills content (drafts for review)

These are the skills to seed in iter 2. All Type A. Content below
is a starting point — edit in place; what's here ships.

### 3.1 `sources/skills/git.md`

```yaml
---
name: git
kind: skill
source: original
intent: [coding, refactor, bug-fix, review, release]
activates-with: []
inspired-by:
  - name: Conventional Commits
    url: https://www.conventionalcommits.org/
---
```

```markdown
# git — how I like to use git

## Branches
- Name `type/description` or `feature-<name>`. Types: `feat/`,
  `fix/`, `chore/`, `docs/`, `refactor/`, `test/`.
- One feature per branch. No omnibus branches.
- Don't rename or delete branches that have been pushed without
  asking.

## Commits
- Conventional Commits: `type(scope): subject`, imperative mood,
  subject under ~70 chars.
- Body: 1–2 sentences on the *why*, not the *what*. The diff shows
  what.
- Co-Authored-By trailer when AI-assisted.
- New commits, not amends. Amend only when the user explicitly
  asks. If a pre-commit hook fails, fix and make a NEW commit —
  amending destroys previous work.

## Staging
- Never `git add -A` / `git add .`. Add specific files by name so
  you don't sweep up `.env`, credentials, or large binaries.
- Warn if files likely to contain secrets are being staged.

## Destructive ops — never without explicit approval
- `push --force`, `push --force-with-lease` to a shared branch
- `reset --hard`, `checkout .`, `restore .`, `clean -f`
- `branch -D` on a branch that has been pushed
- `rebase -i`, `add -i` (interactive ops can't be driven from tools)
- `--no-verify` or any hook skip
- `--no-gpg-sign` or any signing bypass

Flag the action, state the consequences, ask. Don't use
destructiveness as a shortcut when stuck — diagnose root cause.

## PRs
- Title under 70 chars; details go in the body.
- Body is "Summary" (1–3 bullets) + "Test plan" (checklist).
- Never push to main/master directly; open a PR.
- Don't open PRs automatically unless asked.

## Workflow hygiene
- Check `git status`, `git diff`, recent `git log` before committing
  — follow the existing commit style.
- If there's in-flight uncommitted work you didn't expect, stop and
  ask — it may be the user's WIP.
```

### 3.2 `sources/skills/development.md`

```yaml
---
name: development
kind: skill
source: original
intent: [coding, refactor, bug-fix, review]
activates-with: [git]
inspired-by:
  - name: kdevkit/feature-dev.md
    url: https://raw.githubusercontent.com/kusimari/kdevkit/main/feature-dev.md
  - name: kdevkit/agent-dev-loop.md
    url: https://raw.githubusercontent.com/kusimari/kdevkit/main/agent-dev-loop.md
---
```

```markdown
# development — how I want code changes made

## Shape of the change
- Bug fixes stay bug fixes. No drive-by cleanup, no surrounding
  refactor. Match scope to the request.
- Don't design for hypothetical future requirements. Three similar
  lines beats a premature abstraction.
- No half-finished implementations. If it can't be done fully,
  stop and ask.
- No backwards-compat shims or feature flags for internal code. If
  you can just change the code, do.

## What to leave out
- Error handling for scenarios that can't happen. Trust internal
  code and framework guarantees. Only validate at system
  boundaries (user input, external APIs).
- Comments that explain WHAT — well-named identifiers already do
  that. Only write a comment when the WHY is non-obvious: hidden
  constraint, subtle invariant, workaround, surprising behavior.
- References to the current task in code comments ("used by X",
  "added for Y flow", "fix for #123"). That's PR-description
  stuff; it rots as code evolves.
- Defensive renaming of unused variables to `_var`. If it's unused,
  delete it.

## Verification
- For UI / frontend changes: run the dev server, use the feature
  in a browser. Test the golden path AND edge cases. Monitor for
  regressions. Type-check + tests verify *code correctness*, not
  *feature correctness*. If you can't test the UI, say so — don't
  claim success.
- For library / backend code: unit tests cover the change; the
  full suite still passes.
- Before declaring done: read back the actual diff, not what you
  intended to do.

## Reversibility
- Local, reversible actions (editing files, running tests):
  proceed.
- Hard-to-reverse or shared-state actions (pushing, deleting
  files, dropping tables, killing processes, modifying CI, sending
  messages): state what you're about to do and ask.
- If you hit an obstacle, don't use a destructive shortcut (e.g.
  `--no-verify`, `reset --hard`) to make it go away. Investigate
  root cause.

## Planning
- Non-trivial tasks: plan first, align with the user, then execute.
- Trivial tasks (typos, single-line renames, one-file edits):
  just do them.
- Don't write planning docs unless the user asks for one.

## Inspired by
- `kdevkit/feature-dev.md` — structured feature setup (project
  context, feature files, git conventions, phase gating).
- `kdevkit/agent-dev-loop.md` — detect toolchain, enforce
  format/lint/typecheck → review → test → push gate.

Those two docs are the ground floor for "how a session runs." This
skill is the overlay of taste and guardrails I want applied on
top.
```

### 3.3 `sources/skills/writing-style.md`

Ported from a personal draft elsewhere; scrub removes any
site/team/product-specific examples. Keep: stylistic patterns
(voice, sentence structure, punctuation, emphasis).

```yaml
---
name: writing-style
kind: skill
source: original
intent: [narrative, writing, email, announcement]
activates-with: []
self-updates: true
---
```

Body: the full scrubbed style guide (voice & tone, sentence
structure, punctuation, vocabulary, paragraph structure, POV,
emphasis, other patterns), with the How-To-Use section at the top
and Session Log at the bottom. Specific content draft happens
during iter 2 alongside the code; scrub pass is a diff of the
existing personal draft. Final text replaces the draft here before
deploy.

### 3.4 Top-level `CLAUDE.md` (routing instruction)

Content as specified in §2.9. Same file content mirrors into
`GEMINI.md` and `KIRO.md` with only the tool-name substitutions.

---

## 4 · Implementation plan

### 4.1 Branches

The `env` and `mAId` repos each get a `feature-build-layers`
branch on this round. A private sibling env repo gets its own
branch separately, outside this public doc, per NFR5.

| Repo | Remote | Base | New branch |
|---|---|---|---|
| env | github.com:kusimari/env.git | main | feature-build-layers |
| mAId | github.com:kusimari/mAId.git | (empty) | feature-build-layers (seeded after an initial main commit) |

Branch creation is the first implementation step — confirm with
user before pushing to remote.

### 4.2 Iteration 1 — four-layer bootstrap (this branch)

**Deliverables (env repo)**

- `env/build-nix/bootstrap-common.sh` (Layer 2). Curl-aware:
  `readlink -f "${BASH_SOURCE[0]}"` detects clone-vs-curl mode. In
  curl mode, clones env, re-execs from the clone with a
  `--post-clone` sentinel. Clones mAId too. Accepts
  `--pre-nix <script>` / `--post-nix <script>` hooks. Idempotent.
  Shellcheck-clean.
- `env/build-nix/bootstrap-ubuntu.sh` (Layer 1 for Ubuntu).
  Minimal prep (generic, no site-specific auth). Delegates to
  Layer 2 at the end.
- `env/setup-notes.md` update: document the four-layer flow, the
  `maid deploy` step, and how to add a Layer 1 script in a private
  sibling repo.
- `env/.kdevkit/feature/feature-build-layers.md` (this file):
  session log entries as work lands.

**Layer 3 and Layer 4: no changes.**

Private-sibling Layer-1 scripts (e.g. for site-specific auth,
internal tooling, sudoers for Nix) live outside this repo and are
updated in that repo's own branch. They delegate to
`bootstrap-common.sh` once env has been fetched.

**Acceptance (iter 1)**
- `shellcheck -x env/build-nix/*.sh` exits 0.
- `env/build-nix/test.sh` still builds both Linux configs.
- `bootstrap-common.sh --dry-run` twice against a staging `HOME`
  prints "nothing to do" on run 2.
- Manual VM: curl a Layer-1 bootstrap → both repos cloned →
  `env/build-nix/<platform>.sh` completes.

### 4.3 Iteration 2 — mAId core (this branch)

**Order (strict; each step's tests land with the step):**

1. **Scaffolding.**
   - `flake.nix`, `deno.json`, `.gitignore`, `README.md`.
   - `.kdevkit/feature/mAId-bootstrap.md` (pointer to this doc).
   - Empty `sources/{skills,agents,commands,mcp}/`,
     `inbox/issues/`, `inbox/session-log/`, `build/` in gitignore.
   - `inbox/README.md` documenting the protocol.

2. **Schema + sources reader.**
   - `maid/schema.ts`: frontmatter validator.
   - `maid/sources.ts`: walk `sources/` tree, parse, validate.
   - Tests: `tests/schema_test.ts`, `tests/sources_test.ts` —
     cover valid frontmatter, missing required fields, wrong
     values for enums, per-source-type field requirements.

3. **Registry.**
   - `maid/registry.ts`: the table from §2.7. Only claude-code
     populated; gemini-cli/kiro as commented stubs.
   - Tests: `tests/registry_test.ts` — every (tool, kind) lookup
     returns a path template or `unsupported`; path templates
     render correctly with `{HOME}` and `{name}` substitution.

4. **Deploy (link mode only).**
   - `maid/deploy.ts`: for each source, compute destination path
     via registry, create a symlink from destination → source.
     Skip (with warning) if destination exists and is a non-
     symlink or points elsewhere unexpectedly.
   - Top-level `CLAUDE.md`/`GEMINI.md`/`KIRO.md` symlinks handled
     explicitly outside the kind grid.
   - Tests: `tests/deploy_test.ts` — fixture source tree + fake
     `HOME`, run deploy, assert symlinks exist and point where
     expected; run twice, assert no changes on run 2; pre-existing
     file at destination → warning, no overwrite.

5. **Build (stub).**
   - `maid/build.ts`: if any source has `source: external|patch`,
     print "build-mode sources not yet supported — iter 3" and
     exit 1. If no such sources, exit 0 with "nothing to build".
   - Tests: `tests/build_test.ts` — asserts the two branches.

6. **Intent router.**
   - `maid/intent.ts`: keyword-matching classifier. Given a
     message string, extract candidate intent tags (small rule
     set, documented in the file), match against each skill's
     `intent:` frontmatter, compute `activates-with:` transitive
     closure, return `{ intent, skills, ambiguous_with }`.
   - Tests: `tests/intent_test.ts` — fixture skills with known
     intents; assert coding prompts return coding skills,
     narrative prompts return writing-style, ambiguous prompts
     return `ambiguous_with` with multiple candidates.

7. **Inbox.**
   - `maid/inbox.ts`: `add`, `list`, `resolve`.
   - Tests: `tests/inbox_test.ts` — add creates a file with right
     shape; list shows open issues; resolve moves to `_resolved/`.

8. **main.ts dispatcher.**
   - Parse argv, dispatch to the above. `--json` flag switches
     output. `--help` prints usage.
   - Tests: `tests/main_test.ts` — subprocess invocations assert
     exit codes and JSON output shape.

9. **Skills content.**
   - Write `sources/skills/git.md` and
     `sources/skills/development.md` per §3.1–3.2. Content is
     what's drafted here; edits happen in this doc first, then
     copied over.
   - Port `sources/skills/writing-style.md` from the personal
     draft, scrubbed. Scrub checklist in the file header.
   - Write `CLAUDE.md`, `GEMINI.md`, `KIRO.md` per §2.9.

10. **End-to-end smoke test.**
    - `tests/smoke_test.ts`: spawn a fake HOME, run
      `maid deploy` against the real sources/ tree, assert every
      expected symlink resolves correctly.
    - Manual: `nix profile install .` in a real checkout, then
      check `ls -l ~/CLAUDE.md` resolves into the mAId tree;
      `ls -l ~/.claude/skills/git.md` resolves correctly; an AI
      session with "help me write a blog post" loads only
      writing-style, not git/development.

**Acceptance (iter 2)**
- `deno task test` exits 0; all tests pass.
- `deno task check` (typecheck) exits 0.
- `maid deploy --dry-run` on a fresh HOME prints the planned
  symlinks; `maid deploy` creates them; second run is a no-op.
- `maid intent "help me fix the bug in foo.py"` returns
  `{ intent: "bug-fix", skills: ["development", "git"] }`.
- `maid intent "help me write a blog post"` returns
  `{ intent: "narrative", skills: ["writing-style"] }`.
- `maid intent "something"` returns `ambiguous_with` populated.

### 4.4 Test-driven loop

Each step in §4.3 follows: **write failing test → implement →
run test → commit**.

Loop driver (to be used in the follow-up implementation session):

```
while not all steps in §4.3 complete:
  1. Pick the next step.
  2. Write the test fixtures + test cases in tests/<name>_test.ts.
  3. Run `deno task test` — confirm failure is on this step.
  4. Implement maid/<name>.ts.
  5. Run `deno task test` — confirm pass.
  6. Run `deno task check` — typecheck must pass.
  7. Commit: `feat(maid): <step summary>`.
  8. Update inbox/session-log/ if anything surprising came up.
```

### 4.5 Iterations 3–6 (captured; not this branch)

- **Iter 3 — build-mode + gemini-cli target.** Implement transform
  runner; fetch+cache for Type B; base-fetch + delta apply for
  Type C; gemini-cli rows in registry; prove a skill flows
  through.
- **Iter 4 — agents + inbox write-back integration.** Agent
  source format (markdown + `invocation:`, `tools:`). First
  agent: notes-capture. AI-side protocol for appending to inbox
  during sessions.
- **Iter 5 — MCPs.** MCP source format; dep validation; one
  public MCP seeded; private MCP as reference-only.
- **Iter 6 — meeting-prep scheduler.** Outlook/equivalent MCP →
  cross-ref notes → produce brief. Wake-up trigger design.

Each iteration lands on its own branch off `main` (post iter 2
merge), and gets its own spec/design/test section added to this
doc.

---

## 5 · Test plan

### 5.1 Layers

| Layer | Scope | Runner |
|---|---|---|
| Unit | Pure functions in `maid/*.ts` | `deno test` |
| Integration | Subprocess invocations of `maid` against fixture trees | `deno test` |
| Smoke | Real sources/ tree + fake HOME | `deno test` |
| Manual | Real `nix profile install`, real AI session | Checklist |

### 5.2 Test fixtures (`tests/fixtures/`)

- `valid_sources/` — one skill per type (in iter 2, only Type A);
  every kind (skill/agent/command/mcp stubs); known frontmatter.
- `invalid_sources/` — missing required fields; wrong enum values;
  conflicting source-type fields; `kind: skill` with no `name`.
- `fake_home/` — directory skeleton to be used as `$HOME` override
  during deploy tests.
- `expected_symlinks.json` — golden map of destination → source
  for the valid fixture. Deploy test compares actual to golden.

### 5.3 Specific cases

**Schema**
- Valid Type A parses.
- Missing `kind:` → error with file path and line.
- `source: external` without `fetch:` → error.
- `source: patch` without `base.fetch:` → error.
- `kind: cat` (invalid enum) → error naming valid values.

**Sources**
- Walks nested dirs; ignores `_external/` in Type B (iter 3).
- Returns deterministic ordering (alphabetical by name).
- Duplicate `name:` across files → error.

**Registry**
- `(claude-code, skill)` returns a `link` entry with the right
  path template.
- `(gemini-cli, skill)` returns `unsupported` in iter 2.
- Path template `{HOME}/.claude/skills/{name}.md` renders with
  substitutions.

**Deploy**
- Creates symlinks for every registered (source, tool, kind).
- Symlinks are relative where possible (or document absolute —
  decide during implementation; tests encode the choice).
- Second run = no-op (no output lines that would suggest a
  change).
- Destination exists as regular file → warning, skip, non-zero
  exit only with `--strict`.
- Destination exists as symlink to a different source → warning,
  skip.
- Dry-run prints planned operations without performing them.

**Build (stub)**
- No Type B/C sources in tree → exit 0 with "nothing to build".
- Any Type B/C source → exit 1 with "not yet supported".

**Intent**
- Message `"fix the bug in foo.py"` → `intent: bug-fix`,
  `skills: [development, git]` (git via `activates-with:`).
- Message `"help me write a blog post"` → `intent: narrative`,
  `skills: [writing-style]`.
- Message `"hello"` → `ambiguous_with: [development,
  writing-style, ...]`, `skills: []`.
- `activates-with:` transitive closure handled correctly.

**Inbox**
- `add "title"` creates `inbox/issues/<YYYY-MM-DD>-<slug>.md` with
  the frontmatter shape from §2.11.
- `list` returns only open issues.
- `resolve <id>` moves the file to `_resolved/`.

**Main**
- Unknown subcommand → exit 2 with usage.
- `--help` → exit 0 with usage.
- `--json` flag produces valid JSON on stdout.

**Smoke**
- Fake HOME + real sources/ tree: every expected symlink exists
  after `maid deploy`.
- `ls -l` on `~/CLAUDE.md` resolves into the repo.

**Manual**
- Fresh VM: curl → bootstrap → build → deploy → session.
- Claude session with narrative prompt loads only writing-style.
- Claude session with coding prompt loads development + git.
- Ambiguous prompt triggers disambiguation question.

### 5.4 Coverage target

Not enforced numerically. Every exported function in `maid/*.ts`
has at least one test case; every FR from §1.3 / §1.4 has a test
case that maps to it.

---

## 6 · Resolved design decisions

1. Runtime = Deno (TypeScript). Nix only wires `maid` into `$PATH`.
2. Flake = minimal wrapper around `deno run`. Both
   home-manager-input and `nix profile install` paths remain open.
3. `mode: link` vs `mode: build` per-file; default from `source:`.
4. Three source types (original / external / patch); all skills
   on this branch are Type A.
5. Targeting = central `registry.ts`; per-file `targets:` override
   as escape hatch.
6. Intent routing = top-level `CLAUDE.md` instruction + `maid
   intent` helper; NOT a skill (avoids the chicken-and-egg with
   intent-gating).
7. `inbox/` committed; `build/` gitignored.
8. Darwin Layer-1 skipped; existing notes in `darwin.sh:3-7`
   suffice.
9. Writing-style moves to mAId/sources/skills/writing-style.md,
   scrubbed of site-specific content.
10. Classifier in `maid intent` is rules-based in iter 2;
    LLM-based upgrade parked for later.
11. Branch scope = iterations 1 + 2 only.
12. Private-sibling uncommitted work stays outside this branch.

---

## 7 · Files to create/modify on this branch

### env (branch: feature-build-layers)
- CREATE `env/build-nix/bootstrap-common.sh`
- CREATE `env/build-nix/bootstrap-ubuntu.sh`
- UPDATE `env/setup-notes.md`
- UPDATE `env/.kdevkit/feature/feature-build-layers.md` (this
  file)

### mAId (branch: feature-build-layers, seeded from empty main)
- CREATE `flake.nix`, `deno.json`, `.gitignore`, `README.md`
- CREATE `CLAUDE.md`, `GEMINI.md`, `KIRO.md`
- CREATE
  `maid/{main,schema,sources,registry,deploy,build,intent,inbox}.ts`
- CREATE `sources/skills/{git,development,writing-style}.md`
- CREATE `sources/{agents,commands,mcp}/.gitkeep`
- CREATE `inbox/{README.md,issues/.gitkeep,session-log/.gitkeep}`
- CREATE
  `tests/{schema,sources,registry,deploy,build,intent,inbox,main,smoke}_test.ts`
- CREATE `tests/fixtures/{valid_sources,invalid_sources,fake_home}/…`
- CREATE `.kdevkit/feature/mAId-bootstrap.md`

### Private sibling env repo (separate branch, not tracked here)
- UPDATE site-specific Layer-1 bootstrap scripts to delegate to
  `env/build-nix/bootstrap-common.sh`.
- Uncommitted work unrelated to this feature: leave alone.

### Critical existing files to reuse
- `env/build-nix/_common.sh:10-11,35-56,65-77` — pre/post-nix hook
  plumbing Layer 3 already supports.
- `env/build-nix/test.sh` — nix eval + build verification.
- `env/build-nix/darwin.sh:3-7` — already has bootstrap notes for
  macOS; no new TODO needed.

---

## 8 · Open items (for user to decide before implementation loop)

None blocking. Items to mark before kicking off:

- (a) **Intent keyword list** — `maid intent` ships with a small
  rule set. Seed the initial list in this doc for review, or
  draft during implementation and iterate?
- (b) **Symlink relative vs absolute** — §5.3 notes this is decided
  at implementation. Relative symlinks are more portable across
  checkouts; absolute are more debuggable. Preference?
- (c) **`inbox/session-log/` lifecycle** — current design says
  skills append during a session and the AI promotes useful bits
  at wrap-up. Provide `maid log append <skill> <observation>`, or
  is direct file append fine?
- (d) **Draft writing-style.md inline** — §3.3 doesn't include the
  full scrubbed content. Scrub and paste here now, or have the
  scrub happen during implementation and land in a commit?

Mark decisions inline or in a reply — the implementation loop
reads this file as the spec.

---

## 9 · Verification checklist (end-of-branch)

Run before opening PR(s):

- [ ] `shellcheck -x env/build-nix/*.sh` clean
- [ ] `env/build-nix/test.sh` green (nix eval + build)
- [ ] `env/build-nix/bootstrap-common.sh --dry-run` idempotent
- [ ] `cd mAId && deno task test` all green
- [ ] `cd mAId && deno task check` typecheck clean
- [ ] `nix profile install ./mAId` produces `maid` on `$PATH`
- [ ] `maid deploy` creates every expected symlink from §5.3
- [ ] `maid deploy` a second time is a no-op
- [ ] `maid intent` returns the documented results for the three
  canonical prompts
- [ ] Real Claude session loads only the intent-matched skills
- [ ] Fresh VM: curl bootstrap → build → deploy → session works
  end-to-end
- [ ] `env/.kdevkit/feature/feature-build-layers.md` session log
  updated with the work done

---

## Session Log
<!-- Newest at top -->

### 2026-05-06 - Iter 1 refinement: decouple the four layers
- **No chaining.** Each layer is a distinct, single-purpose script
  that exits when its job is done. User runs four commands in
  sequence on a fresh machine, or just Layer 3 for day-2 rebuilds.
  Runner/orchestrator deferred until a concrete need appears.
- Trimmed `env/build-nix/bootstrap-common.sh` to L2-only: removed
  `--envKind`, `--pre-nix`, `--post-nix`, `--post-clone`,
  `invoke_layer_3`, `print_next_steps`, and the curl-mode re-exec
  into Layer 3. Still curl-able and clone-runnable; auto-detects
  both modes. Keeps `--dry-run`, `--help`, mode detection,
  `ensure_workspace`, `ensure_github_ssh`, `clone_or_fetch`.
- Trimmed all four Layer-1 scripts (`bootstrap-ubuntu-mane.sh`,
  `bootstrap-al2-kelasa.sh`, `bootstrap-al2023-kelasa.sh`,
  `bootstrap-darwin-kelasa.sh`): removed `ensure_envkind_default`,
  `hand_off_to_layer_2`, and their invocations. Each script ends
  cleanly after its last `ensure_*` step with a "Layer 1 done"
  log.
- Trimmed `env/build-nix/_common.sh`: removed the pre-nix/post-nix
  `$1`/`$2` source-around-nix plumbing. From 98 → ~55 lines; flow
  is now: replace placeholders → cd flake → eval `NIX_COMMAND` →
  clean stray template dirs → restore placeholders → print
  setup-notes. Refactored the two placeholder-sed blocks into a
  single `replace_placeholders` helper.
- Dropped `"$@"` from each Layer-3 script's
  `source _common.sh` line. L3 scripts now take no args.
- Made `Gorantls-env/desktop/post-nix/flake.nix` multi-system via
  `forAllSystems` over `x86_64-linux`, `aarch64-linux`,
  `x86_64-darwin`, `aarch64-darwin`. The installScript body is
  unchanged; only the outputs are now per-system. Darwin users can
  now `nix run .../post-nix#install` without the flake failing to
  evaluate.
- Rewrote `env/README.md` around decoupled layers:
  Layer design (no chaining, four scripts, curl-ability by
  layer), envKinds (L1/L4 location per envKind using
  `<kelasa-specific env repo>` placeholder), Shell-hook extension
  points (how L3 hooks L1 and L4 via `~/.pre-nix-rc` and
  `~/.post-nix-rc`), Tiered package model.
- Updated `env/setup-notes.md` to show the four-command sequence
  instead of chaining prose. Added hook-extension-point section.

### 2026-05-06 - Iter 1 refactor: envKind-aligned naming + kelasa split
- Source of truth for names is `flake.nix`. All layers renamed to
  the full envKind:
  - Layer 3: `build-nix/{ubuntu,darwin,al2,al2023}.sh` →
    `build-nix/{ubuntu-mane,darwin-kelasa,al2-kelasa,al2023-kelasa}.sh`
    (via `git mv`; content unchanged).
  - Layer 1 public: `build-nix/bootstrap-ubuntu.sh` →
    `build-nix/bootstrap-ubuntu-mane.sh`, now auto-injects
    `--envKind ubuntu-mane`.
  - Layer 2: `bootstrap-common.sh` flag `--platform` → `--envKind`
    (accepts both `--envKind NAME` and `--envKind=NAME`).
- Kelasa Layer 1 moved to `Gorantls-env`:
  - `desktop/bootstrap-al2.sh` → `bootstrap-al2-kelasa.sh` (git mv
    + refactor).
  - `desktop/bootstrap-al2023.sh` → `bootstrap-al2023-kelasa.sh`
    (git mv + refactor).
  - New `desktop/bootstrap-darwin-kelasa.sh` (minimal; docs brew +
    Determinate installer as manual prereqs, self-clones
    Gorantls-env, delegates to Layer 2).
  - Each script: dropped generic steps (workspace, GitHub SSH,
    env clone) now owned by Layer 2; kept kelasa-specific
    (Amazon SSH/mwinit, self-clone, toolbox, sudoers [al2023],
    single-user Nix, nix.conf, zsh/chsh [al2], ~/.pre-nix-rc
    writer). Every step is verify-and-skip idempotent. Ends with
    local-preferred / curl-fallback hand-off to Layer 2,
    auto-injecting `--envKind <name>`.
  - New `desktop/post-nix-run.sh`: thin wrapper around
    `nix run desktop/post-nix#install`, sourceable by Layer 2 as
    `--post-nix`. Flake itself unchanged; `nix run` directly still
    works standalone.
  - Deleted `desktop/pre-nix.sh` — its job (write ~/.pre-nix-rc
    with toolbox PATH) now lives in each AL bootstrap as an
    idempotent `ensure_pre_nix_rc` step. The `--pre-nix` hook
    plumbing in `_common.sh` + Layer 2 stays as a generic
    extension point.
- env/README.md rewritten with four sections: Layer design +
  responsibility table, envKind bootstrap table (using
  `<kelasa-specific env repo>` placeholder, no names leak),
  Shell-hook extension points (~/.pre-nix-rc / ~/.post-nix-rc),
  Tiered package model (summarized from flake.nix:1-18).
- env/setup-notes.md updated: "private sibling env repo" →
  "envKind repo (public or private)"; curl path uses new
  bootstrap-ubuntu-mane.sh; adds shell-hook extension-points
  section.
- Code review (two passes). Fixed: `post-nix-run.sh` used `exec`
  inside a script that gets `source`d from `_common.sh:69` —
  would've killed the caller. Now plain `nix run`. AL2
  `ensure_toolbox`: `-fsS` on curl so HTML error pages don't
  become the Authorization header; explicit cleanup in error
  branches instead of RETURN trap. Wrapped `mkdir`/`chmod` on
  `~/.ssh` and `~/.config/nix/` in `run` so `--dry-run` is truly
  mutation-free. Improved `print_next_steps` placeholder wording.
- Tests: `shellcheck -x` clean across all 6 scripts. Clone-mode,
  curl-mode, and AL2023 dry-runs all trace L1→L2→L3 correctly
  with zero mutations (staging HOME stays empty after two runs).
  NFR5 grep on public env: only hits are legitimate abstract uses
  (`corp`/`internal` as nouns, "Amazon Linux" as OS name,
  `kusimari` as repo owner — spec FR-B3/NFR5 allows repo URLs in
  scripts).

### 2026-05-06 - Iteration 1 landed: four-layer bootstrap
- Created `env/build-nix/bootstrap-common.sh` (Layer 2). Detects
  curl-vs-clone mode via `--post-clone` sentinel + `$ENV_CLONE/.git`
  check (not path-compare — handles symlinks and non-default clone
  paths). Ensures workspace, checks GitHub SSH, clones/fetches env
  + mAId, validates `--pre-nix` / `--post-nix` hooks, forwards them
  to Layer 3 as positional $1 / $2 per `_common.sh`'s existing
  contract. Accepts `--platform` to chain into `build-nix/<name>.sh`
  or prints next-step instructions. `--dry-run` is fully honored
  (no mutations, no ssh-keygen, re-exec is logged-only).
  Arg parser accepts both `--flag value` and `--flag=value`.
- Created `env/build-nix/bootstrap-ubuntu.sh` (Layer 1 Ubuntu).
  Checks `ID=ubuntu`, installs apt prereqs (build-essential, curl,
  git, xz-utils, ca-certificates, openssh-client), installs Nix via
  Determinate installer (multi-user, `--no-confirm`), then hands
  off to Layer 2 — preferring a local clone and falling back to
  curl. Auto-injects `--platform ubuntu` so `curl | bash` runs
  end-to-end through Layer 3. `--dry-run` honored throughout.
- Updated `env/setup-notes.md` with the four-layer cheat-sheet, a
  fresh-machine curl example, the `--pre-nix`/`--post-nix` flag
  shape, and the private-sibling-env-repo delegation contract.
  Added a forward-reference to `nix profile install .` + `maid
  deploy` for once iter 2 lands.
- Rewrote `env/README.md` as a four-layer overview with a
  quick-start, per-platform table, and pointers onward. Darwin row
  explicitly notes "no Layer-1 script; see darwin.sh header".
- NFR5: all new files scrubbed of site/employer/internal-tool
  identifiers. Only repo URLs present: `github.com/kusimari/env`
  and `github.com/kusimari/mAId`.
- Code review (two passes, external agent). Fixed: curl path not
  auto-continuing to Layer 3; `--dry-run` mutating SSH state;
  brittle `sed -n` usage(); symlink-unsafe mode detection;
  silent `chmod +x` on tracked files; GNU-style `--flag=value`
  previously rejected; `ssh-keygen` hang on non-TTY when path
  existed as non-key; Ubuntu Layer 1 ignoring `--dry-run`.
- Tests passed: `shellcheck -x` clean over both new scripts.
  Idempotent dry-run: two runs against a staging HOME produce
  byte-identical output and leave the HOME untouched (only the
  tmpdir inode remains). End-to-end dry-run from faked Ubuntu
  os-release shows L1 → L2 → L3 chain with zero mutations.
  `env/build-nix/test.sh` not run here (requires a real nix build;
  user's domain).
- Not yet done: writing-style draft, intent keyword list, symlink
  relative-vs-absolute decision — all deferred to Iter 2 where
  they're load-bearing.

### 2026-05-05 - Spec/design/impl/test plan locked
- Restructured the doc into spec (§1), design (§2), skills content
  drafts (§3), implementation plan with strict TDD ordering (§4),
  and test plan (§5). Added §6 resolved decisions, §7 file list,
  §8 open items, §9 verification checklist.
- Locked design decisions through interview: Deno runtime (not
  Nix for transforms), minimal flake wrapper, link-vs-build mode
  via frontmatter, three source types (original / external /
  patch), central registry, intent routing lives in top-level
  `CLAUDE.md` (not as a skill), `inbox/` committed, darwin Layer-1
  skipped.
- Branch scope bounded to iterations 1 + 2. Iterations 3–6
  captured so the system is evaluated whole but not implemented
  here.
- Skills drafted inline: `git.md` (branches, commits, staging,
  destructive-op guardrails, PRs); `development.md` (scope
  discipline, verification habits, reversibility, planning;
  inspired-by kdevkit/feature-dev.md and kdevkit/agent-dev-loop.md
  as attribution only); `writing-style.md` (scrub + port pending).
- Public-repo scrub pass: replaced employer/site/tool-specific
  identifiers with abstract "private sibling env repo" references
  per NFR5.

### 2026-04-30 - Scrub site-specific references
- Removed employer-specific names, internal tool names, and
  specific org/user handles from the doc. Replaced with abstract
  references ("private sibling env repo", "site-specific auth").
  Added NFR5 to make the policy explicit. Concrete clone URLs stay
  in the bootstrap scripts, not in this design doc.

### 2026-04-30 - Spun out from misc-updates
- Split bootstrap + mAId work out of `misc-updates` into this new
  feature doc. The mAId sibling repo is already cloned empty at
  `~/env-workplace/mAId`; when work begins, branch off to
  `feature-build-layers`.
- No code written yet. First implementation task is the
  requirements pass with the user (interactive skills content,
  nail down activator mechanism, decide on build-script
  unification question).
