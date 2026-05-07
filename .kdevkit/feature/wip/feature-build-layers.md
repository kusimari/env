# feature-build-layers — mAId (agentic resources CLI)

## Status

🚧 **Spec locked; implementation not started.** This doc is the
review artifact: spec, design, implementation order, and test plan
for the `mAId` repo. Edit in place. The implementation loop reads
this file as its instruction set.

**Prereq landed in a previous iteration:** a four-layer bootstrap
that leaves both `env` and `mAId` cloned at
`~/env-workplace/{env,mAId}` on any supported machine. That work
is documented at `../feature-build-layers.md` (sibling of this
file) — reference it only if you need the bootstrap context. This
doc is self-contained for mAId.

**Branch plan:** this iteration lands on a fresh branch
(`feature-maid` or similar) off post-merge `main` in both repos.

## Scope guardrails

`env` is a public repo. `mAId` will also be public. Both stay
site-agnostic — no employer names, internal tools, private repos,
auth systems, or clone URLs in source or docs. Concrete URLs live
in bootstrap scripts (which already exist; not this iteration's
concern). Machine- or site-specific concerns belong in private
sibling envKind repos, outside this scope.

---

## 0 · Context — what mAId is and why

One tool-agnostic source of truth for agentic resources
(**skills**, **agents**, **commands**, **MCPs**) that compiles to
per-tool views under `$HOME` via symlinks — either directly
(`mode: link`) or after a transform (`mode: build`). The `maid`
CLI turns the authored `sources/` tree into tool-native paths that
Claude Code, Gemini CLI, Kiro, and future tools all consume.

Properties we want:

| Property | Mechanism |
|---|---|
| Generic source, per-tool output | Central `registry.ts` maps `(tool, kind) → (path, transform)`; sources declare only `kind` |
| Composition, not reinvention | Three source types — `original` (curated), `external` (verbatim pointer), `patch` (named delta) — pick the right one per skill |
| Intent-gated activation | Not all skills load every session. A top-level instruction in `CLAUDE.md`/`GEMINI.md`/`KIRO.md` tells the AI to classify session intent and surface only matching skills; `maid intent <msg>` is the reference classifier |
| Self-mutating | Skills that notice patterns edit themselves (`mode: link` = live). Structural changes go to `inbox/issues/` for deliberate follow-up |
| Home stays empty | `$HOME/CLAUDE.md`, `$HOME/.claude/skills`, etc. are all symlinks into the mAId checkout. Single versioned source |
| Nix is OS-wiring only | mAId is a Deno/TypeScript project. `flake.nix` is a minimal wrapper that puts `maid` on `$PATH` |

---

## 1 · Spec

### 1.1 Functional requirements

- **FR-M1** `maid deploy` creates symlinks from tool-native paths
  in `$HOME` to the mAId checkout. Idempotent; re-running is a
  no-op.
- **FR-M2** `maid deploy` NEVER overwrites existing non-symlink
  files in `$HOME`. On collision: print a warning, skip the file,
  continue. User resolves manually.
- **FR-M3** `maid deploy` NEVER touches files outside the
  registered set (`~/.claude/settings.json`, `~/.claude/projects/`,
  `~/.claude/sessions/`, `~/.claude/plans/` are all off-limits).
- **FR-M4** `maid build` resolves `source: external` and `source:
  patch` files into `build/<tool>/<name>.md`. `build/` is
  gitignored. `maid deploy` symlinks into `build/` for those files.
- **FR-M5** `maid intent <first-user-message>` returns JSON with
  `{ classified_intent, selected_skills, ambiguous_with }`. Used
  both by AI tools (via the top-level CLAUDE.md instruction) and
  by humans for debugging the router.
- **FR-M6** `maid inbox add <title> [--body <text>]` appends a
  markdown file under `inbox/issues/<YYYY-MM-DD>-<slug>.md`. AI
  tools follow the same protocol when asked to file a change
  request.
- **FR-M7** Every source file has YAML frontmatter with at least
  `kind:`, `source:`, and `intent:`. `mode:` defaults from
  `source:` (original → link, external/patch → build). Missing
  mandatory fields = hard error on build/deploy.
- **FR-M8** Frontmatter is validated against a schema (one place:
  `maid/schema.ts`). Schema violations print which field, which
  file, which line.
- **FR-M9** `maid` exit code is non-zero on any failure. Output is
  human-readable by default; `--json` flag switches to JSON for
  tooling.
- **FR-M10** mAId's own flake exports a
  `packages.<system>.default` that wraps `deno run --allow-read
  --allow-write --allow-run` pointing at `maid/main.ts` with the
  mAId checkout embedded as a runtime path. No TS compilation at
  build time — source is live.

### 1.2 Non-functional requirements

- **NFR-M1** `maid deploy` completes under 2 seconds on warm cache
  (read sources, create symlinks).
- **NFR-M2** `maid` has zero runtime deps outside Deno stdlib
  (no npm, no node_modules).
- **NFR-M3** All schema violations fail fast with a pointer to the
  offending line (`path/to/file.md:12: missing 'kind:'`).
- **NFR-M4** Deterministic output order — deploying on two
  machines from the same commit produces byte-identical link
  targets.
- **NFR-M5** Tests (`deno test`) run in under 10 seconds total on
  an empty deno cache.

### 1.3 Out of scope for this branch

- Multi-tool transforms (gemini-cli, kiro, claude-desktop) —
  iter 3+
- Agents implementation — iter 4
- MCP dependency system — iter 5
- Meeting-prep scheduler — iter 6
- `maid reload` helper — not needed until build-mode exists
- `gittree` CLI / lazygit rework (separate branch)
- Anything specific to a particular employer's infrastructure —
  belongs in private sibling repos

### 1.4 Success criteria

- `cd ~/env-workplace/mAId && nix profile install .` creates
  working symlinks pointing into the checkout (`ls -l ~/CLAUDE.md`
  resolves into the mAId clone).
- `maid deploy` is idempotent: second run is a no-op.
- `maid intent "help me fix the bug in foo.py"` returns
  `{ intent: "bug-fix", skills: ["development", "git"] }`.
- `maid intent "help me write a blog post"` returns
  `{ intent: "narrative", skills: ["writing-style"] }`.
- A real AI session surfaces only the intent-matched skills for a
  given first user message.

---

## 2 · Design

### 2.1 mAId layout

```
mAId/
├── flake.nix                # minimal — wraps `deno run maid/main.ts`
├── deno.json                # imports map, task runner
├── .gitignore               # build/, .deno_cache/, etc.
├── README.md                # quickstart; points at .kdevkit/
│
├── CLAUDE.md                # TOP-LEVEL instruction file — see §2.7
├── GEMINI.md                # same instructions, Gemini flavor
├── KIRO.md                  # same instructions, Kiro flavor
│
├── maid/                    # the CLI
│   ├── main.ts              # argv parser + dispatcher
│   ├── schema.ts            # frontmatter schema + validator
│   ├── registry.ts          # (tool × kind) → (path, transform, mode)
│   ├── sources.ts           # read + parse sources/ tree
│   ├── deploy.ts            # symlink manager
│   ├── build.ts             # transform runner (stub this iter)
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

### 2.2 Frontmatter schema

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

Validation lives in `maid/schema.ts`. Invalid files fail fast with
a file+line error message.

### 2.3 Source types (provenance, not mode)

| Type | `source:` | Authoring | Runtime | When to use |
|---|---|---|---|---|
| **A** | `original` | Full markdown in mAId | `link` | Your own method, even if inspired by others |
| **B** | `external` | Just frontmatter; content fetched | `build` (fetch + cache) | Public skill you use verbatim; upstream stable |
| **C** | `patch` | Frontmatter + delta in body | `build` (fetch base, apply delta) | Public thing is mostly right, need named override |

On this branch, all skills are Type A. Type B/C land in iter 3+
when `build` transforms work.

### 2.4 Link mode vs. build mode

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
- Stubbed this iter (`maid build` prints "no build-mode sources
  yet" and exits 0 if no `source: external|patch` files exist).

**Rule for AI sessions:**
AI can freely edit `link`-mode files and anything under `inbox/`.
Changes that affect build-mode sources, the registry, or `maid/`
source code must be filed as inbox issues — they require a
deliberate rebuild.

### 2.5 Registry (the one place tool-shape knowledge lives)

Conceptual shape (actual code not included here):

```
registry[tool][kind] = {
  path:       string_template,     # e.g. "{HOME}/.claude/skills/{name}.md"
  transform:  function | null,     # null = link mode; non-null = build
  merge:      boolean,             # true if multiple sources merge
  supported:  boolean,             # false = tool doesn't support this kind
}
```

Registry content for this branch:

| tool | kind | path | transform | notes |
|---|---|---|---|---|
| claude-code | skill | `~/.claude/skills/{name}.md` | null | link |
| claude-code | command | `~/.claude/commands/{name}.md` | null | link; no commands yet |
| claude-code | agent | `~/.claude/agents/{name}.md` | (stub) | iter 4 |
| claude-code | mcp | `~/.claude.json` | (stub) | iter 5; merge=true |
| gemini-cli | * | * | — | **commented stub**, iter 3 |
| kiro | * | * | — | commented stub |

Top-level symlinks (outside the per-kind grid, because they're
the root pointer files):
- `~/CLAUDE.md` → `mAId/CLAUDE.md`
- `~/GEMINI.md` → `mAId/GEMINI.md`
- `~/KIRO.md`   → `mAId/KIRO.md`

### 2.6 Intent routing

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

Implementation of the classifier itself is rules-based this iter
(keyword matching against intent tags), not LLM-powered. Keeps it
deterministic and testable. An LLM-powered upgrade can come later.

### 2.7 Top-level CLAUDE.md / GEMINI.md / KIRO.md

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

### 2.8 Self-update loop — what each case looks like

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

### 2.9 Inbox protocol

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

### 2.10 Flake.nix shape

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

### 2.11 Deno choice — scope of permissions

`maid` runs with explicit `--allow-read --allow-write --allow-run
--allow-env`. Net access (`--allow-net`) is only needed for iter 3
(fetching Type B/C externals); on this branch it's omitted until
the fetch step lands.

---

## 3 · Skills content (drafts for review)

These are the skills to seed this iter. All Type A. Content below
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
during implementation alongside the code; scrub pass is a diff of
the existing personal draft. Final text replaces the draft here
before deploy. See §7 open item (d).

### 3.4 Top-level `CLAUDE.md` (routing instruction)

Content as specified in §2.7. Same file content mirrors into
`GEMINI.md` and `KIRO.md` with only the tool-name substitutions.

---

## 4 · Implementation plan

### 4.1 TDD order (strict; each step's tests land with the step)

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
   - `maid/registry.ts`: the table from §2.5. Only claude-code
     populated; gemini-cli/kiro as commented stubs.
   - Tests: `tests/registry_test.ts` — every (tool, kind) lookup
     returns a path template or `unsupported`; path templates
     render correctly with `{HOME}` and `{name}` substitution.

4. **Deploy (link mode only).**
   - `maid/deploy.ts`: for each source, compute destination path
     via registry, create a symlink from destination → source.
     Skip (with warning) if destination exists and is a
     non-symlink or points elsewhere unexpectedly.
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
   - Write `CLAUDE.md`, `GEMINI.md`, `KIRO.md` per §2.7.

10. **End-to-end smoke test.**
    - `tests/smoke_test.ts`: spawn a fake HOME, run
      `maid deploy` against the real sources/ tree, assert every
      expected symlink resolves correctly.
    - Manual: `nix profile install .` in a real checkout, then
      check `ls -l ~/CLAUDE.md` resolves into the mAId tree;
      `ls -l ~/.claude/skills/git.md` resolves correctly; an AI
      session with "help me write a blog post" loads only
      writing-style, not git/development.

### 4.2 Acceptance

- `deno task test` exits 0; all tests pass.
- `deno task check` (typecheck) exits 0.
- `maid deploy --dry-run` on a fresh HOME prints the planned
  symlinks; `maid deploy` creates them; second run is a no-op.
- `maid intent "help me fix the bug in foo.py"` returns
  `{ intent: "bug-fix", skills: ["development", "git"] }`.
- `maid intent "help me write a blog post"` returns
  `{ intent: "narrative", skills: ["writing-style"] }`.
- `maid intent "something"` returns `ambiguous_with` populated.

### 4.3 TDD loop

Each step in §4.1 follows: **write failing test → implement →
run test → commit**.

Loop driver:

```
while not all steps in §4.1 complete:
  1. Pick the next step.
  2. Write the test fixtures + test cases in tests/<name>_test.ts.
  3. Run `deno task test` — confirm failure is on this step.
  4. Implement maid/<name>.ts.
  5. Run `deno task test` — confirm pass.
  6. Run `deno task check` — typecheck must pass.
  7. Commit: `feat(maid): <step summary>`.
  8. Update inbox/session-log/ if anything surprising came up.
```

### 4.4 Future iterations (captured; not this branch)

- **Iter 3 — build-mode + gemini-cli target.** Implement
  transform runner; fetch+cache for Type B; base-fetch + delta
  apply for Type C; gemini-cli rows in registry; prove a skill
  flows through.
- **Iter 4 — agents + inbox write-back integration.** Agent
  source format (markdown + `invocation:`, `tools:`). First
  agent: notes-capture. AI-side protocol for appending to inbox
  during sessions.
- **Iter 5 — MCPs.** MCP source format; dep validation; one
  public MCP seeded; private MCP as reference-only.
- **Iter 6 — meeting-prep scheduler.** Outlook/equivalent MCP →
  cross-ref notes → produce brief. Wake-up trigger design.

Each future iteration lands on its own branch off `main` and gets
its own spec/design/test section (either appended here or spun
out).

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

- `valid_sources/` — one skill per type (this branch, only Type
  A); every kind (skill/agent/command/mcp stubs); known
  frontmatter.
- `invalid_sources/` — missing required fields; wrong enum
  values; conflicting source-type fields; `kind: skill` with no
  `name`.
- `fake_home/` — directory skeleton to be used as `$HOME`
  override during deploy tests.
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
- `(gemini-cli, skill)` returns `unsupported` this branch.
- Path template `{HOME}/.claude/skills/{name}.md` renders with
  substitutions.

**Deploy**
- Creates symlinks for every registered (source, tool, kind).
- Symlinks are relative where possible (or document absolute —
  decide during implementation; tests encode the choice; see
  §7 open item (b)).
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
- `add "title"` creates `inbox/issues/<YYYY-MM-DD>-<slug>.md`
  with the frontmatter shape from §2.9.
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
- `nix profile install .` in a real mAId checkout.
- Claude session with narrative prompt loads only writing-style.
- Claude session with coding prompt loads development + git.
- Ambiguous prompt triggers disambiguation question.

### 5.4 Coverage target

Not enforced numerically. Every exported function in `maid/*.ts`
has at least one test case; every FR from §1.1 has a test case
that maps to it.

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
8. Writing-style moves to `mAId/sources/skills/writing-style.md`,
   scrubbed of site-specific content.
9. Classifier in `maid intent` is rules-based this iter;
   LLM-based upgrade parked for later.

---

## 7 · Open items (to decide before the implementation loop)

None blocking. Items to mark before kicking off:

- (a) **Intent keyword list** — `maid intent` ships with a small
  rule set. Seed the initial list in this doc for review, or
  draft during implementation and iterate?
- (b) **Symlink relative vs absolute** — §5.3 notes this is
  decided at implementation. Relative symlinks are more portable
  across checkouts; absolute are more debuggable. Preference?
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

## 8 · Files to create on this branch

### mAId repo (branch off post-merge main)

- CREATE `flake.nix`, `deno.json`, `.gitignore`, `README.md`
- CREATE `CLAUDE.md`, `GEMINI.md`, `KIRO.md`
- CREATE `maid/{main,schema,sources,registry,deploy,build,intent,inbox}.ts`
- CREATE `sources/skills/{git,development,writing-style}.md`
- CREATE `sources/{agents,commands,mcp}/.gitkeep`
- CREATE `inbox/{README.md,issues/.gitkeep,session-log/.gitkeep}`
- CREATE
  `tests/{schema,sources,registry,deploy,build,intent,inbox,main,smoke}_test.ts`
- CREATE `tests/fixtures/{valid_sources,invalid_sources,fake_home}/…`
- CREATE `.kdevkit/feature/mAId-bootstrap.md`

### env repo

No changes on this branch. A future iteration may wire mAId in as
a flake input.

---

## 9 · Verification checklist (end-of-branch)

Run before opening the PR:

- [ ] `cd mAId && deno task test` all green
- [ ] `cd mAId && deno task check` typecheck clean
- [ ] `nix profile install ./mAId` puts `maid` on `$PATH`
- [ ] `maid deploy` creates every expected symlink from §5.3
- [ ] `maid deploy` a second time is a no-op
- [ ] `maid intent` returns the documented results for the three
  canonical prompts
- [ ] Real Claude session loads only the intent-matched skills
- [ ] This file's session log updated with the work done

---

## Session Log
<!-- Newest at top -->

<!-- Populated as implementation happens. -->
