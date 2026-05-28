# Backlog: persist-repo-facts-in-project-md

## What

Lift two persistent facts about the `env` repo from per-machine
agent memory into `env/.kdevkit/project.md` so they are durable,
reviewable, and visible to anyone (or any agent on any machine)
working in the repo:

1. **Branch protection on `main`.** The kusimari/env GitHub repo
   rejects direct pushes to `main`; every change (including
   one-line doc fixes) must go through a feature/chore branch
   plus a pull request. The remote response is:
   ```
   remote: - Changes must be made through a pull request.
    ! [remote rejected] main -> main (push declined due to
      repository rule violations)
   ```
   This is non-negotiable from the client side and shapes every
   workflow in the repo.

2. **Multi-repo workplace map.** This repo is one of four that
   together form the user's dev environment. The role split is:
   - `~/env-workplace/env` (this repo, public) — base-env source
     (L1 mane, L2, L3, L5a; flake.nix; home.nix).
   - `~/env-workplace/Gorantls-env` (private) — kelasa-side base
     env (L1 kelasa envKinds, L4, L5b).
   - `~/tool-workplace/ai-workspace/{mAId,Gorantls-agents}` —
     fast-moving AI tooling (skills, agents, ai-sync).
   - `~/dabba/Gorantls-store` — backed-up cross-machine state.

   "Where does this kind of change land" is a recurring
   question and the answer is repo-shaped. project.md's
   Conventions section already gestures at this in
   "Day-2 update flows" but does not lay out the four-repo
   map explicitly.

## Why

Per-machine agent memory is invisible to humans, invisible to
agents on other machines, and easy to lose. Project facts that
shape every session belong in the repo so they survive across
machines, are visible to code review, and serve as
documentation.

Both facts surfaced as memory writes during the
testing-first-class-agent-loop feature work:

- Branch protection: discovered when a "no PR, ship it"
  instruction couldn't be honoured because `main` rejected the
  push. The recovery dance (branch out → reset main → PR) is
  predictable and worth documenting once.
- Repo map: written to memory after the May 2026 layer/
  workplace restructure, but it's a fact about the project
  rather than a session observation.

## Open questions

- **Where in `project.md`?** Branch protection probably belongs
  under a new "Hard constraints" section (kdevkit's §7 already
  references it as a canonical place for repo-level constraints).
  The repo map could either join Hard constraints, extend
  Conventions §"Day-2 update flows", or be a new "Multi-repo
  context" subsection.
- **Does the repo map belong in env at all?** The four-repo map
  spans both env-workplace and tool-workplace. The user's
  primary entry point is env, so documenting it here helps —
  but mAId's project.md may also want a complementary view from
  the AI-tooling side. Possibly a small cross-reference in
  both, with the canonical map in one place.
- **Memory entries to retire on merge.** When this lands,
  delete `project_env_branch_protection.md` and
  `reference_env_repos.md` from per-machine memory at
  `~/.claude/projects/-local-home-gorantls-env-workplace/memory/`.
- **Verify before persisting.** Memory describes what was true
  on a given date. Re-verify branch protection still applies
  (gh repo view) and the repo map still matches the current
  state of all four repos before lifting.
