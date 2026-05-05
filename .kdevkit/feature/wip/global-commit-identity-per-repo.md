# global-commit-identity-per-repo

## Status

📋 **Not started.** Captured during `feature-build-layers` work
when a commit was about to go out with the corporate dev-desktop
hostname email (`gorantls@dev-dsk-<...>.amazon.com`) into a public
GitHub repo. Amended once by hand; want a durable fix so it never
happens again.

## Problem

Git picks the committer identity from the first place it finds one:
repo-local config → global config → environment → hostname-derived
default. On fresh machines or freshly cloned repos with no local
identity set, git silently falls back to
`<user>@<hostname>`, which on corporate dev desktops leaks an
employer-identifying string into the commit log of public repos.

Concrete incident (2026-05-05): a commit on public
`github.com/kusimari/env` was about to go out as
`gorantls@dev-dsk-gorantls-2b-b4bddea0.us-west-2.amazon.com`
before amend. The public commit history already has some of these
from earlier sessions — acceptable but not desirable going
forward.

Mirror concern: corporate-hosted repos want the corporate email
(`gorantls@amazon.com`), not the public one (`kusimari@gmail.com`).
A single global default can't satisfy both.

## Goal

Per-repo (or per-hosting-platform) commit identity is selected
automatically based on the repo's remote URL. No manual
`git config user.email ...` dance on every clone. No accidental
cross-contamination.

## Approach — two candidates

### Option 1: `includeIf` directive in `~/.gitconfig`

Git supports conditional config include based on the current
working directory:

```
[includeIf "gitdir:~/env-workplace/**/.git"]
    path = ~/.gitconfig-public

[includeIf "gitdir:~/workplace/**/.git"]
    path = ~/.gitconfig-corporate
```

With `~/.gitconfig-public` setting `user.email = kusimari@gmail.com`
and `~/.gitconfig-corporate` setting `user.email =
gorantls@amazon.com`. Works automatically by directory location.
Nix/home-manager can render these files.

Trade-off: depends on the user placing repos in the right
directory tree. `env-workplace/` is the standard location for
public/mixed repos, corporate repos live elsewhere.

### Option 2: pre-commit hook that checks remote

Install a global pre-commit hook (via `core.hooksPath`) that reads
the repo's origin URL and refuses the commit if the configured
`user.email` doesn't match the expected identity for that host.

Trade-off: blocks commits rather than auto-fixing them; more code
to maintain; but catches the bug even when the directory heuristic
fails.

### Recommendation

**Ship Option 1 first** (directory-based includeIf) since it's
fully declarative and home-manager-renderable. Add Option 2
(validating hook) later if Option 1 misses cases.

## Scope

- Add home-manager-managed `~/.gitconfig-public` and
  `~/.gitconfig-corporate` (or equivalent split).
- Update `~/.gitconfig` to use `includeIf` based on directory.
- Document the directory convention in `env/setup-notes.md`.
- Consider: should the email configuration move to the `mAId`
  system once that lands, since it's arguably agentic-environment
  config? Probably not — it's git-level, not AI-level.

## Out of scope

- Retroactively rewriting public history to scrub old
  corporate-hostname emails. Noisy; not worth it.
- Multi-account GitHub / SSH key selection — that's a different
  problem solved by `~/.ssh/config` host aliases.

## Implementation notes

- `env/home/` is where home-manager modules live. A small
  `home/git-identity.nix` module would render the three config
  files and wire `programs.git.includes`.
- The public/corporate identity values should NOT live in this
  public `env` repo. They belong in:
  - The `mAId` repo (if we treat identity as agentic-env config), or
  - A *private sibling env repo* (more likely), or
  - A `~/.gitconfig.local` pointed at by a public `include =
    ~/.gitconfig.local` directive.
- Existing `env` CLAUDE.md / feature practice: public repo stays
  site-agnostic. The NFR5 rule applies here — no hardcoded emails
  or employer identifiers in this repo.

## Success criteria

- Commit in `~/env-workplace/<any-repo>/` uses
  `kusimari@gmail.com` without any manual config.
- Commit in a corporate-repo directory uses
  `gorantls@amazon.com` without any manual config.
- Fresh machine bootstrap (via `feature-build-layers`
  `bootstrap-common.sh`) leaves the identity wiring in place.

## Session Log

### 2026-05-05 - Captured
- Spun out during `feature-build-layers` commit prep when the
  dev-desktop hostname email almost went public. Amended once by
  hand; this feature is the durable fix.
