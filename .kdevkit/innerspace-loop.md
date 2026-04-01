# Innerspace Loop

Development workflow for implementing approved features.

## Overview

Systematic workflow for code changes that maintains quality and working state throughout development.

**Key principle:** Any stage can loop back to any prior stage based on feedback or issues discovered.

---

## Development Stages

### 1. Setup Work Branch
- Create branch following project naming conventions (see `.kdevkit/project.md`)
- Ensure clean starting state from main branch
- Set up development environment

### 2. Implementation
- Follow approved plan from Phase 1
- Make incremental, logical commits
- Use conventional commit prefixes:
  - `plan:` - Planning and design commits
  - `dev:` - Development implementation
  - `review:` - Review and refactoring
  - `fixes:` - Bug fixes and corrections

### 3. Quality Gates
- Run project-specific build commands (see `.kdevkit/project.md`)
- Execute test suites and validation scripts
- Follow code quality criteria in `.kdevkit/review.md`

### 4. Testing
- Execute project-specific test strategy (see `.kdevkit/project.md`)
- Verify changes work across target environments
- Check for regressions and breaking changes

### 5. Code Review
- Run incremental review process (see `.kdevkit/review.md`)
- Address findings based on severity:
  - **High:** Must fix before proceeding
  - **Medium:** Should address for maintainability
  - **Low:** Optional improvements
- Re-enter quality gates for non-trivial changes

### 6. Integration
- Ensure clean merge with main branch
- Final validation of all quality gates
- Prepare for human review (Phase 3)

---

## Commit Conventions

Each commit should:
- Leave repository in working state
- Have clear, descriptive message
- Use appropriate conventional prefix
- Focus on single logical change

## Looping Strategy

- **Quality issues:** Return to Implementation
- **Test failures:** Return to Implementation or Quality Gates
- **Review findings:** Address and re-enter Review
- **Integration conflicts:** Return to Setup or Implementation

## Sub-agent Delegation

When using sub-agents:
- Provide isolated, focused context
- Avoid historical pollution from main conversation
- Ensure sub-agent understands project requirements (see `.kdevkit/project.md`)
- Validate sub-agent work through quality gates