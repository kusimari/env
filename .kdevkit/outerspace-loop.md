# Outerspace Loop

Methodology for feature development across projects.

## Overview

Three-phase workflow for structured feature development:

1. **Phase 1** — Requirements & Planning (interactive)
2. **Phase 2** — Implementation (agent-driven)
3. **Phase 3** — Human Review (human-led)

**Key principle:** Never automatically chain phases. Always wait for explicit human approval before proceeding to the next phase.

---

## Phase 1 — Requirements & Planning

**Goal:** Arrive at an approved plan before writing a single line of code.

### Process
1. **Requirements Interview** - Understand what needs to be built
2. **Design Interview** - Define the technical approach
3. **Testing Interview** - Plan verification strategy
4. **Implementation Interview** - Break down development steps

### Deliverables
- Detailed feature file in `.kdevkit/feature/<name>.md`
- Clear acceptance criteria
- Technical design decisions
- Testing strategy

### Exit Criteria
- Human explicitly approves the plan
- All questions answered and documented
- Implementation approach is clear and feasible

---

## Phase 2 — Implementation

**Goal:** Implement the approved plan via `.kdevkit/innerspace-loop.md`, pass all quality and test gates, and push.

### Process
1. Create work branch following project naming conventions (see `.kdevkit/project.md`)
2. Follow innerspace-loop.md for development workflow
3. Run project-specific build and test commands (see `.kdevkit/project.md`)
4. Pass code review gates defined in `.kdevkit/review.md`

### Exit Criteria
- All quality gates passed (thresholds defined in `.kdevkit/project.md`)
- Code pushed to branch
- Ready for human review

---

## Phase 3 — Human Review

**Goal:** Incorporate human feedback and reach `ready-for-merge`.

### Process
1. Present implementation for review
2. Address feedback through quick iterations
3. Handle minor fixes and adjustments
4. Document any deviations from original plan

### Escalation
- **Minor changes:** Continue in Phase 3
- **Significant changes:** Return to Phase 1 for re-approval

### Exit Criteria
- Human approves implementation
- All feedback incorporated
- Branch ready for merge

---

## Notes

- Feature files serve as single source of truth
- Use conventional commits throughout
- Maintain clean, working state at each commit
- Follow project conventions defined in `.kdevkit/project.md`