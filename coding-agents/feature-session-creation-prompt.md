# Feature Session Manager - AI Instructions

I am usually invoked when the user says "Load the text in feature-session-creation-prompt.md, and follow it to create a new session file for feature '[feature-name]' from commit-ish 'commit-ish'", follow these instructions:

## Interview-Based Specification Capture

Before creating implementation plans, conduct structured interviews to generate comprehensive specifications. Each interview creates dual-purpose documentation:
1. **Decision Record**: Captures user thoughts, preferences, and rationale
2. **Agent Instructions**: Provides clear, actionable guidance for coding agents

### Interview Process Overview

1. **Requirements Interview** → Requirements Specification
2. **Design Interview** → Technical Design Specification
3. **Testing Interview** → Test Strategy Specification
4. **Implementation Interview** → Implementation Plan Specification

Use the interview templates below to systematically capture information. Each interview should be conversational but comprehensive, ensuring all critical aspects are covered.

### Requirements Interview Template

**Purpose**: Understand what the feature should do, for whom, and how success will be measured.

**Key Questions**:
1. **Problem Definition**: What problem does this feature solve? Who experiences this problem?
2. **User Stories**: Walk me through how users will interact with this feature. What are their goals?
3. **Functional Requirements**: What specific actions/behaviors must the feature support?
4. **Non-Functional Requirements**: Performance, security, usability, compatibility needs?
5. **Success Criteria**: How will we know this feature is working correctly and providing value?
6. **Constraints**: Technical, business, or timeline limitations we must work within?
7. **Priority**: What's essential vs nice-to-have? What can be deferred to future versions?

**Output**: Requirements Specification (functional/non-functional, user stories, acceptance criteria, success metrics)

### Design Interview Template

**Purpose**: Define technical approach, architecture decisions, and design rationale.

**Key Questions**:
1. **Architecture**: How should this feature fit into the existing system architecture?
2. **Technical Stack**: What technologies, libraries, or frameworks should we use? Why?
3. **Data Model**: What data structures, schemas, or storage needs does this feature have?
4. **Interfaces**: What APIs, UIs, or integration points need to be designed?
5. **Dependencies**: What existing systems, services, or components will this interact with?
6. **Alternatives**: What other approaches were considered? Why was this approach chosen?
7. **Scalability**: How should this handle growth in users, data, or usage patterns?
8. **Security**: What security considerations or requirements apply to this feature?

**Output**: Technical Design Specification (architecture diagrams, API specs, data models, security model)

### Testing Interview Template

**Purpose**: Define how the feature will be validated, tested, and verified.

**Key Questions**:
1. **Test Strategy**: What types of testing are needed? (unit, integration, e2e, performance, security)
2. **Test Cases**: Based on requirements, what specific scenarios should we test?
3. **Edge Cases**: What error conditions, boundary cases, or unusual scenarios should we handle?
4. **Test Data**: What test data sets or test environments will we need?
5. **Automation**: What should be automated vs manual testing?
6. **Performance**: What performance benchmarks or load testing is needed?
7. **Integration Testing**: How will we test interactions with existing systems?
8. **Acceptance Testing**: How will we validate that requirements are met?

**Output**: Test Strategy Specification (test plan, test cases, automation strategy, performance criteria)

### Implementation Interview Template

**Purpose**: Plan the development approach, identify risks, and sequence work.

**Key Questions**:
1. **Development Approach**: What's the overall development strategy? (incremental, MVP-first, etc.)
2. **Task Breakdown**: How should this feature be divided into manageable development tasks?
3. **Dependencies**: What needs to be completed before other work can start?
4. **Risks**: What technical or project risks should we plan for? How do we mitigate them?
5. **Integration**: How will this be integrated and deployed with existing systems?
6. **Rollout**: How should this feature be released? (feature flags, gradual rollout, etc.)
7. **Monitoring**: What logging, metrics, or monitoring should be implemented?
8. **Documentation**: What documentation needs to be created or updated?

**Output**: Implementation Plan Specification (task breakdown, dependencies, risk mitigation, deployment plan)

### Interview Execution Guidelines

**For the AI conducting interviews**:
1. **Be conversational**: Ask follow-up questions based on responses
2. **Probe for details**: Don't accept vague answers; ask for specifics
3. **Challenge assumptions**: Help users think through edge cases and alternatives
4. **Capture rationale**: Record not just what was decided, but why
5. **Link requirements to tests**: Ensure testable acceptance criteria for each requirement
6. **Identify gaps**: Point out missing information or inconsistencies
7. **Summarize**: Repeat back what you heard to confirm understanding

**Interview flow**:
- Start each interview by explaining its purpose
- Work through questions systematically but allow natural conversation
- Take notes during the interview in the appropriate specification format
- End each interview by summarizing key points and asking for confirmation
- Show the generated specification to the user for review and refinement

**Creating actionable specifications**:
- Use precise, measurable language
- Include specific examples where helpful
- Format requirements as testable statements
- Provide enough detail for independent implementation
- Cross-reference between sections (link tests to requirements, tasks to design decisions)

## 1. Prompt for feature-name and commit-ish
- Prompt to accept feature-name and commit-ish if not provided
- commit-ish can be a branch name (like 'main', 'develop'), commit hash, or tag
- The session file will always be created in the new worktree under the coding-agents-aided-features directory

## 2. Create git branch with worktree
- Check if the git branch name we are working on is the same as feature-name
- If the branch name and feature-name are different, show the user the exact git command that will be executed: `git worktree add feature-name -b feature-name commit-ish`
- Ask the user to confirm before executing the git command
- Execute the git worktree command only after user confirmation
- If the branch name is the same as feature-name, then do nothing

## 3. Create New Session File
- Use the updated template below (inline in this file)
- Create a new session file in the new worktree at 'coding-agents-aided-features/[feature-name].md'
- Add instructions to the feature file on how it should be used by a coding agent below '##How to use this session file' in the template. This will help the session file be self contained.
- Replace `[Feature Name]` with the actual feature name
- **Conduct the interview process**: Guide the user through the four interviews (Requirements, Design, Testing, Implementation) to generate comprehensive specifications
- Populate each specification section with the outputs from the interviews

## Important Notes
- **No Co-Author**: Do NOT include "Co-Authored-By: Claude" or any co-author lines in commit messages
- **Commit Style**: Keep commit messages concise and focused on the changes made

---

## TEMPLATE - Copy this for new session files:

```markdown
# [Feature Name]

## How to use this session file
This session file contains comprehensive specifications generated through structured interviews. Each section serves dual purposes:
1. **Decision Record**: Captures user thoughts, preferences, and rationale for future reference
2. **Agent Instructions**: Provides clear, actionable guidance for coding agents to implement the feature

Coding agents should read the entire specification before beginning work, paying particular attention to the Requirements, Technical Design, and Test Strategy sections.

## Feature Brief
<!-- Generated from Requirements Interview: 2-3 sentence executive summary -->

## Requirements Specification
<!-- Generated from Requirements Interview -->

### Functional Requirements
<!-- What the system must do -->

### Non-Functional Requirements
<!-- Performance, security, usability, compatibility requirements -->

### User Stories
<!-- As a [user type], I want [goal] so that [benefit] -->

### Success Criteria
<!-- Measurable outcomes that indicate success -->

### Constraints
<!-- Technical, business, timeline limitations -->

## Technical Design Specification
<!-- Generated from Design Interview -->

### Architecture Overview
<!-- How this feature fits into existing system architecture -->

### Technical Decisions
<!-- Technology stack, frameworks, libraries chosen and why -->

### Data Model
<!-- Data structures, schemas, storage requirements -->

### API/Interface Design
<!-- External interfaces, endpoints, integration points -->

### Security Model
<!-- Security considerations and requirements -->

### Dependencies
<!-- System dependencies and integration points -->

## Test Strategy Specification
<!-- Generated from Testing Interview -->

### Test Plan Overview
<!-- Types of testing required and overall strategy -->

### Test Cases
<!-- Specific test scenarios linked to requirements -->

### Edge Cases & Error Handling
<!-- Boundary conditions and error scenarios -->

### Performance Criteria
<!-- Performance benchmarks and load requirements -->

### Automation Strategy
<!-- What will be automated vs manual testing -->

## Implementation Plan Specification
<!-- Generated from Implementation Interview -->

### Development Approach
<!-- Overall development strategy and methodology -->

### Task Breakdown
<!-- Specific development tasks and their descriptions -->

### Dependencies & Sequencing
<!-- Task dependencies and recommended order -->

### Risk Assessment
<!-- Identified risks and mitigation strategies -->

### Integration Plan
<!-- How feature will be integrated with existing systems -->

### Deployment Strategy
<!-- Release approach, rollout plan, feature flags -->

**Status Legend:** 📋 Not Started | ⏳ In Progress | ✅ Complete

## Session Log
<!-- Instructions: Add entry for each work session. Newest at top -->

### YYYY-MM-DD - Session Focus
- Achievement 1
- Achievement 2
- Current status: Description

## Useful References
<!-- Instructions: Add helpful URLs, docs, tutorials as you find them -->
- [Resource title](URL) - Brief description of why it's useful

## Decision Log
<!-- Record of key decisions made during specification and development -->
### YYYY-MM-DD - Decision Title
**Context**: What led to this decision
**Decision**: What was decided
**Rationale**: Why this decision was made
**Alternatives**: Other options considered
**Impact**: Expected effects of this decision
```

## Using Specifications During Development

**For coding agents working with these session files**:

1. **Start with Requirements**: Always read the Requirements Specification first to understand what success looks like
2. **Follow the Design**: Use the Technical Design Specification as your implementation guide
3. **Test-Driven Development**: Use the Test Strategy Specification to write tests before or alongside implementation
4. **Follow the Plan**: Use the Implementation Plan for task sequencing and dependency management
5. **Record Decisions**: Update the Decision Log when you make implementation choices not covered in the specification
6. **Update Status**: Keep Implementation Plan status updated as you progress
7. **Validate Against Success Criteria**: Regularly check your implementation against the success criteria defined in requirements

**Specification updates**: If you discover during implementation that specifications need refinement, discuss with the user and update the relevant sections to maintain accuracy.
