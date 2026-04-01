# Code Review Process

Comprehensive review process based on Google Engineering Practices.

## Review Stages

### 1. Incremental Review (During Development)

Review each code change against detailed criteria.

#### Review Criteria

**Correctness**
- Code is syntactically correct
- Logic implements intended functionality
- No broken references or missing dependencies
- Proper handling of edge cases

**Resource Safety**
- No unbounded resource consumption
- Proper memory and resource management
- Efficient algorithms and data structures

**Error Handling**
- Graceful handling of errors and exceptions
- Proper fallbacks for failure scenarios
- Clear error messages and logging

**Type Safety**
- Correct types and interfaces
- Consistent data structures
- Proper validation of inputs/outputs

**Security**
- No hardcoded secrets or sensitive information
- Proper input validation and sanitization
- Secure defaults and configurations

**Performance**
- Efficient implementation
- Minimal unnecessary operations
- Appropriate use of caching and optimization

**Maintainability**
- Clear, descriptive naming
- Logical code organization
- Consistent style and formatting
- Appropriate comments for complex logic

### 2. PR Review (Before Pushing)

Examine the entire change's design, complexity, and strategy.

#### Design Review
- **Plan Conformance:** Implementation matches approved design
- **Architectural Consistency:** Fits with existing codebase structure
- **Design Patterns:** Appropriate use of established patterns
- **Module Organization:** Logical grouping and dependencies

#### Complexity Assessment
- **Structural Simplicity:** Avoid over-engineering
- **Dependencies:** Minimize and justify external dependencies
- **Backward Compatibility:** Consider impact on existing functionality

#### Test Coverage
- **Testing Strategy:** Follows project testing requirements (see `.kdevkit/project.md`)
- **Build Validation:** All build processes pass
- **Integration Testing:** Works with existing system

---

## Severity Levels

**High (Critical)**
- Syntax errors or build failures
- Security vulnerabilities or exposed secrets
- Breaking changes to existing functionality
- Issues that would cause bugs, data loss, or resource exhaustion

**Medium (Maintainability)**
- Code organization and readability issues
- Performance concerns or inefficient patterns
- Missing or inadequate documentation
- Minor security improvements

**Low (Style/Polish)**
- Formatting and style inconsistencies
- Optional optimizations
- Minor clarity improvements
- Non-critical documentation updates

---

## Review Output Format

```
## Incremental Review Findings

### High Severity
- **File:** `path/to/file:line`
- **Issue:** Description of critical issue
- **Code:** `problematic code snippet`
- **Suggestion:** Specific fix recommendation

### Medium Severity
[Similar format for medium issues]

### Low Severity
[Similar format for low issues]

## Scoring
- Total Issues: X
- High: X, Medium: X, Low: X
- Quality Score: X/100
- **Result: PASS/FAIL**
```

## Quality Threshold

Quality thresholds and project-specific review criteria are defined in `.kdevkit/project.md`.