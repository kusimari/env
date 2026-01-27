# Feature Session Manager - AI Instructions

I am usually invoked when the user says "Load the text in feature-session-creation-prompt.md, and follow it to create a new session file for feature '[feature-name]' from commit-ish 'commit-ish'", follow these instructions:

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
- Use the template below (inline in this file)
- Create a new session file in the new worktree at 'coding-agents-aided-features/[feature-name].md'
- Add instructions to the feature file on how it should be used by a coding agent below '##How to use this session file' in the template. This will help the session file be self contained.
- Replace `[Feature Name]` with the actual feature name
- Ask the user for the feature brief, requirements, and interact to develop an initial implementation plan

## Important Notes
- **No Co-Author**: Do NOT include "Co-Authored-By: Claude" or any co-author lines in commit messages
- **Commit Style**: Keep commit messages concise and focused on the changes made

---

## TEMPLATE - Copy this for new session files:

```markdown
# [Feature Name]

## How to use this session file

## Feature Brief
<!-- Instructions: 2-3 sentence description of what this feature does and why -->

## Requirements
<!-- Instructions: Checkbox list of what needs to be accomplished -->
- [ ] Requirement 1
- [ ] Requirement 2
- [ ] Requirement 3

## Implementation Plan
<!-- Instructions: Ordered steps to complete the feature. Update status as you progress -->
1. ⏳ **Step 1** - Description
2. 📋 **Step 2** - Description
3. 📋 **Step 3** - Description
4. 📋 **Step 4** - Description

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
```
