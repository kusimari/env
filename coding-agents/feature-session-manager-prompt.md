# Feature Session Manager - AI Instructions

I am usually invoked when the user says "Load the text in feature-session-manager-prompt.md, and follow it to create a new session file for feature '[feature-name]' in location 'location'", follow these instructions:

## 1. Create New Session File
- Use the template below (inline in this file)
- Create a new session file at the directory 'location' with the file name 'feature-name'.md
- Add instructions to the feature file on how it should be used by a coding agent below '##How to use this session file' in the template. This will help the session file be self contained.
- Replace `[Feature Name]` with the actual feature name
- Ask the user for the feature brief, requirements, and interact to develop an initial implementation plan

## 2. Session Management Throughout Development

**When starting each session:**
- Load the existing session file
- Review current status and acknowledge where we left off
- Update implementation plan status as work progresses (📋 → ⏳ → ✅)

**When ending each session:**
- Add new Session Log entry with today's date
- Document key achievements and current status
- Update requirements checklist and implementation plan

**Throughout development:**
- Add useful URLs/references as they're discovered
- Update Feature Brief if understanding evolves
- Capture both successes and blockers

## 3. File Management
- Session files live in `/Users/gorantls/env/coding-agents-aided-features/[feature-name]/`
- Use the template provided below in this file
- Keep Feature Brief concise (2-3 sentences)
- Make requirements specific and testable
- Break implementation into logical, manageable steps

## 4. Status Tracking
**Status Legend:** 📋 Not Started | ⏳ In Progress | ✅ Complete

Update these throughout development to maintain clear progress visibility.

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
