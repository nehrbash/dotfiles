---
name: review-task
description: Review the current conversation to identify mistakes, missed questions, and opportunities to improve skills, CLAUDE.md files, and project documentation in ~/doc/projects/.
---

Review the current conversation context and perform the following steps in order.

## 1. Conversation audit

Re-read the full conversation. Identify:

- **Mistakes made** — incorrect assumptions, wrong file edits, commands that failed unnecessarily, misunderstandings of user intent.
- **Questions that should have been asked** — places where ambiguity led to rework or wrong direction. Note what question would have prevented the issue.

Summarize findings concisely to the user before proceeding.

## 2. Update skills and rules

Check existing skills in `.claude/skills/` and rules in `files/claude/rules/` for any that should be updated based on lessons learned. Only modify if the conversation clearly revealed a gap or incorrect instruction.

## 3. Update project documentation in ~/doc/projects/

All persistent project knowledge lives in `~/doc/projects/`. Each project gets a subdirectory (e.g. `~/doc/projects/dotfiles/`). Use **Org-mode format**.

- **Check first**: Read existing files before writing. Do not duplicate.
- **Conservative**: Only document stable, reusable insights.
- **Format**: Org-mode (`*` headings, `[[target][label]]` links). No `:ID:` properties.
- **Index under 200 lines**: Split detail into sub-files and link from index if needed.

## 4. Update CLAUDE.md files

Update **project CLAUDE.md** if the conversation revealed missing conventions or workflows. Be conservative — only add information that would prevent future mistakes.

Note: Behavioral feedback and user preferences are handled automatically by the Stop hook and `/learn` skill — do not duplicate that work here.

