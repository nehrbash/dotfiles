---
name: review-task
description: Review the current conversation to identify mistakes, missed questions, and opportunities to improve skills, CLAUDE.md files, and project documentation in ~/doc/projects/.
---

Review the current conversation context and perform the following steps in order.

## 1. Conversation audit

Re-read the full conversation. Identify:

- **Mistakes made** — incorrect assumptions, wrong file edits, commands that failed unnecessarily, misunderstandings of user intent.
- **Questions that should have been asked** — places where ambiguity led to rework or wrong direction. Note what question would have prevented the issue.
- **Patterns worth preserving** — successful approaches, user preferences revealed, or conventions discovered.

Summarize findings concisely to the user before proceeding.

## 2. Update skills

Check existing skills in `~/.claude/skills/` for any that should be updated based on lessons learned. Only modify a skill if the conversation clearly revealed a gap or incorrect instruction in it.

## 3. Update project documentation in ~/doc/projects/

All persistent project knowledge lives in `~/doc/projects/`. Each project gets a subdirectory (e.g. `~/doc/projects/dotfiles/`). Use **Org-mode format**.

- **Check first**: Read existing files in the project's `~/doc/projects/<name>/` directory before writing. Do not duplicate content that already exists.
- **Conservative**: Only document insights that are stable and reusable — not session-specific details.
- **Format**: Use Org-mode markup (`*` headings, `[[target][label]]` links).
- **Do NOT create `:ID:` properties** — those are managed by Emacs/Org-roam only.
- **Index file**: Each project directory should have an `index.org` that serves as the entry point and links to other files in that directory.
- **New files**: Only create if the topic is substantial enough to warrant a separate file. Otherwise append to existing files or `index.org`.

## 4. Update CLAUDE.md files

Update the **project CLAUDE.md** (in the project root) if the conversation revealed missing conventions, incorrect instructions, or new workflows. The project CLAUDE.md must include an explicit reference to the project's documentation index:

```
See ~/doc/projects/<name>/index.org for detailed project documentation.
```

Update `~/doc/CLAUDE.md` only if the conversation revealed something about the overall ~/doc knowledge base conventions.

Be conservative — only add information that would prevent future mistakes.

