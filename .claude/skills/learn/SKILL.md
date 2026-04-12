---
name: learn
description: Extract behavioral feedback and personal preferences from the current conversation and save them as memories or rules
---

Review the current conversation for behavioral patterns to persist. Focus exclusively on:

## What to extract

1. **Corrections** — Places the user said "don't do X", "I prefer Y", "stop doing Z", or redirected your approach.
2. **Style preferences** — Communication style, verbosity, formatting, tone, emoji usage, etc.
3. **Workflow preferences** — How the user likes to work (e.g., prefer subagents, skip confirmations, commit style).
4. **Tool preferences** — Which tools to use or avoid, how to use them.

## What to skip

- Project-specific knowledge (belongs in `~/doc/projects/`)
- Things already captured in existing memories or rules
- One-off task context that won't apply to future sessions

## How to persist

For each finding, decide:

- **Feedback memory** (`~/.claude/projects/<project>/memory/feedback_*.md`) — If it's specific to this project or a soft preference.
- **Rule file** (`files/claude/rules/`) — If it's a strong, universal preference that should be version-controlled and apply everywhere.

Before writing, read existing memories (`MEMORY.md`) and rules to avoid duplicates. Update existing entries rather than creating new ones when possible.

Summarize what you saved to the user.
