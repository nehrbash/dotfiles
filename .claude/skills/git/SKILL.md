---
name: git
description: Stage and commit changes with a descriptive message
disable-model-invocation: true
argument-hint: "[optional commit message hint]"
---

Stage and commit the current changes:

1. Run `git status` and `git diff` (staged + unstaged) in parallel to understand what changed.
2. Run `git log --oneline -5` to match the repo's commit message style.
3. Stage relevant files by name (avoid `git add -A`; never stage secrets or large binaries).
4. Write a concise commit message that focuses on **why**, not what. If $ARGUMENTS is provided, use it as guidance for the message.
5. Commit the changes. Do NOT push.
