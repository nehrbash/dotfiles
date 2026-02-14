---
name: new-skill
description: Interactively create a new Claude Code skill
disable-model-invocation: true
argument-hint: "[skill-name]"
---

Walk the user through creating a new Claude Code skill step by step.

## Step 1 — Name and scope

If $ARGUMENTS is provided, use it as the skill name. Otherwise ask the user for a name (lowercase, hyphens allowed).

Ask whether this should be a **personal** skill (`~/.claude/skills/<name>/`) or a **project** skill (`.claude/skills/<name>/`).

## Step 2 — Gather requirements

Ask the user these questions (use AskUserQuestion):

1. **What should the skill do?** — get a plain-language description of the desired behavior.
2. **Who triggers it?**
   - User only (`disable-model-invocation: true`) — good for actions with side effects
   - Claude only (`user-invocable: false`) — background knowledge Claude applies automatically
   - Both (default) — Claude can invoke it when the description matches
3. **Does it need arguments?** — if yes, ask for an `argument-hint` (e.g. `[filename]`, `[issue-number]`).
4. **Should it run in isolation?** — if the skill does heavy read-only analysis, suggest `context: fork` with an appropriate `agent` type.
5. **Should tool access be restricted?** — if the skill should be read-only or limited, ask which tools to allow.

## Step 3 — Write the SKILL.md

Using the answers, generate the `SKILL.md` file with:

- Correct YAML frontmatter (`name`, `description`, and any optional fields gathered above).
- Clear, concise markdown instructions (under 500 lines).
- Use `$ARGUMENTS` / `$0` / `$1` where the user wants input substitution.

Create the directory and write the file.

## Step 4 — Trigger analysis

Before finalizing, carefully review the skill for triggering issues. Consider and briefly report to the user:

- **False positives** — Is the `description` broad enough that Claude might invoke the skill when the user didn't intend it? If so, tighten the wording or add `disable-model-invocation: true`.
- **False negatives** — Would a user who wants this skill use phrasing that doesn't match the description? If so, broaden the description or add synonyms.
- **Collisions** — Could the description overlap with other common skills (e.g. a "review" skill clashing with PR review)? If so, make it more specific.

Suggest concrete revisions if any issues are found. Apply them with user approval.

## Step 5 — Token efficiency review

Review the skill's instructions and identify steps where Claude would spend many tokens doing work that a shell script or CLI command could handle instead. Common opportunities:

- Listing/searching files → `find`, `grep`, `glob` in a script
- Parsing structured data (JSON, YAML) → `jq`, `yq`
- Gathering git context → a small shell snippet in the instructions
- Repetitive formatting → a template file or `sed`/`awk`
- Heavy analysis → `claude -p` as a subprocess with a focused prompt

Present any suggestions to the user. If they agree, update the skill instructions to delegate that work to commands.

## Step 6 — Confirm

Show the user the final file contents and the slash command they can use to invoke it. Mention they can test it immediately with `/<name>`.
