---
name: capture-plan
description: Save a Claude Code plan to ~/doc/projects/<name>/ as org-mode and add a PROJECT entry with TODOs to ~/doc/projects.org. Use when the user asks to "capture this plan", "save the plan to projects", "store the plan", "make a project entry", "add to org agenda", "/capture-plan", or otherwise wants a plan persisted into their org agenda so it isn't forgotten.
argument-hint: "<project-name> [plan-file-path]"
---

# capture-plan

Persist a plan into the user's org-mode project system so it shows up in the
agenda. Two outputs:

1. Notes file: `~/doc/projects/<project-name>/<slug>.org` (full plan content
   converted to org-mode).
2. Agenda entry: `** PROJECT <title>` appended under `* Work :@work:` in
   `~/doc/projects.org`, linking to the notes file with TODO sub-items
   decomposing the actionable phases.

## When to use

The user has just finished a planning session (with or without `/plan`) and
wants the plan persisted somewhere durable, not just left in the ephemeral
`~/.claude/plans/` file. Trigger phrases include "save this somewhere",
"add to org agenda", "make a work project for this", "I'll forget if it's
not in projects.org".

## Inputs

- `$1` (required): project name — the directory under `~/doc/projects/`.
  Use existing names (e.g., `gateway`, `dialectra`, `dotfiles`) when they
  match the codebase. Create a new dir if the project is new.
- `$2` (optional): explicit path to the source plan file. Defaults to the
  most recent plan in `~/.claude/plans/` if a `/plan` session just finished,
  or the contents of the current conversation if no plan file exists.

## Workflow

### Step 1 — Pick source material

If the user just finished `/plan`, the plan file path is in the
ExitPlanMode result message. Otherwise locate the most recent file:

```bash
ls -t ~/.claude/plans/*.md 2>/dev/null | head -1
```

If no plan file exists, synthesize one from the current conversation
content — the user knows what they planned and the context contains it.

### Step 2 — Choose a slug + title

- **Slug**: short kebab-case name for the filename, e.g., `core-refactor`,
  `aws-s3-integration`, `auth-rewrite`. Derive from the plan's main topic.
- **Title**: human-readable phrase for the agenda PROJECT heading, e.g.,
  "Refactor core/ → hexagonal services". Keep it under 60 chars.

Confirm both with the user if ambiguous.

### Step 3 — Write the org notes file

Create `~/doc/projects/<project-name>/` if it doesn't exist, then write
`~/doc/projects/<project-name>/<slug>.org`.

Convert the plan from markdown to org-mode:

| Markdown        | Org-mode               |
|-----------------+------------------------|
| `# Heading`     | `* Heading`            |
| `## Heading`    | `** Heading`           |
| `### Heading`   | `*** Heading`          |
| `` `code` ``    | `~code~`               |
| `**bold**`      | `*bold*`               |
| `*italic*`      | `/italic/`             |
| ` ```lang ` ... | `#+begin_src lang` ... `#+end_src` |
| `[txt](url)`    | `[[url][txt]]`         |
| `- bullet`      | `- bullet` (unchanged) |
| `\| col \|`     | `\| col \|` (unchanged, will auto-align) |

Preamble at top of the org file:

```org
#+title: <Plan title>
#+date: [YYYY-MM-DD Day]
```

Per the user's `~/.claude/CLAUDE.md`:
- **Never add `:ID:` properties** — Org-roam manages those in Emacs.
- Use plain `[[file:...][label]]` links, not `[[id:...]]`.
- No emojis.

### Step 4 — Insert the agenda entry

Read `~/doc/projects.org`. Find the `* Work :@work:` section (or
`* Personal :personal:` if the user indicates this is personal — ask if
unclear). Locate an existing `** PROJECT ...` heading inside it.

Insert the new entry immediately *before* that existing PROJECT heading
(top of the projects sub-list — most recent first). Format:

```org
** PROJECT <Title>
   notes: [[file:~/doc/projects/<project-name>/<slug>.org][<slug>.org]]
*** TODO <Phase 1 short description>
*** TODO <Phase 2.1 short description>
*** TODO <Phase 2.2 short description>
... (one TODO per actionable step from the plan)
```

Decomposition rules for TODOs:

- One TODO per phase, sub-phase, or migration step the plan defines.
- Inline a one-sentence hint on tricky items (use indented body text below
  the heading, not in the heading itself).
- Skip "Context", "Risks", "Verification" sections — those are reference
  material in the notes file, not work items.
- Aim for 5–15 TODOs total. If the plan has more atomic steps, group them.

### Step 5 — Confirm

Report back:
- Path to the saved org file.
- Heading text and number of TODOs added to `projects.org`.
- The link the user can click in Emacs to open the notes.

## Conventions reference

User's project structure:

- `~/doc/projects.org` — agenda master, has `* Work :@work:` and `* Personal :personal:` top-level sections. Each project is a `** PROJECT` heading.
- `~/doc/projects/<name>/` — detailed notes per project. May contain `index.org`, topic-specific files, or both.
- The `~/doc/projects/<name>/index.org` file (if present) is a 200-line-max overview per the user's documentation rules. Don't append plan content to `index.org` — use a dedicated file.

Existing project dirs (as of writing): `dialectra`, `dotfiles`, `zmk-charybdis`, `gateway`, `timecard`. Match casing and naming when extending.

## Example invocation

User: "save this plan as a project under gateway"

Result:
- `~/doc/projects/gateway/<slug>.org` created with full plan content.
- `~/doc/projects.org` gains `** PROJECT <Title>` under `* Work :@work:` with linked TODOs.
