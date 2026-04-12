# Documentation Rules

## Before Exploring Code

Check `~/doc/projects/<name>/index.org` first. `~/doc` is an additional working directory.

## After Exploring Code (5+ file reads)

Write findings to `~/doc/projects/<name>/index.org` (create if needed). Document:
- Key file paths and purposes
- Architecture decisions and non-obvious behavior
- Useful commands, config formats, dependency relationships

Do NOT document: session-specific context, info already in CLAUDE.md, speculative conclusions.

## Size Constraint

**Index files MUST stay under 200 lines.** These are quick-reference guides, not exhaustive docs.
- Be terse: one-line bullet points, no prose paragraphs
- Only record what you can't derive by reading code or git history
- If an index exceeds 150 lines, split detail into sub-files (e.g. `~/doc/projects/<name>/architecture.org`) and link from the index
- Prefer tables over lists when summarizing many items (paths, commands, mappings)
- Prune stale entries when updating — replace, don't append

## Format

- Org-mode markup (`*` headings, `[[target][label]]` links)
- No `:ID:` properties (managed by Emacs/Org-roam)
- No Markdown files in `~/doc/`
