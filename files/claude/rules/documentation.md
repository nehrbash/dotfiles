# Documentation Rules

## Before Exploring Code

Check `~/doc/projects/<name>/index.org` first. `~/doc` is an additional working directory.

## After Exploring Code (5+ file reads)

Write findings to `~/doc/projects/<name>/index.org` (create if needed). Document:
- Key file paths and purposes
- Architecture decisions and non-obvious behavior
- Useful commands, config formats, dependency relationships

Do NOT document: session-specific context, info already in CLAUDE.md, speculative conclusions.

## Format

- Org-mode markup (`*` headings, `[[target][label]]` links)
- No `:ID:` properties (managed by Emacs/Org-roam)
- No Markdown files in `~/doc/`
