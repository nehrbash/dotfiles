---
name: elisp-style
description: Apply modern Emacs Lisp coding conventions when writing or editing elisp code, Emacs configuration, or .el files
user-invocable: false
---

When writing or editing Emacs Lisp code, read the style reference at `~/doc/Roam/20260214130119-modern-elisp-emacs-31.org` and follow its conventions. Key rules:

## Mandatory

- Always include `;;; -*- lexical-binding: t; -*-` in file headers
- Use `(require 'cl-lib)` never `(require 'cl)`
- Prefix all top-level symbols with the package name; use `--` for private symbols
- Use `#'function-name` (sharp-quote) when passing functions
- Use `when`/`unless` instead of single-branch `if`
- Use `setopt` for user options (Emacs 29+), `setq` for internal variables
- End package files with `(provide 'pkg)` and `;;; pkg.el ends here`

## Prefer

- `seq-` functions for generic sequence operations
- `pcase` for structural matching and destructuring
- `cl-defstruct` for data structures, `cl-defgeneric`/`cl-defmethod` for polymorphism
- `thread-first`/`thread-last` when they improve readability
- `(1+ x)` and `(1- x)` over `(+ x 1)` and `(- x 1)`
- `(< a x b)` over `(and (< a x) (< x b))`
- Named functions for hooks/keybindings (not inline lambdas)

## Macros

- Always add `(declare (indent N) (debug SPEC))` to macro definitions
- Prefer backquote templates over manual list construction

## Documentation

- Docstrings: imperative first line ("Return..." not "Returns..."), args in CAPS
- Comments: `;;;` sections, `;;` explanations, `;` margin notes

## Do NOT

- Create `:ID:` properties in org files
- Hard-code keybindings in `C-c LETTER` (reserved for users)
- Use deprecated APIs (`eval-after-load`, `cl.el`, `flet`)
- End face names with `-face`
