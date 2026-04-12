---
paths:
  - "files/emacs/**"
  - "*.el"
---

# Emacs Rules

- `Emacs.org` is the literate source — tangles to `init.el`. Edit Org, not init.el.
- **After editing `Emacs.org`, always tangle it:** `emacsclient -e '(org-babel-tangle-file "~/src/dotfiles/files/emacs/Emacs.org")'`
- If the daemon isn't running, fall back to: `emacs --batch -l org --eval '(org-babel-tangle-file "...")'`
- Custom elisp: `files/emacs/lisp/`
- Runs as Shepherd user service (daemon mode)

## Querying the live Emacs daemon

Use `emacsclient -e` to inspect state, look up docs, and debug — Emacs is self-documenting:

```bash
# Help / discovery
emacsclient -e '(describe-function '\''FUNC)'
emacsclient -e '(describe-variable '\''VAR)'
emacsclient -e '(apropos-internal "PATTERN")'

# Find where a function is defined
emacsclient -e '(find-lisp-object-file-name '\''FUNC (symbol-function '\''FUNC))'

# Check variable values, buffer state, etc.
emacsclient -e 'user-emacs-directory'
emacsclient -e '(with-current-buffer "BUFFER" major-mode)'

# Reload / eval
emacsclient -e '(load-file "~/.config/emacs/init.el")'
```

Use these proactively when debugging Emacs issues or exploring unfamiliar packages.
