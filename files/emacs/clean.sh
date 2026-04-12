#!/usr/bin/env bash
# Clean Emacs config for a fresh elpaca rebuild.
# Also removes the install_lock so first-run tasks re-execute.

set -euo pipefail

EMACS_DIR="${HOME}/.config/emacs"
CACHE_DIR="${HOME}/.cache"

rm -rf \
  "${EMACS_DIR}/elpaca" \
  "${EMACS_DIR}/eln-cache" \
  "${EMACS_DIR}/tramp" \
  "${EMACS_DIR}/auto-save-list" \
  "${EMACS_DIR}/tree-sitter" \
  "${EMACS_DIR}/persist" \
  "${EMACS_DIR}/org-roam.db" \
  "${EMACS_DIR}/install_lock" \
  "${CACHE_DIR}/org-persist"

echo "Emacs config cleaned. Restart Emacs to rebuild."
