# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal dotfiles for an Arch Linux + Hyprland (Wayland) desktop environment. Managed with **GNU Stow** — running `stow .` from the repo root creates symlinks into `$HOME`. The `.stow-local-ignore` file excludes non-config items (org files, install scripts, images, etc.).

## Key Commands

```bash
# Apply dotfiles (symlink everything)
stow .

# Full bootstrap (interactive, prompts for each section)
./install.sh

# Install packages from list
paru --needed -S - < pkglist.txt

# Build Emacs (custom PKGBUILD)
cd ~/.dotfiles/pkg/my-emacs && makepkg -si
```

There is no test suite or linter — configs are validated at runtime.

## Architecture

**Literate programming**: `README.org` tangles to `install.sh` and `pkglist.txt`. `Emacs.org` (5000+ lines) tangles to `.emacs.d/init.el`. Edit the `.org` files as the source of truth for those outputs.

**Major config areas** (all under `.config/` unless noted):
- `hypr/` — Hyprland WM (split across `hyprland.conf`, `env.conf`, `monitors.conf`, `workspaces.conf`, etc.)
- `eww/` — Elkowar's Wacky Widgets bar system (Yuck markup + SCSS + shell/Go scripts)
- `alacritty/` — Terminal emulator (TOML)
- `swaync/` — Notification daemon
- `.zsh/`, `.zshrc`, `.zshenv` — Zsh shell config with Pure prompt (git submodule), aliases, functions
- `.emacs.d/` — Generated from `Emacs.org`; custom elisp in `.emacs.d/lisp/`
- `config/` — GNU Guix system/home definitions (Scheme)

**Git submodules**: Pure prompt at `.zsh/pure/`. Run `git submodule update --init --recursive` after cloning.

**Custom scripts**: `.local/bin/` contains user scripts (screenshot, spotify toggle, org2pdf, etc.).

See `~/doc/projects/dotfiles/index.org` for detailed project documentation.

## Conventions

- `.stow-local-ignore` uses **Perl regex** — use `^/` to anchor patterns to repo root (e.g. `^/config` to avoid matching `.config`)
- When adding top-level directories not meant to be stowed, update `.stow-local-ignore`
- NVIDIA-specific env vars are set in `hypr/env.conf` and `.zshenv`
- Eww widget bar uses Go (`hyprshell`) for Hyprland IPC integration
- Firefox CSS lives in `.config/chrome/` and gets symlinked into the Firefox profile by `install.sh`
- Spicetify theme: Onepunch/light
