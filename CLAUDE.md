# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal dotfiles for a **GNU Guix System** + Hyprland (Wayland) desktop environment. System config at `config/systems/redfish.scm`, home environment at `config/home/redfish.scm`. Dotfiles are deployed via `home-files-service-type` and `home-xdg-configuration-files-service-type` in `redfish.scm` — source files in the repo are referenced by `local-file` and symlinked into place by `guix home reconfigure`.

## Key Commands

```bash
# Update Guix channels (includes NonGuix)
guix pull -C config/channels.scm

# Reconfigure system
sudo guix system reconfigure config/systems/redfish.scm

# Reconfigure home environment
guix home reconfigure config/home/redfish.scm -L .
```

There is no test suite or linter — configs are validated at runtime.

## Directory Structure

```
dotfiles/
├── alacritty/       → ~/.config/alacritty/      (terminal)
├── cava/            → ~/.config/cava/            (audio visualizer)
├── config/                                       (guix system/home configs)
│   ├── home/redfish.scm                          (home environment)
│   ├── systems/redfish.scm                       (system config)
│   └── packages/                                 (custom package defs)
├── direnv/          → ~/.config/direnv/
├── emacs/           → ~/.emacs.d/ (config parts) (Emacs config)
│   ├── Emacs.org                                 (literate config, tangles to init.el)
│   ├── early-init.el
│   ├── init.el                                   (generated from Emacs.org)
│   └── lisp/                                     (custom elisp packages)
├── eww/             → ~/.config/eww/             (widget bar)
├── greetd/                                       (login manager, referenced by system config)
├── gtk/             → ~/.gtkrc-2.0
├── hypr/            → ~/.config/hypr/            (Hyprland WM)
├── icons/                                        (SVG icons for Org/eww)
├── misc/            → ~/.config/ (single files)  (electron-flags, mimeapps, spotify-launcher)
├── mise/            → ~/.config/mise/
├── music/                                        (sound files for notifications)
├── nwg-*/           → ~/.config/nwg-*/           (nwg desktop tools)
├── nyxt/            → ~/.config/nyxt/
├── paru/            → ~/.config/paru/
├── pictures/                                     (wallpapers, referenced directly by hyprland)
├── scripts/         → ~/.local/bin/              (user scripts)
├── share/           → ~/.local/share/            (desktop files, fonts)
├── sptlrx/          → ~/.config/sptlrx/
├── swaync/          → ~/.config/swaync/
├── systemd/         → ~/.config/systemd/
├── thunar/          → ~/.config/Thunar/
└── zsh/             → ~/.config/zsh/ + zsh dotfiles
    ├── aliasrc, emacs_functions, functions
    └── zshrc, zshenv, zprofile
```

## Architecture

**Literate programming**: `emacs/Emacs.org` tangles to `~/.emacs.d/init.el`.

**Guix configs** (`config/`):
- `systems/redfish.scm` — system config (NVIDIA, greetd, filesystems)
- `systems/base-system.scm` — shared OS base
- `home/redfish.scm` — home environment (packages, dotfiles, services, shell)
- `channels.scm` — Guix + NonGuix channels

**Wallpapers** are at `pictures/wallpaper/` and referenced directly as `~/dotfiles/pictures/wallpaper/` in hyprland configs (no symlink needed).

**Custom scripts**: `scripts/` deployed to `~/.local/bin/`.

See `~/doc/projects/dotfiles/index.org` for detailed project documentation.

## Conventions

- Hyprland is launched by greetd (system config); no separate launcher script
- NVIDIA-specific env vars are set in `hypr/env.conf`
- `hypr/monitors.conf` is gitignored (machine-specific)
- Eww widget bar uses Go (`hyprshell`) for Hyprland IPC integration
- Spicetify theme: Onepunch/light
- No stow — guix home manages all symlinks via `local-file` references
