# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal dotfiles for a **GNU Guix System** + Hyprland (Wayland) desktop environment. System config at `systems/redfish.scm`, home environment at `home/redfish.scm`. Dotfiles are deployed via `home-files-service-type` and `home-xdg-configuration-files-service-type` in `redfish.scm` — source files in the repo are referenced by `local-file` and symlinked into place by `guix home reconfigure`.

## Key Commands

```bash
# Update Guix channels (includes NonGuix)
guix pull -C channels.scm

# Reconfigure system
sudo guix system reconfigure systems/redfish.scm

# Reconfigure home environment
guix home reconfigure home/redfish.scm -L .
```

There is no test suite or linter — configs are validated at runtime.

## Directory Structure

```
dotfiles/
├── home/redfish.scm                              (home environment)
├── systems/                                       (system configs)
│   ├── redfish.scm                                (system config: NVIDIA, greetd, filesystems)
│   └── base-system.scm                            (shared OS base)
├── packages/                                      (custom Guix package definitions)
├── channels.scm                                   (Guix + NonGuix channels)
├── files/                                         (program-specific config files)
│   ├── alacritty/     → ~/.config/alacritty/      (terminal)
│   ├── caelestia/     → ~/.config/caelestia/      (desktop shell config)
│   ├── cava/          → ~/.config/cava/           (audio visualizer)
│   ├── direnv/        → ~/.config/direnv/
│   ├── emacs/         → ~/.emacs.d/ (config parts)(Emacs config)
│   │   ├── Emacs.org                              (literate config, tangles to init.el)
│   │   ├── early-init.el
│   │   ├── init.el                                (generated from Emacs.org)
│   │   └── lisp/                                  (custom elisp packages)
│   ├── eww/           → ~/.config/eww/            (widget bar)
│   ├── greetd/                                    (login manager, referenced by system config)
│   ├── gtk/           → ~/.gtkrc-2.0
│   ├── hypr/          → ~/.config/hypr/           (Hyprland WM)
│   ├── misc/          → ~/.config/ (single files) (electron-flags, mimeapps, spotify-launcher)
│   ├── mise/          → ~/.config/mise/
│   ├── music/                                     (sound files for notifications)
│   ├── nyxt/          → ~/.config/nyxt/
│   ├── paru/          → ~/.config/paru/
│   ├── sptlrx/       → ~/.config/sptlrx/
│   ├── swaync/        → ~/.config/swaync/
│   ├── systemd/       → ~/.config/systemd/
│   ├── thunar/        → ~/.config/Thunar/
│   ├── xdg-desktop-portal/ → ~/.config/xdg-desktop-portal/
│   └── zsh/           → ~/.config/zsh/ + zsh dotfiles
├── pictures/                                      (wallpapers, referenced by hyprland)
├── scripts/           → ~/.local/bin/             (user scripts)
├── share/             → ~/.local/share/           (desktop files, fonts)
├── icons/                                         (SVG icons for Org/eww)
├── CLAUDE.md
└── README.org
```

## Architecture

**Literate programming**: `files/emacs/Emacs.org` tangles to `~/.emacs.d/init.el`.

**Guix configs** (at repo root):
- `systems/redfish.scm` — system config (NVIDIA, greetd, filesystems)
- `systems/base-system.scm` — shared OS base
- `home/redfish.scm` — home environment (packages, dotfiles, services, shell)
- `channels.scm` — Guix + NonGuix channels
- `packages/` — custom package definitions

**Caelestia desktop shell**: Quickshell-based shell replacing eww. Upstream at https://github.com/caelestia-dots/caelestia.
- `files/caelestia/` — shell config (QML modules, `shell.json`)
- `files/caelestia/shell.json` — main config: appearance, bar, launcher, wallpaper paths, services
- `packages/caelestia-shell.scm` — QML C++ plugin package
- `packages/caelestia-cli.scm` — CLI tool (`caelestia` command)
- `packages/quickshell-git.scm` — Quickshell compositor package
- Shell and resizer run as **Shepherd user services** (not exec-once in hyprland.conf) — see `home/services/shepherd.scm`
- Layer rules in `files/hypr/hyprland.conf` must match upstream (only blur `caelestia-drawers`, not all layers)

**Wallpapers** are at `pictures/wallpaper/` and referenced in `files/caelestia/shell.json` `paths.wallpaperDir`.

**Custom scripts**: `scripts/` deployed to `~/.local/bin/`.

**Shepherd user services** (`home/services/shepherd.scm`): emacs daemon, wayland-compositor sentinel, hyprpolkitagent, caelestia-shell, caelestia-resizer, podman-socket, ssh-agent. Services requiring Hyprland depend on the `wayland-compositor` sentinel service.

See `~/doc/projects/dotfiles/index.org` for detailed project documentation.

## Conventions

- Hyprland is launched by greetd (system config); no separate launcher script
- NVIDIA-specific env vars are set in `home/redfish.scm` environment variables
- `files/hypr/monitors.conf` is gitignored (machine-specific)
- Spicetify theme: Onepunch/light
- No stow — guix home manages all symlinks via `local-file` references
- Desktop services (shell, resizer, polkit) are Shepherd services, not hyprland exec-once
