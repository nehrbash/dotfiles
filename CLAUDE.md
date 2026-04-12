# CLAUDE.md

Personal dotfiles for **GNU Guix System** + Hyprland (Wayland). No test suite or linter — configs validated at runtime.

## Key Commands

```bash
guix pull -C channels.scm                         # update channels (includes NonGuix)
sudo guix system reconfigure systems/redfish.scm   # system
guix home reconfigure home/redfish.scm -L .        # home environment
```

## Structure

- `systems/oceania.scm` — shared OS base (users, services, packages)
- `systems/redfish.scm` — machine config (NVIDIA, bootloader, filesystems); inherits oceania
- `home/redfish.scm` — home environment (packages, dotfiles, services, shell)
- `packages/` — custom Guix package definitions
- `channels.scm` — Guix + NonGuix channels
- `files/` — program configs, deployed via `local-file` symlinks (no stow)
- `scripts/` → `~/.local/bin/`
- `pictures/wallpaper/` — referenced in `files/caelestia/shell.json`

## Key Architecture

- Hyprland launched by greetd; NVIDIA env vars in `home/redfish.scm`
- `files/hypr/monitors.conf` is gitignored (machine-specific)
- Shepherd user services (`home/services/shepherd.scm`): emacs daemon, wayland-compositor sentinel, caelestia-shell, hyprpolkitagent, podman-socket, ssh-agent
- Services needing Hyprland depend on `wayland-compositor` sentinel
- Caelestia (Quickshell-based shell) replaces eww; runs as Shepherd service, not exec-once
- `files/emacs/Emacs.org` tangles to `init.el`; custom elisp in `files/emacs/lisp/`

## Workflow Rules

- After committing changes to home-affecting files (`home/`, `files/`, `packages/`, `scripts/`), always run `make update-home` without asking.

See `~/doc/projects/dotfiles/index.org` for deeper project knowledge.
