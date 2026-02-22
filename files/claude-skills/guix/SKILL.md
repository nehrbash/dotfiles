---
name: guix
description: Apply Guix workflow knowledge when helping with guix home reconfigure, guix system reconfigure, guix pull, channel updates, package definitions, or Guix config file changes in this dotfiles project
user-invocable: false
---

# Guix Workflow Knowledge

Apply this knowledge whenever working with Guix configs, packages, or commands in this dotfiles repo.

## Key Commands

```bash
# Update channels (always use repo's channels.scm)
guix pull -C $DOTFILES_DIR/channels.scm

# Reconfigure home — needs -L to find custom packages
guix home reconfigure $DOTFILES_DIR/home/redfish.scm -L $DOTFILES_DIR

# Reconfigure system (requires sudo)
sudo -E guix system reconfigure $DOTFILES_DIR/systems/redfish.scm

# Build without installing (dry run / check for errors)
guix home build $DOTFILES_DIR/home/redfish.scm -L $DOTFILES_DIR
```

**`DOTFILES_DIR`** is set by guix home to the repo root (auto-detected from `redfish.scm` location). **`-L $DOTFILES_DIR`** is needed so Guile finds custom modules in `packages/`.

## Run reconfigure autonomously

After editing any file in `config/home/redfish.scm` or `config/packages/`, **run `guix home reconfigure` immediately** — do not ask the user to run it. It applies the changes and validates the config. If it fails, diagnose and fix the error.

## Config Locations

| File | Purpose |
|------|---------|
| `home/redfish.scm` | Home environment (packages, dotfiles, services, shell) |
| `systems/redfish.scm` | System config (NVIDIA, greetd, filesystems) |
| `systems/base-system.scm` | Shared OS base |
| `channels.scm` | Guix + NonGuix channels |
| `packages/` | Custom package definitions (loaded via `GUIX_PACKAGE_PATH`) |

## guix home actions

```
reconfigure        switch to a new home environment configuration
roll-back          switch to the previous home environment configuration
build              build without installing (validate config)
describe           describe the current home environment
list-generations   list all home environment generations
delete-generations delete old home environment generations
search             search for existing service types
container          run home environment in a container
```

Useful flags:
- `--dry-run` / `-n` — show what would be built without building
- `--keep-failed` / `-K` — keep build tree on failure (helps diagnose errors)
- `--allow-downgrades` — allow rolling back to older channel revisions

## guix repl — for testing and debugging

The `guix repl` command starts a Guile REPL in the Guix environment. Use it to test package definitions, inspect modules, or evaluate Scheme expressions before committing them to a config file.

```bash
# Interactive REPL
guix repl

# Load and test a specific module
guix repl -- /path/to/script.scm

# REPL with custom load path (if GUIX_PACKAGE_PATH not set)
guix repl -L ~/dotfiles

# Test a package definition interactively
guix repl
> (use-modules (config packages my-package))
> (use-modules (guix packages))
> (package-name my-package)
```

Use `guix repl` to:
- Validate Scheme syntax before reconfiguring
- Inspect package fields (`package-name`, `package-version`, `package-inputs`)
- Test `gexp` expressions
- Debug service type definitions

## How Dotfiles Are Deployed

Files are deployed via `home-files-service-type` and `home-xdg-configuration-files-service-type` in `redfish.scm`. Source files use `(local-file "../../path/to/file")` relative to the config file, and Guix symlinks them into `~/.config/` etc. as **read-only symlinks into the Guix store**.

The result: `~/.config/foo/bar` → read-only symlink. Apps that try to write to these files fail with `OSError: [Errno 30] Read-only file system`.

## Common Issues

### Read-only config file
When an app fails with `OSError: [Errno 30] Read-only file system`:
1. Update the source file in `$DOTFILES_DIR/`
2. Run `guix home reconfigure` to rebuild symlinks, OR
3. For immediate workaround: `sudo rm ~/.config/.../file && cp $DOTFILES_DIR/.../file ~/.config/.../file`

### Missing config key (app writes to config on startup)
Add the key to the dotfiles source file and reconfigure. The app cannot write the key itself.

### Broken symlink on reconfigure
Guix fails with `stat: No such file or directory` if a broken symlink exists at the target path. Fix: remove the broken symlink first, then reconfigure.

### Module not found
If `guix home reconfigure` errors with "no code for module", check that `GUIX_PACKAGE_PATH` is set and the module file path matches the `(define-module ...)` declaration.

## Git Identity

No global git config is set. Use:
```bash
git -c user.email="stephen.nehrbass@gmail.com" -c user.name="nehrbash" commit ...
```
