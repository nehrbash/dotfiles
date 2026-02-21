---
name: caelestia
description: Apply Caelestia desktop shell knowledge when modifying QML modules, shell.json config, Quickshell services, or caelestia package definitions
user-invocable: false
---

# Caelestia Desktop Shell

Apply this knowledge when working with the Caelestia shell (Quickshell-based desktop environment on Hyprland/Wayland).

## Architecture Overview

Caelestia is a **QML desktop shell** running on Quickshell (Qt6 compositor framework). Three components:

| Component | Package | Shepherd Service | Purpose |
|-----------|---------|-----------------|---------|
| Shell | `quickshell-git` + QML files | `caelestia-shell` | Desktop UI (bar, launcher, dashboard, notifications, lock screen, OSD) |
| CLI | `caelestia-cli` (Python) | — | Wallpaper/scheme management, IPC control |
| Resizer | `caelestia-cli` | `caelestia-resizer` | Auto-resize/reposition windows (PiP) |

Upstream: https://github.com/caelestia-dots

## File Locations

### Config & QML (deployed by Guix home)
- **`files/caelestia/shell.json`** — Runtime config (appearance, bar, launcher, services, paths). Live-reloaded by shell.
- **`files/caelestia/quickshell/`** — All QML source (~143 files)
  - `shell.qml` — Root entry point
  - `config/Config.qml` — Singleton: loads/saves shell.json, hot-reload via FileView watcher
  - `config/Appearance.qml` — Shorthand singleton for appearance props
  - `config/*Config.qml` — Typed config structures (19 files)
  - `services/` — System integration singletons (18 files)
  - `modules/` — UI components (bar, launcher, dashboard, controlcenter, lock, notifications, osd, session, sidebar, background, windowinfo, drawers)
  - `components/` — Reusable QML controls (buttons, sliders, switches, effects, containers)
  - `utils/` — Helpers (Paths, Strings, Icons, Searcher)
  - `plugin/` — C++ QML plugin source (built by caelestia-shell package)
  - `extras/` — C++ extras (HyprExtras for keyboard/device state)

### Package Definitions
- **`packages/caelestia-shell.scm`** — C++ plugin only (CMake), repo: `caelestia-dots/shell`
- **`packages/caelestia-cli.scm`** — Python CLI (pyproject), repo: `caelestia-dots/cli`
- **`packages/quickshell-git.scm`** — Quickshell compositor (CMake+Ninja), repo: `git.outfoxxed.me/quickshell/quickshell`

### Services
- **`home/services/shepherd.scm`** — Shepherd service definitions
  - `caelestia-shell`: `qs -c caelestia -n` with QML_IMPORT_PATH, QS_ICON_THEME, CAELESTIA_XKB_RULES_PATH
  - `caelestia-resizer`: `caelestia resizer -d`
  - Both require `wayland-compositor` sentinel, respawn with 5s delay

### Logs
- Shell: `~/.local/state/log/caelestia-shell.log`
- Resizer: `~/.local/state/log/caelestia-resizer.log`
- Quickshell internal: `/run/user/1000/quickshell/by-id/<instance>/log.qslog`

## Key Patterns

### Singleton Services
All major systems are QML Singletons accessed globally:
```qml
Config.bar.persistent       // Config property
Hypr.activeToplevel         // Hyprland state
Audio.volume                // Pipewire audio
Colours.m3.primary          // Material Design 3 colors
```

### Hyprland IPC Event Handling (`services/Hypr.qml`)
- Subscribes to raw Hyprland socket events via `Connections { target: Hyprland }`
- Filters out `*v2` events (duplicates)
- Routes events to appropriate refresh calls: `refreshToplevels()`, `refreshWorkspaces()`, `refreshMonitors()`
- `windowtitle` events are debounced (200ms timer) to prevent crash from rapid title changes
- Keyboard lock state (Caps/Num) tracked and toasted

### Config System (`config/Config.qml`)
- `FileView` watches `~/.config/caelestia/shell.json` with `watchChanges: true`
- `JsonAdapter` deserializes JSON into typed QML properties
- `save()` debounces writes (500ms timer) and sets `recentlySaved` flag to prevent reload loop
- Edit shell.json externally -> instant reload

### Color/Theme System (`services/Colours.qml`)
- Watches `~/.local/state/caelestia/scheme.json` for Material You palette
- `ImageAnalyser` calculates wallpaper luminance for adaptive transparency
- `layer(n)` applies transparency per elevation level (0=base, 1-3=elevated)
- Preview mode for non-destructive scheme testing

### IPC Bridge Pattern
Services expose CLI control via `IpcHandler { target: "name" }`:
```
caelestia wallpaper -f <path>      # Apply wallpaper + generate scheme
caelestia scheme set -m light      # Switch mode
caelestia shell controlCenter open # Open settings
```

### Wallpaper Flow
1. `caelestia wallpaper -f <path>` (CLI) writes path to `~/.local/state/caelestia/wallpaper/path.txt`
2. Generates Material You scheme -> `~/.local/state/caelestia/scheme.json`
3. Shell watches both files, updates background + colors reactively

## Services Reference

| Service | File | Key Properties/Functions |
|---------|------|------------------------|
| Hypr | `services/Hypr.qml` | `activeToplevel`, `focusedWorkspace`, `focusedMonitor`, `dispatch()` |
| Audio | `services/Audio.qml` | `volume`, `muted`, `setVolume()`, `incrementVolume()`, `setAudioSink()` |
| Brightness | `services/Brightness.qml` | `brightness`, `setBrightness()`, supports DDC-CI/brightnessctl/asdbctl |
| Colours | `services/Colours.qml` | `m3` (palette), `mode`, `setMode()`, `layer()`, `on()` |
| Wallpapers | `services/Wallpapers.qml` | `setWallpaper()`, `preview()`, extends Searcher |
| Players | `services/Players.qml` | `active`, `togglePlaying()`, `previous()`, `next()` |
| SystemUsage | `services/SystemUsage.qml` | `cpuPerc`, `cpuTemp`, `memPerc`, `gpuPerc`, reads /proc/* |
| Time | `services/Time.qml` | `timeStr`, `format(fmt)`, respects 12/24hr config |
| Network | `services/Network.qml` | Network state via NetworkManager |
| Notifs | `services/Notifs.qml` | D-Bus notification daemon |
| Visibilities | `services/Visibilities.qml` | Per-monitor module visibility state |

## Module Structure

Each module in `modules/` typically has:
- `Wrapper.qml` — Creates instances per screen, handles visibility/animation
- `Content.qml` or main component — The actual UI
- Subcomponents in subdirectories

Key modules: `bar/`, `launcher/`, `dashboard/`, `controlcenter/`, `notifications/`, `lock/`, `osd/`, `session/`, `sidebar/`, `background/`, `windowinfo/`, `drawers/`

## Common Modifications

### Adding a bar entry
1. Create component in `modules/bar/components/`
2. Register in bar entries system (see `Bar.qml`)
3. Add config toggle in `shell.json` under `bar.entries`

### Adding a config option
1. Add field to appropriate `*Config.qml` in `config/`
2. Add default value in `shell.json`
3. Access via `Config.<section>.<field>` in QML

### Modifying event handling
Edit `services/Hypr.qml` `onRawEvent`. Use debounce timers for high-frequency events.

### Changing wallpaper directory
Edit `paths.wallpaperDir` in `shell.json`

## Known Issues
- Quickshell can crash with "pure virtual method called" on rapid Hyprland IPC events (e.g., fast window title changes). Mitigated by debouncing `windowtitle` events.
- BlueZ integration fails silently if bluetooth service not running
- PwNode channelVolumes/channelMap size mismatch warning on some audio devices (cosmetic)
