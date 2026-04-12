---
paths:
  - "*.scm"
  - "home/**"
  - "systems/**"
  - "packages/**"
  - "channels.scm"
---

# Guix Rules

- No stow — `guix home` manages symlinks via `local-file`
- Validate: `guix home reconfigure home/redfish.scm -L .`
- Shepherd services: `home/services/shepherd.scm`
- Wayland-dependent services use `wayland-compositor` sentinel
