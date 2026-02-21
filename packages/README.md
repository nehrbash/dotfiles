# Custom Guix Packages

## quickshell-git

Quickshell built from git (staging branch) — the caelestia shell config requires
features not yet in the 0.2.1 release.

**Upstream:** <https://git.outfoxxed.me/quickshell/quickshell>

### Updating

1. Clone the repo and checkout the desired commit/branch:
   ```bash
   git clone --recursive https://git.outfoxxed.me/quickshell/quickshell.git /tmp/quickshell
   cd /tmp/quickshell && git checkout staging
   ```
2. Compute the hash:
   ```bash
   guix hash --serializer=nar -x .
   ```
3. Update `quickshell-git.scm`: set `version`, `commit`, and `sha256`.
4. Build and reconfigure:
   ```bash
   guix home reconfigure home/redfish.scm -L .
   ```

### Wrapper details

The `wrap-program` phase adds `QML_IMPORT_PATH` (Qt6 QML modules) and
`QT_PLUGIN_PATH` (qtwayland + qtsvg plugins) so quickshell can find Qt
components from the Guix store. The wrapper uses `prefix` (not `=`) so
user-set `QML_IMPORT_PATH` (e.g. for caelestia-shell QML plugin) appends
after the store paths.

### Known issues

- **`pure virtual method called` crash (SIGABRT/signal 6):** Intermittent
  crash after running for ~30-60 seconds. This is a C++ use-after-free bug
  in quickshell (destroyed object's vtable method invoked). Master
  `dacfa9d` already includes the fix `191085a` (ipc: use deleteLater() in
  IpcServerConnection to prevent use-after-free). The staging branch
  (`5eb6f51`) is actually **older** than master and is missing this fix
  plus the preprocessor fix needed for caelestia config loading. The
  shepherd service has `respawn? #t` to auto-restart on crash if it
  still occurs.

- **Icon theme:** Quickshell does NOT use `QT_ICON_THEME`. Use either
  `//@ pragma IconTheme <name>` in `shell.qml` or the `QS_ICON_THEME`
  env var.

- **Service management:** The shepherd service must run `qs` directly in
  the foreground (`qs -c caelestia -n`), NOT via `caelestia shell -d`.
  The `-d` flag daemonizes quickshell so the parent exits immediately,
  causing shepherd to think the service died and respawn it in a loop.

## caelestia-shell

C++ QML plugin for the caelestia desktop shell. Built with cmake-build-system.

**Inputs:** qtbase, qtdeclarative, libqalculate, gmp, mpfr, pipewire, aubio, libcava

## caelestia-cli

Python CLI for caelestia. Patched for Python 3.11 via `caelestia-python311.patch`.

## libcava

Builds cavacore.c as a shared library with `cava.pc`. Propagates fftw.

## fonts, slack, quickshell-git

See individual `.scm` files for details.
