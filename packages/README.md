# Custom Guix Packages

## caelestia-shell

C++ QML plugin for the caelestia desktop shell. Built with cmake-build-system from
the local checkout at `files/caelestia/quickshell` — no pinned git hash needed.

**Inputs:** qtbase, qtdeclarative, libqalculate, gmp, mpfr, pipewire, aubio, libcava

## quickshell

Quickshell Wayland shell runtime. Built from local checkout at `~/src/quickshell`.

## libcava

Builds cavacore.c as a shared library with `cava.pc`. Propagates fftw.

## emacs-gptel-git

Latest gptel (LLM client for Emacs) from git.

## emacs-gptel-agent

gptel-agent extension for agentic tool-use in gptel.

## emacs-eca

Emacs Claude Agent (ECA) package.

## gruvbox

Gruvbox GTK theme.

## vscode

Visual Studio Code binary package.

## nwg-displays

Wayland display configuration tool (Python/GTK).

## fonts, slack

See individual `.scm` files for details.
