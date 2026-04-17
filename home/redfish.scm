(define-module (home redfish)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-apps)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages node)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (guix gexp)
  #:use-module (desktop caelestia)
  #:use-module (home common)
  #:use-module (home services emacs)
  #:use-module (home services llama-server)
  #:use-module (home services podman)
  #:use-module (home services ssh-agent)
  #:use-module (home services nvidia-resume)
  #:use-module (home services flatpak)
  #:use-module (home services zen-browser)
  #:use-module (home services dotfiles-symlinks)
  #:use-module (home services claude-code)
  #:use-module (packages ai-models)
  #:use-module (packages fonts)
  #:use-module (packages gh)
  #:use-module (packages glab)
  #:use-module (packages mise)
  #:use-module (packages slack)
  #:use-module (packages vscode)
  #:use-module (packages claude-agent-acp))

;; Resolve dotfiles root from this module's location on the load path.
(define %dotfiles-dir
  (canonicalize-path
   (string-append (dirname (search-path %load-path "home/redfish.scm"))
                  "/..")))

;; Host-specific env vars on top of %common-*-env-vars.
;; NVIDIA + Guix-System-only paths (gcr askpass, guix-home XKB, etc).
(define %redfish-env-vars
  '(("PATH" .
     "$PATH:$HOME/.local/bin:$HOME/.claude/bin:$HOME/.local/share/uv/bin:$HOME/.cargo/bin:$HOME/.local/share/go/bin")
    ;; gcr-ssh-askpass is the GUI fallback for passphrase prompts when
    ;; there is no controlling TTY (e.g. autostart). SSH_ASKPASS_REQUIRE
    ;; is intentionally omitted so ssh-add still uses the terminal when
    ;; available — required for PKCS#11 PIN prompts that gcr can't handle.
    ("SSH_ASKPASS" . "$HOME/.guix-home/profile/libexec/gcr-ssh-askpass")
    ;; Caelestia XKB rules path (Guix has no /usr/share/X11).
    ("CAELESTIA_XKB_RULES_PATH"
     . "$HOME/.guix-home/profile/share/X11/xkb/rules/base.lst")
    ;; NVIDIA driver selection (search-paths handle library discovery).
    ("LIBVA_DRIVER_NAME" . "nvidia")
    ("GBM_BACKEND" . "nvidia-drm")
    ("__GLX_VENDOR_LIBRARY_NAME" . "nvidia")
    ("__GL_GSYNC_ALLOWED" . "1")
    ("__GL_VRR_ALLOWED" . "1")
    ("NVD_BACKEND" . "direct")
    ("DOCKER_HOST" . "unix://$XDG_RUNTIME_DIR/podman/podman.sock")
    ;; Global LD_LIBRARY_PATH is needed here so Hyprland / Quickshell /
    ;; Qt applications pick up the nongnu NVIDIA driver libs installed
    ;; into /run/current-system/profile/lib.  Do NOT replicate this on
    ;; bluefish (Arch) — it breaks foreign binaries there.
    ("LD_LIBRARY_PATH"
     . "/run/current-system/profile/lib:$HOME/.guix-home/profile/lib")))

(home-environment
 (packages
  (append
   ;; Core CLI + build tools.
   (list git git-lfs curl wget ripgrep jq
         (specification->package "bind")
         (specification->package "sqlite")
         (specification->package "gcc-toolchain")
         pkg-config libtool cmake gnu-make ninja
         (specification->package "file")
         (specification->package "tree")
         (specification->package "lsof")
         (specification->package "strace")
         (specification->package "zip")
         (specification->package "unzip")
         (specification->package "pv")
         (specification->package "bc"))
   ;; Terminal + shell.
   (list libvterm alacritty eza starship ncurses
         zsh-autosuggestions zsh-syntax-highlighting zsh-completions
         zsh-autopair zsh-history-substring-search fzf fzf-tab)
   ;; Editor.
   (list vscode)
   ;; General-purpose fonts (Caelestia-specific fonts come from desktop).
   (list font-iosevka font-iosevka-aile
         font-google-noto-sans-cjk font-google-noto-emoji
         font-iosevka-term-nf)
   ;; Media (Caelestia-specific audio deps live in desktop).
   (list vlc sox)
   ;; Local AI stack (ffmpeg pulled in via vlc for whisper.el).
   (list llama-cpp-git whisper-cpp
         gemma-4-e4b-it-gguf
         whisper-ggml-large-v3-turbo-q5)
   %caelestia-desktop-packages
   ;; TeX Live.
   (list texlive-scheme-basic
         texlive-tools
         texlive-fancyhdr texlive-xcolor texlive-caption texlive-titling
         texlive-ragged2e texlive-setspace texlive-wrapfig texlive-ulem
         texlive-adjustbox texlive-capt-of texlive-minted texlive-fontspec
         texlive-psnfss
         texlive-mathdesign texlive-bera texlive-tex-gyre
         texlive-libertine texlive-lm texlive-kpfonts
         texlive-dvisvgm)
   ;; Dev toolchains + language servers.
   (list go python rust node (list rust "cargo")
         claude-agent-acp
         gopls gofumpt go-staticcheck rust-analyzer
         shellcheck gh glab mise sassc
         tree-sitter-css tree-sitter-go tree-sitter-html
         tree-sitter-javascript tree-sitter-json tree-sitter-markdown
         tree-sitter-python tree-sitter-rust tree-sitter-toml
         tree-sitter-typescript tree-sitter-yaml)
   ;; Misc.
   (list aspell aspell-dict-en enchant isync
         htop atop lm-sensors xorriso slack
         podman-compose bluez)))

 (services
  (append
   (list
    ;; Shared base (files, XDG, env, zsh, dconf GTK theme).
    (common-home-files-service %dotfiles-dir)
    (common-xdg-mime-service)
    (common-xdg-configuration-files-service %dotfiles-dir)
    (common-env-vars-service %dotfiles-dir %redfish-env-vars)
    (common-zsh-service %dotfiles-dir)
    (home-dotfiles-symlinks-service %dotfiles-dir)
    (common-gtk-dconf-service)
    (common-spicetify-service)

    ;; Host services — redfish runs a full Guix Home session.
    (service home-emacs-daemon-service-type)
    (service home-llama-server-service-type)
    (service home-podman-socket-service-type)
    (service home-ssh-agent-service-type)
    (service home-nvidia-resume-service-type)
    (service home-flatpak-service-type)
    (service home-zen-browser-service-type)
    (service home-claude-code-service-type))

   ;; Caelestia/Hyprland desktop (shell, wayland sentinel, resizer,
   ;; spicetify watcher, plus dbus/pipewire/xdph/polkit/keyring).
   %caelestia-home-services
   %caelestia-guix-session-services)))
