(define-module (home redfish))

;; Resolve dotfiles root from this module's location on the load path
(define %dotfiles-dir
  (canonicalize-path
   (string-append (dirname (search-path %load-path "home/redfish.scm"))
                  "/..")))

(use-modules (gnu)
             (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu home services shepherd)
             (gnu packages)
             (gnu packages version-control)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages ssh)
             (gnu packages admin)
             (gnu packages terminals)
             (gnu packages video)
             (gnu packages fonts)
             (gnu packages aspell)
             (gnu packages enchant)
             (gnu packages text-editors)
             (gnu packages rust)
             (gnu packages rust-apps)
             (gnu packages curl)
             (gnu packages shellutils)
             (gnu packages golang)
             (gnu packages golang-apps)
             (gnu packages golang-check)
             (gnu packages golang-xyz)
             (gnu packages tree-sitter)
             (gnu packages wm)
             (gnu packages xdisorg)
             (gnu packages glib)
             (gnu packages gtk)
             (gnu packages gnome-xyz)
             (gnu packages pkg-config)
             (gnu packages music)
             (gnu packages mail)
             (gnu packages texlive)
             (gnu packages haskell-apps)
             (gnu packages tex)
             (gnu packages compression)
             (gnu packages wget)
             (gnu packages dns)
             (gnu packages web)
             (gnu packages node)
             (gnu packages freedesktop)
             (gnu packages polkit)
             (gnu packages package-management)
             (gnu packages image)
             (gnu packages cmake)
             (gnu packages ninja)
             (gnu packages linux)
             (gnu packages audio)
             (gnu packages maths)
             (gnu packages hardware)
             (gnu packages xorg)
             (gnu packages containers)
             (gnu packages pulseaudio)
             (gnu home services desktop)
             (gnu home services sound)
             (guix gexp)
             (packages zsh-pure-prompt)
             (packages gruvbox)
             (packages fonts)
             (packages quickshell-git)
             (packages caelestia-cli)
             (packages caelestia-shell)
             (packages libcava)
             (packages slack)
             (packages vscode)
             (nongnu packages editors)
             (home services shepherd)
             (home services activation))

(home-environment

 (packages
  (list
   ;; Core tools
   git
   curl
   wget
   ripgrep
   jq
   (specification->package "bind")
   (specification->package "sqlite")
   (specification->package "gcc-toolchain")
   cmake
   ninja

   ;; Terminal & shell
   alacritty
   eza
   zsh-pure-prompt
   zsh-autosuggestions
   zsh-syntax-highlighting

   ;; Node.js
   node

   ;; Editor
   emacs-next-pgtk

   ;; Fonts
   font-iosevka
   font-iosevka-aile
   font-awesome
   font-google-material-design-icons
   font-rubik
   font-caskaydia-cove-nf
   font-material-symbols-rounded

   ;; Wayland / Hyprland
   hyprpolkitagent
   hyprland
   xdg-desktop-portal-hyprland
   xdg-desktop-portal-gtk
   wl-clipboard
   grim
   playerctl
   brightnessctl
   swayidle
   wlr-randr
   ;; Desktop shell
   quickshell-git
   caelestia-cli
   caelestia-shell
   ddcutil

   ;; Audio
   pavucontrol
   cava
   aubio
   libqalculate
   libcava

   ;; Video
   vlc

   ;; Spellcheck
   aspell
   aspell-dict-en
   enchant

   ;; Mail
   isync

   ;; TeX
   texlive-scheme-full

   ;; Languages
   go
   rust
   (list rust "cargo")

   ;; Go tools
   gopls
   gofumpt
   go-staticcheck

   ;; Tree-sitter grammars
   tree-sitter-css
   tree-sitter-go
   tree-sitter-html
   tree-sitter-javascript
   tree-sitter-json
   tree-sitter-markdown
   tree-sitter-python
   tree-sitter-rust
   tree-sitter-toml
   tree-sitter-typescript
   tree-sitter-yaml

   ;; System monitors
   htop
   atop
   lm-sensors
   xkeyboard-config

   xdg-utils

   ;; Podman
   podman
   (list podman "docker")
   podman-compose

   ;; Editors (proprietary)
   vscodium
   vscode

   ;; Chat
   slack

   ;; Dev tools
   shellcheck

   ;; SSH
   openssh

   ;; Flatpak
   flatpak

   ;; Cursor theme
   bibata-cursor-theme

   ;; GTK theme + icons
   gruvbox-dark-gtk
   gruvbox-plus-icon-theme))

 (services
  (list

   ;; Home-directory dotfiles
   (service home-files-service-type
            `((".gtkrc-2.0"
               ,(local-file "../files/gtk/gtkrc-2.0" "gtkrc-2.0"))
              (".emacs.d/init.el"
               ,(local-file "../files/emacs/init.el" "init.el"))
              (".emacs.d/early-init.el"
               ,(local-file "../files/emacs/early-init.el" "early-init.el"))
              (".emacs.d/lisp"
               ,(local-file "../files/emacs/lisp" #:recursive? #t))
              (".emacs.d/snippets"
               ,(local-file "../files/emacs/snippets" #:recursive? #t))
              (".emacs.d/img"
               ,(local-file "../files/emacs/img" #:recursive? #t))
              (".local/bin"
               ,(local-file "../scripts" #:recursive? #t))
              (".local/share/applications/emacsclient.desktop"
               ,(local-file "../share/applications/emacsclient.desktop"))
              (".npmrc"
               ,(plain-file "npmrc" "prefix=~/.npm-global\n"))
              (".face"
               ,(local-file "../files/emacs/img/sloth-head.jpg" "face"))
))

   ;; XDG config files are symlinked directly from dotfiles in the
   ;; activation gexp below (not via home-xdg-configuration-files-service-type)
   ;; so edits take effect immediately without reconfigure.

   ;; Environment variables (consolidates .zshenv)
   (simple-service 'my-env-vars
                   home-environment-variables-service-type
                   (append
                    (list (cons "DOTFILES_DIR" %dotfiles-dir)
                          (cons "GUIX_PACKAGE_PATH" %dotfiles-dir))
                    '(("EDITOR" . "emacs -nw")
                     ("TERM" . "xterm-256color")
                     ("PATH" . "$PATH:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/.local/share/go/bin:$HOME/.local/share/npm/bin")
                     ("GOPATH" . "$HOME/.local/share/go")
                     ("NPM_CONFIG_PREFIX" . "$HOME/.local/share/npm")
                     ("SSH_AUTH_SOCK" . "$XDG_RUNTIME_DIR/ssh-agent.socket")
                     ("PKG_CONFIG_PATH" . "/usr/lib/pkgconfig:$PKG_CONFIG_PATH")
                     ("XDG_DATA_DIRS" . "$HOME/.guix-home/profile/share:$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share")
                     ("EMACSLOADPATH" . "$HOME/.guix-home/profile/lib/emacs:")
                     ;; NVIDIA/Wayland — needed before start-hyprland runs
                     ("LIBVA_DRIVER_NAME" . "nvidia")
                     ("GBM_BACKEND" . "nvidia-drm")
                     ("__GLX_VENDOR_LIBRARY_NAME" . "nvidia")
                     ("WLR_NO_HARDWARE_CURSORS" . "1")
                     ("LD_LIBRARY_PATH" . "/run/current-system/profile/lib")
                     ("__EGL_VENDOR_LIBRARY_DIRS" . "/run/current-system/profile/share/glvnd/egl_vendor.d")
                     ("GBM_BACKENDS_PATH" . "/run/current-system/profile/lib/gbm")
                     ("LIBGL_DRIVERS_PATH" . "/run/current-system/profile/lib/dri")
                     ;; VSCodium: use Microsoft marketplace (for Dev Containers etc.)
                     ("VSCODE_GALLERY_SERVICE_URL" . "https://marketplace.visualstudio.com/_apis/public/gallery")
                     ("VSCODE_GALLERY_CACHE_URL" . "https://vscode.blob.core.windows.net/gallery/index")
                     ("VSCODE_GALLERY_ITEM_URL" . "https://marketplace.visualstudio.com/items")
                     ;; Dev Containers: point to rootless podman socket
                     ("DOCKER_HOST" . "unix://$XDG_RUNTIME_DIR/podman/podman.sock")
                     ("DOCKER_API_VERSION" . "1.41")
                     ;; Caelestia QML plugin
                     ("QML_IMPORT_PATH" . "$HOME/.guix-home/profile/lib/qt6/qml")
                     ;; Caelestia XKB rules path (Guix has no /usr/share/X11)
                     ("CAELESTIA_XKB_RULES_PATH" . "$HOME/.guix-home/profile/share/X11/xkb/rules/base.lst")
                     ;; Wayland/Desktop session
                     ("XDG_CURRENT_DESKTOP" . "Hyprland")
                     ("XDG_SESSION_DESKTOP" . "Hyprland")
                     ("XDG_SESSION_TYPE" . "wayland")
                     ("SDL_VIDEODRIVER" . "wayland")
                     ("MOZ_ENABLE_WAYLAND" . "1")
                     ("CLUTTER_BACKEND" . "wayland")
                     ("ELECTRON_OZONE_PLATFORM_HINT" . "auto")
                     ("_JAVA_AWT_WM_NONREPARENTING" . "1")
                     ;; Qt
                     ("QT_AUTO_SCREEN_SCALE_FACTOR" . "1")
                     ("QT_QPA_PLATFORM" . "wayland;xcb")
                     ("QT_QPA_PLATFORMTHEME" . "qt5ct")
                     ("QT_WAYLAND_DISABLE_WINDOWDECORATION" . "1")
                     ;; Cursor
                     ("XCURSOR_THEME" . "Bibata-Modern-Amber")
                     ("HYPRCURSOR_THEME" . "Bibata-Modern-Amber")
                     ("XCURSOR_SIZE" . "24")
                     ("HYPRCURSOR_SIZE" . "24")
                     ;; Display scaling
                     ("GDK_SCALE" . "1")
                     ("WLR_DRM_NO_ATOMIC" . "1")
                     ("NVD_BACKEND" . "direct"))))

   ;; Zsh
   (service home-zsh-service-type
            (home-zsh-configuration
             (zshenv
              (list (plain-file "zshenv" "setopt NULL_GLOB\n")))
             (zshrc
              (list (local-file "../files/zsh/zshrc" "zshrc")))))

   ;; Automated setup (idempotent, runs on every reconfigure)
   (post-setup-activation-service %dotfiles-dir)

   ;; D-Bus user session (required by PipeWire)
   (service home-dbus-service-type)

   ;; Audio (PipeWire + WirePlumber + PulseAudio emulation)
   (service home-pipewire-service-type)

   ;; Shepherd user services
   (service home-shepherd-service-type
            (home-shepherd-configuration
             (services %shepherd-services)))

   )))
