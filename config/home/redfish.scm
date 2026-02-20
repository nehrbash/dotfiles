(define-module (home redfish))

(use-modules (gnu)
             (gnu home)
             (gnu home services)
             (gnu home services xdg)
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
             (gnu packages containers)
             (gnu packages pulseaudio)
             (gnu home services desktop)
             (gnu home services sound)
             (guix gexp)
             (packages zsh-pure-prompt)
             (packages nwg-displays)
             (packages gruvbox)
)

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

   ;; GTK build deps (needed for cargo install eww)
   pkg-config
   gtk+
   gtk-layer-shell
   glib
   pango
   cairo
   gdk-pixbuf
   libdbusmenu

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
   swww
   nwg-launchers
   nwg-displays

   ;; Audio
   pavucontrol

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

   xdg-utils

   ;; Podman
   podman

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
               ,(local-file "../../gtk/gtkrc-2.0" "gtkrc-2.0"))
              (".emacs.d/init.el"
               ,(local-file "../../emacs/init.el" "init.el"))
              (".emacs.d/early-init.el"
               ,(local-file "../../emacs/early-init.el" "early-init.el"))
              (".emacs.d/lisp"
               ,(local-file "../../emacs/lisp" #:recursive? #t))
              (".emacs.d/snippets"
               ,(local-file "../../emacs/snippets" #:recursive? #t))
              (".emacs.d/img"
               ,(local-file "../../emacs/img" #:recursive? #t))
              (".local/bin"
               ,(local-file "../../scripts" #:recursive? #t))
              (".local/share/applications/emacsclient.desktop"
               ,(local-file "../../share/applications/emacsclient.desktop"))
              (".npmrc"
               ,(plain-file "npmrc" "prefix=~/.npm-global\n"))
))

   ;; XDG config files (~/.config/)
   (service home-xdg-configuration-files-service-type
            `(;; Hyprland
              ("hypr/hyprland.conf"
               ,(local-file "../../hypr/hyprland.conf"))
              ("hypr/env.conf"
               ,(local-file "../../hypr/env.conf"))
              ("hypr/plugins.conf"
               ,(local-file "../../hypr/plugins.conf"))
              ("hypr/spotify.conf"
               ,(local-file "../../hypr/spotify.conf"))
              ("hypr/hypridle.conf"
               ,(local-file "../../hypr/hypridle.conf"))
              ("hypr/workspaces.conf"
               ,(local-file "../../hypr/workspaces.conf"))
              ("hypr/hyprpaper.conf"
               ,(local-file "../../hypr/hyprpaper.conf"))
              ;; Other config dirs
              ("gtk-3.0"
               ,(local-file "../../gtk/gtk-3.0" #:recursive? #t))
              ("gtk-4.0"
               ,(local-file "../../gtk/gtk-4.0" #:recursive? #t))
              ("alacritty"
               ,(local-file "../../alacritty" #:recursive? #t))
              ("cava"
               ,(local-file "../../cava" #:recursive? #t))
              ("direnv"
               ,(local-file "../../direnv" #:recursive? #t))
              ("eww"
               ,(local-file "../../eww" #:recursive? #t))
              ("mise"
               ,(local-file "../../mise" #:recursive? #t))
              ("nwg-drawer"
               ,(local-file "../../nwg-drawer" #:recursive? #t))
              ("nwg-launchers"
               ,(local-file "../../nwg-launchers" #:recursive? #t))
              ("nwg-panel"
               ,(local-file "../../nwg-panel" #:recursive? #t))
              ("nwg-wrapper"
               ,(local-file "../../nwg-wrapper" #:recursive? #t))
              ("nyxt"
               ,(local-file "../../nyxt" #:recursive? #t))
              ("paru"
               ,(local-file "../../paru" #:recursive? #t))
              ("sptlrx"
               ,(local-file "../../sptlrx" #:recursive? #t))
              ("swaync"
               ,(local-file "../../swaync" #:recursive? #t))
              ("systemd"
               ,(local-file "../../systemd" #:recursive? #t))
              ("Thunar"
               ,(local-file "../../thunar" #:recursive? #t))
              ;; Config files
              ("electron-flags.conf"
               ,(local-file "../../misc/electron-flags.conf"))
              ("mimeapps.list"
               ,(local-file "../../misc/mimeapps.list"))
              ("spotify-launcher.conf"
               ,(local-file "../../misc/spotify-launcher.conf"))
              ;; Zsh support files (under ZDOTDIR)
              ("zsh/aliasrc"
               ,(local-file "../../zsh/aliasrc"))
              ("zsh/emacs_functions"
               ,(local-file "../../zsh/emacs_functions"))
              ("zsh/functions"
               ,(local-file "../../zsh/functions"))))

   ;; Environment variables (consolidates .zshenv)
   (simple-service 'my-env-vars
                   home-environment-variables-service-type
                   '(("EDITOR" . "emacs -nw")
                     ("TERM" . "xterm-256color")
                     ("PATH" . "$PATH:$HOME/.local/bin:$HOME/.config/eww/bin:$HOME/.cargo/bin:$HOME/.local/share/go/bin:$HOME/.local/share/npm/bin")
                     ("GOPATH" . "$HOME/.local/share/go")
                     ("NPM_CONFIG_PREFIX" . "$HOME/.local/share/npm")
                     ("SSH_AUTH_SOCK" . "$XDG_RUNTIME_DIR/ssh-agent.socket")
                     ("PKG_CONFIG_PATH" . "/usr/lib/pkgconfig:$PKG_CONFIG_PATH")
                     ("GUIX_PACKAGE_PATH" . "$HOME/dotfiles/config")
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
                     ("LIBGL_DRIVERS_PATH" . "/run/current-system/profile/lib/dri")))

   ;; Zsh
   (service home-zsh-service-type
            (home-zsh-configuration
             (zshenv
              (list (plain-file "zshenv" "setopt NULL_GLOB\nexport GUIX_PACKAGE_PATH=\"$HOME/dotfiles/config\"\n")))
             (zshrc
              (list (local-file "../../zsh/zshrc" "zshrc")))
             (zprofile
              (list (local-file "../../zsh/zprofile" "zprofile")))))

   ;; Automated setup (idempotent, runs on every reconfigure)
   (simple-service 'post-setup
                   home-activation-service-type
                   #~(begin
                       ;; Flatpak: add remote + install apps
                       (system* "flatpak" "remote-add" "--user"
                                "--if-not-exists" "flathub"
                                "https://dl.flathub.org/repo/flathub.flatpakrepo")
                       (for-each
                        (lambda (app)
                          (system* "flatpak" "install" "--user"
                                   "--noninteractive" "flathub" app))
                        '("app.zen_browser.zen"
                          "com.spotify.Client"
                          "com.slack.Slack"
                          "com.visualstudio.code"
                          "com.discordapp.Discord"
                          "com.valvesoftware.Steam"))

                       ;; Slack: allow keyring access for login token storage
                       (system* "flatpak" "override" "--user"
                                "--talk-name=org.freedesktop.secrets"
                                "com.slack.Slack")

                       ;; VS Code: Podman-based devcontainers
                       ;; Requires rootless-podman-service-type in system config
                       ;; (provides mount --make-shared / and cgroups v2 setup)
                       (system* "flatpak" "override" "--user"
                                (string-append "--filesystem="
                                               (getenv "XDG_RUNTIME_DIR")
                                               "/podman")
                                "--filesystem=~/.docker"
                                "--filesystem=~/.guix-home/profile/bin/podman"
                                "--filesystem=/gnu/store:ro"
                                (string-append "--env=DOCKER_HOST=unix://"
                                               (getenv "XDG_RUNTIME_DIR")
                                               "/podman/podman.sock")
                                "--env=DOCKER_API_VERSION=1.41"
                                "--env=DOCKER_BUILDKIT=1"
                                (string-append "--env=PATH="
                                               (getenv "HOME")
                                               "/.guix-home/profile/bin"
                                               ":/run/current-system/profile/bin"
                                               ":/app/bin:/usr/bin")
                                "com.visualstudio.code")

;; hyprshell: build if binary missing
                       (let ((bin (string-append (getenv "HOME")
                                                 "/.local/share/go/bin/hyprshell")))
                         (unless (file-exists? bin)
                           (setenv "GOPATH" (string-append (getenv "HOME") "/.local/share/go"))
                           (system* #$(file-append go "/bin/go")
                                    "install"
                                    "github.com/nehrbash/hyprshell@latest")))

                       ;; eww: build if binary missing (wayland-only)
                       ;; Uses ~/.local/bin/eww-install which patches dbusmenu-glib
                       ;; for glib >=0.16 ObjectExt API compatibility.
                       (let ((bin (string-append (getenv "HOME") "/.cargo/bin/eww"))
                             (installer (string-append (getenv "HOME") "/.local/bin/eww-install")))
                         (unless (file-exists? bin)
                           (system* installer)))

                       ;; claude-code: install via npm if missing
                       (let ((bin (string-append (getenv "HOME") "/.local/share/npm/bin/claude")))
                         (unless (file-exists? bin)
                           (setenv "npm_config_prefix"
                                   (string-append (getenv "HOME") "/.local/share/npm"))
                           (system* #$(file-append node "/bin/npm")
                                    "install" "-g" "@anthropic-ai/claude-code")))

                       ;; Set GTK icon/theme via dconf (settings.ini is overridden by dconf)
                       (system* #$(file-append (specification->package "dconf") "/bin/dconf")
                                "write" "/org/gnome/desktop/interface/icon-theme"
                                "'Gruvbox-Plus-Dark'")
                       (system* #$(file-append (specification->package "dconf") "/bin/dconf")
                                "write" "/org/gnome/desktop/interface/gtk-theme"
                                "'gruvbox-dark-gtk'")

                       ;; Reload Hyprland config if running in a session
                       (when (getenv "HYPRLAND_INSTANCE_SIGNATURE")
                         (system* "hyprctl" "reload"))))

   ;; D-Bus user session (required by PipeWire)
   (service home-dbus-service-type)

   ;; Audio (PipeWire + WirePlumber + PulseAudio emulation)
   (service home-pipewire-service-type)

   ;; Shepherd user services
   (service home-shepherd-service-type
            (home-shepherd-configuration
             (services
              (list

               ;; Emacs daemon
               (shepherd-service
                (provision '(emacs))
                (documentation "Run Emacs as a daemon.")
                (start #~(make-forkexec-constructor
                          (list #$(file-append emacs-next-pgtk "/bin/emacs")
                                "--fg-daemon")
                          #:log-file
                          (string-append (getenv "XDG_STATE_HOME")
                                         "/log/emacs.log")))
                (stop #~(make-kill-destructor))
                (respawn? #f))

               ;; Wayland compositor sentinel — polls until a wayland socket appears,
               ;; sets WAYLAND_DISPLAY in shepherd's environment, then marks itself done.
               (shepherd-service
                (provision '(wayland-compositor))
                (documentation "Wait for the Wayland compositor socket to appear.")
                (start #~(lambda _
                           (let* ((runtime-dir (or (getenv "XDG_RUNTIME_DIR")
                                                   (string-append "/run/user/"
                                                                  (number->string (getuid)))))
                                  (find-sock (lambda ()
                                               (let lp ((names '("wayland-1" "wayland-0")))
                                                 (cond
                                                  ((null? names) #f)
                                                  ((file-exists? (string-append runtime-dir "/" (car names)))
                                                   (car names))
                                                  (else (lp (cdr names))))))))
                             (let loop ((n 0))
                               (let ((name (find-sock)))
                                 (cond
                                  (name
                                   (setenv "WAYLAND_DISPLAY" name)
                                   #t)
                                  ((> n 60) (error "wayland-compositor: timed out waiting for socket"))
                                  (else
                                   (usleep 500000)
                                   (loop (+ n 1)))))))))
                (one-shot? #t))

               ;; Polkit authentication agent
               (shepherd-service
                (provision '(hyprpolkitagent))
                (requirement '(wayland-compositor))
                (documentation "Run hyprpolkitagent for polkit authentication.")
                (start #~(make-forkexec-constructor
                          (list #$(file-append hyprpolkitagent
                                               "/libexec/hyprpolkitagent"))
                          #:environment-variables
                          (cons "QT_QPA_PLATFORM=wayland" (environ))
                          #:log-file
                          (string-append (getenv "XDG_STATE_HOME")
                                         "/log/hyprpolkitagent.log")))
                (stop #~(make-kill-destructor))
                (respawn? #t))

               ;; swww wallpaper daemon
               (shepherd-service
                (provision '(swww-daemon))
                (requirement '(wayland-compositor))
                (documentation "Run swww wallpaper daemon.")
                (start #~(lambda _
                           ((make-forkexec-constructor
                             (list #$(file-append swww "/bin/swww-daemon"))
                             #:environment-variables
                             (let* ((runtime-dir (or (getenv "XDG_RUNTIME_DIR")
                                                     (string-append "/run/user/"
                                                                    (number->string (getuid)))))
                                    (wl (or (getenv "WAYLAND_DISPLAY")
                                            (let lp ((ns '("wayland-1" "wayland-0")))
                                              (cond ((null? ns) "wayland-1")
                                                    ((file-exists? (string-append runtime-dir "/" (car ns)))
                                                     (car ns))
                                                    (else (lp (cdr ns))))))))
                               (cons (string-append "WAYLAND_DISPLAY=" wl)
                                     (environ)))
                             #:log-file
                             (string-append (getenv "XDG_STATE_HOME")
                                            "/log/swww-daemon.log")))))
                (stop #~(make-kill-destructor))
                (respawn? #t))

               ;; Podman socket (Docker-compatible API for devcontainers)
               (shepherd-service
                (provision '(podman-socket))
                (documentation "Expose rootless podman as a Docker-compatible socket.")
                (start #~(lambda _
                           (let* ((runtime-dir (or (getenv "XDG_RUNTIME_DIR")
                                                   (string-append "/run/user/"
                                                                  (number->string (getuid)))))
                                  (sock-dir (string-append runtime-dir "/podman")))
                             (unless (file-exists? sock-dir)
                               (mkdir sock-dir))
                             ((make-forkexec-constructor
                               (list #$(file-append podman "/bin/podman")
                                     "system" "service" "--time=0"
                                     (string-append "unix://" sock-dir "/podman.sock"))
                               #:log-file
                               (string-append (getenv "XDG_STATE_HOME")
                                              "/log/podman-socket.log"))))))
                (stop #~(make-kill-destructor))
                (respawn? #t))

               ;; ssh-agent
               (shepherd-service
                (provision '(ssh-agent))
                (documentation "Run ssh-agent.")
                (start #~(make-forkexec-constructor
                          (list #$(file-append openssh "/bin/ssh-agent")
                                "-D" "-a"
                                (string-append (getenv "XDG_RUNTIME_DIR")
                                               "/ssh-agent.socket"))
                          #:log-file
                          (string-append (getenv "XDG_STATE_HOME")
                                         "/log/ssh-agent.log")))
                (stop #~(make-kill-destructor)))))))

   )))
