(use-modules (gnu)
             (gnu home)
             (gnu home services)
             (gnu home services xdg)
             (gnu home services shells)
             (gnu home services shepherd)
             (gnu packages)
             (gnu packages version-control)
             (gnu packages emacs)
             (gnu packages ssh)
             (gnu packages admin)
             (gnu packages terminals)
             (gnu packages video)
             (gnu packages fonts)
             (gnu packages aspell)
             (gnu packages text-editors)
             (gnu packages rust)
             (gnu packages rust-apps)
             (gnu packages curl)
             (gnu packages shellutils)
             (gnu packages golang)
             (gnu packages wm)
             (gnu packages xdisorg)
             (gnu packages glib)
             (gnu packages gtk)
             (gnu packages pkg-config)
             (gnu packages music)
             (gnu packages mail)
             (gnu packages texlive)
             (gnu packages haskell-apps)
             (gnu packages tex)
             (gnu packages compression)
             (gnu packages wget)
             (gnu packages web)
             (gnu packages node)
             (gnu packages freedesktop)
             (gnu packages package-management)
             (gnu packages image)
             (gnu packages linux)
             (gnu packages pulseaudio)
             (gnu home services desktop)
             (gnu home services sound)
             (guix gexp)
             (config packages zsh-pure-prompt))

(home-environment

 (packages
  (list
   ;; Core tools
   git
   curl
   wget
   ripgrep
   jq

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

   ;; Audio
   pavucontrol

   ;; Video
   vlc

   ;; Spellcheck
   aspell
   aspell-dict-en

   ;; Mail
   isync

   ;; TeX
   texlive-scheme-full

   ;; Languages
   go
   rust
   (list rust "cargo")

   ;; Dev tools
   shellcheck

   ;; SSH
   openssh

   ;; Flatpak
   flatpak))

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
              (".local/share/fonts/weathericons.ttf"
               ,(local-file "../../share/fonts/weathericons.ttf"))
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
              ("hypr/monitors.conf"
               ,(local-file "../../hypr/monitors.conf"))
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
              ("nwg-displays"
               ,(local-file "../../nwg-displays" #:recursive? #t))
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
                     ("PATH" . "$PATH:$HOME/.local/bin:$HOME/go/bin:$HOME/.cargo/bin:$HOME/.npm-global/bin")
                     ("SSH_AUTH_SOCK" . "$XDG_RUNTIME_DIR/ssh-agent.socket")
                     ("PKG_CONFIG_PATH" . "/usr/lib/pkgconfig:$PKG_CONFIG_PATH")
                     ("GUIX_PACKAGE_PATH" . "$HOME/dotfiles")))

   ;; Zsh
   (service home-zsh-service-type
            (home-zsh-configuration
             (zshenv
              (list (plain-file "zshenv" "setopt NULL_GLOB\n")))
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
                          "com.slack.Slack"))

                       ;; hyprshell: build if binary missing
                       (let ((bin (string-append (getenv "HOME")
                                                 "/go/bin/hyprshell")))
                         (unless (file-exists? bin)
                           (mkdir-p (dirname bin))
                           (setenv "GOBIN" (dirname bin))
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
                       (let ((bin (string-append (getenv "HOME") "/.npm-global/bin/claude")))
                         (unless (file-exists? bin)
                           (setenv "npm_config_prefix"
                                   (string-append (getenv "HOME") "/.npm-global"))
                           (system* #$(file-append node "/bin/npm")
                                    "install" "-g" "@anthropic-ai/claude-code")))))

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
