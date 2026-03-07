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
             (gnu home services xdg)
             (gnu home services desktop)
             (gnu home services sound)
             (gnu packages shellutils)
             (guix gexp)
             (packages package-groups)
             (home services quickshell)
             (home services wayland)
             (home services emacs)
             (home services caelestia)
             (home services spicetify)
             (home services portal)
             (home services polkit)
             (home services podman)
             (home services keyring)
             (home services ssh-agent)
             (home services flatpak)
             (home services uv-python)
             (home services zen-browser)
             (home services dotfiles-symlinks)
             (home services dconf))

(home-environment

 (packages
  (append %core-packages %terminal-packages %editor-packages
          %font-packages %desktop-packages %media-packages
          %texlive-packages %dev-packages %misc-packages))

 (services
  (list

   ;; Home-directory dotfiles
   (service home-files-service-type
            `((".gtkrc-2.0"
               ,(local-file "../files/gtk/gtkrc-2.0" "gtkrc-2.0"))
              (".local/bin"
               ,(local-file "../scripts" #:recursive? #t))
              (".local/share/applications/emacsclient.desktop"
               ,(local-file "../share/applications/emacsclient.desktop"))
              (".face"
               ,(local-file "../files/emacs/img/sloth-head.jpg" "face"))

))

   ;; XDG MIME defaults & desktop entries
   (service home-xdg-mime-applications-service-type
            (home-xdg-mime-applications-configuration
             (default '((inode/directory . emacsclient.desktop)
                        (x-scheme-handler/http . app.zen_browser.zen.desktop)
                        (x-scheme-handler/https . app.zen_browser.zen.desktop)
                        (text/html . app.zen_browser.zen.desktop)
                        (application/xhtml+xml . app.zen_browser.zen.desktop)
                        (application/pdf . app.zen_browser.zen.desktop)
                        (x-scheme-handler/slack . slack.desktop)))
             (desktop-entries
              (list
               (xdg-desktop-entry
                (file "app.zen_browser.zen")
                (name "Zen Browser")
                (type 'application)
                (config
                 '((exec . "flatpak run app.zen_browser.zen %u")
                   (icon . "app.zen_browser.zen")
                   (categories "Network" "WebBrowser")
                   (mime-type "text/html" "text/xml" "application/xhtml+xml"
                              "x-scheme-handler/http" "x-scheme-handler/https"
                              "application/x-xpinstall" "application/pdf"
                              "application/json")
                   (startup-notify . #t)
                   (startup-w-m-class . "zen")))
                (actions
                 (list
                  (xdg-desktop-action
                   (action 'new-window)
                   (name "Open a New Window")
                   (config '((exec . "flatpak run app.zen_browser.zen --new-window %u"))))
                  (xdg-desktop-action
                   (action 'new-private-window)
                   (name "Open a New Private Window")
                   (config '((exec . "flatpak run app.zen_browser.zen --private-window %u")))))))))))

   ;; XDG config files (~/.config/)
   (service home-xdg-configuration-files-service-type
            `(("uv/uv.toml"
               ,(local-file "../files/uv/uv.toml" "uv.toml"))
              ("electron-flags.conf"
               ,(local-file "../files/electron-flags.conf"))))

   ;; Other XDG config files are symlinked directly from dotfiles in the
   ;; activation gexp below so edits take effect immediately without reconfigure.

   ;; Environment variables (consolidates .zshenv)
   (simple-service 'my-env-vars
                   home-environment-variables-service-type
                   (append
                    (list (cons "DOTFILES_DIR" %dotfiles-dir)
                          (cons "GUIX_PACKAGE_PATH" %dotfiles-dir))
                    '(("EDITOR" . "emacs -nw")
                     ("TERM" . "xterm-256color")
                     ("PATH" . "$PATH:$HOME/.local/bin:$HOME/.local/share/uv/bin:$HOME/.cargo/bin:$HOME/.local/share/go/bin")
                     ("GOPATH" . "$HOME/.local/share/go")
                     ("NPM_CONFIG_PREFIX" . "$HOME/.local/share/npm")
                     ("SSH_AUTH_SOCK" . "$XDG_RUNTIME_DIR/ssh-agent.socket")
                     ;; gcr-ssh-askpass is used as a GUI fallback for passphrase
                     ;; prompts when there is no controlling TTY (e.g. autostart).
                     ;; SSH_ASKPASS_REQUIRE is intentionally omitted so that
                     ;; ssh-add uses the terminal when one is available — this is
                     ;; required for PKCS#11 PIN prompts (gcr-ssh-askpass cannot
                     ;; handle them).
                     ("SSH_ASKPASS" . "$HOME/.guix-home/profile/libexec/gcr-ssh-askpass")
                     ("KEYRING_SOCK" . "$XDG_RUNTIME_DIR/keyring")
                     ;; Flatpak dirs not covered by search-paths
                     ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share")
                     ;; Emacs native-comp subdirs (distinct from search-path share/emacs/site-lisp)
                     ("EMACSLOADPATH" . "$HOME/.guix-home/profile/lib/emacs:")
                     ;; NVIDIA driver selection (search-paths handle library discovery)
                     ("LIBVA_DRIVER_NAME" . "nvidia")
                     ("GBM_BACKEND" . "nvidia-drm")
                     ("__GLX_VENDOR_LIBRARY_NAME" . "nvidia")
                     ("__GL_GSYNC_ALLOWED" . "1")
                     ("__GL_VRR_ALLOWED" . "1")
                     ("LD_LIBRARY_PATH" . "/run/current-system/profile/lib:$HOME/.guix-home/profile/lib")
                     ;; Dev Containers: point to rootless podman socket
                     ("DOCKER_HOST" . "unix://$XDG_RUNTIME_DIR/podman/podman.sock")
                     ("DOCKER_API_VERSION" . "1.41")
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
                     ("NVD_BACKEND" . "direct"))))

   ;; Zsh
   (service home-zsh-service-type
            (home-zsh-configuration
             (zshenv
              (list (plain-file "zshenv" "setopt NULL_GLOB\n")))
             (zshrc
              (list (mixed-text-file "zsh-plugins"
                     "source " zsh-autosuggestions "/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh\n"
                     "source " zsh-autopair "/share/zsh/plugins/zsh-autopair/zsh-autopair.zsh\n"
                     "source " zsh-history-substring-search "/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh\n"
                     ;; syntax-highlighting must be last
                     "source " zsh-syntax-highlighting "/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh\n")
                    (local-file "../files/zsh/zshrc" "zshrc")
                    ;; fzf-tab must load after compinit (which runs in zshrc)
                    (mixed-text-file "zsh-fzf-tab"
                     "source " fzf-tab "/share/zsh/plugins/fzf-tab/fzf-tab.zsh\n")))))

   ;; Automated setup (idempotent, runs on every reconfigure)
   (home-dotfiles-symlinks-service %dotfiles-dir)

   ;; GTK icon/theme via dconf (settings.ini is overridden by dconf)
   (simple-service 'gtk-dconf home-dconf-service-type
                   (list (dconf-entry
                          (key "/org/gnome/desktop/interface/icon-theme")
                          (value "'Gruvbox-Plus-Dark'"))
                         (dconf-entry
                          (key "/org/gnome/desktop/interface/gtk-theme")
                          (value "'gruvbox-dark-gtk'"))))

   ;; D-Bus user session (required by PipeWire)
   (service home-dbus-service-type)

   ;; Audio (PipeWire + WirePlumber + PulseAudio emulation)
   (service home-pipewire-service-type)

   ;; Quickshell desktop shell (caelestia config)
   (service home-quickshell-service-type)

   ;; Shepherd user services
   (service home-wayland-compositor-service-type)
   (service home-emacs-daemon-service-type)
   (service home-caelestia-resizer-service-type)
   (service home-spicetify-setup-service-type %dotfiles-dir)
   (service home-caelestia-spicetify-watcher-service-type)
   (service home-xdph-service-type)
   (service home-hyprpolkitagent-service-type)
   (service home-podman-socket-service-type)
   (service home-gnome-keyring-service-type)
   (service home-ssh-agent-service-type)

   ;; Activation services
   (service home-flatpak-service-type)
   (service home-uv-python-service-type)
   (service home-zen-browser-service-type)

   )))
