;; Shared home-environment fragments used by every host.
;;
;; Each export is a small builder that takes host parameters and
;; returns a concrete service / env-vars / zsh record.  Hosts splice
;; them into their own service list and add machine-specific pieces on
;; top (NVIDIA, spicetify, ssh-agent options, etc.).
;;
;; The goal is a strict no-duplication policy between host configs:
;; anything that appears in both redfish.scm and bluefish.scm lives
;; here.

(define-module (home common)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages shellutils)
  #:use-module (guix gexp)
  #:use-module (home services dconf)
  #:use-module (home services spicetify)
  #:export (common-home-files-service
            common-xdg-mime-service
            common-xdg-configuration-files-service
            common-env-vars-service
            common-zsh-service
            common-gtk-dconf-service
            common-spicetify-service
            %common-base-env-vars
            %common-wayland-env-vars
            %common-qt-env-vars
            %common-cursor-env-vars))


;;;
;;; home-files-service-type — ~/ level files.
;;;

(define (common-home-files-service dotfiles-dir)
  "Deploy the small set of dotfiles that must live directly under
@code{$HOME} (not @code{$XDG_CONFIG_HOME}) on every host."
  (service home-files-service-type
           `((".gtkrc-2.0"
              ,(local-file (string-append dotfiles-dir
                                          "/files/gtk/gtkrc-2.0")
                           "gtkrc-2.0"))
             (".local/bin"
              ,(local-file (string-append dotfiles-dir "/scripts")
                           #:recursive? #t))
             (".local/share/applications/emacsclient.desktop"
              ,(local-file (string-append dotfiles-dir
                                          "/share/applications/emacsclient.desktop")))
             (".face"
              ,(local-file (string-append dotfiles-dir
                                          "/files/emacs/img/sloth-head.jpg")
                           "face")))))


;;;
;;; XDG MIME defaults + Zen Browser desktop entry.
;;;

(define %zen-browser-desktop-entry
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
      (config '((exec . "flatpak run app.zen_browser.zen --private-window %u"))))))))

(define (common-xdg-mime-service)
  "MIME defaults + the Zen Browser .desktop entry, shared by all hosts."
  (service home-xdg-mime-applications-service-type
           (home-xdg-mime-applications-configuration
            (default
             '((inode/directory . emacsclient.desktop)
               (x-scheme-handler/http . app.zen_browser.zen.desktop)
               (x-scheme-handler/https . app.zen_browser.zen.desktop)
               (text/html . app.zen_browser.zen.desktop)
               (application/xhtml+xml . app.zen_browser.zen.desktop)
               (application/pdf . app.zen_browser.zen.desktop)
               (x-scheme-handler/slack . slack.desktop)))
            (desktop-entries (list %zen-browser-desktop-entry)))))


;;;
;;; Miscellaneous XDG config files.
;;;

(define (common-xdg-configuration-files-service dotfiles-dir)
  "Deploy ~/.config files that don't belong to a specific program
service (uv, electron)."
  (service home-xdg-configuration-files-service-type
           `(("uv/uv.toml"
              ,(local-file (string-append dotfiles-dir
                                          "/files/uv/uv.toml")
                           "uv.toml"))
             ("electron-flags.conf"
              ,(local-file (string-append dotfiles-dir
                                          "/files/electron-flags.conf"))))))


;;;
;;; Environment variables — split into logical groups so hosts can
;;; pick and choose.  Hosts pass the unions they want into
;;; `common-env-vars-service'.
;;;

(define %common-base-env-vars
  '(("EDITOR" . "emacs -nw")
    ("TERM" . "xterm-256color")
    ("GOPATH" . "$HOME/.local/share/go")
    ("NPM_CONFIG_PREFIX" . "$HOME/.local/share/npm")
    ("SSH_AUTH_SOCK" . "$XDG_RUNTIME_DIR/ssh-agent.socket")
    ("KEYRING_SOCK" . "$XDG_RUNTIME_DIR/keyring")
    ("XDG_DATA_DIRS" .
     "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share")
    ("DOCKER_HOST" . "unix://$XDG_RUNTIME_DIR/podman/podman.sock")
    ("DOCKER_API_VERSION" . "1.41")))

(define %common-wayland-env-vars
  '(("XDG_CURRENT_DESKTOP" . "Hyprland")
    ("XDG_SESSION_DESKTOP" . "Hyprland")
    ("XDG_SESSION_TYPE" . "wayland")
    ("SDL_VIDEODRIVER" . "wayland")
    ("MOZ_ENABLE_WAYLAND" . "1")
    ("CLUTTER_BACKEND" . "wayland")
    ("ELECTRON_OZONE_PLATFORM_HINT" . "auto")
    ("_JAVA_AWT_WM_NONREPARENTING" . "1")))

(define %common-qt-env-vars
  '(("QT_AUTO_SCREEN_SCALE_FACTOR" . "1")
    ("QT_QPA_PLATFORM" . "wayland;xcb")
    ("QT_QPA_PLATFORMTHEME" . "qt5ct")
    ("QT_WAYLAND_DISABLE_WINDOWDECORATION" . "1")))

(define %common-cursor-env-vars
  '(("XCURSOR_THEME" . "Bibata-Modern-Amber")
    ("HYPRCURSOR_THEME" . "Bibata-Modern-Amber")
    ("XCURSOR_SIZE" . "24")
    ("HYPRCURSOR_SIZE" . "24")
    ("GDK_SCALE" . "1")))

(define* (common-env-vars-service dotfiles-dir #:optional (extra-vars '()))
  "Build the home-environment-variables service.  DOTFILES-DIR seeds
the @code{DOTFILES_DIR} / @code{GUIX_PACKAGE_PATH} pair; EXTRA-VARS is
an optional alist of host-specific entries appended after the common
set."
  (simple-service 'common-env-vars
                  home-environment-variables-service-type
                  (append
                   (list (cons "DOTFILES_DIR" dotfiles-dir)
                         (cons "GUIX_PACKAGE_PATH" dotfiles-dir))
                   %common-base-env-vars
                   %common-wayland-env-vars
                   %common-qt-env-vars
                   %common-cursor-env-vars
                   extra-vars)))


;;;
;;; Zsh.
;;;

(define (common-zsh-service dotfiles-dir)
  "Zsh configuration with the four plugins loaded in the right order
and the repo's zshrc sourced in the middle."
  (service home-zsh-service-type
           (home-zsh-configuration
            (zshenv
             (list (plain-file "zshenv" "setopt NULL_GLOB\n")))
            (zshrc
             (list
              (mixed-text-file "zsh-plugins"
               "source " zsh-autosuggestions
                 "/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh\n"
               "source " zsh-autopair
                 "/share/zsh/plugins/zsh-autopair/zsh-autopair.zsh\n"
               "source " zsh-history-substring-search
                 "/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh\n"
               ;; syntax-highlighting must be last.
               "source " zsh-syntax-highlighting
                 "/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh\n")
              (local-file (string-append dotfiles-dir "/files/zsh/zshrc")
                          "zshrc")
              ;; fzf-tab must load after compinit (which runs in zshrc).
              (mixed-text-file "zsh-fzf-tab"
               "source " fzf-tab
                 "/share/zsh/plugins/fzf-tab/fzf-tab.zsh\n"))))))


;;;
;;; Spicetify — caelestia theme for Spotify Flatpak.
;;;

(define (common-spicetify-service)
  "Spicetify overlay + theme application, shared by all hosts."
  (service home-spicetify-setup-service-type))


;;;
;;; GTK icon / theme via dconf.
;;;

(define (common-gtk-dconf-service)
  "dconf entries that set the GTK icon and theme (which override
settings.ini on modern GTK)."
  (simple-service 'gtk-dconf home-dconf-service-type
                  (list (dconf-entry
                         (key "/org/gnome/desktop/interface/icon-theme")
                         (value "'Gruvbox-Plus-Dark'"))
                        (dconf-entry
                         (key "/org/gnome/desktop/interface/gtk-theme")
                         (value "'gruvbox-dark-gtk'")))))
