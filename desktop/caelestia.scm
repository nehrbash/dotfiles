;; Caelestia/Hyprland desktop meta-module.
;;
;; Bundles everything needed to run the Caelestia (Quickshell) desktop on
;; Hyprland as a single importable module, in the spirit of
;; (gnu services desktop) / gnome-desktop-service-type:
;;
;;   - The `quickshell-caelestia-plugin' C++/QML package.
;;   - The `home-quickshell-service-type' that launches the shell.
;;   - `%caelestia-desktop-packages' — everything the shell expects on PATH
;;     or in the Qt/GTK profile (fonts, wayland utils, cursor/icon themes,
;;     audio visualization libs, the upstream quickshell binary, the
;;     caelestia QML plugin, ...).
;;   - `%caelestia-home-services' — Shepherd user services the shell needs
;;     on any host (wayland-compositor sentinel, quickshell itself, the
;;     caelestia resizer daemon, the spicetify theme watcher).
;;   - `%caelestia-guix-session-services' — extra session services only
;;     needed on Guix System hosts (on Arch/bluefish these are provided by
;;     systemd): dbus, pipewire, xdph, hyprpolkitagent.
;;
;; Host home-environments import this module and splice the desired bits
;; into their own packages/services lists.

(define-module (desktop caelestia)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages cdrom)              ; ddcutil
  #:use-module (gnu packages cpp)                ; cli11
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)        ; xdg-desktop-portal-*
  #:use-module (gnu packages gcc)                ; gcc-14
  #:use-module (gnu packages gl)                 ; mesa
  #:use-module (gnu packages gnome)              ; libnotify, gnome-keyring
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages image)              ; slurp
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)              ; brightnessctl, pipewire, lm-sensors
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)              ; playerctl
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)             ; hyprpolkitagent
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages wm)                 ; hyprland, fuzzel, ...
  #:use-module (gnu packages xdisorg)            ; wl-clipboard, grim, hyprpicker, ...
  #:use-module (gnu packages xorg)               ; xkeyboard-config, libxcb
  #:use-module (gnu services configuration)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (home services caelestia)
  #:use-module (home services shepherd-helpers)
  #:use-module (home services wayland)
  #:use-module (packages fonts)                  ; caelestia fonts (local)
  #:use-module (packages gpu-screen-recorder)
  #:use-module (packages gruvbox)
  #:use-module (packages libcava)
  #:export (quickshell-git
            quickshell-caelestia-plugin
            home-quickshell-configuration
            home-quickshell-service-type
            home-xdph-service-type
            home-hyprpolkitagent-service-type
            home-gnome-keyring-service-type
            %default-quickshell-plugins
            %caelestia-desktop-packages
            home-keyboard-sound-service-type
            %caelestia-home-services
            %caelestia-guix-session-services))


;;;
;;; Quickshell (git) — built from the local clone at ~/src/quickshell.
;;;
;; The upstream Guix @code{quickshell} package (currently 0.2.1) predates
;; @code{Quickshell.Wayland.IdleInhibitor}, which the Caelestia shell
;; config requires.  We also need the wrapper to preserve the runtime
;; @env{QML_IMPORT_PATH} so the local Caelestia QML plugin (installed
;; into the Guix Home profile) is discoverable — upstream Guix hard-
;; overrides @env{QML_IMPORT_PATH} in its wrapper.
(define quickshell-git
  (package
    (name "quickshell")
    (version "git")
    (source (local-file "../../quickshell"
                        "quickshell-checkout"
                        #:recursive? #t))
    (build-system cmake-build-system)
    (propagated-inputs (list qtbase qtdeclarative qtsvg))
    (native-inputs (list ninja
                         gcc-14
                         pkg-config
                         qtshadertools
                         spirv-tools
                         wayland-protocols
                         cli11))
    (inputs (list jemalloc
                  libdrm
                  libxcb
                  libxkbcommon
                  linux-pam
                  polkit
                  mesa
                  pipewire
                  qtbase
                  qtdeclarative
                  qtwayland
                  vulkan-headers
                  wayland))
    (arguments
     (list #:tests? #f
           #:configure-flags
           #~(list "-GNinja"
                   "-DDISTRIBUTOR=\"dotfiles local build\""
                   "-DDISTRIBUTOR_DEBUGINFO_AVAILABLE=NO"
                   ;; Newer upstream warns if this is unset.
                   "-DINSTALL_QMLDIR=lib/qt6/qml"
                   ;; cpptrace isn't packaged for Guix.
                   "-DCRASH_HANDLER=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'build (lambda _ (invoke "cmake" "--build" ".")))
               (replace 'install (lambda _ (invoke "cmake" "--install" ".")))
               (add-after 'install 'wrap-program
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; `prefix' semantics: the runtime value of
                   ;; QML_IMPORT_PATH is prepended to the baked-in one,
                   ;; so setting QML_IMPORT_PATH in the shepherd service
                   ;; lets the Caelestia plugin be discovered.
                   (wrap-program (string-append #$output "/bin/quickshell")
                     `("QML_IMPORT_PATH" ":" prefix
                       (,(getenv "QML_IMPORT_PATH")))))))))
    (home-page "https://quickshell.outfoxxed.me")
    (synopsis "QtQuick-based desktop shell toolkit (git version)")
    (description
     "Quickshell is a flexible QtQuick-based toolkit for creating and
customizing toolbars, notification centers, and other desktop
environment tools in a live programming environment.  This is the git
version built from a local clone in @file{~/src/quickshell}.")
    (license license:lgpl3)))


;;;
;;; Caelestia QML plugin (local build — not in upstream Guix).
;;;

;; C++ QML plugin for the Caelestia shell config. Provides audio
;; visualization, calculator, and other native services consumed by the
;; QML shell in files/quickshell/.
(define quickshell-caelestia-plugin
  (package
    (name "quickshell-caelestia-plugin")
    (version "1.5.0")
    (source
     (local-file "../files/quickshell"
                 "caelestia-shell-source"
                 #:recursive? #t))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list
         "-DENABLE_MODULES=plugin"
         "-DINSTALL_QMLDIR=lib/qt6/qml"
         (string-append "-DVERSION=" #$version)
         "-DGIT_REVISION=local")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list qtbase
           qtdeclarative
           libqalculate
           gmp
           mpfr
           pipewire
           aubio
           libcava))
    (home-page "https://github.com/caelestia-dots/shell")
    (synopsis "Caelestia QML plugin for Quickshell")
    (description
     "C++ QML plugin providing audio visualization, calculator, and other
services for the Caelestia desktop shell running on Quickshell.")
    (license license:gpl3+)))


;;;
;;; home-quickshell-service-type — launch the Caelestia shell.
;;;

(define %default-quickshell-plugins
  (list quickshell-caelestia-plugin))

(define-configuration/no-serialization home-quickshell-configuration
  (package
   (file-like quickshell-git)
   "The quickshell package to use.")
  (config
   (string "")
   "Name of the quickshell configuration to launch (passed to @option{-c}).
Leave empty (the default) to use the default config at
@file{~/.config/quickshell/shell.qml}.")
  (plugins
   (list %default-quickshell-plugins)
   "Quickshell plugin packages to install into the profile.  Defaults to
the local @code{quickshell-caelestia-plugin}; override with @code{'()} on
hosts where the plugin is incompatible with the chosen quickshell build.")
  (icon-theme
   (string "Gruvbox-Plus-Dark")
   "Value for the @env{QS_ICON_THEME} environment variable."))

(define (home-quickshell-shepherd-service config)
  (let ((qs         (home-quickshell-configuration-package config))
        (cfg        (home-quickshell-configuration-config config))
        (icon-theme (home-quickshell-configuration-icon-theme config)))
    (list
     (shepherd-service
      (provision '(quickshell))
      (requirement '(wayland-compositor))
      (documentation "Run quickshell desktop shell.")
      (start
       #~(lambda _
           ((make-forkexec-constructor
             (append
              (list #$(file-append qs "/bin/quickshell"))
              (if (string-null? #$cfg) '() (list "-c" #$cfg))
              (list "-n"))
             #:environment-variables
             (cons*
              ;; Qt plugins from the Guix Home profile (e.g. qtwayland).
              (string-append "QT_PLUGIN_PATH="
                             (getenv "HOME")
                             "/.guix-home/profile/lib/qt6/plugins")
              ;; QML modules from the profile — picks up the Caelestia
              ;; plugin (@code{quickshell-caelestia-plugin}).  The
              ;; quickshell-git wrapper merges this with its build-time
              ;; QML path via `prefix' semantics.
              (string-append "QML_IMPORT_PATH="
                             (getenv "HOME")
                             "/.guix-home/profile/lib/qt6/qml")
              "QT_QPA_PLATFORM=wayland"
              (string-append "QS_ICON_THEME=" #$icon-theme)
              (string-append "WAYLAND_DISPLAY="
                             (or (getenv "WAYLAND_DISPLAY") "wayland-1"))
              ;; lm-sensors needed by the system-info widget.
              (string-append "PATH="
                             #$(file-append lm-sensors "/bin")
                             ":" (getenv "PATH"))
              ;; XKB rules path for the keyboard layout widget.
              (string-append "CAELESTIA_XKB_RULES_PATH="
                             #$(file-append xkeyboard-config
                                            "/share/X11/xkb/rules/base.lst"))
              ;; Strip vars we are overriding above from the inherited env.
              (filter (lambda (e)
                        (not (or (string-prefix? "QT_PLUGIN_PATH=" e)
                                 (string-prefix? "QML_IMPORT_PATH=" e)
                                 (string-prefix? "QT_QPA_PLATFORM=" e)
                                 (string-prefix? "WAYLAND_DISPLAY=" e)
                                 (string-prefix? "PATH=" e))))
                      (environ)))
             #:log-file
             (string-append (getenv "XDG_STATE_HOME")
                            "/log/quickshell.log")))))
      (stop #~(make-kill-destructor))
      (respawn? #t)
      (respawn-limit #~'(3 . 10))
      (respawn-delay 5)))))

(define (home-quickshell-profile-service config)
  (cons* (home-quickshell-configuration-package config)
         qtwayland
         (home-quickshell-configuration-plugins config)))

(define home-quickshell-service-type
  (service-type
   (name 'home-quickshell)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-quickshell-shepherd-service)
          (service-extension home-profile-service-type
                             home-quickshell-profile-service)))
   (default-value (home-quickshell-configuration
                   (config "caelestia")))
   (description
    "Run a quickshell desktop shell configuration as a Shepherd service.
The shell is (re)started automatically on login and respawned if it
exits unexpectedly.")))


;;;
;;; Desktop package list.
;;;

(define %caelestia-desktop-packages
  (append
   (list
    ;; --- Compositor & core Wayland/Hyprland tooling ----------------
    hyprland
    libnotify
    wl-clipboard grim slurp swappy hyprpicker cliphist
    fuzzel
    playerctl brightnessctl wlr-randr ddcutil
    gpu-screen-recorder
    xdg-utils xkeyboard-config

    ;; --- Cursor / icon / GTK themes --------------------------------
    bibata-cursor-theme
    gruvbox-dark-gtk
    gruvbox-plus-icon-theme

    ;; --- Fonts required by the Caelestia shell config --------------
    ;;   sans/clock = Rubik, mono = CaskaydiaCove NF,
    ;;   material   = Material Symbols Rounded.
    font-rubik
    font-caskaydia-cove-nf
    font-material-symbols-rounded

    ;; --- Audio visualization / calculator deps ---------------------
    cava libcava aubio
    libqalculate
    pavucontrol)

   ;; --- Quickshell itself plus the local Caelestia QML plugin ------
   (list quickshell-git
         quickshell-caelestia-plugin)))


;;;
;;; Session helper services (Guix System only — on Arch systemd owns
;;; these).  Each service-type is a thin shepherd wrapper around an
;;; upstream package binary and is only used by this meta-module.
;;;

;; xdg-desktop-portal-hyprland: D-Bus activation fails on Guix because
;; the upstream .service file references systemd, so we run it under
;; shepherd instead.
(define home-xdph-service-type
  (service-type
   (name 'home-xdph)
   (extensions
    (list (service-extension home-shepherd-service-type
                             (lambda _
                               (list
                                (make-simple-shepherd-service
                                 'xdg-desktop-portal-hyprland
                                 #~(list #$(file-append
                                            xdg-desktop-portal-hyprland
                                            "/libexec/xdg-desktop-portal-hyprland"))
                                 #:requirement '(wayland-compositor)
                                 #:documentation
                                 "Run xdg-desktop-portal-hyprland for screen sharing."
                                 #:environment-variables
                                 #~(filter
                                    (lambda (s)
                                      (not (string-prefix? "LD_LIBRARY_PATH=" s)))
                                    (environ))))))
          (service-extension home-profile-service-type
                             (lambda _
                               (list xdg-desktop-portal-hyprland
                                     xdg-desktop-portal-gtk)))))
   (default-value #f)
   (description "Run xdg-desktop-portal-hyprland under shepherd.")))

;; hyprpolkitagent: polkit authentication prompts for Hyprland.
(define home-hyprpolkitagent-service-type
  (service-type
   (name 'home-hyprpolkitagent)
   (extensions
    (list (service-extension home-shepherd-service-type
                             (lambda _
                               (list
                                (make-simple-shepherd-service
                                 'hyprpolkitagent
                                 #~(list #$(file-append hyprpolkitagent
                                                        "/libexec/hyprpolkitagent"))
                                 #:requirement '(wayland-compositor)
                                 #:documentation
                                 "Run hyprpolkitagent for polkit authentication."
                                 #:environment-variables
                                 #~(cons (string-append
                                          "QT_PLUGIN_PATH="
                                          #$(file-append qtwayland
                                                         "/lib/qt6/plugins")
                                          ":"
                                          #$(file-append qtbase
                                                         "/lib/qt6/plugins"))
                                         (environ))))))
          (service-extension home-profile-service-type
                             (lambda _ (list hyprpolkitagent)))))
   (default-value #f)
   (description "Run hyprpolkitagent for polkit authentication prompts.")))

;; gnome-keyring-daemon: exposes org.freedesktop.secrets over D-Bus for
;; libsecret consumers (VS Code, etc.).
(define home-gnome-keyring-service-type
  (service-type
   (name 'home-gnome-keyring)
   (extensions
    (list (service-extension home-shepherd-service-type
                             (lambda _
                               (list
                                (make-simple-shepherd-service
                                 'gnome-keyring
                                 #~(list #$(file-append gnome-keyring
                                                        "/bin/gnome-keyring-daemon")
                                         "--start" "--foreground"
                                         "--components=secrets")
                                 #:documentation
                                 "Run gnome-keyring-daemon (secrets component)."))))
          (service-extension home-profile-service-type
                             (lambda _ (list gnome-keyring)))))
   (default-value #f)
   (description
    "Run gnome-keyring-daemon exposing the org.freedesktop.secrets D-Bus
API for VS Code, libsecret, etc.")))


;;;
;;; Keyboard hotplug sounds — connect/disconnect chimes.
;;;

(define keyboard-sound-monitor-script
  (program-file "keyboard-sound-monitor"
    #~(begin
        (use-modules (ice-9 popen)
                     (ice-9 rdelim))
        (let ((port (open-input-pipe
                     (string-append #$(file-append eudev "/bin/udevadm")
                                    " monitor --subsystem-match=input"
                                    " --property --udev")))
              (connect-sound    #$(local-file "../files/music/device-connect.wav"))
              (disconnect-sound #$(local-file "../files/music/device-disconnect.wav"))
              (pw-play          #$(file-append pipewire "/bin/pw-play")))
          (let loop ((block '())
                     (last-play 0))
            (let ((line (read-line port)))
              (cond
               ((eof-object? line) #t)
               ((string-null? (string-trim-both line))
                ;; End of event block — check for keyboard event with cooldown.
                (let* ((now (current-time))
                       (is-keyboard (member "ID_INPUT_KEYBOARD=1" block))
                       (is-add     (member "ACTION=add" block))
                       (is-remove  (member "ACTION=remove" block))
                       (play?      (and is-keyboard
                                        (or is-add is-remove)
                                        (> (- now last-play) 2))))
                  (when play?
                    (let ((pid (primitive-fork)))
                      (when (zero? pid)
                        (execl pw-play "pw-play"
                               (if is-add connect-sound disconnect-sound))
                        (primitive-exit 1))))
                  (loop '() (if play? now last-play))))
               (else
                (loop (cons (string-trim-both line) block)
                      last-play)))))))))

(define home-keyboard-sound-service-type
  (service-type
   (name 'home-keyboard-sound)
   (extensions
    (list (service-extension home-shepherd-service-type
                             (lambda _
                               (list
                                (make-simple-shepherd-service
                                 'keyboard-sound
                                 #~(list #$keyboard-sound-monitor-script)
                                 #:documentation
                                 "Play a sound on keyboard hotplug."))))))
   (default-value #f)
   (description "Monitor udev for keyboard hotplug events and play connect/disconnect sounds.")))


;;;
;;; Home service lists.
;;;

;; Services needed on every host that runs the Caelestia shell.
(define %caelestia-home-services
  (list (service home-wayland-compositor-service-type)
        (service home-quickshell-service-type)
        (service home-caelestia-resizer-service-type)
        (service home-caelestia-spicetify-watcher-service-type)
        (service home-keyboard-sound-service-type)))

;; Extra session services only needed on Guix System hosts.  On Arch
;; (bluefish) systemd provides dbus, pipewire, gnome-keyring, polkit, ...
;; so these must be omitted there.
(define %caelestia-guix-session-services
  (list (service home-dbus-service-type)
        (service home-pipewire-service-type)
        (service home-xdph-service-type)
        (service home-hyprpolkitagent-service-type)
        (service home-gnome-keyring-service-type)))
