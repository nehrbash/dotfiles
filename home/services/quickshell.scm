(define-module (home services quickshell)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages linux)        ; lm-sensors
  #:use-module (gnu packages xorg)         ; xkeyboard-config
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (packages quickshell)
  #:export (home-quickshell-configuration
            home-quickshell-service-type))

;;;
;;; Configuration record.
;;;

(define-configuration/no-serialization home-quickshell-configuration
  (package
   (file-like quickshell-git)
   "The quickshell package to use.")
  (config
   (string "caelestia")
   "Name of the quickshell configuration to launch (passed to @option{-c}).")
  (icon-theme
   (string "Gruvbox-Plus-Dark")
   "Value for the @env{QS_ICON_THEME} environment variable."))

;;;
;;; Shepherd service.
;;;

(define (home-quickshell-shepherd-service config)
  (let ((qs         (home-quickshell-configuration-package config))
        (cfg        (home-quickshell-configuration-config config))
        (icon-theme (home-quickshell-configuration-icon-theme config)))
    (list
     (shepherd-service
      (provision (list (string->symbol (string-append cfg "-shell"))))
      (requirement '(wayland-compositor))
      (documentation
       (string-append "Run the " cfg " quickshell desktop shell."))
      (start
       #~(lambda _
           ((make-forkexec-constructor
             (list #$(file-append qs "/bin/quickshell")
                   "-c" #$cfg "-n")
             #:environment-variables
             (cons*
              ;; Qt plugins from the Guix Home profile (e.g. qtwayland).
              (string-append "QT_PLUGIN_PATH="
                             (getenv "HOME")
                             "/.guix-home/profile/lib/qt6/plugins")
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
                                 (string-prefix? "QT_QPA_PLATFORM=" e)
                                 (string-prefix? "WAYLAND_DISPLAY=" e)
                                 (string-prefix? "PATH=" e))))
                      (environ)))
             #:log-file
             (string-append (getenv "XDG_STATE_HOME")
                            "/log/" #$cfg "-shell.log")))))
      (stop #~(make-kill-destructor))
      (respawn? #t)
      (respawn-limit #~'(3 . 10))
      (respawn-delay 5)))))

;;;
;;; Profile — ensure the package is installed.
;;;

(define (home-quickshell-profile-service config)
  (list (home-quickshell-configuration-package config)))

;;;
;;; Service type.
;;;

(define home-quickshell-service-type
  (service-type
   (name 'home-quickshell)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-quickshell-shepherd-service)
          (service-extension home-profile-service-type
                             home-quickshell-profile-service)))
   (default-value (home-quickshell-configuration))
   (description
    "Run a quickshell desktop shell configuration as a Shepherd service.
The shell is (re)started automatically on login and respawned if it
exits unexpectedly.")))
