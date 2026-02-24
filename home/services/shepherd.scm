(define-module (home services shepherd)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xorg)
  #:use-module (guix gexp)
  #:use-module (packages caelestia-shell)
  #:use-module (packages quickshell)
  #:export (%shepherd-services))

(define %shepherd-services
  (list

   ;; Emacs daemon
   (shepherd-service
    (provision '(emacs))
    (requirement '(wayland-compositor))
    (documentation "Run Emacs as a daemon.")
    (start #~(make-forkexec-constructor
              (list #$(file-append emacs-next-pgtk "/bin/emacs")
                    "--fg-daemon")
              #:environment-variables
              (cons (string-append "WAYLAND_DISPLAY="
                                   (or (getenv "WAYLAND_DISPLAY") "wayland-1"))
                    (environ))
              #:log-file
              (string-append (getenv "XDG_STATE_HOME")
                             "/log/emacs.log")))
    (stop #~(make-kill-destructor))
    (respawn? #t)
    (respawn-limit #~'(3 . 10))
    (respawn-delay 5))

   ;; Wayland compositor sentinel — polls until a wayland socket AND
   ;; Hyprland IPC socket appear, sets WAYLAND_DISPLAY and
   ;; HYPRLAND_INSTANCE_SIGNATURE in shepherd's environment.
   (shepherd-service
    (provision '(wayland-compositor))
    (documentation "Wait for the Wayland compositor and Hyprland sockets.")
    (start #~(lambda _
               (let* ((runtime-dir (or (getenv "XDG_RUNTIME_DIR")
                                       (string-append "/run/user/"
                                                      (number->string (getuid)))))
                      (find-wayland (lambda ()
                                      (let lp ((names '("wayland-1" "wayland-0")))
                                        (cond
                                         ((null? names) #f)
                                         ((file-exists? (string-append runtime-dir "/" (car names)))
                                          (car names))
                                         (else (lp (cdr names)))))))
                      (find-hyprland (lambda ()
                                       (let ((hypr-dir (string-append runtime-dir "/hypr")))
                                         (and (file-exists? hypr-dir)
                                              (let* ((dir (opendir hypr-dir))
                                                     (result (let lp ()
                                                               (let ((entry (readdir dir)))
                                                                 (cond
                                                                  ((eof-object? entry) #f)
                                                                  ((member entry '("." "..")) (lp))
                                                                  (else entry))))))
                                                (closedir dir)
                                                result))))))
                 (let loop ((n 0))
                   (let ((wl (find-wayland))
                         (hypr (find-hyprland)))
                     (cond
                      ((and wl hypr)
                       (setenv "WAYLAND_DISPLAY" wl)
                       (setenv "HYPRLAND_INSTANCE_SIGNATURE" hypr)
                       #t)
                      ((> n 60) (error "wayland-compositor: timed out waiting for sockets"))
                      (else
                       (usleep 500000)
                       (loop (+ n 1)))))))))
    (one-shot? #t))

   ;; Polkit authentication agent
   (shepherd-service
    (provision '(hyprpolkitagent))
    (requirement '(wayland-compositor))
    (documentation "Run hyprpolkitagent for polkit authentication.")
    (start #~(lambda _
               ((make-forkexec-constructor
                 (list #$(file-append hyprpolkitagent
                                      "/libexec/hyprpolkitagent"))
                 #:environment-variables
                 (cons (string-append
                        "QT_PLUGIN_PATH="
                        #$(file-append qtwayland "/lib/qt6/plugins")
                        ":" #$(file-append (specification->package "qtbase") "/lib/qt6/plugins"))
                       (environ))
                 #:log-file
                 (string-append (getenv "XDG_STATE_HOME")
                                "/log/hyprpolkitagent.log")))))
    (stop #~(make-kill-destructor))
    (respawn? #t)
    (respawn-limit #~'(3 . 10))
    (respawn-delay 5))

   ;; Caelestia desktop shell (quickshell)
   (shepherd-service
    (provision '(caelestia-shell))
    (requirement '(wayland-compositor))
    (documentation "Run caelestia shell (quickshell-based desktop shell).")
    (start #~(lambda _
               ((make-forkexec-constructor
                 (list #$(file-append quickshell-git "/bin/quickshell")
                       "-c" "caelestia" "-n")
                 #:environment-variables
                 (cons* (string-append
                         "QT_PLUGIN_PATH="
                         (getenv "HOME") "/.guix-home/profile/lib/qt6/plugins")
                        "QT_QPA_PLATFORM=wayland"
                        "QS_ICON_THEME=Gruvbox-Plus-Dark"
                        (string-append
                         "WAYLAND_DISPLAY="
                         (or (getenv "WAYLAND_DISPLAY") "wayland-1"))
                        (string-append
                         "PATH="
                         #$(file-append (specification->package "lm-sensors") "/bin")
                         ":" (getenv "PATH"))
                        (string-append
                         "CAELESTIA_XKB_RULES_PATH="
                         #$(file-append (specification->package "xkeyboard-config")
                                        "/share/X11/xkb/rules/base.lst"))
                        (filter (lambda (e)
                                  (not (or (string-prefix? "QT_PLUGIN_PATH=" e)
                                           (string-prefix? "QT_QPA_PLATFORM=" e)
                                           (string-prefix? "WAYLAND_DISPLAY=" e)
                                           (string-prefix? "PATH=" e))))
                                (environ)))
                 #:log-file
                 (string-append (getenv "XDG_STATE_HOME")
                                "/log/caelestia-shell.log")))))
    (stop #~(make-kill-destructor))
    (respawn? #t)
    (respawn-limit #~'(3 . 10))
    (respawn-delay 5))

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
    (stop #~(make-kill-destructor)))))
