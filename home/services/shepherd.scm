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
  #:use-module (gnu packages gnome)
  #:use-module (guix gexp)
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

   ;; GNOME Keyring daemon — exposes org.freedesktop.secrets D-Bus API
   ;; Creates /run/user/1000/keyring/ socket used by VS Code, libsecret, etc.
   (shepherd-service
    (provision '(gnome-keyring))
    (documentation "Run gnome-keyring-daemon (secrets component only).")
    (start #~(make-forkexec-constructor
              (list #$(file-append gnome-keyring "/bin/gnome-keyring-daemon")
                    "--start"
                    "--foreground"
                    "--components=secrets")
              #:log-file
              (string-append (getenv "XDG_STATE_HOME")
                             "/log/gnome-keyring.log")))
    (stop #~(make-kill-destructor))
    (respawn? #t)
    (respawn-limit #~'(3 . 10))
    (respawn-delay 5))

   ;; ssh-agent
   ;; The -P flag whitelists PKCS#11 providers that ssh-agent will accept from
   ;; `ssh-add -s`.  OpenSSH 8.9+ refuses any provider not matching the
   ;; pattern; libykcs11.so lives in the Guix store (symlinked via
   ;; ~/.guix-home/profile/lib/libykcs11.so) so we must allow that glob.
   (shepherd-service
    (provision '(ssh-agent))
    (documentation "Run ssh-agent.")
    (start #~(make-forkexec-constructor
              (list #$(file-append openssh "/bin/ssh-agent")
                    "-D" "-a"
                    (string-append (getenv "XDG_RUNTIME_DIR")
                                   "/ssh-agent.socket")
                    "-P" "/gnu/store/*/lib/libykcs11.so*")
              #:log-file
              (string-append (getenv "XDG_STATE_HOME")
                             "/log/ssh-agent.log")))
    (stop #~(make-kill-destructor)))))
