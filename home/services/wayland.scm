(define-module (home services wayland)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (gnu packages glib)
  #:export (home-wayland-compositor-service-type))

(define (home-wayland-compositor-shepherd-service _)
  (list
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
                       (setenv "DISPLAY" (or (getenv "DISPLAY") ":0"))
                       (system* #$(file-append dbus "/bin/dbus-update-activation-environment")
                                "DISPLAY" "WAYLAND_DISPLAY"
                                "XDG_CURRENT_DESKTOP")
                       #t)
                      (else
                       (when (zero? (modulo n 120))
                         (display "wayland-compositor: still waiting for sockets...\n"))
                       (usleep 500000)
                       (loop (+ n 1)))))))))
    (one-shot? #t))))

(define home-wayland-compositor-service-type
  (service-type
   (name 'home-wayland-compositor)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-wayland-compositor-shepherd-service)))
   (default-value #f)
   (description
    "Wait for the Wayland compositor and Hyprland IPC sockets to appear,
then set WAYLAND_DISPLAY and HYPRLAND_INSTANCE_SIGNATURE in shepherd's
environment.")))
