(define-module (home services podman)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages containers)
  #:use-module (guix gexp)
  #:export (home-podman-socket-service-type))

(define (home-podman-socket-shepherd-service _)
  (list
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
    (respawn? #t))))

(define (home-podman-socket-profile-service _)
  (list podman (list podman "docker")))

(define home-podman-socket-service-type
  (service-type
   (name 'home-podman-socket)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-podman-socket-shepherd-service)
          (service-extension home-profile-service-type
                             home-podman-socket-profile-service)))
   (default-value #f)
   (description
    "Expose rootless podman as a Docker-compatible socket for
devcontainers and other Docker-API consumers.")))
