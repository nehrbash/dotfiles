(define-module (home services keyring)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages gnome)
  #:use-module (guix gexp)
  #:use-module (home services shepherd-helpers)
  #:export (home-gnome-keyring-service-type))

(define (home-gnome-keyring-shepherd-service _)
  (list
   (make-simple-shepherd-service
    'gnome-keyring
    #~(list #$(file-append gnome-keyring "/bin/gnome-keyring-daemon")
            "--start" "--foreground" "--components=secrets")
    #:documentation "Run gnome-keyring-daemon (secrets component only).")))

(define (home-gnome-keyring-profile-service _)
  (list gnome-keyring))

(define home-gnome-keyring-service-type
  (service-type
   (name 'home-gnome-keyring)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-gnome-keyring-shepherd-service)
          (service-extension home-profile-service-type
                             home-gnome-keyring-profile-service)))
   (default-value #f)
   (description
    "Run gnome-keyring-daemon exposing the org.freedesktop.secrets D-Bus
API for VS Code, libsecret, etc.")))
