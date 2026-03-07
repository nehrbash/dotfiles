(define-module (home services portal)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gtk)
  #:use-module (guix gexp)
  #:use-module (home services shepherd-helpers)
  #:export (home-xdph-service-type))

(define (home-xdph-shepherd-service _)
  (list
   (make-simple-shepherd-service
    'xdg-desktop-portal-hyprland
    #~(list #$(file-append xdg-desktop-portal-hyprland
                           "/libexec/xdg-desktop-portal-hyprland"))
    #:requirement '(wayland-compositor)
    #:documentation "Run xdg-desktop-portal-hyprland for screen sharing."
    #:environment-variables
    #~(filter (lambda (s)
                (not (string-prefix? "LD_LIBRARY_PATH=" s)))
              (environ)))))

(define (home-xdph-profile-service _)
  (list xdg-desktop-portal-hyprland xdg-desktop-portal-gtk))

(define home-xdph-service-type
  (service-type
   (name 'home-xdph)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-xdph-shepherd-service)
          (service-extension home-profile-service-type
                             home-xdph-profile-service)))
   (default-value #f)
   (description
    "Run xdg-desktop-portal-hyprland as a shepherd service (D-Bus
activation fails on Guix because the .service file references systemd).")))
