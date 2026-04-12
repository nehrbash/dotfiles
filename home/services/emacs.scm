(define-module (home services emacs)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages emacs)
  #:use-module (guix gexp)
  #:use-module (home services shepherd-helpers)
  #:export (home-emacs-daemon-service-type))

(define (home-emacs-daemon-shepherd-service _)
  (list
   (make-simple-shepherd-service
    'emacs
    #~(list #$(file-append emacs-next-pgtk "/bin/emacs") "--fg-daemon")
    #:requirement '(wayland-compositor)
    #:environment-variables
    #~(cons "GDK_BACKEND=wayland" (environ))
    #:documentation "Run Emacs as a daemon.")))

(define (home-emacs-daemon-profile-service _)
  (list emacs-next-pgtk))

(define home-emacs-daemon-service-type
  (service-type
   (name 'home-emacs-daemon)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-emacs-daemon-shepherd-service)
          (service-extension home-profile-service-type
                             home-emacs-daemon-profile-service)))
   (default-value #f)
   (description "Run Emacs as a foreground daemon under shepherd.")))
