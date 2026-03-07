(define-module (home services polkit)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages qt)
  #:use-module (guix gexp)
  #:use-module (home services shepherd-helpers)
  #:export (home-hyprpolkitagent-service-type))

(define (home-hyprpolkitagent-shepherd-service _)
  (list
   (make-simple-shepherd-service
    'hyprpolkitagent
    #~(list #$(file-append hyprpolkitagent "/libexec/hyprpolkitagent"))
    #:requirement '(wayland-compositor)
    #:documentation "Run hyprpolkitagent for polkit authentication."
    #:environment-variables
    #~(cons (string-append
             "QT_PLUGIN_PATH="
             #$(file-append qtwayland "/lib/qt6/plugins")
             ":" #$(file-append (specification->package "qtbase") "/lib/qt6/plugins"))
            (environ)))))

(define (home-hyprpolkitagent-profile-service _)
  (list hyprpolkitagent))

(define home-hyprpolkitagent-service-type
  (service-type
   (name 'home-hyprpolkitagent)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-hyprpolkitagent-shepherd-service)
          (service-extension home-profile-service-type
                             home-hyprpolkitagent-profile-service)))
   (default-value #f)
   (description "Run hyprpolkitagent for polkit authentication prompts.")))
