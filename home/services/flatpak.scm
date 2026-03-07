(define-module (home services flatpak)
  #:use-module (gnu home services)
  #:use-module (gnu packages package-management)
  #:use-module (guix gexp)
  #:export (home-flatpak-service-type))

(define (home-flatpak-activation apps)
  #~(begin
      (system* "flatpak" "remote-add" "--user"
               "--if-not-exists" "flathub"
               "https://dl.flathub.org/repo/flathub.flatpakrepo")
      (for-each
       (lambda (app)
         (system* "flatpak" "install" "--user"
                  "--noninteractive" "flathub" app))
       '#$apps)))

(define (home-flatpak-profile-service _)
  (list flatpak))

(define home-flatpak-service-type
  (service-type
   (name 'home-flatpak)
   (extensions
    (list (service-extension home-activation-service-type
                             home-flatpak-activation)
          (service-extension home-profile-service-type
                             home-flatpak-profile-service)))
   (default-value '("app.zen_browser.zen"
                     "com.spotify.Client"
                     "com.discordapp.Discord"
                     "com.valvesoftware.Steam"))
   (description "Install Flatpak applications from Flathub on activation.")))
