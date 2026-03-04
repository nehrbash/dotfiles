(define-module (systems base-system)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services cups)
  #:use-module (gnu services xorg)
  #:use-module (gnu services linux)
  #:use-module (gnu packages base)
  #:use-module (gnu packages shells)
  #:use-module (guix gexp)
  #:export (%base-packages-extra
            %base-services-extra
            %substitute-urls
            %authorized-keys))

(define %substitute-urls
  '("https://bordeaux-us-east-mirror.cbaines.net"
    "https://substitutes.nonguix.org"))

;; Authorized substitute signing keys
(define %authorized-keys
  (append %default-authorized-guix-keys
          (list (plain-file "nonguix.pub"
                            "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))))

;; Common packages for all systems
(define %base-packages-extra
  (list zsh))

;; Shared services for all systems.
;; Note: network-manager, wpa-supplicant, and ntp are already in %desktop-services.
(define %base-services-extra
  (list
   ;; Provide /lib64/ld-linux-x86-64.so.2 so foreign binaries (Bun, etc.) work.
   (extra-special-file "/lib64/ld-linux-x86-64.so.2"
                       (file-append glibc "/lib/ld-linux-x86-64.so.2"))
   (service openssh-service-type
            (openssh-configuration
             (permit-root-login 'prohibit-password)))
   (service cups-service-type
            (cups-configuration
             (web-interface? #t)))))
