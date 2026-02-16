(define-module (config systems base-system)
  #:use-modules (gnu)
  #:use-modules (gnu system)
  #:use-modules (gnu services base)
  #:use-modules (gnu services desktop)
  #:use-modules (gnu services networking)
  #:use-modules (gnu services ssh)
  #:use-modules (gnu services cups)
  #:use-modules (gnu services xorg)
  #:use-modules (gnu services linux)
  #:use-modules (gnu packages shells)
  #:use-modules (guix gexp)
  #:use-modules (guix channels)
  #:use-modules (gnu services base)
  #:export (%base-packages-extra
            %base-services-extra
            %substitute-urls
            %authorized-keys))

;; Substitute servers (official + NonGuix)
(define %substitute-urls
  '("https://bordeaux.guix.gnu.org"
    "https://ci.guix.gnu.org"
    "https://substitutes.nonguix.org"))

;; Authorized substitute signing keys
(define %authorized-keys
  (append %default-authorized-guix-keys
          (list (plain-file "nonguix.pub"
                            "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))))

;; Common packages for all systems
(define %base-packages-extra
  (list zsh))

;; Shared services for all systems
(define %base-services-extra
  (list
   (service openssh-service-type
            (openssh-configuration
             (permit-root-login 'prohibit-password)))
   (service cups-service-type
            (cups-configuration
             (web-interface? #t)))
   (service network-manager-service-type)
   (service wpa-supplicant-service-type)
   (service ntp-service-type)))
