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
  #:export (%base-packages-extra
            %base-services-extra))

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
