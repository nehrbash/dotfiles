(define-module (config systems base-system)
  #:use-modules (gnu)
  #:use-modules (guix)
  #:export (base-system))

(use-service-modules cups desktop networking ssh xorg)
