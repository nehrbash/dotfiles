(define-module (config home ocean)
  #:use-modules (gnu)
  #:use-modules (gnu home)
  #:use-modules (gnu packages version-control))

(home-environment
  (packages (list git)))
