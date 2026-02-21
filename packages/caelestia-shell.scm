(define-module (packages caelestia-shell)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages linux)
  #:use-module (packages libcava))

;; Caelestia QML plugin — C++ Qt6 module for the Caelestia desktop shell.
;; Only the 'plugin' module is built (the QML shell config files are cloned
;; separately by the home activation gexp).
(define-public caelestia-shell
  (package
    (name "caelestia-shell")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/caelestia-dots/shell")
             (commit "4be8fc9693e439c487f091413289b782d78130e7")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fc4w1b4rqjda5kn36bfl18fvhvalfw6ny6z6dmyrzdd0d2rzxb0"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list
         "-DENABLE_MODULES=plugin"
         "-DINSTALL_QMLDIR=lib/qt6/qml"
         (string-append "-DVERSION=" #$version)
         "-DGIT_REVISION=4be8fc9693e439c487f091413289b782d78130e7")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list qtbase
           qtdeclarative
           libqalculate
           gmp
           mpfr
           pipewire
           aubio
           libcava))
    (home-page "https://github.com/caelestia-dots/shell")
    (synopsis "Caelestia QML plugin for Quickshell")
    (description
     "C++ QML plugin providing audio visualization, calculator, and other
services for the Caelestia desktop shell running on Quickshell.")
    (license license:gpl3+)))
