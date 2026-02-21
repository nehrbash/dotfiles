(define-module (packages caelestia-cli)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages image))

(define-public python-materialyoucolor
  (package
    (name "python-materialyoucolor")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "materialyoucolor" version))
       (sha256
        (base32 "1l68ic5ghl14jyxxs9679rdr6pk532qjnd5n9z3ynzqr93mzsp3g"))))
    ;; The PyPI sdist ships setup.py/setup.cfg (not pyproject.toml),
    ;; so use the classic python build system.
    (build-system python-build-system)
    (arguments
     (list #:tests? #f))
    (home-page "https://github.com/T-Dynamos/materialyoucolor-python")
    (synopsis "Material You color generation for Python")
    (description
     "Python implementation of Material You (Material Design 3) color
palette generation from an image or seed color.")
    (license license:asl2.0)))

(define-public caelestia-cli
  (package
    (name "caelestia-cli")
    ;; Pinned commit; update together with sha256 when upgrading.
    (version "0.1.0-git-25c473c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/caelestia-dots/cli")
             (commit "25c473c18ee951f3bdc5928708b7a6608e473484")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ajdncqalc2gilaihmdyxsd9vnfk0irm248b74shdamnv0gr82pp"))
       (patches
        (list (local-file "caelestia-python311.patch")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-version
            (lambda _
              ;; hatch-vcs needs a valid PEP 440 version; provide one since
              ;; this is a git checkout without tags.
              (setenv "HATCH_VCS_PRETEND_VERSION" "0.1.0"))))))
    (native-inputs
     (list python-hatchling python-hatch-vcs))
    (propagated-inputs
     (list python-pillow python-materialyoucolor))
    (home-page "https://github.com/caelestia-dots/cli")
    (synopsis "CLI companion for caelestia-dots/shell")
    (description
     "Command-line interface for the caelestia desktop shell.
Provides the @command{caelestia} command used to toggle panels,
apply themes, and interact with the running Quickshell instance.")
    (license license:gpl3+)))
