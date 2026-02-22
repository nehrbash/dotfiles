(define-module (packages emacs-gptel-git)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages emacs-xyz))

(define-public emacs-gptel-git
  (package
    (name "emacs-gptel-git")
    (version "0.9.9.3-1.fdc6047")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/karthink/gptel")
             (commit "fdc604710ba07a31a5cc9c18bdaea3447c1356e2")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rq3lb42sx22r43l7p4hdfij1dkpqdxalw2ddsimksdixnf8brdn"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-compat emacs-transient))
    (home-page "https://github.com/karthink/gptel")
    (synopsis "Simple LLM client for Emacs")
    (description
     "gptel is a simple, no-frills ChatGPT client for Emacs.")
    (license license:gpl3+)))
