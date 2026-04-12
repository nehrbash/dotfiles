(define-module (packages gh)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public gh
  (package
    (name "gh")
    (version "2.72.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/cli/cli/releases/download/v"
             version "/gh_" version "_linux_amd64.tar.gz"))
       (sha256
        (base32 "0f3zqwab3mplzgxsfx96s67g6n76aijv00hkaccn3kvm21wsklzz"))))
    (supported-systems '("x86_64-linux"))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~'(("bin/gh" "bin/gh"))))
    (home-page "https://cli.github.com")
    (synopsis "GitHub CLI")
    (description
     "GitHub's official command line tool for pull requests, issues, releases,
and repository management.")
    (license license:expat)))
