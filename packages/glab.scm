(define-module (packages glab)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public glab
  (package
    (name "glab")
    (version "1.86.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://gitlab.com/gitlab-org/cli/-/releases/v"
             version "/downloads/glab_" version "_linux_amd64.tar.gz"))
       (sha256
        (base32 "10f2dznd7w7nqqh7l14d1r044nqcvddhradyxl3h9rmc9rmv8apa"))))
    (supported-systems '("x86_64-linux"))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("glab" "bin/glab"))))
    (home-page "https://gitlab.com/gitlab-org/cli")
    (synopsis "GitLab CLI tool")
    (description
     "GLab is an open source GitLab CLI tool bringing GitLab's features to your
command line.  It supports merge requests, issues, CI/CD pipelines, and more.
It also includes an MCP server for AI tool integration.")
    (license license:expat)))
