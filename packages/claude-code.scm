(define-module (packages claude-code)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((nonguix licenses) #:prefix license:))

(define gcs-bucket
  "https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases")

(define-public claude-code
  (package
    (name "claude-code")
    (version "2.1.66")
    (source
     (origin
       (method url-fetch)
       (uri (string-append gcs-bucket "/" version "/linux-x64/claude"))
       (file-name (string-append "claude-code-" version))
       (sha256
        (base32 "197fkpm2a39297dkm1mjv469m85pk0kfv2c65wijalay1w27ghi3"))))
    (supported-systems '("x86_64-linux"))
    (build-system copy-build-system)
    (arguments
     (list #:validate-runpath? #f
           #:strip-binaries? #f
           #:install-plan
           #~`((,#$(string-append "claude-code-" version) "bin/claude"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'make-executable
                 (lambda* (#:key outputs #:allow-other-keys)
                   (chmod (string-append (assoc-ref outputs "out")
                                         "/bin/claude")
                          #o755))))))
    (home-page "https://claude.com/product/claude-code")
    (synopsis "Anthropic's agentic coding tool")
    (description
     "Claude Code is an agentic coding tool that lives in your terminal,
understands your codebase, and helps you code faster through natural language
commands.  This is a Bun-compiled binary that requires @file{/lib64/ld-linux-x86-64.so.2}
(see @code{extra-special-file} in the system configuration).")
    (license (license:nonfree
              "https://www.anthropic.com/legal/terms"))))
