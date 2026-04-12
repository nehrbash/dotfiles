(define-module (packages mise)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages base)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module ((guix licenses) #:prefix license:))

(define-public mise
  (package
    (name "mise")
    (version "2026.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/jdx/mise/releases/download/v"
             version "/mise-v" version "-linux-x64.tar.gz"))
       (sha256
        (base32 "1gxjl28nccd4b0l4zhlfsy2xkwyaxx3yhic23d3mnx95vc6qy7cl"))))
    (supported-systems '("x86_64-linux"))
    (build-system copy-build-system)
    (arguments
     (list #:validate-runpath? #f
           #:install-plan
           #~'(("bin/mise" "bin/mise")
               ("man/man1/" "share/man/man1/"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'patch-elf
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (mise (string-append out "/bin/mise"))
                          (patchelf (string-append
                                    (assoc-ref inputs "patchelf") "/bin/patchelf"))
                          (libc (assoc-ref inputs "glibc"))
                          (gcc-lib (assoc-ref inputs "gcc"))
                          (ld-linux (string-append libc "/lib/ld-linux-x86-64.so.2"))
                          (rpath (string-append libc "/lib:"
                                                gcc-lib "/lib")))
                     (invoke patchelf "--set-interpreter" ld-linux mise)
                     (invoke patchelf "--set-rpath" rpath mise)))))))
    (native-inputs (list patchelf))
    (inputs (list glibc `(,gcc "lib")))
    (home-page "https://mise.jdx.dev/")
    (synopsis "Polyglot runtime and tool version manager")
    (description
     "mise (formerly rtx) is a polyglot tool version manager.  It manages
development tool versions (Node.js, Python, Go, Rust, etc.) per-project via
@file{mise.toml}, provides a task runner, and handles environment variables.")
    (license license:expat)))
