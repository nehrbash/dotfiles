(define-module (packages docker-compose-v2)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define-public docker-compose-v2
  (package
    (name "docker-compose-v2")
    (version "5.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/docker/compose/releases/download/v"
             version "/docker-compose-linux-x86_64"))
       (sha256
        (base32 "0rxgswyspjypv0g7yffianpsiz22m28ymnjgqmwwg9rx7mr0z21d"))))
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (let* ((out (assoc-ref %outputs "out"))
                      (bin (string-append out "/bin")))
                 (mkdir-p bin)
                 (copy-file (assoc-ref %build-inputs "source")
                            (string-append bin "/docker-compose"))
                 (chmod (string-append bin "/docker-compose") #o755)))))
    (home-page "https://github.com/docker/compose")
    (synopsis "Define and run multi-container applications with Docker")
    (description
     "Docker Compose V2 is a tool for defining and running multi-container
Docker applications.  It is implemented as a Docker CLI plugin.")
    (license license:asl2.0)))
