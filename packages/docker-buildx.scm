(define-module (packages docker-buildx)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define-public docker-buildx
  (package
    (name "docker-buildx")
    (version "0.31.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/docker/buildx/releases/download/v"
             version "/buildx-v" version ".linux-amd64"))
       (sha256
        (base32 "07kv64h44ph21zx76l3a4ihkqc0j5d955n3l90xi4f4iybxsz3nw"))))
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
                            (string-append bin "/docker-buildx"))
                 (chmod (string-append bin "/docker-buildx") #o755)))))
    (home-page "https://github.com/docker/buildx")
    (synopsis "Docker CLI plugin for extended build capabilities with BuildKit")
    (description
     "Buildx is a Docker CLI plugin for extended build capabilities using
BuildKit.  It provides features like multi-platform builds, build cache,
and advanced Dockerfile syntax support.")
    (license license:asl2.0)))
