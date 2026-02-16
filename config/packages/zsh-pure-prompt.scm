(define-module (config packages zsh-pure-prompt)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public zsh-pure-prompt
  (package
    (name "zsh-pure-prompt")
    (version "1.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sindresorhus/pure")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jcb5cg1539iy89vm9d59g8lnp3dm0yv88mmlhkp9zwx3bihwr06"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("pure.zsh" "share/zsh/site-functions/prompt_pure_setup")
         ("async.zsh" "share/zsh/site-functions/async"))))
    (home-page "https://github.com/sindresorhus/pure")
    (synopsis "Pretty, minimal, and fast Zsh prompt")
    (description "Pure is a pretty, minimal, and fast Zsh prompt theme.")
    (license license:expat)))
