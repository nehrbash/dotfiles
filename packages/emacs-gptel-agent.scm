(define-module (packages emacs-gptel-agent)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (packages emacs-gptel-git))

(define-public emacs-gptel-agent
  (package
    (name "emacs-gptel-agent")
    (version "0.0.1-1.79803c5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/karthink/gptel-agent")
             (commit "79803c50efbcbdbf9a5ceba07fb99054da2f9e15")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bn211f6v5myx3wqbm7zrxy25kszy6kgx187c7vkqq5lnhqi9nqz"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-gptel-git))
    (home-page "https://github.com/karthink/gptel-agent")
    (synopsis "Agentic AI assistant for Emacs with tool use")
    (description
     "gptel-agent provides agentic capabilities for gptel including web search,
bash command execution, file operations, and Emacs introspection tools.")
    (license license:gpl3+)))
