(define-module (packages emacs-eca)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages compression))

(define-public emacs-eca
  (package
    (name "emacs-eca")
    (version "0.0.1-1.32943e6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/editor-code-assistant/eca-emacs")
             (commit "32943e61016b1ae6f205a1483e3a5853dd64e3f8")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "145sj84msnnp5xz75695klycnpnisx3xs0lhlg772l4h18ygfk3j"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-dash
           emacs-f
           emacs-s
           emacs-compat
           emacs-markdown-mode
           emacs-transient
           unzip))
    (home-page "https://github.com/editor-code-assistant/eca-emacs")
    (synopsis "Editor Code Assistant integration for Emacs")
    (description
     "ECA is an AI-powered pair-programming client for Emacs that provides
interactive chat, code suggestions, context management, and tool use via
an external eca server process.")
    (license license:gpl3+)))
