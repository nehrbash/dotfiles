(define-module (packages gruvbox)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public gruvbox-dark-gtk
  (package
    (name "gruvbox-dark-gtk")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/jmattheis/gruvbox-dark-gtk/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "0ii5xzas7jkfqffqwp9c3wwlra4bdj7b74ax3j5yqxzzfqcb8qrb"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("." "share/themes/gruvbox-dark-gtk"
          #:exclude-regexp ("^\\.github" "^README" "^LICENSE")))))
    (home-page "https://github.com/jmattheis/gruvbox-dark-gtk")
    (synopsis "Gruvbox dark GTK theme")
    (description "A dark GTK2/GTK3 theme using the Gruvbox color palette.")
    (license license:gpl3+)))

(define-public gruvbox-plus-icon-theme
  (package
    (name "gruvbox-plus-icon-theme")
    (version "6.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/SylEleuth/gruvbox-plus-icon-pack/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "0plfxm5pq3hx6cfcw0qkwbk1y1c09j0gwqmzi5fs1svi0bnjnwmb"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("Gruvbox-Plus-Dark" "share/icons/Gruvbox-Plus-Dark")
         ("Gruvbox-Plus-Light" "share/icons/Gruvbox-Plus-Light"))))
    (home-page "https://github.com/SylEleuth/gruvbox-plus-icon-pack")
    (synopsis "Gruvbox Plus icon theme")
    (description "A complete icon theme based on the Gruvbox color palette,
including both dark and light variants.")
    (license license:gpl3+)))
