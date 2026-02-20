(define-module (packages nwg-displays)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk))

(define-public nwg-displays
  (package
    (name "nwg-displays")
    (version "0.3.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/nwg-piotr/nwg-displays/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "1s96mlpzhgaigi0zzqk1lq72pg2ll7l2xcbnwyizm7qvqa9ykzrm"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-scripts-init
           (lambda _
             (call-with-output-file "nwg_displays/scripts/__init__.py"
               (lambda (p) (display "" p)))))
         (delete 'sanity-check)
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gi-path (string-join
                              (list (string-append (assoc-ref inputs "gtk+")
                                                   "/lib/girepository-1.0")
                                    (string-append (assoc-ref inputs "gtk-layer-shell")
                                                   "/lib/girepository-1.0"))
                              ":")))
               (wrap-program (string-append out "/bin/nwg-displays")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-path)))))))))
    (propagated-inputs
     (list python-pygobject))
    (inputs
     (list gtk+ gtk-layer-shell))
    (home-page "https://github.com/nwg-piotr/nwg-displays")
    (synopsis "Output management utility for Wayland compositors")
    (description
     "nwg-displays is a GTK3-based output management utility for Wayland
compositors including Hyprland and sway.  It provides a graphical interface
for configuring monitor layouts, resolutions, and refresh rates.")
    (license license:expat)))
