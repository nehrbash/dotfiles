(define-module (packages quickshell-git)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages qt))

;; Quickshell built from git HEAD — caelestia-dots/shell requires
;; features not yet in the 0.2.1 release.
;;
;; To update: pin a new commit, then recompute sha256 with:
;;   guix hash --serializer=nar -x /path/to/cloned-source
(define-public quickshell-git
  (package
    (inherit quickshell)
    (name "quickshell-git")
    ;; Pinned commit; update together with the sha256 when upgrading.
    (version "0-git-dacfa9d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.outfoxxed.me/quickshell/quickshell")
             (commit "dacfa9de829ac7cb173825f593236bf2c21f637e")
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nv3673jn6mc1a8q2281cx14qh2lgz9n01p4wzwfgfqpjqvyf1cy"))))
    (inputs
     (modify-inputs (package-inputs quickshell)
       (append glib)
       (append polkit)))
    (arguments
     (list
      #:tests? #f
      #:generator "Ninja"
      #:configure-flags
      #~(list
         "-DCRASH_REPORTER=OFF"
         "-DDISTRIBUTOR=\"GNU Guix\""
         "-DDISTRIBUTOR_DEBUGINFO_AVAILABLE=NO"
         "-DINSTALL_QML_PREFIX=lib/qt6/qml"
         "-DINSTALL_HEADERS=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-program
            (lambda _
              (let ((qtwayland #$(this-package-input "qtwayland"))
                    (qtsvg #$(this-package-input "qtsvg")))
                (wrap-program (string-append #$output "/bin/quickshell")
                  `("QML_IMPORT_PATH" ":" prefix (,(getenv "QML_IMPORT_PATH")))
                  `("QT_PLUGIN_PATH" ":" prefix
                    (,(string-append qtwayland "/lib/qt6/plugins")
                     ,(string-append qtsvg "/lib/qt6/plugins"))))))))))))
