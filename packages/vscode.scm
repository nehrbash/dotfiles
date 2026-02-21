(define-module (packages vscode)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module ((nonguix licenses) #:prefix license:))

(define-public vscode
  (package
    (name "vscode")
    (version "1.109.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://update.code.visualstudio.com/"
             version "/linux-deb-x64/stable"))
       (file-name (string-append "vscode-" version ".deb"))
       (sha256
        (base32 "02jqmw82d0bbzf38zn7hd0k686cn2pjnl2dz7ajm47miis93ff0w"))))
    (supported-systems '("x86_64-linux"))
    (build-system chromium-binary-build-system)
    (arguments
     (list #:validate-runpath? #f
           #:wrapper-plan
           #~'(("share/code/code"
                (("out" "/share/code")))
               ("share/code/chrome_crashpad_handler"
                (("out" "/share/code"))))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'binary-unpack 'setup-cwd
                 (lambda _
                   (copy-recursively "usr/" ".")
                   (delete-file-recursively "usr")
                   ;; Fix .desktop files
                   (substitute* "share/applications/code.desktop"
                     (("/usr/share/code/")
                      (string-append #$output "/share/code/"))
                     (("/usr/bin/")
                      (string-append #$output "/bin/")))
                   (substitute* "share/applications/code-url-handler.desktop"
                     (("/usr/share/code/")
                      (string-append #$output "/share/code/"))
                     (("/usr/bin/")
                      (string-append #$output "/bin/")))))
               (add-after 'install 'create-bin-symlink
                 (lambda _
                   (mkdir-p (string-append #$output "/bin"))
                   (symlink (string-append #$output "/share/code/bin/code")
                            (string-append #$output "/bin/code")))))))
    (home-page "https://code.visualstudio.com/")
    (synopsis "Visual Studio Code")
    (description
     "Visual Studio Code is a source-code editor developed by Microsoft
for Windows, Linux, and macOS.")
    (license (license:nonfree
              "https://code.visualstudio.com/License/"))))
