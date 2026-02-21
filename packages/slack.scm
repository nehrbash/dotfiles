(define-module (packages slack)
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

(define-public slack
  (package
    (name "slack")
    (version "4.47.69")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://downloads.slack-edge.com/desktop-releases/linux/x64/"
             version "/slack-desktop-" version "-amd64.deb"))
       (sha256
        (base32 "19bbj3lk9vwqgjabsgisjldsxwwq3na7525vvijyfs59kq3y7mbv"))))
    (supported-systems '("x86_64-linux"))
    (build-system chromium-binary-build-system)
    (arguments
     (list #:validate-runpath? #f
           #:wrapper-plan
           #~'(("lib/slack/slack"
                (("out" "/lib/slack")))
               ("lib/slack/chrome_crashpad_handler"
                (("out" "/lib/slack"))))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'binary-unpack 'setup-cwd
                 (lambda _
                   (copy-recursively "usr/" ".")
                   (delete-file-recursively "usr")
                   ;; The deb puts the app in /usr/lib/slack/
                   ;; It's already at lib/slack/ after the copy
                   ;; Fix the .desktop file
                   (substitute* "share/applications/slack.desktop"
                     (("/usr/bin/")
                      (string-append #$output "/bin/"))
                     (("/usr/share/")
                      (string-append #$output "/share/")))))
               (add-after 'install 'patch-app-asar
                 ;; Disable WebRTCPipeWireCapturer feature detection so
                 ;; screen sharing works on Wayland without prompting
                 ;; for the wrong capture method.
                 (lambda _
                   (use-modules (ice-9 binary-ports)
                                (rnrs bytevectors))
                   (let* ((asar (string-append #$output
                                               "/lib/slack/resources/app.asar"))
                          (data (call-with-input-file asar get-bytevector-all
                                                      #:binary #t))
                          (old (string->utf8 "WebRTCPipeWireCapturer"))
                          (new (string->utf8 "LebRTCPipeWireCapturer"))
                          (len (bytevector-length old)))
                     (let loop ((i 0))
                       (when (< i (- (bytevector-length data) len))
                         (if (let check ((j 0))
                               (if (= j len) #t
                                   (and (= (bytevector-u8-ref data (+ i j))
                                           (bytevector-u8-ref old j))
                                        (check (+ j 1)))))
                             (begin
                               (bytevector-copy! new 0 data i len)
                               (loop (+ i len)))
                             (loop (+ i 1)))))
                     (call-with-output-file asar
                       (lambda (port)
                         (put-bytevector port data))
                       #:binary #t)))))))
    (home-page "https://slack.com/")
    (synopsis "Slack desktop client")
    (description
     "Slack is a messaging app for business that connects people to the
information they need.")
    (license (license:nonfree
              "https://slack.com/terms-of-service/user"))))
