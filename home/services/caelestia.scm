(define-module (home services caelestia)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages)
  #:use-module (gnu packages glib)
  #:use-module (guix gexp)
  #:use-module (home services shepherd-helpers)
  #:export (home-caelestia-resizer-service-type
            home-caelestia-spicetify-watcher-service-type))

(define %caelestia-bin
  "/.local/share/uv/bin/caelestia")

(define (home-caelestia-resizer-activation _)
  #~(let* ((home (getenv "HOME"))
           (uv-bin #$(file-append (specification->package "uv") "/bin/uv"))
           (caelestia-bin (string-append home #$%caelestia-bin)))
      (unless (file-exists? caelestia-bin)
        (setenv "UV_PYTHON_PREFERENCE" "managed")
        (setenv "UV_TOOL_BIN_DIR"
                (string-append home "/.local/share/uv/bin"))
        (system* uv-bin "tool" "install"
                 "--from"
                 "caelestia @ git+https://github.com/caelestia-dots/cli@25c473c18ee951f3bdc5928708b7a6608e473484"
                 "caelestia"))))

(define (home-caelestia-resizer-shepherd-service _)
  (list
   (make-simple-shepherd-service
    'caelestia-resizer
    #~(list (string-append (getenv "HOME") #$%caelestia-bin)
            "resizer" "-d")
    #:requirement '(wayland-compositor)
    #:documentation "Run caelestia resizer daemon for automatic window management.")))

(define (home-caelestia-resizer-profile-service _)
  (list (list glib "bin")))

(define home-caelestia-resizer-service-type
  (service-type
   (name 'home-caelestia-resizer)
   (extensions
    (list (service-extension home-activation-service-type
                             home-caelestia-resizer-activation)
          (service-extension home-shepherd-service-type
                             home-caelestia-resizer-shepherd-service)
          (service-extension home-profile-service-type
                             home-caelestia-resizer-profile-service)))
   (default-value #f)
   (description
    "Install the caelestia CLI tool via uv and run the resizer daemon
for automatic window management (PiP, etc.).")))

;;;
;;; Spicetify color watcher — reacts to caelestia theme color changes.
;;;

(define (home-caelestia-spicetify-watcher-shepherd-service _)
  (list
   (shepherd-service
    (provision '(spicetify-watcher))
    (requirement '(quickshell))
    (documentation "Watch caelestia color.ini and run spicetify refresh on changes.")
    (start #~(lambda _
               (use-modules (ice-9 threads))
               (let* ((home      (getenv "HOME"))
                      (color-ini (string-append home
                                   "/.config/spicetify/Themes/caelestia/color.ini"))
                      (spice-bin (string-append home "/.spicetify/spicetify")))
                 (call-with-new-thread
                  (lambda ()
                    (use-modules (ice-9 inotify))
                    (let* ((inotify (make-inotify))
                           (watch-dir (dirname color-ini))
                           (_ (add-watch! inotify watch-dir
                                          (list 'close-write 'moved-to))))
                      (let loop ()
                        (for-each
                         (lambda (event)
                           (when (and (inotify-event-name event)
                                      (string=? (inotify-event-name event)
                                                "color.ini"))
                             (when (file-exists? spice-bin)
                               (system* spice-bin "refresh"))))
                         (read-inotify-event inotify))
                        (loop)))))
                 #t)))
    (stop #~(lambda (pid) #t))
    (respawn? #t)
    (respawn-limit #~'(5 . 30))
    (respawn-delay 5))))

(define home-caelestia-spicetify-watcher-service-type
  (service-type
   (name 'home-caelestia-spicetify-watcher)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-caelestia-spicetify-watcher-shepherd-service)))
   (default-value #f)
   (description
    "Watch caelestia color.ini and run spicetify refresh on color changes.")))
