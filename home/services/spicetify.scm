(define-module (home services spicetify)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:export (home-spicetify-setup-service-type))

;; Activation service: build a writable overlay of the read-only Flatpak
;; Spotify app so spicetify can patch it, then re-apply the caelestia
;; theme whenever the overlay is rebuilt.
;;
;; Prerequisites the user provides manually, once:
;;   - com.spotify.Client flatpak installed
;;   - spicetify CLI installed at ~/.spicetify/spicetify
;;     (https://spicetify.app/docs/getting-started)
;;   - caelestia theme symlinked into ~/.config/spicetify/Themes by
;;     home-dotfiles-symlinks-service.
;;
;; This activation rebuilds the overlay only when the active Flatpak
;; version changes (tracked via a sentinel file).

(define (home-spicetify-activation _)
  #~(begin
      (use-modules (ice-9 rdelim))
      (let* ((home      (getenv "HOME"))
             (spice-bin (string-append home "/.spicetify/spicetify"))
             (app-ro    (string-append home
                          "/.local/share/flatpak/app/com.spotify.Client"
                          "/x86_64/stable/active/files"))
             (app-rw    (string-append home "/.local/share/spotify-app"))
             (spot-rw   (string-append home "/.local/share/spotify")))

        ;; Bail out silently if Spotify flatpak or spicetify CLI not installed.
        (when (and (file-exists? app-ro)
                   (file-exists? spice-bin))

          ;; --- 1. Build / refresh the writable overlay ---
          (let* ((sentinel (string-append app-rw "/.guix-spotify-source"))
                 (current  (if (file-exists? sentinel)
                               (call-with-input-file sentinel read-line)
                               ""))
                 (active-link (string-append
                               home
                               "/.local/share/flatpak/app/com.spotify.Client"
                               "/x86_64/stable/active"))
                 (new-ver  (if (false-if-exception
                                (eq? 'symlink (stat:type (lstat active-link))))
                               (readlink active-link)
                               "unknown")))
            (unless (string=? current new-ver)
              (display (string-append
                        "spotify-app: rebuilding overlay for " new-ver "\n"))

              (when (file-exists? spot-rw)
                (system* "rm" "-rf" spot-rw))
              (mkdir-p spot-rw)
              (system* "cp" "-a"
                       (string-append app-ro "/extra/share/spotify/.")
                       spot-rw)
              (system* "chmod" "-R" "a+wr" spot-rw)

              (when (file-exists? app-rw)
                (system* "rm" "-rf" app-rw))
              (mkdir-p app-rw)
              (for-each
               (lambda (e)
                 (let ((src (string-append app-ro "/" e))
                       (dst (string-append app-rw "/" e)))
                   (when (file-exists? src)
                     (system* "cp" "-a" src dst))))
               '("bin" "lib" "share" "manifest.json"))

              (let ((extra-ro (string-append app-ro "/extra"))
                    (extra-rw (string-append app-rw "/extra")))
                (mkdir-p (string-append extra-rw "/share"))
                (when (file-exists? (string-append extra-ro "/bin"))
                  (system* "cp" "-a"
                           (string-append extra-ro "/bin")
                           (string-append extra-rw "/bin")))
                (symlink spot-rw
                         (string-append extra-rw "/share/spotify")))

              (call-with-output-file sentinel
                (lambda (p) (display new-ver p)))
              (system* spice-bin "backup" "apply")))

          ;; --- 2. (Re-)apply caelestia theme ---
          (system* spice-bin "config" "spotify_path" spot-rw)
          (system* spice-bin "config" "prefs_path"
                   (string-append home
                     "/.var/app/com.spotify.Client"
                     "/config/spotify/prefs"))
          (system* spice-bin "config" "current_theme" "caelestia")
          (system* spice-bin "config" "color_scheme" "caelestia")
          (system* spice-bin "apply")))))

(define home-spicetify-setup-service-type
  (service-type
   (name 'home-spicetify-setup)
   (extensions
    (list (service-extension home-activation-service-type
                             home-spicetify-activation)))
   (default-value #f)
   (description
    "Build a writable Spotify overlay for the Flatpak com.spotify.Client
and re-apply the caelestia spicetify theme whenever the overlay is
rebuilt.  The spicetify CLI itself must be installed manually.")))
