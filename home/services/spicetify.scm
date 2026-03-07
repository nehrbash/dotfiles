(define-module (home services spicetify)
  #:use-module (gnu home services)
  #:use-module (gnu packages curl)
  #:use-module (guix gexp)
  #:export (home-spicetify-setup-service-type))

;;;
;;; Activation — install spicetify CLI, build spotify-app overlay, apply theme.
;;;

(define (home-spicetify-activation dotfiles-dir)
  #~(begin
      (use-modules (ice-9 rdelim))
      (let* ((home      (getenv "HOME"))
             (spice-bin (string-append home "/.spicetify/spicetify"))
             ;; Flatpak read-only app root
             (app-ro    (string-append home
                          "/.local/share/flatpak/app/com.spotify.Client"
                          "/x86_64/stable/active/files"))
             ;; Writable overlay root (used as --app-path)
             (app-rw    (string-append home "/.local/share/spotify-app"))
             ;; Writable patched spotify data dir
             (spot-rw   (string-append home "/.local/share/spotify")))

        ;; --- 1. Install spicetify CLI ---
        (unless (file-exists? spice-bin)
          (system* #$(file-append curl "/bin/curl")
                   "-fsSL"
                   "https://raw.githubusercontent.com/spicetify/cli/main/install.sh"
                   "-o" "/tmp/spicetify-install.sh")
          (system* "sh" "/tmp/spicetify-install.sh"))

        ;; --- 2. Build spotify-app overlay ---
        (when (file-exists? app-ro)
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

              (when (file-exists? spice-bin)
                (system* spice-bin "backup" "apply")))))

        ;; --- 3. Configure and apply spicetify ---
        (when (and (file-exists? spice-bin)
                   (file-exists? spot-rw))
          (setenv "PATH"
                  (string-append home "/.spicetify:"
                                 (getenv "PATH")))
          (system* spice-bin "config"
                   "spotify_path" spot-rw)
          (system* spice-bin "config"
                   "prefs_path"
                   (string-append home
                     "/.var/app/com.spotify.Client"
                     "/config/spotify/prefs"))
          ;; Symlink caelestia theme user.css from dotfiles
          (let ((target (string-append #$dotfiles-dir
                                       "/files/spicetify/Themes/caelestia/user.css"))
                (link   (string-append home
                                       "/.config/spicetify/Themes/caelestia/user.css")))
            (let ((parent (dirname link)))
              (unless (file-exists? parent) (mkdir-p parent)))
            (when (or (not (file-exists? link))
                      (and (false-if-exception
                            (eq? 'symlink (stat:type (lstat link))))
                           (not (string=? (readlink link) target))))
              (when (file-exists? link) (delete-file link))
              (symlink target link)
              (display (string-append target " => " link "\n"))))
          (system* spice-bin "config"
                   "current_theme" "caelestia")
          (system* spice-bin "config"
                   "color_scheme" "caelestia")
          (system* spice-bin "apply")))))

;;;
;;; Service type.
;;;

(define home-spicetify-setup-service-type
  (service-type
   (name 'home-spicetify-setup)
   (extensions
    (list (service-extension home-activation-service-type
                             home-spicetify-activation)))
   (default-value "")
   (description
    "Install spicetify CLI, build a writable Spotify overlay for Flatpak,
and configure the caelestia theme.")))
