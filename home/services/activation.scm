(define-module (home services activation)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu packages node)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages curl)
  #:use-module (guix gexp)
  #:export (post-setup-activation-service))

;; Build the activation gexp.  DOTFILES-DIR is the repo root (a string
;; that will be embedded in the gexp via #$).
(define (post-setup-activation-service dotfiles-dir)
  (simple-service 'post-setup
                  home-activation-service-type
                  #~(begin
                      ;; Symlink helper: ensure LINK points to TARGET.
                      ;; Updates stale symlinks, backs up non-symlink dirs, deletes non-symlink files.
                      (define (ensure-symlink target link)
                        (let ((parent (dirname link)))
                          (unless (file-exists? parent) (mkdir-p parent))
                          (let ((is-link (false-if-exception
                                          (eq? 'symlink (stat:type (lstat link))))))
                            (cond
                             ;; Existing symlink pointing to wrong target → replace
                             ((and is-link (not (string=? (readlink link) target)))
                              (delete-file link)
                              (symlink target link))
                             ;; Not a symlink but something exists → back up or remove
                             ((and (not is-link) (file-exists? link))
                              (if (eq? 'directory (stat:type (stat link)))
                                  (rename-file link (string-append link ".bak"))
                                  (delete-file link))
                              (symlink target link))
                             ;; Nothing exists → create
                             ((not is-link)
                              (symlink target link)))
                            (display (string-append target " => " link "\n")))))

                      ;; Flatpak: add remote + install apps
                      (system* "flatpak" "remote-add" "--user"
                               "--if-not-exists" "flathub"
                               "https://dl.flathub.org/repo/flathub.flatpakrepo")
                      (for-each
                       (lambda (app)
                         (system* "flatpak" "install" "--user"
                                  "--noninteractive" "flathub" app))
                       '("app.zen_browser.zen"
                         "com.spotify.Client"
                         "com.discordapp.Discord"
                         "com.valvesoftware.Steam"))

                      ;; Symlink ~/.config dirs/files directly from dotfiles
                      (let* ((home (getenv "HOME"))
                             (dots #$dotfiles-dir)
                             (conf (string-append home "/.config")))
                        ;; Directory symlinks (dotfiles-dir → ~/.config/name)
                        (for-each
                         (lambda (pair)
                           (ensure-symlink (string-append dots "/" (car pair))
                                           (string-append conf "/" (cdr pair))))
                         '(("files/hypr" . "hypr")
                           ("files/gtk/gtk-3.0" . "gtk-3.0")
                           ("files/gtk/gtk-4.0" . "gtk-4.0")
                           ("files/alacritty" . "alacritty")
                           ("files/cava" . "cava")
                           ("files/direnv" . "direnv")
                           ("files/mise" . "mise")
                           ("files/nyxt" . "nyxt")
                           ("files/paru" . "paru")
                           ("files/systemd" . "systemd")
                           ("files/quickshell" . "quickshell")
                           ("files/xdg-desktop-portal" . "xdg-desktop-portal")))
                        ;; Wallpaper symlink for hyprpaper (doesn't expand env vars)
                        (ensure-symlink (string-append dots "/pictures/wallpaper")
                                        (string-append home "/.local/share/wallpaper"))
                        ;; Individual file symlinks
                        (for-each
                         (lambda (pair)
                           (ensure-symlink (string-append dots "/" (car pair))
                                           (string-append conf "/" (cdr pair))))
                         '(("files/misc/electron-flags.conf" . "electron-flags.conf")
                           ("files/misc/mimeapps.list" . "mimeapps.list")
                           ("files/misc/spotify-launcher.conf" . "spotify-launcher.conf")
                           ("files/zsh/aliasrc" . "zsh/aliasrc")
                           ("files/zsh/emacs_functions" . "zsh/emacs_functions")
                           ("files/zsh/functions" . "zsh/functions")
                           ))

                        ;; SSH config — copied as a real file so bind-mounts
                        ;; into containers work (symlinks to the store don't
                        ;; resolve inside the container).
                        (let* ((ssh-dir (string-append home "/.ssh"))
                               (target  (string-append ssh-dir "/config"))
                               (source  (string-append dots "/files/ssh/config")))
                          (unless (file-exists? ssh-dir)
                            (mkdir ssh-dir)
                            (chmod ssh-dir #o700))
                          (when (or (file-exists? target)
                                    (false-if-exception
                                     (eq? 'symlink (stat:type (lstat target)))))
                            (delete-file target))
                          (copy-file source target)
                          (chmod target #o600)))

                      ;; claude-code: install via npm if missing
                      (let ((bin (string-append (getenv "HOME") "/.local/share/npm/bin/claude")))
                        (unless (file-exists? bin)
                          (setenv "npm_config_prefix"
                                  (string-append (getenv "HOME") "/.local/share/npm"))
                          (system* #$(file-append node "/bin/npm")
                                   "install" "-g" "@anthropic-ai/claude-code")))

                      ;; spicetify + spotify-app overlay
                      ;; 1. Install spicetify CLI if missing.
                      ;; 2. Build ~/.local/share/spotify-app/ — a symlink tree that
                      ;;    mirrors the Flatpak /app but replaces extra/share/spotify
                      ;;    with a writable copy so spicetify patches persist.
                      ;; 3. Apply spicetify patches to the writable copy.
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
                        ;; Only rebuild when the Flatpak app exists and our overlay is
                        ;; stale (missing or pointing at a different commit).
                        (when (file-exists? app-ro)
                          (let* ((sentinel (string-append app-rw "/.guix-spotify-source"))
                                 (current  (if (file-exists? sentinel)
                                               (call-with-input-file sentinel read-line)
                                               ""))
                                 ;; Use the commit hash (readlink of 'active' symlink)
                                 ;; as a version tag so we rebuild when Spotify updates.
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

                              ;; (re-)create writable spotify data dir by copying from store
                              ;; (the store is read-only OSTree; we need a writable copy)
                              (when (file-exists? spot-rw)
                                (system* "rm" "-rf" spot-rw))
                              (mkdir-p spot-rw)
                              (system* "cp" "-a"
                                       (string-append app-ro "/extra/share/spotify/.")
                                       spot-rw)
                              (system* "chmod" "-R" "a+wr" spot-rw)

                              ;; Build the --app-path overlay: copy the small parts of /app
                              ;; (~14 MB: bin, lib, share, manifest.json, extra/bin)
                              ;; as real files, then symlink extra/share/spotify → spot-rw.
                              ;; Symlinks alone don't work because --app-path mounts the dir
                              ;; as /app inside bwrap, breaking absolute symlinks to the store.
                              (when (file-exists? app-rw)
                                (system* "rm" "-rf" app-rw))
                              (mkdir-p app-rw)

                              ;; Copy non-extra top-level entries
                              (for-each
                               (lambda (e)
                                 (let ((src (string-append app-ro "/" e))
                                       (dst (string-append app-rw "/" e)))
                                   (when (file-exists? src)
                                     (system* "cp" "-a" src dst))))
                               '("bin" "lib" "share" "manifest.json"))

                              ;; Copy extra/bin (the actual Spotify binary, ~5 MB)
                              (let ((extra-ro (string-append app-ro "/extra"))
                                    (extra-rw (string-append app-rw "/extra")))
                                (mkdir-p (string-append extra-rw "/share"))
                                (when (file-exists? (string-append extra-ro "/bin"))
                                  (system* "cp" "-a"
                                           (string-append extra-ro "/bin")
                                           (string-append extra-rw "/bin")))
                                ;; Point extra/share/spotify → writable patched copy.
                                ;; flatpak run uses --filesystem=spot-rw so this absolute
                                ;; symlink resolves correctly inside the bwrap sandbox.
                                (symlink spot-rw
                                         (string-append extra-rw "/share/spotify")))

                              ;; Write sentinel
                              (call-with-output-file sentinel
                                (lambda (p) (display new-ver p)))

                              ;; --- 3. Apply spicetify patches to the writable copy ---
                              ;; The wrapper script (scripts/spotify) passes:
                              ;;   flatpak run --app-path=app-rw --filesystem=spot-rw
                              ;; so the patched files at spot-rw are visible inside
                              ;; the bwrap sandbox at their host absolute path.
                              (when (file-exists? spice-bin)
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
                                (system* spice-bin "backup" "apply")
                                (system* spice-bin "apply"))))))

                      ;; Set GTK icon/theme via dconf (settings.ini is overridden by dconf)
                      (system* #$(file-append (specification->package "dconf") "/bin/dconf")
                               "write" "/org/gnome/desktop/interface/icon-theme"
                               "'Gruvbox-Plus-Dark'")
                      (system* #$(file-append (specification->package "dconf") "/bin/dconf")
                               "write" "/org/gnome/desktop/interface/gtk-theme"
                               "'gruvbox-dark-gtk'")

                      ;; Reload Hyprland config if running in a session
                      (when (getenv "HYPRLAND_INSTANCE_SIGNATURE")
                        (system* "hyprctl" "reload")))))
