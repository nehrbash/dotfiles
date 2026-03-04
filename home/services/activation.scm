(define-module (home services activation)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
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
                      (use-modules (ice-9 rdelim)     ; read-line
                                   (ice-9 string-fun)) ; string-trim-right

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

                      ;; Ensure ~/.config/quickshell is a real directory, not a
                      ;; symlink to the repo root (qs ignores named configs when
                      ;; shell.qml exists directly under quickshell/).
                      (let ((qs-dir (string-append (getenv "HOME") "/.config/quickshell")))
                        (when (and (false-if-exception
                                    (eq? 'symlink (stat:type (lstat qs-dir))))
                                   ;; Only replace if it's pointing at the old repo root
                                   (string-suffix? "files/quickshell"
                                                   (readlink qs-dir)))
                          (delete-file qs-dir)
                          (mkdir qs-dir)))

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
                           ("files/quickshell" . "quickshell/caelestia")
                           ("files/xdg-desktop-portal" . "xdg-desktop-portal")
                           ;; Emacs — symlinked so edits take effect without reconfigure
                           ("files/emacs/lisp" . "emacs/lisp")
                           ("files/emacs/snippets" . "emacs/snippets")
                           ("files/emacs/img" . "emacs/img")))
                        ;; Claude Code skills — direct symlink so edits take effect immediately
                        (ensure-symlink (string-append dots "/files/claude-skills")
                                        (string-append home "/.claude/skills"))
                        ;; Wallpaper symlink for hyprpaper (doesn't expand env vars)
                        (ensure-symlink (string-append dots "/pictures/wallpaper")
                                        (string-append home "/.local/share/wallpaper"))
                        ;; Expose Guix fonts to Flatpak apps via ~/.local/share/fonts
                        (ensure-symlink (string-append home
                                          "/.guix-home/profile/share/fonts/truetype")
                                        (string-append home
                                          "/.local/share/fonts/guix-truetype"))
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
                           ;; Emacs individual files
                           ("files/emacs/init.el" . "emacs/init.el")
                           ("files/emacs/early-init.el" . "emacs/early-init.el")))

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

                      ;; caelestia-cli: install via uv tool if missing.
                      ;; Requires Python >=3.13; uv uses its managed CPython download.
                      ;; The uv-managed Python 3.13 binary needs its ELF interpreter
                      ;; patched to point at the Guix store glibc (no /lib64 on Guix).
                      (let* ((home     (getenv "HOME"))
                             (uv-bin  #$(file-append (specification->package "uv") "/bin/uv"))
                             (py313   (string-append home
                                         "/.local/share/uv/python"
                                         "/cpython-3.13.2-linux-x86_64-gnu"
                                         "/bin/python3.13"))
                             (caelestia-bin (string-append home
                                               "/.local/share/uv/bin/caelestia")))
                        ;; Patch Python 3.13 ELF interpreter once so it runs on Guix.
                        ;; uv downloads a FHS Python that hardcodes /lib64/ld-linux-x86-64.so.2
                        ;; which doesn't exist on Guix.  We redirect it to the Guix store glibc
                        ;; by reading our own ELF interpreter path (we are Guile, already running).
                        (when (file-exists? py313)
                          (let* (;; Our own interpreter path is the right glibc ld.so.
                                 (guile-exe (readlink "/proc/self/exe"))
                                 (ld-cmd    (string-append
                                             "readelf -l " guile-exe
                                             " 2>/dev/null | grep 'interpreter'"
                                             " | sed 's/.*\\[//;s/\\].*//'"))
                                 (self-ld   (string-trim-right
                                             (with-output-to-string
                                               (lambda ()
                                                 (system ld-cmd)))))
                                 ;; Detect whether patching is needed.
                                 (ok        (zero? (status:exit-val
                                                    (system (string-append
                                                             py313 " --version >/dev/null 2>&1"))))))
                            (when (and (not ok)
                                       (not (string-null? self-ld))
                                       (file-exists? self-ld))
                              (system* "guix" "shell" "patchelf" "--"
                                       "patchelf" "--set-interpreter" self-ld py313))))
                        ;; Install caelestia tool if not already installed.
                        (unless (file-exists? caelestia-bin)
                          (setenv "UV_PYTHON_PREFERENCE" "managed")
                          (setenv "UV_TOOL_BIN_DIR"
                                  (string-append home "/.local/share/uv/bin"))
                          (system* uv-bin "tool" "install"
                                   "--from"
                                   "caelestia @ git+https://github.com/caelestia-dots/cli@25c473c18ee951f3bdc5928708b7a6608e473484"
                                   "caelestia")))

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

                              ;; Fresh overlay needs a backup before applying
                              (when (file-exists? spice-bin)
                                (system* spice-bin "backup" "apply")))))

                        ;; --- 3. Configure and apply spicetify ---
                        ;; Runs on every reconfigure so theme config is never lost.
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
                          ;; caelestia theme — user.css lives in dotfiles,
                          ;; color.ini is written dynamically by caelestia CLI
                          (ensure-symlink
                           (string-append #$dotfiles-dir
                                          "/files/spicetify/Themes/caelestia/user.css")
                           (string-append home
                                          "/.config/spicetify/Themes/caelestia/user.css"))
                          (system* spice-bin "config"
                                   "current_theme" "caelestia")
                          (system* spice-bin "config"
                                   "color_scheme" "caelestia")
                          (system* spice-bin "apply")))

                      ;; Zen Browser — symlink caelestia-generated userChrome.css
                      ;; Find the default profile path from profiles.ini
                      (let* ((home (getenv "HOME"))
                             (zen-dir (string-append
                                       home "/.var/app/app.zen_browser.zen/.zen"))
                             (profiles-ini (string-append
                                            zen-dir "/profiles.ini")))
                        (when (file-exists? profiles-ini)
                          ;; Scan for Default= inside [Install...] section
                          (let ((profile-rel
                                 (call-with-input-file profiles-ini
                                   (lambda (p)
                                     (let loop ((in-install? #f))
                                       (let ((l (read-line p)))
                                         (cond
                                          ((eof-object? l) #f)
                                          ((string-prefix? "[Install" l)
                                           (loop #t))
                                          ((and in-install? (string-prefix? "[" l))
                                           #f)
                                          ((and in-install? (string-prefix? "Default=" l))
                                           (substring l (string-length "Default=")))
                                          (else (loop in-install?)))))))))
                            (when profile-rel
                              (let ((zen-chrome (string-append
                                                  zen-dir "/" profile-rel "/chrome")))
                                (when (file-exists? zen-chrome)
                                  (ensure-symlink
                                   (string-append home
                                     "/.local/state/caelestia/theme/zen-userChrome.css")
                                   (string-append zen-chrome "/userChrome.css"))))))))

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
