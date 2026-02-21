(define-module (home services activation)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu packages node)
  #:use-module (gnu packages glib)
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
                              (symlink target link))))))

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
                           ("files/swaync" . "swaync")
                           ("files/systemd" . "systemd")
                           ("files/caelestia/quickshell" . "quickshell/caelestia")
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
                           ("files/caelestia/shell.json" . "caelestia/shell.json"))))

                      ;; claude-code: install via npm if missing
                      (let ((bin (string-append (getenv "HOME") "/.local/share/npm/bin/claude")))
                        (unless (file-exists? bin)
                          (setenv "npm_config_prefix"
                                  (string-append (getenv "HOME") "/.local/share/npm"))
                          (system* #$(file-append node "/bin/npm")
                                   "install" "-g" "@anthropic-ai/claude-code")))

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
