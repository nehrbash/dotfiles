(define-module (home services dotfiles-symlinks)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:export (home-dotfiles-symlinks-service))

;; Build the activation gexp.  DOTFILES-DIR is the repo root (a string
;; that will be embedded in the gexp via #$).
(define (home-dotfiles-symlinks-service dotfiles-dir)
  (simple-service 'dotfiles-symlinks
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
                           ;; Spicetify caelestia theme
                           ("files/spicetify/Themes/caelestia"
                            . "spicetify/Themes/caelestia")
                           ;; Emacs — symlinked so edits take effect without reconfigure
                           ("files/emacs/lisp" . "emacs/lisp")
                           ("files/emacs/snippets" . "emacs/snippets")
                           ("files/emacs/img" . "emacs/img")))
                        ;; Claude Code — symlinked so edits take effect immediately
                        (ensure-symlink (string-append dots "/files/claude/CLAUDE.md")
                                        (string-append home "/.claude/CLAUDE.md"))
                        (ensure-symlink (string-append dots "/files/claude/settings.json")
                                        (string-append home "/.claude/settings.json"))
                        (ensure-symlink (string-append dots "/files/claude/rules")
                                        (string-append home "/.claude/rules"))
                        (ensure-symlink (string-append dots "/files/claude/skills")
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
                         '(("files/zsh/aliasrc" . "zsh/aliasrc")
                           ("files/zsh/emacs_functions" . "zsh/emacs_functions")
                           ("files/zsh/functions" . "zsh/functions")
                           ;; Emacs individual files
                           ("files/emacs/init.el" . "emacs/init.el")
                           ("files/emacs/early-init.el" . "emacs/early-init.el")
                           ("files/emacs/clean.sh" . "emacs/clean.sh")
                           ("files/starship/starship.toml" . "starship.toml"))))

                      ;; Reload Hyprland config if running in a session
                      (when (getenv "HYPRLAND_INSTANCE_SIGNATURE")
                        (system* "hyprctl" "reload")))))
