(define-module (home bluefish))

;; Resolve dotfiles root from this module's location on the load path.
(define %dotfiles-dir
  (canonicalize-path
   (string-append (dirname (search-path %load-path "home/bluefish.scm"))
                  "/..")))

(use-modules (gnu home)
             (gnu services)
             (gnu packages enchant)
             (gnu packages shellutils)
             (gnu packages terminals)
             (guix gexp)
             (desktop caelestia)
             (home common)
             (home services emacs)
             (home services ssh-agent)
             (home services flatpak)
             (home services zen-browser)
             (home services dotfiles-symlinks)
             (packages package-groups))

;; Host-specific env vars layered on top of %common-*-env-vars.
;; Arch differences: $HOME/.local/share/bin in PATH, UV_TOOL_BIN_DIR,
;; system gcr-ssh-askpass, system XKB rules, and crucially *no*
;; LD_LIBRARY_PATH — forcing Guix libs onto Arch binaries breaks them.
(define %bluefish-env-vars
  '(("PATH" .
     "$PATH:$HOME/.local/bin:$HOME/.local/share/bin:$HOME/.local/share/uv/bin:$HOME/.cargo/bin:$HOME/.local/share/go/bin")
    ("UV_TOOL_BIN_DIR" . "$HOME/.local/share/uv/bin")
    ("SSH_ASKPASS" . "/usr/lib/gcr-ssh-askpass")
    ("CAELESTIA_XKB_RULES_PATH" . "/usr/share/X11/xkb/rules/base.lst")))

(home-environment
 ;; On Arch most packages come from pacman/yay.  Guix supplies the zsh
 ;; plugin packages (referenced in common-zsh-service so Guix builds
 ;; them as gexp dependencies) plus the full Caelestia/Hyprland desktop
 ;; stack from (desktop caelestia).
 (packages (append
            (list alacritty
                  zsh-autosuggestions zsh-autopair
                  zsh-history-substring-search zsh-syntax-highlighting
                  fzf-tab enchant)
            %font-packages
            %caelestia-desktop-packages))

 (services
  (append
   (list
    ;; Shared base.
    (common-home-files-service %dotfiles-dir)
    (common-xdg-mime-service)
    (common-xdg-configuration-files-service %dotfiles-dir)
    (common-env-vars-service %dotfiles-dir %bluefish-env-vars)
    (common-zsh-service %dotfiles-dir)
    (home-dotfiles-symlinks-service %dotfiles-dir)
    (common-gtk-dconf-service)
    (common-spicetify-service)

    ;; Host services.  dbus, pipewire, gnome-keyring, polkit stay with
    ;; systemd on Arch.  xdph runs under shepherd since Hyprland comes
    ;; from Guix (Arch's xdph package is tied to Arch's hyprland).
    (service home-xdph-service-type)
    (service home-emacs-daemon-service-type)
    (service home-ssh-agent-service-type
             (home-ssh-agent-configuration
              (pkcs11-whitelist "/usr/lib/libykcs11.so*")))
    (service home-flatpak-service-type)
    (service home-zen-browser-service-type))

   %caelestia-home-services)))
