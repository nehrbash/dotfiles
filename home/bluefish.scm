(define-module (home bluefish)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (guix gexp)
  #:use-module (desktop caelestia)
  #:use-module (home common)
  #:use-module (home services emacs)
  #:use-module (home services ssh-agent)
  #:use-module (home services flatpak)
  #:use-module (home services zen-browser)
  #:use-module (home services dotfiles-symlinks)
  #:use-module (packages fonts))

;; Resolve dotfiles root from this module's location on the load path.
(define %dotfiles-dir
  (canonicalize-path
   (string-append (dirname (search-path %load-path "home/bluefish.scm"))
                  "/..")))

;; Host-specific env vars layered on top of %common-*-env-vars.
;; Arch differences: $HOME/.local/share/bin in PATH, UV_TOOL_BIN_DIR,
;; system gcr-ssh-askpass, system XKB rules, and crucially *no*
;; LD_LIBRARY_PATH — forcing Guix libs onto Arch binaries breaks them.
(define %bluefish-env-vars
  '(("PATH" .
     "$PATH:$HOME/.local/bin:$HOME/.local/share/bin:$HOME/.local/share/uv/bin:$HOME/.cargo/bin:$HOME/.local/share/go/bin")
    ("UV_TOOL_BIN_DIR" . "$HOME/.local/share/uv/bin")
    ("SSH_ASKPASS" . "/usr/lib/gcr-ssh-askpass")
    ("CAELESTIA_XKB_RULES_PATH" . "/usr/share/X11/xkb/rules/base.lst")
    ;; Intel Iris Xe (Alder Lake) — use iHD VA-API driver for hw decode.
    ("LIBVA_DRIVER_NAME" . "iHD")
    ("VDPAU_DRIVER" . "va_gl")))

(home-environment
 ;; On Arch most packages come from pacman/yay.  Guix supplies the zsh
 ;; plugin packages (referenced in common-zsh-service so Guix builds
 ;; them as gexp dependencies) plus the full Caelestia/Hyprland desktop
 ;; stack from (desktop caelestia).
 (packages (append
            ;; Core CLI tools shared with redfish.
            (list git git-lfs curl wget ripgrep jq
                  (specification->package "bind")
                  (specification->package "sqlite")
                  (specification->package "gcc-toolchain")
                  pkg-config
                  (specification->package "file")
                  (specification->package "tree")
                  (specification->package "lsof")
                  (specification->package "strace")
                  (specification->package "zip")
                  (specification->package "unzip")
                  (specification->package "pv")
                  (specification->package "bc"))
            (list alacritty
                  zsh-autosuggestions zsh-autopair
                  zsh-history-substring-search zsh-syntax-highlighting
                  fzf-tab enchant)
            ;; General-purpose fonts.
            (list font-iosevka font-iosevka-aile
                  font-google-noto-sans-cjk font-google-noto-emoji
                  font-iosevka-term-nf)
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
