(define-module (home redfish))

;; Resolve dotfiles root from this module's location on the load path.
(define %dotfiles-dir
  (canonicalize-path
   (string-append (dirname (search-path %load-path "home/redfish.scm"))
                  "/..")))

(use-modules (gnu home)
             (gnu services)
             (guix gexp)
             (packages package-groups)
             (desktop caelestia)
             (home common)
             (home services emacs)
             (home services llama-server)
             (home services podman)
             (home services ssh-agent)
             (home services nvidia-resume)
             (home services flatpak)
             (home services zen-browser)
             (home services dotfiles-symlinks)
             (home services claude-code))

;; Host-specific env vars on top of %common-*-env-vars.
;; NVIDIA + Guix-System-only paths (gcr askpass, guix-home XKB, etc).
(define %redfish-env-vars
  '(("PATH" .
     "$PATH:$HOME/.local/bin:$HOME/.claude/bin:$HOME/.local/share/uv/bin:$HOME/.cargo/bin:$HOME/.local/share/go/bin")
    ;; gcr-ssh-askpass is the GUI fallback for passphrase prompts when
    ;; there is no controlling TTY (e.g. autostart). SSH_ASKPASS_REQUIRE
    ;; is intentionally omitted so ssh-add still uses the terminal when
    ;; available — required for PKCS#11 PIN prompts that gcr can't handle.
    ("SSH_ASKPASS" . "$HOME/.guix-home/profile/libexec/gcr-ssh-askpass")
    ;; Caelestia XKB rules path (Guix has no /usr/share/X11).
    ("CAELESTIA_XKB_RULES_PATH"
     . "$HOME/.guix-home/profile/share/X11/xkb/rules/base.lst")
    ;; NVIDIA driver selection (search-paths handle library discovery).
    ("LIBVA_DRIVER_NAME" . "nvidia")
    ("GBM_BACKEND" . "nvidia-drm")
    ("__GLX_VENDOR_LIBRARY_NAME" . "nvidia")
    ("__GL_GSYNC_ALLOWED" . "1")
    ("__GL_VRR_ALLOWED" . "1")
    ("NVD_BACKEND" . "direct")
    ("DOCKER_HOST" . "unix://$XDG_RUNTIME_DIR/podman/podman.sock")
    ;; Global LD_LIBRARY_PATH is needed here so Hyprland / Quickshell /
    ;; Qt applications pick up the nongnu NVIDIA driver libs installed
    ;; into /run/current-system/profile/lib.  Do NOT replicate this on
    ;; bluefish (Arch) — it breaks foreign binaries there.
    ("LD_LIBRARY_PATH"
     . "/run/current-system/profile/lib:$HOME/.guix-home/profile/lib")))

(home-environment
 (packages
  (append %core-packages %terminal-packages %editor-packages
          %font-packages %media-packages %ai-packages
          %caelestia-desktop-packages
          %texlive-packages %dev-packages %misc-packages))

 (services
  (append
   (list
    ;; Shared base (files, XDG, env, zsh, dconf GTK theme).
    (common-home-files-service %dotfiles-dir)
    (common-xdg-mime-service)
    (common-xdg-configuration-files-service %dotfiles-dir)
    (common-env-vars-service %dotfiles-dir %redfish-env-vars)
    (common-zsh-service %dotfiles-dir)
    (home-dotfiles-symlinks-service %dotfiles-dir)
    (common-gtk-dconf-service)
    (common-spicetify-service)

    ;; Host services — redfish runs a full Guix Home session.
    (service home-emacs-daemon-service-type)
    (service home-llama-server-service-type)
    (service home-podman-socket-service-type)
    (service home-ssh-agent-service-type)
    (service home-nvidia-resume-service-type)
    (service home-flatpak-service-type)
    (service home-zen-browser-service-type)
    (service home-claude-code-service-type))

   ;; Caelestia/Hyprland desktop (shell, wayland sentinel, resizer,
   ;; spicetify watcher, plus dbus/pipewire/xdph/polkit/keyring).
   %caelestia-home-services
   %caelestia-guix-session-services)))
