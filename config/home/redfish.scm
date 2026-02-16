(use-modules (gnu)
             (gnu home)
             (gnu home services)
             (gnu home services dotfiles)
             (gnu home services xdg)
             (gnu home services shells)
             (gnu home services shepherd)
             (gnu packages)
             (gnu packages version-control)
             (gnu packages emacs)
             (gnu packages ssh)
             (gnu packages admin)
             (gnu packages terminals)

             (gnu packages video)
             (gnu packages fonts)
             (gnu packages aspell)
             (gnu packages text-editors)
             (gnu packages rust-apps)
             (gnu packages curl)
             (gnu packages shellutils)
             (gnu packages golang)
             (gnu packages wm)
             (gnu packages xdisorg)
             (gnu packages glib)
             (gnu packages music)
             (gnu packages mail)
             (gnu packages texlive)
             (gnu packages haskell-apps)
             (gnu packages compression)
             (gnu packages wget)
             (gnu packages web)
             (gnu packages freedesktop)
             (gnu packages image)
             (gnu packages linux)
             (gnu packages pulseaudio)
             (guix gexp)
             (config packages zsh-pure-prompt))

(home-environment

 (packages
  (list
   ;; Core tools
   git
   curl
   wget
   ripgrep
   jq

   ;; Terminal & shell
   alacritty
   exa
   zsh-pure-prompt

   ;; Editor
   emacs-next-pgtk

   ;; Fonts
   font-iosevka
   font-iosevka-aile
   font-awesome

   ;; Wayland / Hyprland
   hyprland
   xdg-desktop-portal-hyprland
   xdg-desktop-portal-gtk
   wl-clipboard
   grim
   playerctl
   brightnessctl
   swayidle
   wlr-randr

   ;; Audio
   pavucontrol

   ;; Video
   vlc

   ;; Spellcheck
   aspell
   aspell-dict-en

   ;; Mail
   isync

   ;; TeX
   texlive

   ;; Languages
   go

   ;; Dev tools
   shellcheck

   ;; SSH
   openssh))

 (services
  (list

   ;; Dotfiles — replaces GNU Stow
   (service home-dotfiles-service-type
            (home-dotfiles-configuration
             (layout 'stow)
             (directories '("~/.dotfiles"))
             (excluded '("^\\.config/hypr"))))

   ;; Hyprland config files (managed explicitly)
   (service home-xdg-configuration-files-service-type
            `(("hypr/hyprland.conf"
               ,(local-file "../../.config/hypr/hyprland.conf"))
              ("hypr/env.conf"
               ,(local-file "../../.config/hypr/env.conf"))
              ("hypr/monitors.conf"
               ,(local-file "../../.config/hypr/monitors.conf"))
              ("hypr/plugins.conf"
               ,(local-file "../../.config/hypr/plugins.conf"))
              ("hypr/spotify.conf"
               ,(local-file "../../.config/hypr/spotify.conf"))
              ("hypr/hypridle.conf"
               ,(local-file "../../.config/hypr/hypridle.conf"))
              ("hypr/workspaces.conf"
               ,(local-file "../../.config/hypr/workspaces.conf"))
              ("hypr/hyprpaper.conf"
               ,(local-file "../../.config/hypr/hyprpaper.conf"))))

   ;; Environment variables (consolidates .zshenv)
   (simple-service 'my-env-vars
                   home-environment-variables-service-type
                   '(("EDITOR" . "emacs -nw")
                     ("TERM" . "xterm-256color")
                     ("PATH" . "$PATH:$HOME/.local/bin:$HOME/.config/eww/bin:$HOME/.cargo/bin:$HOME/.npm-global/bin")
                     ("SSH_AUTH_SOCK" . "$XDG_RUNTIME_DIR/ssh-agent.socket")
                     ("PKG_CONFIG_PATH" . "/usr/lib/pkgconfig:$PKG_CONFIG_PATH")))

   ;; Zsh
   (service home-zsh-service-type
            (home-zsh-configuration
             (zshrc
              (list (local-file "../../.zshrc" "zshrc")))
             (zprofile
              (list (plain-file "zprofile" "")))))

   ;; Automated setup (idempotent, runs on every reconfigure)
   (simple-service 'post-setup
                   home-activation-service-type
                   #~(begin
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
                          "com.slack.Slack"))

                       ;; hyprshell: build if binary missing
                       (let ((bin (string-append (getenv "HOME")
                                                 "/.config/eww/bin/hyprshell")))
                         (unless (file-exists? bin)
                           (mkdir-p (dirname bin))
                           (setenv "GOBIN" (dirname bin))
                           (system* #$(file-append go "/bin/go")
                                    "install"
                                    "github.com/nehrbash/hyprshell@latest")))))

   ;; Shepherd user services
   (service home-shepherd-service-type
            (home-shepherd-configuration
             (services
              (list

               ;; Emacs daemon
               (shepherd-service
                (provision '(emacs))
                (documentation "Run Emacs as a daemon.")
                (start #~(make-forkexec-constructor
                          (list #$(file-append emacs-next-pgtk "/bin/emacs")
                                "--fg-daemon")
                          #:log-file
                          (string-append (getenv "XDG_STATE_HOME")
                                         "/log/emacs.log")))
                (stop #~(make-kill-destructor))
                (respawn? #f))

               ;; ssh-agent
               (shepherd-service
                (provision '(ssh-agent))
                (documentation "Run ssh-agent.")
                (start #~(make-forkexec-constructor
                          (list #$(file-append openssh "/bin/ssh-agent")
                                "-D" "-a"
                                (string-append (getenv "XDG_RUNTIME_DIR")
                                               "/ssh-agent.socket"))
                          #:log-file
                          (string-append (getenv "XDG_STATE_HOME")
                                         "/log/ssh-agent.log")))
                (stop #~(make-kill-destructor)))))))

   )))
