(define-module (config home ocean)
  #:use-modules (gnu)
  #:use-modules (gnu home)
  #:use-modules (gnu home services)
  #:use-modules (gnu home services dotfiles)
  #:use-modules (gnu home services shells)
  #:use-modules (gnu home services shepherd)
  #:use-modules (gnu packages)
  #:use-modules (gnu packages version-control)
  #:use-modules (gnu packages emacs)
  #:use-modules (gnu packages ssh)
  #:use-modules (gnu packages admin)
  #:use-modules (gnu packages terminals)
  #:use-modules (gnu packages web-browsers)
  #:use-modules (gnu packages video)
  #:use-modules (gnu packages fonts)
  #:use-modules (gnu packages aspell)
  #:use-modules (gnu packages text-editors)
  #:use-modules (gnu packages rust-apps)
  #:use-modules (gnu packages curl)
  #:use-modules (gnu packages shellutils)
  #:use-modules (gnu packages golang)
  #:use-modules (gnu packages wm)
  #:use-modules (gnu packages xdisorg)
  #:use-modules (gnu packages glib)
  #:use-modules (gnu packages music)
  #:use-modules (gnu packages mail)
  #:use-modules (gnu packages tex)
  #:use-modules (gnu packages check)
  #:use-modules (gnu packages compression)
  #:use-modules (guix gexp))

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
             (directories '("~/.dotfiles"))))

   ;; Environment variables (consolidates .zshenv)
   (service home-environment-variables-service-type
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
