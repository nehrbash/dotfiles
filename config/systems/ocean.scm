(define-module (config systems ocean)
  #:use-modules (gnu)
  #:use-modules (gnu system)
  #:use-modules (guix)
  #:use-modules (guix gexp)
  #:use-modules (gnu services desktop)
  #:use-modules (gnu services xorg)
  #:use-modules (gnu services linux)
  #:use-modules (gnu services sddm)
  #:use-modules (gnu packages wm)
  #:use-modules (nongnu packages nvidia)
  #:use-modules (nongnu services nvidia)
  #:use-modules (nongnu packages linux)
  #:use-modules (nongnu system linux-initrd)
  #:use-modules (config systems base-system))

(use-service-modules desktop networking)

(operating-system
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (kernel-arguments '("modprobe.blacklist=nouveau"
                      "nvidia_drm.modeset=1"))
  (kernel-loadable-modules (list nvidia-driver))

  (locale "en_US.utf8")
  (timezone "America/New_York")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "home-cin")

  (users (cons* (user-account
                  (name "nehrbash")
                  (comment "Stephen Nehrbass")
                  (group "users")
                  (home-directory "/home/nehrbash")
                  (supplementary-groups '("wheel" "netdev" "audio" "video"
                                          "input" "seat")))
                %base-user-accounts))

  (packages (append %base-packages-extra
                    %base-packages))

  (services
   (append
    %base-services-extra
    (list
     ;; NVIDIA
     (service nvidia-service-type)

     ;; greetd — launches Hyprland (replaces GNOME)
     (service greetd-service-type
              (greetd-configuration
               (greeter-supplementary-groups '("video" "input"))
               (terminals
                (list (greetd-terminal-configuration
                       (terminal-vt "1")
                       (terminal-switch #t)
                       (default-session-command
                         (greetd-agreety-session
                          (command (file-append hyprland "/bin/Hyprland")))))))))

     ;; Flatpak for proprietary apps (Zen Browser, Spotify, Slack, etc.)
     (service flatpak-service-type))

    ;; Filter out gdm from %desktop-services since we use greetd
    (modify-services %desktop-services
      (delete gdm-service-type)
      (guix-service-type config =>
                         (guix-configuration
                          (inherit config)
                          (substitute-urls %substitute-urls)
                          (authorized-keys %authorized-keys))))))

  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))

  (swap-devices (list (swap-space
                        (target (uuid
                                 "bb835dfe-725f-4fc9-834c-35d1a88ffd68")))))

  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "8002-04B5"
                                       'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device (uuid
                                  "38f5ef5a-1c85-4acb-9062-780595c4c975"
                                  'ext4))
                         (type "ext4"))
                       %base-file-systems)))
