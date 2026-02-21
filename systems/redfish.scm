(define-module (systems redfish))

(use-modules (gnu)
             (gnu system)
             (gnu system shadow)
             (gnu system accounts)
             (guix)
             (guix gexp)
             (gnu services desktop)
             (gnu services xorg)
             (gnu services linux)
             (gnu services base)
             (gnu services containers)
             (gnu services pm)
             (gnu packages admin)
             (gnu packages wm)
             (gnu packages shells)
             (nongnu packages nvidia)
             (nongnu services nvidia)
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (systems base-system)
             (gnu system privilege)
             (srfi srfi-1))

(define %redfish-services
  (append
   %base-services-extra
   (list
    (service rootless-podman-service-type
             (rootless-podman-configuration
              (containers-registries
               (plain-file "registries.conf"
                           "unqualified-search-registries = ['docker.io']\n"))
              (subuids (list (subid-range (name "nehrbash")
                                         (start 100000)
                                         (count 65536))))
              (subgids (list (subid-range (name "nehrbash")
                                         (start 100000)
                                         (count 65536))))))
   (service nvidia-service-type)
   (service power-profiles-daemon-service-type)
    (service greetd-service-type
             (greetd-configuration
              (greeter-supplementary-groups '("video" "input"))
              (terminals
               (list (greetd-terminal-configuration
                      (terminal-vt "1")
                      (terminal-switch #t)
                      (default-session-command
                        (greetd-user-session
                         (command (file-append tuigreet "/bin/tuigreet"))
                         (command-args
                          (list "--time" "--remember" "--remember-session"
                                "--sessions"
                                "/run/current-system/profile/share/wayland-sessions"))))))))))
   (remove (lambda (s)
             (or (eq? (service-kind s) gdm-service-type)
                 (and (eq? (service-kind s) mingetty-service-type)
                      (string=? "tty1" (mingetty-configuration-tty
                                         (service-parameters s))))))
           (modify-services %desktop-services
             (guix-service-type config =>
                                (guix-configuration
                                 (inherit config)
                                 (substitute-urls %substitute-urls)
                                 (authorized-keys %authorized-keys)))))))

(operating-system
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (kernel-arguments '("modprobe.blacklist=nouveau"
                      "nvidia_drm.modeset=1"
                      "quiet" "loglevel=3"))

  (locale "en_US.utf8")
  (timezone "America/New_York")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "redfish")

  (groups (cons* (user-group (name "seat"))
                %base-groups))

  (users (cons* (user-account
                  (name "nehrbash")
                  (comment "Stephen Nehrbass")
                  (group "users")
                  (home-directory "/home/nehrbash")
                  (shell (file-append zsh "/bin/zsh"))
                  (supplementary-groups '("wheel" "netdev" "audio" "video"
                                          "input" "seat" "cgroup")))
                %base-user-accounts))

  (packages (append (list hyprland tuigreet)
                    %base-packages-extra
                    %base-packages))

  (services %redfish-services)

  (privileged-programs %default-privileged-programs)

  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/nvme0n1"))
                (keyboard-layout keyboard-layout)))

  (swap-devices (list (swap-space
                        (target (uuid
                                 "f1a2747c-5559-44c2-8951-1f48840b00ce")))))

  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "D8EB-6158" 'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device (uuid
                                  "183ea0af-a5e2-4a04-bb79-6c94e36c7736"
                                  'ext4))
                         (type "ext4"))
                       (file-system
                         (mount-point "/mnt/arch")
                         (device (uuid
                                  "82ad9826-41b8-4b10-acc5-70093d02b41b"
                                  'btrfs))
                         (type "btrfs"))
                       %base-file-systems)))
