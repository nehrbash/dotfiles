(define-module (config systems redfish)
  #:use-modules (gnu)
  #:use-modules (guix)
  #:use-modules (gnu home services ssh)
  #:use-modules (gnu services desktop)
  #:use-modules (gnu services xorg)
  #:use-modules (nongnu packages nvidia)
  #:use-modules (nongnu services nvidia)
  #:use-modules (nongnu packages linux)
  #:use-modules (nongnu system linux-initrd)
  #:use-modules (config systems base-system))
(use-service-modules cups desktop networking ssh xorg)
e(use-service-modules cups desktop networking ssh xorg)
(operating-system
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (kernel-arguments '("modprobe.blacklist=nouveau"
		      ;; wayland
		      "nvidia_drm.modeset=1"))
  (locale "en_US.utf8")
  (timezone "America/New_York")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "home-cin")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "nehrbash")
                  (comment "Stephen Nehrbass")
                  (group "users")
                  (home-directory "/home/nehrbash")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; The list of system services.
  (services
   (append (list (service nvidia-service-type)
                 (service gnome-desktop-service-type)
                 (service openssh-service-type)
                 (service cups-service-type)
                 (set-xorg-configuration
                  (xorg-configuration
                   (drivers '("nvidia"))
                   (modules (cons nvda %default-xorg-modules))
                   (keyboard-layout keyboard-layout))))
           %desktop-services))
  

  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "bb835dfe-725f-4fc9-834c-35d1a88ffd68")))))

  ;; File system mount configuration
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

