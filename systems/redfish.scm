(define-module (systems redfish))

(use-modules (gnu)
             (gnu system)
             (guix gexp)
             (gnu services linux)
             (gnu services shepherd)
             (nongnu packages nvidia)
             (nongnu services nvidia)
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (systems oceania)
             (srfi srfi-1))

(operating-system
  (inherit %oceania-os)
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (kernel-arguments '("modprobe.blacklist=nouveau"
                      "nvidia_drm.modeset=1"
                      "nvidia_modeset.vblank_sem_control=0"
                      "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
                      "nvidia.NVreg_TemporaryFilePath=/var/tmp"
                      "resume=UUID=183ea0af-a5e2-4a04-bb79-6c94e36c7736"
                      "resume_offset=68769792"
                      "quiet" "loglevel=3"))

  (host-name "redfish")

  (services (append
             (list
              (service nvidia-service-type)
              (service kernel-module-loader-service-type '("nct6775"))
              ;; Enable persistence mode so PreserveVideoMemoryAllocations works
              (simple-service 'nvidia-persistence shepherd-root-service-type
                (list (shepherd-service
                        (provision '(nvidia-persistence))
                        (requirement '(udev))
                        (one-shot? #t)
                        (start #~(lambda _
                                   (zero? (system* #$(file-append nvda "/bin/nvidia-smi")
                                                   "-pm" "1")))))))
              (extra-special-file "/etc/elogind/system-sleep/nvidia"
                                  (computed-file "nvidia-sleep"
                                    #~(begin
                                        (call-with-output-file #$output
                                          (lambda (port)
                                            (display "#!/bin/sh\ncase $1 in\n  pre)\n    case $2 in\n      suspend) echo suspend > /proc/driver/nvidia/suspend ;;\n      hibernate) echo hibernate > /proc/driver/nvidia/suspend ;;\n    esac\n    ;;\n  post)\n    echo resume > /proc/driver/nvidia/suspend\n    ;;\nesac\n" port)))
                                        (chmod #$output #o755)))))
             (operating-system-user-services %oceania-os)))

  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/nvme0n1"))
                (keyboard-layout (operating-system-keyboard-layout %oceania-os))))

  (swap-devices (list (swap-space
                        (target "/swapfile"))))

  (file-systems (cons* (file-system
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
                         (type "btrfs")
                         (mount-may-fail? #t)
                         (check? #f)
                         (create-mount-point? #t))
                       %base-file-systems)))
