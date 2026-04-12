(define-module (systems redfish))

(use-modules (gnu)
             (gnu system)
             (guix gexp)
             (gnu services base)
             (gnu services linux)
             (gnu services shepherd)
             (nongnu packages nvidia)
             (nongnu services nvidia)
             (gnu packages linux)
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (systems oceania)
             (srfi srfi-1))

;; Custom kernel tuned for AMD Ryzen 7 7700X (Zen 4).
(define linux-optimized
  (customize-linux
   #:name "linux-optimized"
   #:linux linux
   #:configs
   '("CONFIG_X86_NATIVE_CPU=y"
     "CONFIG_HZ_1000=y"
     "# CONFIG_HZ_250 is not set"
     "CONFIG_HZ=1000"
     "CONFIG_PREEMPT=y"
     "# CONFIG_PREEMPT_VOLUNTARY is not set"
     "# CONFIG_NUMA is not set"
     "# CONFIG_AMD_NUMA is not set"
     "# CONFIG_X86_64_ACPI_NUMA is not set"
     "# CONFIG_ACPI_NUMA is not set"
     "# CONFIG_NUMA_BALANCING is not set"
     "# CONFIG_X86_NUMACHIP is not set")))

(operating-system
  (inherit %oceania-os)
  (kernel linux-optimized)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (kernel-arguments '("modprobe.blacklist=nouveau"
                      "nvidia_drm.modeset=1"
                      "nvidia_modeset.vblank_sem_control=0"
                      "nvidia.NVreg_PreserveVideoMemoryAllocations=1"
                      "nvidia.NVreg_TemporaryFilePath=/var/tmp"
                      "quiet" "loglevel=3"))

  (host-name "redfish")

  (services (append
             (list
              ;; Pin substitutes.nonguix.org to its IPv6 address. The v4
              ;; endpoint (144.76.7.123) is unreachable from this network
              ;; as of 2026-04-07, which causes guix substitute --query
              ;; to hang on connect. Drop this entry once nonguix v4 is
              ;; reachable again.
              (simple-service 'pin-nonguix-v6 hosts-service-type
                              (list (host "2a01:4f8:190:8242::1"
                                          "substitutes.nonguix.org")))
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
                ;; Use by-id because NVMe slot enumeration isn't stable:
                ;; what shows as nvme1n1 today may be nvme0n1 after reboot.
                ;; This is the Crucial P3 1TB containing /boot + /.
                (targets (list "/dev/disk/by-id/nvme-CT1000P3SSD8_24464C12E963"))
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
