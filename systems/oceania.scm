(define-module (systems oceania)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (gnu system accounts)
  #:use-module (guix)
  #:use-module (guix gexp)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services cups)
  #:use-module (gnu services xorg)
  #:use-module (gnu services linux)
  #:use-module (gnu services containers)
  #:use-module (gnu services pm)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services security-token)
  #:use-module (gnu packages base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages shells)
  #:use-module (gnu system privilege)
  #:use-module (srfi srfi-1)
  #:use-module (packages gpu-screen-recorder)
  #:export (%oceania-os))

(define %substitute-urls
  '("https://bordeaux-us-east-mirror.cbaines.net"
    "https://substitutes.nonguix.org"))

(define %authorized-keys
  (append %default-authorized-guix-keys
          (list (plain-file "nonguix.pub"
                            "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))))

(define %oceania-services
  (append
   (list
    ;; Provide /lib64/ld-linux-x86-64.so.2 so foreign binaries (Bun SFEs) work.
    ;; Bun single-file executables cannot be patchelf'd (corrupts page alignment)
    ;; and ld-linux wrappers fail (/proc/self/exe resolves to wrapper).
    ;; Upstream: https://github.com/oven-sh/bun/issues/24742 (open as of 2025-11)
    (extra-special-file "/lib64/ld-linux-x86-64.so.2"
                        (file-append glibc "/lib/ld-linux-x86-64.so.2"))
    (service openssh-service-type
             (openssh-configuration
              (permit-root-login 'prohibit-password)))
    (service cups-service-type
             (cups-configuration
              (web-interface? #t)))
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
    (service libvirt-service-type
             (libvirt-configuration
              (unix-sock-group "libvirt")))
    (service virtlog-service-type)
    (service power-profiles-daemon-service-type)
    (service pcscd-service-type)
    (extra-special-file "/etc/dbus-1/system.d/increase-limits.conf"
                        (plain-file "increase-limits.conf"
                                    "<busconfig>
  <limit name=\"max_connections_per_user\">1024</limit>
</busconfig>\n"))
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

(define %oceania-os
  (operating-system
    (host-name "oceania")
    (locale "en_US.utf8")
    (timezone "America/New_York")
    (keyboard-layout (keyboard-layout "us"))

    (groups (cons* (user-group (name "seat"))
                   %base-groups))

    (users (cons* (user-account
                    (name "nehrbash")
                    (comment "Stephen Nehrbass")
                    (group "users")
                    (home-directory "/home/nehrbash")
                    (shell (file-append zsh "/bin/zsh"))
                    (supplementary-groups '("wheel" "netdev" "audio" "video"
                                            "input" "seat" "cgroup"
                                            "libvirt" "kvm")))
                  %base-user-accounts))

    (packages (append (list hyprland tuigreet)
                      (list zsh)
                      %base-packages))

    (services %oceania-services)

    (privileged-programs
     (cons* (privileged-program
             (program (file-append gpu-screen-recorder "/bin/gsr-kms-server"))
             (capabilities "cap_sys_admin+ep"))
            %default-privileged-programs))

    ;; Placeholder — each machine overrides these.
    (bootloader (bootloader-configuration
                  (bootloader grub-bootloader)
                  (targets '("/dev/sda"))))
    (file-systems %base-file-systems)))
