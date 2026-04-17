(define-module (packages gpu-screen-recorder)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages))

;; gpu-screen-recorder — hardware-accelerated screen recorder for NVIDIA,
;; AMD, and Intel GPUs on Linux.
(define-public gpu-screen-recorder
  (package
    (name "gpu-screen-recorder")
    (version "5.12.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://dec05eba.com/snapshot/gpu-screen-recorder.git."
             version ".tar.gz"))
       (sha256
        (base32 "0yavw4b66vkmp666lsr21k784vr29qq90hjz8vgzhc6ifvwvw1rj"))))
    (build-system meson-build-system)
    (native-inputs
     (list meson
           ninja
           pkg-config
           vulkan-headers
           ;; wayland-scanner is needed for the protocol/ sub-directory
           wayland))
    (inputs
     (list dbus
           ffmpeg
           libcap
           libdrm
           libglvnd
           libjpeg-turbo
           libva
           libxcomposite
           libxdamage
           libxfixes
           libxrandr
           libx11
           pipewire
           pulseaudio
           wayland))
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-Dsystemd=false"     ; Guix uses Shepherd, not systemd
              "-Dcapabilities=false" ; setcap requires root, skip
              "-Dportal=true"        ; PipeWire portal capture
              "-Dapp_audio=true"     ; per-app audio via PipeWire
              "--buildtype=release")
      #:phases
      #~(modify-phases %standard-phases
          ;; The tarball has no top-level directory so files land flat in
          ;; the build root.  The unpack phase chdir's into 'external/'
          ;; (the first subdirectory), but meson.build is one level up.
          (add-after 'unpack 'fix-source-dir
            (lambda _
              (chdir "..")))
          ;; gpu-screen-recorder resolves gsr-kms-server via dirname of
          ;; /proc/self/exe first, and only falls back to PATH when no
          ;; sibling is found.  On Guix the store bin/ is immutable so we
          ;; can't grant cap_sys_admin there; instead we relocate the
          ;; helper to libexec/ so PATH lookup wins and finds the
          ;; capability-wrapped copy under /run/privileged/bin/ (installed
          ;; via the system's privileged-programs declaration).
          (add-after 'install 'relocate-kms-server
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (libexec (string-append #$output
                                             "/libexec/gpu-screen-recorder")))
                (mkdir-p libexec)
                (rename-file (string-append bin "/gsr-kms-server")
                             (string-append libexec "/gsr-kms-server"))))))))
    (home-page "https://git.dec05eba.com/gpu-screen-recorder")
    (synopsis "Hardware-accelerated screen recorder for Linux")
    (description
     "gpu-screen-recorder is a shadowplay-like screen recorder that captures
frames directly from the GPU, avoiding expensive CPU round-trips.  It
supports NVIDIA, AMD (VA-API), and Intel GPUs and can record individual
windows, monitors, or selected regions.  It is used by @code{caelestia
record} for screen recording.")
    (license license:gpl3)))
