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
    (version "5.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://dec05eba.com/snapshot/gpu-screen-recorder.git."
             version ".tar.gz"))
       (sha256
        (base32 "1p9v41vaiif7hi0k5aqxvfh7ry8nig7qg6na40rhn7d723wq7d36"))))
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
              (chdir ".."))))))
    (home-page "https://git.dec05eba.com/gpu-screen-recorder")
    (synopsis "Hardware-accelerated screen recorder for Linux")
    (description
     "gpu-screen-recorder is a shadowplay-like screen recorder that captures
frames directly from the GPU, avoiding expensive CPU round-trips.  It
supports NVIDIA, AMD (VA-API), and Intel GPUs and can record individual
windows, monitors, or selected regions.  It is used by @code{caelestia
record} for screen recording.")
    (license license:gpl3)))
