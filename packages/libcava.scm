(define-module (packages libcava)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages commencement))

;; libcava: the cavacore DSP library from cava, built as a shared library
;; with a pkg-config file.  The caelestia shell plugin requires it.
;; Upstream cava only builds cavacore as a static library (CMake) or
;; not at all (autotools), so we build it manually here.
(define-public libcava
  (package
    (inherit cava)
    (name "libcava")
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((src #$(package-source cava))
                 (out #$output)
                 (fftw #$(this-package-input "fftw"))
                 (toolchain #$(this-package-native-input "gcc-toolchain"))
                 (gcc (string-append toolchain "/bin/gcc"))
                 (lib (string-append out "/lib"))
                 (inc (string-append out "/include/cava"))
                 (pc  (string-append lib "/pkgconfig")))
            ;; Put binutils + gcc on PATH so 'as' is found
            (setenv "PATH" (string-append toolchain "/bin"))
            ;; Copy sources to a writable work dir
            (mkdir-p "/tmp/cava-build")
            (copy-file (string-append src "/cavacore.c")
                       "/tmp/cava-build/cavacore.c")
            (copy-file (string-append src "/cavacore.h")
                       "/tmp/cava-build/cavacore.h")
            ;; Compile shared library
            (invoke gcc "-shared" "-fPIC" "-o" "/tmp/cava-build/libcava.so"
                    "/tmp/cava-build/cavacore.c"
                    (string-append "-I" src)
                    (string-append "-I" fftw "/include")
                    (string-append "-L" fftw "/lib")
                    "-lfftw3" "-lm")
            ;; Install
            (mkdir-p lib)
            (mkdir-p inc)
            (mkdir-p pc)
            (copy-file "/tmp/cava-build/libcava.so"
                       (string-append lib "/libcava.so"))
            (copy-file "/tmp/cava-build/cavacore.h"
                       (string-append inc "/cavacore.h"))
            ;; Write pkg-config file (named 'cava' to match what cmake looks for)
            (call-with-output-file (string-append pc "/cava.pc")
              (lambda (port)
                (format port "prefix=~a~%" out)
                (format port "libdir=${prefix}/lib~%")
                (format port "includedir=${prefix}/include~%")
                (format port "~%")
                (format port "Name: cava~%")
                (format port "Description: cavacore DSP library~%")
                (format port "Version: ~a~%" #$(package-version cava))
                (format port "Libs: -L${libdir} -lcava~%")
                ;; fftw3.h is included by cavacore.h, so consumers need it too
                (format port "Requires: fftw3~%")
                (format port "Cflags: -I${includedir}~%")))))))
    (native-inputs
     (list gcc-toolchain))
    (propagated-inputs
     (list fftw))
    (inputs '())
    (synopsis "cavacore DSP shared library (for caelestia shell plugin)")
    (description
     "Builds the cavacore audio processing engine from cava as a shared
library with a pkg-config file, for use by the caelestia shell plugin.")))
