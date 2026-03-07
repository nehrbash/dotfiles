(define-module (home services uv-python)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:export (home-uv-python-service-type))

(define (home-uv-python-activation _)
  #~(begin
      (use-modules (ice-9 string-fun))
      (let* ((home   (getenv "HOME"))
             (py313  (string-append home
                        "/.local/share/uv/python"
                        "/cpython-3.13.2-linux-x86_64-gnu"
                        "/bin/python3.13")))
        ;; Patch Python 3.13 ELF interpreter once so it runs on Guix.
        (when (file-exists? py313)
          (let* ((guile-exe (readlink "/proc/self/exe"))
                 (ld-cmd    (string-append
                             "readelf -l " guile-exe
                             " 2>/dev/null | grep 'interpreter'"
                             " | sed 's/.*\\[//;s/\\].*//'"))
                 (self-ld   (string-trim-right
                             (with-output-to-string
                               (lambda ()
                                 (system ld-cmd)))))
                 (ok        (zero? (status:exit-val
                                    (system (string-append
                                             py313 " --version >/dev/null 2>&1"))))))
            (when (and (not ok)
                       (not (string-null? self-ld))
                       (file-exists? self-ld))
              (system* "guix" "shell" "patchelf" "--"
                       "patchelf" "--set-interpreter" self-ld py313)))))))

(define home-uv-python-service-type
  (service-type
   (name 'home-uv-python)
   (extensions
    (list (service-extension home-activation-service-type
                             home-uv-python-activation)))
   (default-value #f)
   (description
    "Patch uv-managed Python 3.13 ELF interpreter for Guix.")))
