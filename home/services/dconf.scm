(define-module (home services dconf)
  #:use-module (gnu home services)
  #:use-module (gnu packages gnome)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (home-dconf-service-type
            dconf-entry
            dconf-entry-key
            dconf-entry-value))

(define-record-type* <dconf-entry>
  dconf-entry make-dconf-entry dconf-entry?
  (key   dconf-entry-key)    ; string, e.g. "/org/gnome/desktop/interface/icon-theme"
  (value dconf-entry-value)) ; string, the GVariant value e.g. "'Gruvbox-Plus-Dark'"

(define (dconf-activation-gexp entries)
  "Return a gexp that writes ENTRIES via dconf, only when dbus is reachable."
  #~(let ((bus-addr (getenv "DBUS_SESSION_BUS_ADDRESS")))
      (when (and bus-addr
                 (string-prefix? "unix:path=" bus-addr)
                 (file-exists? (substring bus-addr
                                          (string-length "unix:path="))))
        #$@(map (lambda (entry)
                  #~(system* #$(file-append dconf "/bin/dconf")
                             "write"
                             #$(dconf-entry-key entry)
                             #$(dconf-entry-value entry)))
                entries))))

(define home-dconf-service-type
  (service-type
   (name 'home-dconf)
   (description "Set dconf keys during home activation.")
   (extensions
    (list (service-extension home-activation-service-type
                             dconf-activation-gexp)))
   (compose concatenate)
   (extend append)
   (default-value '())))
