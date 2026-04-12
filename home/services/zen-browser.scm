(define-module (home services zen-browser)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:export (home-zen-browser-service-type))

(define (home-zen-browser-activation _)
  #~(begin
      (use-modules (ice-9 rdelim))
      (let* ((home (getenv "HOME"))
             (zen-dir (string-append
                       home "/.var/app/app.zen_browser.zen/.zen"))
             (profiles-ini (string-append zen-dir "/profiles.ini")))
        (when (file-exists? profiles-ini)
          (let ((profile-rel
                 (call-with-input-file profiles-ini
                   (lambda (p)
                     (let loop ((in-install? #f))
                       (let ((l (read-line p)))
                         (cond
                          ((eof-object? l) #f)
                          ((string-prefix? "[Install" l)
                           (loop #t))
                          ((and in-install? (string-prefix? "[" l))
                           #f)
                          ((and in-install? (string-prefix? "Default=" l))
                           (substring l (string-length "Default=")))
                          (else (loop in-install?)))))))))
            (when profile-rel
              (let ((zen-chrome (string-append
                                  zen-dir "/" profile-rel "/chrome")))
                (when (file-exists? zen-chrome)
                  (let* ((link (string-append zen-chrome "/userChrome.css"))
                         (target (string-append home
                                   "/.local/state/caelestia/theme/zen-userChrome.css"))
                         ;; Use lstat (not file-exists?) so we detect the
                         ;; link itself even if its target does not exist.
                         (lst (false-if-exception (lstat link)))
                         (current (and lst
                                       (eq? 'symlink (stat:type lst))
                                       (false-if-exception (readlink link)))))
                    (unless (and current (string=? current target))
                      (when lst (delete-file link))
                      (symlink target link)
                      (display (string-append target " => " link "\n"))))))))))))

(define home-zen-browser-service-type
  (service-type
   (name 'home-zen-browser)
   (extensions
    (list (service-extension home-activation-service-type
                             home-zen-browser-activation)))
   (default-value #f)
   (description
    "Symlink caelestia-generated userChrome.css into the Zen Browser
default profile.")))
