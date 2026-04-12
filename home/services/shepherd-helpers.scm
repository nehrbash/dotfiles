(define-module (home services shepherd-helpers)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:export (make-simple-shepherd-service))

(define* (make-simple-shepherd-service name command
                                       #:key
                                       (requirement '())
                                       (documentation "")
                                       (environment-variables #~(environ))
                                       (log-file-name #f))
  "Return a shepherd-service that runs COMMAND via make-forkexec-constructor
with standard respawn settings. NAME is a symbol for the provision.
COMMAND and ENVIRONMENT-VARIABLES are gexps. LOG-FILE-NAME, if provided,
overrides the default log file name (without path)."
  (let ((log-name (or log-file-name (symbol->string name))))
    (shepherd-service
     (provision (list name))
     (requirement requirement)
     (documentation documentation)
     (start #~(lambda _
                ((make-forkexec-constructor
                  #$command
                  #:environment-variables #$environment-variables
                  #:log-file
                  (string-append (getenv "XDG_STATE_HOME")
                                 "/log/" #$log-name ".log")))))
     (stop #~(make-kill-destructor))
     (respawn? #t)
     (respawn-limit #~'(3 . 10))
     (respawn-delay 5))))
