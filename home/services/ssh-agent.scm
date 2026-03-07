(define-module (home services ssh-agent)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages security-token)
  #:use-module (guix gexp)
  #:export (home-ssh-agent-service-type))

(define (home-ssh-agent-shepherd-service _)
  (list
   (shepherd-service
    (provision '(ssh-agent))
    (documentation "Run ssh-agent.")
    (start #~(lambda _
               ((make-forkexec-constructor
                 (list #$(file-append openssh "/bin/ssh-agent")
                       "-D" "-a"
                       (string-append (getenv "XDG_RUNTIME_DIR")
                                      "/ssh-agent.socket")
                       "-P" "/gnu/store/*/lib/libykcs11.so*")
                 #:log-file
                 (string-append (getenv "XDG_STATE_HOME")
                                "/log/ssh-agent.log")))))
    (stop #~(make-kill-destructor)))))

(define (home-ssh-agent-profile-service _)
  (list openssh yubico-piv-tool))

(define home-ssh-agent-service-type
  (service-type
   (name 'home-ssh-agent)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-ssh-agent-shepherd-service)
          (service-extension home-profile-service-type
                             home-ssh-agent-profile-service)))
   (default-value #f)
   (description
    "Run ssh-agent with PKCS#11 provider whitelist for YubiKey.")))
