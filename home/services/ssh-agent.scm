(define-module (home services ssh-agent)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages security-token)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:export (home-ssh-agent-configuration
            home-ssh-agent-service-type))

(define-configuration/no-serialization home-ssh-agent-configuration
  (pkcs11-whitelist
   (string "/gnu/store/*/lib/libykcs11.so*")
   "Glob pattern passed to @option{-P} for PKCS#11 provider whitelist.")
  (extra-packages
   (list '())
   "Additional packages to add to the profile (e.g. yubico-piv-tool)."))

(define (home-ssh-agent-shepherd-service config)
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
                       "-P" #$(home-ssh-agent-configuration-pkcs11-whitelist config))
                 #:log-file
                 (string-append (getenv "XDG_STATE_HOME")
                                "/log/ssh-agent.log")))))
    (stop #~(make-kill-destructor)))))

(define (home-ssh-agent-profile-service config)
  (cons openssh (home-ssh-agent-configuration-extra-packages config)))

(define home-ssh-agent-service-type
  (service-type
   (name 'home-ssh-agent)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-ssh-agent-shepherd-service)
          (service-extension home-profile-service-type
                             home-ssh-agent-profile-service)))
   (default-value (home-ssh-agent-configuration
                   (extra-packages (list yubico-piv-tool))))
   (description
    "Run ssh-agent with PKCS#11 provider whitelist for YubiKey.")))
