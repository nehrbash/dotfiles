(define-module (home services claude-code)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:export (home-claude-code-service-type))

(define gcs-bucket
  "https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases")

(define npm-version-url
  "https://registry.npmjs.org/@anthropic-ai/claude-code/latest")

(define (home-claude-code-activation _)
  #~(begin
      (use-modules (ice-9 popen) (ice-9 rdelim))
      (let* ((home    (getenv "HOME"))
           (bin-dir (string-append home "/.claude/bin"))
           (claude  (string-append bin-dir "/claude")))
      (unless (file-exists? claude)
        (display "claude-code: not found, installing latest…\n")
        (mkdir-p bin-dir)
        ;; Get latest version from npm registry, download the binary.
        (let ((version-port (open-input-pipe
                             (string-append
                              "curl -sf " #$npm-version-url
                              " | grep -o '\"version\":\"[^\"]*\"'"
                              " | head -1"
                              " | sed 's/\"version\":\"\\(.*\\)\"/\\1/'"))))
          (let ((version (read-line version-port)))
            (close-pipe version-port)
            (when (and (string? version) (not (string-null? version)))
              (let* ((url (string-append #$gcs-bucket "/" version
                                         "/linux-x64/claude"))
                     (tmp (string-append claude ".tmp")))
                (display (string-append "claude-code: downloading v" version "…\n"))
                (when (zero? (system* "curl" "-fSL" "-o" tmp url))
                  (chmod tmp #o755)
                  (rename-file tmp claude)
                  (display "claude-code: installed successfully\n"))))))))))

(define home-claude-code-service-type
  (service-type
   (name 'home-claude-code)
   (extensions
    (list (service-extension home-activation-service-type
                             home-claude-code-activation)))
   (default-value #f)
   (description
    "Install Claude Code to ~/.claude/bin if not already present.
Claude Code manages its own upgrades after the initial install.")))
