(define-module (packages claude-agent-acp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public claude-agent-acp
  (package
    (name "claude-agent-acp")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/zed-industries/claude-agent-acp/releases/download/v"
             version "/claude-agent-acp-linux-x64.tar.gz"))
       (sha256
        (base32 "1l53am0bgx00x1dbpim1a6gs9bamd7h63nxcj4bjhcx2jcghbgyz"))))
    (supported-systems '("x86_64-linux"))
    (build-system copy-build-system)
    (arguments
     (list #:validate-runpath? #f
           #:strip-binaries? #f
           #:install-plan
           #~'(("claude-agent-acp" "bin/claude-agent-acp"))))
    (home-page "https://github.com/zed-industries/claude-agent-acp")
    (synopsis "ACP server for Claude Agent SDK")
    (description
     "claude-agent-acp implements an ACP (Agent Client Protocol) agent using the
Claude Agent SDK.  It allows ACP-compatible clients like agent-shell to interact
with Claude Code.  This is a Bun-compiled binary that requires
@file{/lib64/ld-linux-x86-64.so.2} (see @code{extra-special-file} in the system
configuration).")
    (license license:expat)))
