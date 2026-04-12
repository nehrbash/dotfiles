(define-module (home services llama-server)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages machine-learning)  ; llama-cpp
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages vulkan)      ; vulkan-headers, shaderc (glslc)
  #:use-module (gnu services configuration)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (home services shepherd-helpers)
  #:use-module (packages ai-models)
  #:export (llama-cpp-git
            home-llama-server-configuration
            home-llama-server-service-type))

;; Upstream Guix ships llama-cpp at build b8445, which predates the
;; `gemma4' architecture.  Build from the local checkout at
;; ~/src/llama.cpp so Gemma 4 GGUFs load.  Uses the bundled ggml (not
;; the Guix @code{ggml} package) because the vendored version is
;; tightly coupled to llama.cpp's own tensor arch enum.
(define llama-cpp-git
  (package/inherit llama-cpp
    (name "llama-cpp")
    (version "git")
    (source (local-file "../../../llama.cpp"
                        "llama-cpp-checkout"
                        #:recursive? #t
                        #:select?
                        (lambda (file stat)
                          ;; Exclude only VCS metadata and out-of-tree
                          ;; build output dirs.  Don't exclude /models/
                          ;; — llama.cpp has src/models/ in the tree.
                          (not (or (string-contains file "/.git/")
                                   (string-contains file "/build/")
                                   (string-contains file "/build-release/")
                                   (string-contains file "/build-debug/"))))))
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DBUILD_SHARED_LIBS=ON"
              "-DLLAMA_USE_SYSTEM_GGML=OFF"
              "-DGGML_VULKAN=ON"
              "-DLLAMA_BUILD_TESTS=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-shebangs 'fix-python-shebang
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((script (string-append #$output
                                           "/bin/convert_hf_to_gguf.py")))
                (when (file-exists? script)
                  (substitute* script
                    (("^#!.*/bin/python3")
                     (string-append "#!"
                                    (search-input-file inputs "bin/env")
                                    " python3"))))))))))
    (native-inputs
     (modify-inputs (package-native-inputs llama-cpp)
       (prepend pkg-config shaderc)))
    (inputs
     (modify-inputs (package-inputs llama-cpp)
       (prepend vulkan-headers vulkan-loader)))))

;; Default model: Gemma 4 E4B Instruct, Unsloth UD-Q4_K_XL, fetched via
;; the `gemma-4-e4b-it-gguf' package in (packages ai-models).
(define %default-model
  (file-append gemma-4-e4b-it-gguf
               "/share/ai-models/gemma-4-E4B-it-UD-Q4_K_XL.gguf"))

(define-configuration/no-serialization home-llama-server-configuration
  (package
   (file-like llama-cpp-git)
   "llama-cpp package providing @command{llama-server}.")
  (model
   (file-like %default-model)
   "GGUF model file to serve.  Defaults to Gemma 4 E4B UD-Q4_K_XL.")
  (host
   (string "127.0.0.1")
   "Address @command{llama-server} binds to.")
  (port
   (integer 8080)
   "Port @command{llama-server} listens on.")
  (context-size
   (integer 16384)
   "Maximum prompt + response tokens kept in the KV cache.")
  (gpu-layers
   (integer 999)
   "Number of model layers to offload to the GPU.  999 = all of them.")
  (extra-args
   (list '())
   "Additional command-line arguments appended verbatim."))

(define (home-llama-server-shepherd-service config)
  (let ((pkg      (home-llama-server-configuration-package config))
        (model    (home-llama-server-configuration-model config))
        (host     (home-llama-server-configuration-host config))
        (port     (home-llama-server-configuration-port config))
        (ctx      (home-llama-server-configuration-context-size config))
        (ngl      (home-llama-server-configuration-gpu-layers config))
        (extra    (home-llama-server-configuration-extra-args config)))
    (list
     (make-simple-shepherd-service
      'llama-server
      #~(append
         (list #$(file-append pkg "/bin/llama-server")
               "--model" #$model
               "--host" #$host
               "--port" #$(number->string port)
               "--ctx-size" #$(number->string ctx)
               "--n-gpu-layers" #$(number->string ngl)
               "--jinja"          ; needed for Gemma's chat template
               "--flash-attn" "on")
         '#$extra)
      #:documentation
      "Run llama.cpp's OpenAI-compatible server against a local GGUF.
Used as a gptel backend in Emacs."))))

(define (home-llama-server-profile-service config)
  (list (home-llama-server-configuration-package config)))

(define home-llama-server-service-type
  (service-type
   (name 'home-llama-server)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-llama-server-shepherd-service)
          (service-extension home-profile-service-type
                             home-llama-server-profile-service)))
   (default-value (home-llama-server-configuration))
   (description
    "Run @command{llama-server} as a Shepherd user service, exposing a
local GGUF model via llama.cpp's OpenAI-compatible HTTP API.")))
