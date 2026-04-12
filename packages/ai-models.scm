;; AI model weights packaged as fixed-output derivations.
;;
;; Keeping models in the Guix store means:
;;   - They are content-addressed and deduplicated.
;;   - Reconfigures don't refetch them.
;;   - Shepherd services can reference them by exact store path.
;;
;; The weight files are pulled directly from Hugging Face CDN URLs that
;; resolve to the git-LFS object.  The sha256 values come from the HF
;; API tree listing (`.lfs.oid`), converted hex -> nix-base32.

(define-module (packages ai-models)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))


;;;
;;; Gemma 4 E4B Instruct — Unsloth Dynamic Q4_K_XL quantization.
;;; ~5.1 GB.  The "effective 4B" edge model; fits an 8 GB GPU with
;;; room for KV cache.  Text-only (no mmproj shipped here).
;;;

(define-public gemma-4-e4b-it-gguf
  (package
    (name "gemma-4-e4b-it-gguf")
    (version "UD-Q4_K_XL")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://huggingface.co/unsloth/gemma-4-E4B-it-GGUF/resolve/"
             "main/gemma-4-E4B-it-UD-Q4_K_XL.gguf"))
       (file-name "gemma-4-E4B-it-UD-Q4_K_XL.gguf")
       (sha256
        (base32 "1zsr7qb07w1k2rj37rvccic68g26yxlib4bj4zsqilgi36sy76lb"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~`((,(string-append #$name ".gguf")
                "share/ai-models/gemma-4-E4B-it-UD-Q4_K_XL.gguf"))
           ;; The source IS the single binary blob; the copy build's
           ;; unpack phase would try to extract it as an archive.
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda* (#:key source #:allow-other-keys)
                   (copy-file source (string-append #$name ".gguf")))))))
    (home-page "https://huggingface.co/unsloth/gemma-4-E4B-it-GGUF")
    (synopsis "Gemma 4 E4B instruction-tuned GGUF (UD-Q4_K_XL)")
    (description
     "Google Gemma 4 E4B Instruct model, Unsloth Dynamic Q4_K_XL
quantization.  The E4B (\"effective 4B\") variant is optimized for
edge and single-GPU inference; it supports a 128K context window and
native audio input (requires a separate mmproj file, not shipped in
this package).  Designed to be loaded by @code{llama-server} from the
@code{llama-cpp} package.")
    (license license:asl2.0)))


;;;
;;; Whisper large-v3-turbo, Q5_0 quantization (~574 MB).
;;; Fast, English WER within ~0.1% of the full large-v3.  Runs on CPU
;;; in real time on modern x86, keeping the GPU free for the LLM.
;;;

(define-public whisper-ggml-large-v3-turbo-q5
  (package
    (name "whisper-ggml-large-v3-turbo-q5")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-large-v3-turbo-q5_0.bin")
       (file-name "ggml-large-v3-turbo-q5_0.bin")
       (sha256
        (base32 "1qm7zxamlvac564c3270wqqqks5wc7532q3fqi01zbfmkiq22hir"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("ggml-large-v3-turbo-q5_0.bin"
                "share/ai-models/ggml-large-v3-turbo-q5_0.bin"))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda* (#:key source #:allow-other-keys)
                   (copy-file source "ggml-large-v3-turbo-q5_0.bin"))))))
    (home-page "https://huggingface.co/ggerganov/whisper.cpp")
    (synopsis "Whisper large-v3-turbo speech-to-text model (Q5_0 GGML)")
    (description
     "OpenAI Whisper large-v3-turbo ASR model in GGML Q5_0 quantization,
as distributed by the whisper.cpp project.  Intended to be consumed by
the @code{whisper-cpp} inference binary for offline speech-to-text
transcription.")
    (license license:expat)))
