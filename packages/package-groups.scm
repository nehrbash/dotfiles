(define-module (packages package-groups)
  #:use-module (gnu packages)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages base)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages audio)              ; sox
  #:use-module (gnu packages video)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages node)
  #:use-module (gnu packages golang-apps)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages cdrom)              ; xorriso
  #:use-module (gnu packages linux)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages machine-learning)   ; whisper-cpp
  #:use-module (home services llama-server)      ; llama-cpp-git
  #:use-module (packages ai-models)
  #:use-module (packages fonts)
  #:use-module (packages gh)
  #:use-module (packages glab)
  #:use-module (packages mise)
  #:use-module (packages slack)
  #:use-module (packages vscode)
  #:use-module (packages claude-agent-acp)
  #:export (%core-packages %terminal-packages %editor-packages
            %font-packages %media-packages
            %texlive-packages %dev-packages %misc-packages
            %ai-packages))

(define %core-packages
  (list git git-lfs curl wget ripgrep jq
        (specification->package "bind")
        (specification->package "sqlite")
        (specification->package "gcc-toolchain")
        pkg-config libtool cmake gnu-make ninja
        (specification->package "file")
        (specification->package "tree")
        (specification->package "lsof")
        (specification->package "strace")
        (specification->package "zip")
        (specification->package "unzip")
        (specification->package "pv")
        (specification->package "bc")))

(define %terminal-packages
  (list libvterm alacritty eza starship ncurses
        zsh-autosuggestions zsh-syntax-highlighting zsh-completions
        zsh-autopair zsh-history-substring-search fzf fzf-tab))

(define %editor-packages
  (list vscode))

;; General-purpose fonts.  Fonts consumed specifically by the Caelestia
;; shell config live in (desktop caelestia) %caelestia-desktop-packages.
(define %font-packages
  (list font-iosevka font-iosevka-aile
        font-google-noto-sans-cjk font-google-noto-emoji
        font-iosevka-term-nf))

;; General-purpose media tools.  Caelestia-specific audio deps (cava,
;; libcava, aubio, libqalculate, pavucontrol) live in (desktop caelestia).
(define %media-packages
  (list vlc sox))

(define %texlive-packages
  (list texlive-scheme-basic
        texlive-tools                   ; tabularx, longtable, rotating
        texlive-fancyhdr texlive-xcolor texlive-caption texlive-titling
        texlive-ragged2e texlive-setspace texlive-wrapfig texlive-ulem
        texlive-adjustbox texlive-capt-of texlive-minted texlive-fontspec
        texlive-psnfss                  ; mathptmx, helvet, courier, mathpazo
        texlive-mathdesign texlive-bera texlive-tex-gyre
        texlive-libertine texlive-lm texlive-kpfonts
        texlive-dvisvgm))

(define %dev-packages
  (list go python rust node (list rust "cargo")
        claude-agent-acp
        gopls gofumpt go-staticcheck rust-analyzer
        shellcheck gh glab mise sassc
        tree-sitter-css tree-sitter-go tree-sitter-html
        tree-sitter-javascript tree-sitter-json tree-sitter-markdown
        tree-sitter-python tree-sitter-rust tree-sitter-toml
        tree-sitter-typescript tree-sitter-yaml))

(define %misc-packages
  (list aspell aspell-dict-en enchant isync
        htop atop lm-sensors xorriso slack
        podman-compose bluez))

;; Local AI stack: inference runtimes from upstream Guix plus the model
;; weights from (packages ai-models).  ffmpeg is pulled in from
;; %media-packages (vlc already depends on it) for whisper.el's audio
;; recording pipeline — no duplicate import needed.
(define %ai-packages
  (list llama-cpp-git
        whisper-cpp
        gemma-4-e4b-it-gguf
        whisper-ggml-large-v3-turbo-q5))
