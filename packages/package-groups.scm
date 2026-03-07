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
  #:use-module (gnu packages wm)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages music)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages maths)
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
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages containers)
  #:use-module (packages gpu-screen-recorder)
  #:use-module (packages gruvbox)
  #:use-module (packages fonts)
  #:use-module (packages libcava)
  #:use-module (packages glab)
  #:use-module (packages mise)
  #:use-module (packages slack)
  #:use-module (packages vscode)
  #:use-module (packages claude-code)
  #:use-module (packages claude-agent-acp)
  #:export (%core-packages %terminal-packages %editor-packages
            %font-packages %desktop-packages %media-packages
            %texlive-packages %dev-packages %misc-packages))

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

(define %font-packages
  (list font-iosevka font-iosevka-aile font-awesome
        font-google-material-design-icons font-google-noto-sans-cjk
        font-google-noto-emoji font-rubik font-caskaydia-cove-nf
        font-iosevka-term-nf font-material-symbols-rounded))

(define %desktop-packages
  (list hyprland libnotify
        wl-clipboard grim slurp swappy hyprpicker fuzzel cliphist
        playerctl brightnessctl wlr-randr ddcutil
        gpu-screen-recorder xdg-utils xkeyboard-config
        bibata-cursor-theme gruvbox-dark-gtk gruvbox-plus-icon-theme))

(define %media-packages
  (list pavucontrol cava aubio libqalculate libcava vlc))

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
        claude-code claude-agent-acp
        gopls gofumpt go-staticcheck rust-analyzer
        shellcheck glab mise sassc
        tree-sitter-css tree-sitter-go tree-sitter-html
        tree-sitter-javascript tree-sitter-json tree-sitter-markdown
        tree-sitter-python tree-sitter-rust tree-sitter-toml
        tree-sitter-typescript tree-sitter-yaml))

(define %misc-packages
  (list aspell aspell-dict-en enchant isync
        htop atop lm-sensors xorriso slack
        podman-compose))
