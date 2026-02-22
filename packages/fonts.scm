(define-module (packages fonts)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system font)
  #:use-module ((guix licenses) #:prefix license:))

(define-public font-rubik
  (package
    (name "font-rubik")
    (version "0-git-e337a5f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googlefonts/rubik")
             (commit "e337a5f69a9bea30e58d05bd40184d79cc099628")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hg9611iiwzk8cff4ivfq7vbanks1q2xz68ijc5rq4cac05fg8cp"))))
    (build-system font-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'chdir
           (lambda _ (chdir "fonts/variable"))))))
    (home-page "https://fonts.google.com/specimen/Rubik")
    (synopsis "Sans-serif font family with slightly rounded corners")
    (description "Rubik is a sans-serif font family with slightly rounded
corners, designed by Hubert and Fischer, Meir Sadan, Cyreal.")
    (license license:silofl1.1)))

(define-public font-caskaydia-cove-nf
  (package
    (name "font-caskaydia-cove-nf")
    (version "3.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
             version "/CascadiaCode.zip"))
       (sha256
        (base32 "03ll8p12gbcc9nnv6chzmzqw803j1c374p4fk6dbd9m0a4rrk04d"))))
    (build-system font-build-system)
    (home-page "https://www.nerdfonts.com/")
    (synopsis "CaskaydiaCove Nerd Font — patched Cascadia Code with icons")
    (description "CaskaydiaCove is a Nerd Font patched version of Cascadia
Code, including Powerline glyphs and developer icons.")
    (license license:silofl1.1)))

(define-public font-iosevka-term-nf
  (package
    (name "font-iosevka-term-nf")
    (version "3.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/ryanoasis/nerd-fonts/releases/download/v"
             version "/IosevkaTerm.zip"))
       (sha256
        (base32 "0manr6z1vc4fqyv6710196sh0yvwdclmvig7cn73wqih8zz12rsi"))))
    (build-system font-build-system)
    (home-page "https://www.nerdfonts.com/")
    (synopsis "IosevkaTerm Nerd Font — patched Iosevka Term with icons")
    (description "IosevkaTerm is a Nerd Font patched version of Iosevka Term,
including Powerline glyphs and developer icons for terminal use.")
    (license license:silofl1.1)))

(define-public font-material-symbols-rounded
  (package
    (name "font-material-symbols-rounded")
    (version "2024")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/google/material-design-icons/raw/master/"
             "variablefont/MaterialSymbolsRounded%5BFILL%2CGRAD%2Copsz%2Cwght%5D.ttf"))
       (file-name (string-append name "-" version ".ttf"))
       (sha256
        (base32 "06f1lxpz4wwx8d9j8pzn0x0dxcz0sj9h7ihkk7rrbhvpgbczvslg"))))
    (build-system font-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (mkdir "source")
             (copy-file source "source/MaterialSymbolsRounded.ttf")
             (chdir "source"))))))
    (home-page "https://fonts.google.com/icons")
    (synopsis "Material Symbols Rounded icon font by Google")
    (description "Variable icon font from Google's Material Symbols collection,
using the Rounded style variant.")
    (license license:asl2.0)))
