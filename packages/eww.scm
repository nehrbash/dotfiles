;; Custom Guix package definition for eww (Elkowar's Wacky Widgets).
;;
;; NOTE: Several required Rust crates are not yet packaged in Guix:
;;   rust-gtk-0.17, rust-gdk-0.17, rust-gtk-layer-shell-0.6,
;;   rust-cached-0.48, rust-grass-0.13, rust-simple-signal-1,
;;   rust-extend-1, rust-jaq-{core,parse,std,interpret,syn}-1
;;
;; Until those are available, install eww via cargo in the home activation
;; script (see home/redfish.scm) and reference this file only as a
;; placeholder / future package definition.
(define-module (packages eww))
