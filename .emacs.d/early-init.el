;;; early-init.el --- loads before init is loaded.
;;; Commentary:
;;; Code:

;; Increase garbage collection threshold
(setq gc-cons-threshold (* 50 1000 1000))

;; Reduce startup visual clutter
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;; Set default font
(set-face-attribute 'default t
                    :family "Source Code Pro"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Set default font for new frames
(add-to-list 'default-frame-alist '(font . "Source Code Pro-11"))

;; Set fringe width
(fringe-mode '(10 . 10))

;; Set header-line height
(set-face-attribute 'header-line nil :height 100)

;; Disable toolbar, scroll bar, and menu bar
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(menu-bar-mode -1)

;; Disable package.el at startup, I use straight.
(setq package-enable-at-startup nil)
;; Enable pixel scrolling
(pixel-scroll-precision-mode t)

(provide 'early-init)
;;; early-init.el ends here
