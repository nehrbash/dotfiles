;;; early-init.el --- loads before init is loaded.
;;; Commentary:
;;; Code:
(setq gc-cons-threshold (* 50 1000 1000))
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(add-to-list 'default-frame-alist '(font . "Source Code Pro 12"))
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120
                    :weight 'normal
                    :width 'normal)
(set-fontset-font "fontset-default" '(#x5d0 . #x5ff) "FontAwesome")
(fringe-mode '(3 . 6))
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(menu-bar-mode -1)
(setq package-enable-at-startup nil)
(pixel-scroll-precision-mode t)

(setq custom-file (concat user-emacs-directory "var/custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; So we can detect this having been loaded
(provide 'early-init)
;;; early-init.el ends here
