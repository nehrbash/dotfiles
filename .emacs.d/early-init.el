(setq gc-cons-threshold (* 50 1000 1000))
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'normal
                    :width 'normal)
;;(add-to-list 'default-frame-alist '(font .  "Source Code Pro Medium 14"))
;;(set-face-attribute 'default nil :height 110)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(menu-bar-mode -1)

(setq package-enable-at-startup nil)

;; So we can detect this having been loaded
(provide 'early-init)
