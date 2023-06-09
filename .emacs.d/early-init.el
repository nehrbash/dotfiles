;;; early-init.el --- loads before init is loaded.
;;; Commentary:
;;; Code:


(defvar doom--file-name-handler-alist file-name-handler-alist) ;; temp restore later
(setq file-name-handler-alist nil)

;; Increase garbage collection threshold and seed optimizations
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      frame-inhibit-implied-resize t
      initial-major-mode 'fundamental-mode)

;; dial back down
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1
          file-name-handler-alist doom--file-name-handler-alist)))

;; Reduce startup visual clutter
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;; Set default font for new frames
(add-to-list 'default-frame-alist '(font . "Source Code Pro-11"))

;; Disable toolbar, scroll bar, and menu bar
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(menu-bar-mode -1)

;; Disable package.el at startup, I use straight.
(setq package-enable-at-startup nil
      straight-enable-package-integration nil)
;; don't add that `custom-set-variables' block to my init.el!
(setq package--init-file-ensured t)

(provide 'early-init)
;;; early-init.el ends here
