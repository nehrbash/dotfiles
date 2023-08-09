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

;; Set default font for new frames overwritten by default-frame-alist
(setq initial-frame-alist '((alpha-background . 90) (font . "Source Code Pro-11") (left-fringe . 10) (right-fringe . 10) (vertical-scroll-bars . nil)))
;; Disable toolbar, scroll bar, and menu bar
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(menu-bar-mode -1)

;; Note: does not seem to make a difference where I put this so I prefer it here
;; so that Emacs.org can only be about configuring package.

;; Disable package.el at startup since we're using straight.el
(setq package-enable-at-startup nil)

;; Silence compiler warnings to avoid disruption
(setq-default comp-async-report-warnings-errors nil)
(setq-default warning-minimum-level :emergency)

(provide 'early-init)
;;; early-init.el ends here
