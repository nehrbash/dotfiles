;;; early-init.el --- loads before init is loaded.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:


(defvar doom--file-name-handler-alist file-name-handler-alist) ;; temp restore later
(setq file-name-handler-alist nil)

;; Increase garbage collection threshold and seed optimizations
(setq
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6
 frame-inhibit-implied-resize t
 initial-major-mode 'fundamental-mode)

;; dial back down
(add-hook 'emacs-startup-hook
		  (lambda ()
			(setq
			 gc-cons-threshold 16777216 ; 16mb
			 gc-cons-percentage 0.1
			 file-name-handler-alist doom--file-name-handler-alist)))

;; Reduce startup visual clutter
(setq
 inhibit-startup-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 initial-scratch-message nil)

;; Set default font for new frames overwritten by default-frame-alist
(setq initial-frame-alist
	  '((left-fringe . 10)
		(right-fringe . 10)
		(vertical-scroll-bars . nil)
		)
	  default-frame-alist
	  '(;;(alpha-background . 90)
		(font . "Iosevka")
		(left-fringe . 10)
		(right-fringe . 10)
		(vertical-scroll-bars . nil)
		))
;; Disable toolbar, scroll bar, and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)
;; (set-window-scroll-bars (minibuffer-window) nil nil)

;; disable mode line replaced with doom-modeline later
(setq-default mode-line-format nil)

(provide 'early-init)
;;; early-init.el ends here
