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
 inhibit-default-init t
 initial-scratch-message nil)

;; Set default font for new frames overwritten by default-frame-alist
(setq initial-frame-alist
	  '((font . "Iosevka")
		(left-fringe . 10)
		(width . 700)
		(height . 400)
		(right-fringe . 10))
	  default-frame-alist
	  '(;;(alpha-background . 90)
		(font . "Iosevka")
		(left-fringe . 10)
		(right-fringe . 10)
		(width . 800)
		(height . 600)
		))

(add-hook 'after-make-frame-functions
		  (lambda (frame)
			(modify-frame-parameters
			 frame '((scroll-bar-width . 10)))
			(set-window-scroll-bars
			 (minibuffer-window frame) 0 nil 0 nil t)
			(set-window-fringes
			 (minibuffer-window frame) 0 0 nil t)))


;; Disable toolbar, scroll bar, and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

(provide 'early-init)
;;; early-init.el ends here
