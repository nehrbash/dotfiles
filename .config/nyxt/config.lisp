;; emacs keybinds
(define-configuration buffer
  ((default-modes
	(pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))
;; adblocker
(define-configuration web-buffer
  ((default-modes
	(pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%))))
