;; emacs keybinds
(define-configuration buffer
  ((default-modes
	(pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))
;; adblocker
(define-configuration web-buffer
  ((default-modes
	(pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%))))

(nyxt:define-nyxt-user-system-and-load "nyxt-user/invader-proxy"
  :depends-on ("invader"))


(defun mpv (url)
  "MPV launches with given url using the fast profile."
  (uiop:run-program (list "mpv" url "&")))

	;; Create a function to download videos with youtube-dl in alacritty
(defun youtube-dl (url)
  "Download videos and audio with youtube-dl in alacritty for feedback"
  (uiop:run-program
   (list "alacritty" "-e" "youtube-dl" "-o ~/Videos/%(title)s.%(ext)s" url)))

	;; Let's create a function to hint videos, convert the url to a sting, and play them in MPV
	(define-command hint-mpv (&key nyxt/web-mode::annotate-visible-only-p)
	  "Show a set of element hints, and copy the URL of the user inputted one."
	  (nyxt/web-mode:query-hints "Copy element URL"
								 (lambda (nyxt/web-mode::result)
								   ;; this converts the url to a string to be used in mpv
								   (let*
									   ((url
										  (format nil "~a"
												  (url (first nyxt/web-mode::result)))))
									 ;; here we take that string and pipe it into mpv
									 (mpv url)))
								 :annotate-visible-only-p
								 nyxt/web-mode::annotate-visible-only-p))

	;; Let's create a function to hint videos, convert the url to a sting, and download with ytdl
	(define-command hint-ytdl (&key nyxt/web-mode::annotate-visible-only-p)
	  "Show a set of element hints, and copy the URL of the user inputted one."
	  (nyxt/web-mode:query-hints "Copy element URL"
								 (lambda (nyxt/web-mode::result)
								   ;; this converts the url to a string to be used in mpv
								   (let*
									   ((url
										  (format nil "~a"
												  (url (first nyxt/web-mode::result)))))
									 ;; here we take that string and pipe it into mpv
									 (youtube-dl url)))
								 :annotate-visible-only-p
								 nyxt/web-mode::annotate-visible-only-p))

	;; ;; These are my own keys that are layered over vi-normal. A lot of these
	;; ;; are similar to qutebrowser.
	;; (defvar *chris-keymap* (make-keymap "chris-map"))
	;; (define-key *chris-keymap*
	;;   "K" 'switch-buffer-next
	;;   "J" 'switch-buffer-previous
	;;   "b" 'switch-buffer
	;;   "v" 'hint-mpv
	;;   "V" 'hint-ytdl
	;;   "d" 'delete-current-buffer
	;;   "D" 'delete-buffer
	;;   "r" 'reload-current-buffer
	;;   "R" 'reload-buffers)
