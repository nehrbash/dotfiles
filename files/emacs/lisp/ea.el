;;; ea.el --- emacs everywhere -*- lexical-binding: t; -*-

(defun ea-commit ()
  (interactive)
  (let ((clipboard (if (region-active-p)
					   (buffer-substring-no-properties (region-beginning) (region-end))
					 (buffer-string))))
	(with-temp-buffer
	  (insert clipboard)
	  (call-process-region (point-min) (point-max) "wtype" nil nil nil "-d" "1"))
	(kill-buffer "*Emacs Everywhere*")
	(delete-frame)))

(defun ea-abort ()
  (interactive)
  (remove-hook 'delete-frame-functions 'ea-on-delete t)
  (kill-buffer "*Emacs Everywhere*")
  (delete-frame))

(defun ea-on-delete (frame)
  (kill-buffer "*Emacs Everywhere*"))

(defun emacs-everywhere ()
  (interactive)
  (switch-to-buffer "*Emacs Everywhere*")
  (select-frame-set-input-focus (selected-frame))
  (add-hook 'delete-frame-functions 'ea-on-delete nil t)
  (org-mode)
  (gptel-mode)
  (local-set-key (kbd "M-RET") 'ea-commit)
  (local-set-key (kbd "M-DEL") 'ea-abort))

(provide 'ea)
;;; ea.el ends here
