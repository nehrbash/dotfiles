;;; vterm-tabs.el --- vterm multiplexer -*- lexical-binding: t; -*-

;; Author: Stephen Nehrbass
;; Keywords: terminals, processes
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Managing multiple vterm buffers in Emacs.
;; This started as a fork of multi-term.el but doesn't really resemble the
;; original anymore.
;;
;; Required packages:
;;  `vterm'
;;  `svg-tabs'

;;; Code:

(require 'cl-lib)
(require 'vterm)
(require 'project)
(require 'tab-line)
(require 'svg-tabs)
(require 'compile)
(require 'flymake)

(defgroup vterm-tabs nil
  "Multi term manager."
  :group 'vterm
  :prefix "vterm-tabs-")

(defcustom vterm-tabs-buffer-name "shell"
  "The vterm buffer name."
  :type 'string
  :group 'vterm-tabs)

(defcustom vterm-tabs-compile-buffer-name "Compile"
  "The compile buffer name."
  :type 'string
  :group 'vterm-tabs)

(defvar vterm-tabs-window nil
  "Window displaying the vterm sidebar.")

(defvar vterm-tabs-buffer-list nil
  "List of non-dedicated terminal buffers managed by `vterm-tabs'.")

(defvar vterm-tabs-last-buffer nil
  "The last accessed vterm buffer.")

(defvar vterm-tabs-last-height nil
  "The last remembered height of the vterm window.")

(defvar vterm-tabs--last-project-root nil
  "Store the last project root for `vterm-tabs-project'.")

(defvar vterm-tabs--last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(defvar vterm-tabs--magit-buffer nil
  "Last project Magit buffer.")

(defvar vterm-tabs--diagnostics-buffer nil
  "Last project diagnostics buffer.")

;;; Compilation buffer management

(defun vterm-tabs--save-compilation-buffer (&rest _)
  "Save the compilation buffer to find it later."
  (setq vterm-tabs--last-compilation-buffer next-error-last-buffer))

(defun vterm-tabs--find-prev-compilation (orig &optional edit-command)
  "Find the previous compilation buffer and recompile there.
If EDIT-COMMAND is nil and we are not in a compilation mode,
attempt to use `vterm-tabs--last-compilation-buffer'."
  (if (and (null edit-command)
		   (not (derived-mode-p 'compilation-mode))
		   vterm-tabs--last-compilation-buffer
		   (buffer-live-p vterm-tabs--last-compilation-buffer))
	  (let ((compilation-buffer vterm-tabs--last-compilation-buffer))
		(if (window-live-p vterm-tabs-window)
			(progn
			  (set-window-buffer vterm-tabs-window compilation-buffer)
			  (select-window vterm-tabs-window)
			  (with-current-buffer compilation-buffer
				(funcall orig edit-command)))
		  (with-current-buffer compilation-buffer
			(funcall orig edit-command))))
	;; Fallback to the original behavior if no previous compilation buffer
	(funcall orig edit-command)))

(defun vterm-tabs--colorize-compilation-buffer ()
  "Apply ANSI color codes to compilation buffer output."
  (when (derived-mode-p 'compilation-mode)
	(ansi-color-apply-on-region compilation-filter-start (point-max))))

;;; Buffer management

(defun vterm-tabs-create-or-switch ()
  "Create a new vterm buffer or switch to an existing one."
  (interactive)
  (let ((buffer (generate-new-buffer (generate-new-buffer-name vterm-tabs-buffer-name))))
	(with-current-buffer buffer
	  (vterm-mode)
	  (add-hook 'kill-buffer-hook #'vterm-tabs-remove-buffer nil t)
	  (push buffer vterm-tabs-buffer-list))
	(vterm-tabs-switch buffer)))

(defun vterm-tabs-remove-buffer ()
  "Remove the current buffer from `vterm-tabs-buffer-list' when killed."
  (setq vterm-tabs-buffer-list (delq (current-buffer) vterm-tabs-buffer-list))
  (when (eq vterm-tabs-last-buffer (current-buffer))
	(setq vterm-tabs-last-buffer nil)))

(defun vterm-tabs-create-at-directory (directory)
  "Create a new vterm buffer in DIRECTORY and switch to it."
  (let ((default-directory directory))
	(vterm-tabs-create-or-switch)))

;;; Interactive commands

;;;###autoload
(defun vterm-tabs-toggle ()
  "Toggle the visibility of the vterm sidebar."
  (interactive)
  (when-let ((project (project-current)))
	(setq vterm-tabs--last-project-root (project-root project)))
  (if (and vterm-tabs-window (window-live-p vterm-tabs-window))
	  (progn
		(setq vterm-tabs-last-buffer (window-buffer vterm-tabs-window)
			  vterm-tabs-last-height (window-total-height vterm-tabs-window))
		(delete-window vterm-tabs-window)
		(setq vterm-tabs-window nil))
	(if (and vterm-tabs-last-buffer (buffer-live-p vterm-tabs-last-buffer))
		(vterm-tabs-switch vterm-tabs-last-buffer)
	  (vterm-tabs-create-or-switch))))

;;;###autoload
(defun vterm-tabs-project ()
  "Create a new vterm buffer at the current project's root."
  (interactive)
  (let* ((project (project-current))
		 (project-root (if project
						   (setq vterm-tabs--last-project-root (project-root project))
						 (or vterm-tabs--last-project-root
							 (user-error "No project found")))))
	(vterm-tabs-create-at-directory project-root)))

;;;###autoload
(defun vterm-tabs-home ()
  "Create a new vterm buffer at the user's home directory."
  (interactive)
  (vterm-tabs-create-at-directory (expand-file-name "~/")))

(defun vterm-tabs-rename-buffer (name)
  "Rename vterm buffer to NAME."
  (interactive "MRename vterm buffer: ")
  (rename-buffer (format "%s - %s" vterm-tabs-buffer-name name)))

(defun vterm-tabs-next (&optional offset)
  "Go to the next term buffer.
If OFFSET is non-nil, skip OFFSET number of buffers."
  (interactive "P")
  (vterm-tabs--switch-internal 'next (or offset 1)))

(defun vterm-tabs-prev (&optional offset)
  "Go to the previous term buffer.
If OFFSET is non-nil, skip OFFSET number of buffers."
  (interactive "P")
  (vterm-tabs--switch-internal 'previous (or offset 1)))

;;; Internal functions

(defun vterm-tabs--switch-internal (direction offset)
  "Switch to next/previous vterm buffer.
DIRECTION should be 'next or 'previous.
OFFSET specifies how many buffers to skip."
  (when-let ((all-buffers (vterm-tabs--all-buffers)))
	(let* ((buffer-list-len (length all-buffers))
		   (current-index (or (seq-position all-buffers (current-buffer)) 0))
		   (target-index (pcase direction
						   ('next (mod (+ current-index offset) buffer-list-len))
						   ('previous (mod (- current-index offset) buffer-list-len))
						   (_ 0))))
	  (vterm-tabs-switch (nth target-index all-buffers)))))

(defun vterm-tabs-switch (buffer)
  "Switch to a vterm BUFFER, reusing the existing window."
  (setq vterm-tabs-last-buffer buffer)
  (if (and vterm-tabs-window (window-live-p vterm-tabs-window))
	  (progn
		(let ((was-dedicated (window-dedicated-p vterm-tabs-window)))
		  (set-window-dedicated-p vterm-tabs-window nil)
		  (set-window-buffer vterm-tabs-window buffer)
		  (set-window-dedicated-p vterm-tabs-window was-dedicated))
		(select-window vterm-tabs-window))
	(let ((new-window (display-buffer-in-side-window
					   buffer '((side . bottom)))))
	  (set-window-dedicated-p new-window t)
	  (set-window-parameter new-window 'no-other-window t)
	  (select-window new-window)
	  ;; Restore the last remembered height
	  (when vterm-tabs-last-height
		(window-resize new-window
					   (- vterm-tabs-last-height (window-height new-window))
					   nil 'preserve))
	  (setq vterm-tabs-window new-window)))
  ;; Enable tab-line-mode in the vterm window
  (with-current-buffer buffer
	(unless tab-line-mode
	  (tab-line-mode 1)))
  (setq-local mode-line-format nil))

(defun vterm-tabs--tab-group (buffer)
  "Group buffers by major mode for tab display.
Returns a group name for BUFFER if it matches relevant modes."
  (with-current-buffer buffer
	(when (derived-mode-p 'flymake-project-diagnostics-mode
						  'compilation-mode
						  'vterm-mode)
	  "🦥")))

(defun vterm-tabs--all-buffers ()
  "Return list of buffers to display during vterm-tabs-mode.
Include vterm buffers, compilation buffer, Magit status, and diagnostics."
  (let* ((project (project-current))
	   (project-root (and project (project-root project)))
	   (magit-buffer
		(when (and project-root (featurep 'magit))
		  (or (and vterm-tabs--magit-buffer
				   (buffer-live-p vterm-tabs--magit-buffer)
				   vterm-tabs--magit-buffer)
			  (condition-case nil
				  (setq vterm-tabs--magit-buffer
						(save-window-excursion
						  (let ((magit-display-buffer-noselect t)
								(magit-display-buffer-function #'ignore))
							(magit-status-setup-buffer project-root))))
				(error nil)))))
	   (diagnostics-buffer
		(when project-root
		  (or (and vterm-tabs--diagnostics-buffer
				   (buffer-live-p vterm-tabs--diagnostics-buffer)
				   vterm-tabs--diagnostics-buffer)
			  (condition-case nil
				  (let* ((buffer (flymake--project-diagnostics-buffer project-root)))
					(with-current-buffer buffer
					  (flymake-project-diagnostics-mode)
					  (setq-local flymake--project-diagnostic-list-project project)
					  (revert-buffer)
					  (setq vterm-tabs--diagnostics-buffer buffer)
					  buffer))
				(error nil)))))
	   (compilation-buffer
		(or (and vterm-tabs--last-compilation-buffer
				 (buffer-live-p vterm-tabs--last-compilation-buffer)
				 vterm-tabs--last-compilation-buffer)
			(let ((buf (get-buffer-create vterm-tabs-compile-buffer-name)))
			  (with-current-buffer buf
				(compilation-mode)
				(setq-local mode-line-format nil))
			  (setq vterm-tabs--last-compilation-buffer buf)
			  buf))))
  ;; Order: magit -> diagnostics -> compile -> vterm buffers
  (seq-filter #'buffer-live-p
			  (append (list magit-buffer diagnostics-buffer compilation-buffer)
					  vterm-tabs-buffer-list))))

;;; Display buffer function

(defun vterm-tabs-display-buffer (buffer _alist)
  "Custom display function for BUFFER to handle specific modes."
  (when (with-current-buffer buffer
		  (derived-mode-p 'compilation-mode 'vterm-mode
						  'flymake-project-diagnostics-mode 'magit-mode))
	(vterm-tabs-switch buffer)
	vterm-tabs-window))

;;; Tab selection advice

(defun vterm-tabs--select-buffer (orig-fun &rest args)
  "Advice for `tab-line-select-tab-buffer' to `ORIG-FUN' to handle dedicated windows.
Pass through any additional `ARGS'."
  (let ((window (selected-window)))
	(set-window-dedicated-p window nil)
	(apply orig-fun args)
	(set-window-dedicated-p (selected-window) t)))

;;; Keymap

(defvar vterm-tabs-mode-map
  (let ((map (make-sparse-keymap)))
	(keymap-set map "<f6>" #'vterm-tabs-toggle)
	(keymap-set map "C-M-r" #'vterm-tabs-rename-buffer)
	(keymap-set map "C-M-t" #'vterm-tabs-home)
	(keymap-set map "C-M-p" #'vterm-tabs-project)
	(keymap-set map "C-M-f" #'vterm-tabs-next)
	(keymap-set map "C-M-b" #'vterm-tabs-prev)
	map)
  "Keymap for `vterm-tabs-mode'.")

;;; Minor mode definition

;;;###autoload
(define-minor-mode vterm-tabs-mode
  "Minor mode to handle tabs in vterm."
  :lighter nil
  :keymap vterm-tabs-mode-map
  (when vterm-tabs-mode
	(setq-local tab-line-tabs-function #'vterm-tabs--all-buffers
				tab-line-tab-name-function #'svg-tabs--svg-line-tab-name-buffer)
	(tab-line-mode 1)))

(defun vterm-tabs-mode--on ()
  "Turn on vterm-tabs-mode for relevant major modes."
  (when (derived-mode-p 'compilation-mode 'vterm-mode
						'flymake-project-diagnostics-mode 'magit-status-mode)
	(vterm-tabs-mode 1)))

(defun vterm-tabs--magit-display-buffer (buffer)
  "Custom Magit display function for BUFFER."
  (vterm-tabs-display-buffer buffer nil))

;;;###autoload
(define-globalized-minor-mode global-vterm-tabs-mode
  vterm-tabs-mode vterm-tabs-mode--on
  :group 'vterm-tabs
  (if global-vterm-tabs-mode
	  (progn
		(advice-add 'tab-line-select-tab-buffer :around #'vterm-tabs--select-buffer)
		(advice-add 'compilation-start :after #'vterm-tabs--save-compilation-buffer)
		(advice-add 'recompile :around #'vterm-tabs--find-prev-compilation)
		(add-hook 'compilation-filter-hook #'vterm-tabs--colorize-compilation-buffer)
		;; Simple display buffer rule - let derived-mode-p handle the logic
		(push '((derived-mode-p 'compilation-mode 'vterm-mode
							   'flymake-project-diagnostics-mode 'magit-mode)
				vterm-tabs-display-buffer)
			  display-buffer-alist)
		;; Set Magit to use our display function (if available)
		(when (featurep 'magit)
		  (setq magit-display-buffer-function #'vterm-tabs--magit-display-buffer)))
	(advice-remove 'tab-line-select-tab-buffer #'vterm-tabs--select-buffer)
	(advice-remove 'compilation-start #'vterm-tabs--save-compilation-buffer)
	(advice-remove 'recompile #'vterm-tabs--find-prev-compilation)
	(remove-hook 'compilation-filter-hook #'vterm-tabs--colorize-compilation-buffer)
	;; Clean up display buffer rules
	(setq display-buffer-alist
		  (seq-remove (lambda (rule) (eq (cadr rule) 'vterm-tabs-display-buffer))
					  display-buffer-alist))
	(when (featurep 'magit)
	  (setq magit-display-buffer-function #'magit-display-buffer-traditional))))

(provide 'vterm-tabs)
;;; vterm-tabs.el ends here
