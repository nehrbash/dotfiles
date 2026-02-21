;;; vterm-tabs.el --- Vterm multiplexer with tab-line  -*- lexical-binding: t; -*-

;; Author: Stephen Nehrbass
;; Keywords: terminals, processes
;; Version: 2.0
;; Package-Requires: ((emacs "31.1") (vterm "0.0.2"))

;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Manage multiple vterm buffers in a bottom side window with tab-line
;; navigation.  Also integrates compilation, magit-status, and flymake
;; project diagnostics into the same panel.

;;; Code:

(require 'cl-lib)
(require 'vterm)
(require 'project)
(require 'tab-line)
(require 'compile)

;;; Options

(defgroup vterm-tabs nil
  "Vterm multiplexer with tab-line."
  :group 'vterm
  :prefix "vterm-tabs-")

(defcustom vterm-tabs-buffer-name "shell"
  "Base name for new vterm buffers."
  :type 'string
  :group 'vterm-tabs)

(defcustom vterm-tabs-compile-buffer-name "Compile"
  "Name for the persistent compilation buffer."
  :type 'string
  :group 'vterm-tabs)

(defcustom vterm-tabs-height 0.33
  "Initial window height as a fraction of the frame."
  :type 'float
  :group 'vterm-tabs)

(defcustom vterm-tabs-min-height 10
  "Minimum window height in lines."
  :type 'natnum
  :group 'vterm-tabs)

;;; Internal state

(defvar vterm-tabs--window nil
  "The live side window managed by `vterm-tabs'.")

(defvar vterm-tabs--buffer-list nil
  "Vterm buffers created by `vterm-tabs'.")

(defvar vterm-tabs--last-buffer nil
  "Last buffer shown in the vterm side window.")

(defvar vterm-tabs--last-height nil
  "Remembered height of the vterm side window.")

(defvar vterm-tabs--last-project-root nil
  "Project root last seen when toggling the panel.")

(defvar vterm-tabs--last-compilation-buffer nil
  "Most recent compilation buffer.")

(defvar vterm-tabs--magit-buffer nil
  "Cached magit-status buffer for the current project.")

(defvar vterm-tabs--diagnostics-buffer nil
  "Cached flymake project-diagnostics buffer.")

;;; Compilation helpers

(defun vterm-tabs--save-compilation-buffer (&rest _)
  "Record the most recent compilation buffer."
  (setq vterm-tabs--last-compilation-buffer next-error-last-buffer))

(defun vterm-tabs--find-prev-compilation (orig &optional edit-command)
  "Recompile in the saved compilation buffer when appropriate.
Passes EDIT-COMMAND through to ORIG."
  (if (and (null edit-command)
           (not (derived-mode-p 'compilation-mode))
           (buffer-live-p vterm-tabs--last-compilation-buffer))
      (let ((buf vterm-tabs--last-compilation-buffer))
        (if (window-live-p vterm-tabs--window)
            (progn
              (set-window-buffer vterm-tabs--window buf)
              (select-window vterm-tabs--window)
              (with-current-buffer buf (funcall orig edit-command)))
          (with-current-buffer buf (funcall orig edit-command))))
    (funcall orig edit-command)))

(defun vterm-tabs--colorize-compilation ()
  "Apply ANSI colors to compilation output."
  (when (derived-mode-p 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

;;; Buffer creation / registration

(defun vterm-tabs--create ()
  "Create and return a new vterm buffer registered with `vterm-tabs'."
  (let ((buffer (generate-new-buffer vterm-tabs-buffer-name)))
    (with-current-buffer buffer
      (vterm-mode)
      (add-hook 'kill-buffer-hook #'vterm-tabs--unregister nil t)
      (push buffer vterm-tabs--buffer-list))
    buffer))

(defun vterm-tabs--unregister ()
  "Remove the current buffer from `vterm-tabs--buffer-list'."
  (setq vterm-tabs--buffer-list (delq (current-buffer) vterm-tabs--buffer-list))
  (when (eq vterm-tabs--last-buffer (current-buffer))
    (setq vterm-tabs--last-buffer nil)))

(defun vterm-tabs--create-at (directory)
  "Create a vterm buffer with DIRECTORY as `default-directory'."
  (let ((default-directory directory))
    (vterm-tabs--create)))

;;; Buffer collection
;;
;; `vterm-tabs--all-buffers' is called by `tab-line' on every redisplay.
;; It must be side-effect free — no buffer creation, no mode changes.
;; All initialisation happens in `vterm-tabs--init-panel', called once
;; when the panel is opened.

(defun vterm-tabs--init-compilation ()
  "Ensure a compilation stub buffer exists and cache it."
  (unless (buffer-live-p vterm-tabs--last-compilation-buffer)
    (let ((buf (get-buffer-create vterm-tabs-compile-buffer-name)))
      (with-current-buffer buf
        (unless (derived-mode-p 'compilation-mode)
          (compilation-mode))
        (setq-local mode-line-format nil))
      (setq vterm-tabs--last-compilation-buffer buf))))

(defun vterm-tabs--init-magit (root)
  "Ensure a magit-status buffer for ROOT exists and cache it."
  (when (and (featurep 'magit)
             (not (buffer-live-p vterm-tabs--magit-buffer)))
    (condition-case nil
        (setq vterm-tabs--magit-buffer
              (let ((magit-display-buffer-function  #'ignore)
                    (magit-display-buffer-noselect  t))
                (magit-status-setup-buffer root)))
      (error nil))))

(defun vterm-tabs--init-diagnostics (project)
  "Ensure a flymake diagnostics buffer for PROJECT exists and cache it."
  (require 'flymake nil t)
  (when (and (fboundp 'flymake--project-diagnostics-buffer)
             (not (buffer-live-p vterm-tabs--diagnostics-buffer)))
    (condition-case nil
        (let* ((root (project-root project))
               (buf  (flymake--project-diagnostics-buffer root)))
          (with-current-buffer buf
            (flymake-project-diagnostics-mode)
            (setq-local flymake--project-diagnostic-list-project project)
            (revert-buffer))
          (setq vterm-tabs--diagnostics-buffer buf))
      (error nil))))

(defun vterm-tabs--init-panel ()
  "Initialise all panel buffers for the current project context.
Call this before opening the panel, not during tab-line rendering."
  (let* ((project (project-current))
         (root    (and project (project-root project))))
    (when root (vterm-tabs--init-magit root))
    (when project (vterm-tabs--init-diagnostics project))
    (vterm-tabs--init-compilation)))

(defun vterm-tabs--all-buffers ()
  "Return panel buffers ordered: magit → diagnostics → compile → shells.
Scrub stale refs and guard against a killed hscroll buffer in tab-line."
  ;; Emacs's `tab-line-auto-hscroll' crashes when its internal buffer
  ;; has been killed (e.g. by midnight or manual cleanup).  Resurrect
  ;; it here because this runs before the hscroll code on every render.
  (when (and (boundp 'tab-line-auto-hscroll-buffer)
             (not (buffer-live-p tab-line-auto-hscroll-buffer)))
    (setq tab-line-auto-hscroll-buffer
          (generate-new-buffer " *tab-line-hscroll*")))
  (unless (buffer-live-p vterm-tabs--magit-buffer)
    (setq vterm-tabs--magit-buffer nil))
  (unless (buffer-live-p vterm-tabs--diagnostics-buffer)
    (setq vterm-tabs--diagnostics-buffer nil))
  (unless (buffer-live-p vterm-tabs--last-compilation-buffer)
    (setq vterm-tabs--last-compilation-buffer nil))
  (setq vterm-tabs--buffer-list
        (seq-filter #'buffer-live-p vterm-tabs--buffer-list))
  (delq nil (append (list vterm-tabs--magit-buffer
                          vterm-tabs--diagnostics-buffer
                          vterm-tabs--last-compilation-buffer)
                    vterm-tabs--buffer-list)))

;;; Display

(defun vterm-tabs--switch (buffer)
  "Display BUFFER in the vterm side window, creating the window if needed."
  (vterm-tabs--init-panel)
  (setq vterm-tabs--last-buffer buffer)
  (if (window-live-p vterm-tabs--window)
      (progn
        (let ((dedicated (window-dedicated-p vterm-tabs--window)))
          (set-window-dedicated-p vterm-tabs--window nil)
          (set-window-buffer vterm-tabs--window buffer)
          (set-window-dedicated-p vterm-tabs--window dedicated))
        (select-window vterm-tabs--window))
    (let ((win (display-buffer-in-side-window
                buffer `((side . bottom)
                         (window-height . ,vterm-tabs-height)))))
      (set-window-dedicated-p win t)
      (set-window-parameter win 'no-other-window t)
      (when vterm-tabs--last-height
        (let ((target (max vterm-tabs--last-height vterm-tabs-min-height)))
          (window-resize win
                         (- target (window-height win))
                         nil 'preserve)))
      (setq vterm-tabs--window win)
      (select-window win)))
  (with-current-buffer buffer
    (unless vterm-tabs-mode (vterm-tabs-mode 1))))

(defconst vterm-tabs--display-rule
  '((or (derived-mode . vterm-mode)
        (derived-mode . compilation-mode)
        (derived-mode . flymake-project-diagnostics-mode)
        (derived-mode . magit-status-mode))
    vterm-tabs-display-buffer)
  "The `display-buffer-alist' rule owned by `vterm-tabs'.
Routes all panel-managed modes to `vterm-tabs-display-buffer'.
Installed idempotently by `global-vterm-tabs-mode'.")

(defun vterm-tabs--non-panel-buffer-p (buffer _alist)
  "Return non-nil when BUFFER should go to the main window.
True only when the panel is selected and BUFFER is not a panel mode."
  (and (window-live-p vterm-tabs--window)
       (eq (selected-window) vterm-tabs--window)
       (not (with-current-buffer buffer
              (derived-mode-p 'vterm-mode 'compilation-mode
                              'flymake-project-diagnostics-mode
                              'magit-status-mode)))))

(defconst vterm-tabs--main-window-rule
  '(vterm-tabs--non-panel-buffer-p
    (display-buffer-use-some-window)
    (inhibit-same-window . t))
  "Route non-panel buffers to the main window when called from the panel.")

(defun vterm-tabs-display-buffer (buffer _alist)
  "Display-buffer action that routes BUFFER into the vterm side panel."
  (vterm-tabs--switch buffer)
  vterm-tabs--window)

;;; Navigation

(defun vterm-tabs--cycle (direction offset)
  "Move OFFSET tabs in DIRECTION (\\='next or \\='previous) through the panel."
  (when-let* ((all (vterm-tabs--all-buffers))
              (len (length all))
              (idx (or (seq-position all (current-buffer)) 0))
              (target (pcase direction
                        ('next     (mod (+ idx offset) len))
                        ('previous (mod (- idx offset) len)))))
    (vterm-tabs--switch (nth target all))))

;;; Tab-line select advice

(defun vterm-tabs--select-buffer (orig &rest args)
  "Temporarily undedicate the selected window around ORIG tab selection.
Passes ARGS through."
  (let ((win (selected-window)))
    (set-window-dedicated-p win nil)
    (apply orig args)
    (set-window-dedicated-p (selected-window) t)))

;;; Interactive commands

;;;###autoload
(defun vterm-tabs-toggle ()
  "Toggle the vterm side panel."
  (interactive)
  (when-let* ((project (project-current)))
    (setq vterm-tabs--last-project-root (project-root project)))
  (if (window-live-p vterm-tabs--window)
      (progn
        (setq vterm-tabs--last-buffer (window-buffer vterm-tabs--window)
              vterm-tabs--last-height (max (window-total-height vterm-tabs--window)
                                           vterm-tabs-min-height))
        (delete-window vterm-tabs--window)
        (setq vterm-tabs--window nil))
    (vterm-tabs--switch
     (or (and (buffer-live-p vterm-tabs--last-buffer) vterm-tabs--last-buffer)
         (vterm-tabs--create)))))

;;;###autoload
(defun vterm-tabs-project ()
  "Open a new vterm buffer at the current project root."
  (interactive)
  (let* ((project (project-current))
         (root    (if project
                      (setq vterm-tabs--last-project-root (project-root project))
                    (or vterm-tabs--last-project-root
                        (user-error "No project found")))))
    (vterm-tabs--switch (vterm-tabs--create-at root))))

;;;###autoload
(defun vterm-tabs-home ()
  "Open a new vterm buffer at the home directory."
  (interactive)
  (vterm-tabs--switch (vterm-tabs--create-at (expand-file-name "~/"))))

(defun vterm-tabs-rename-buffer (name)
  "Rename the current vterm buffer to NAME."
  (interactive "MRename vterm: ")
  (rename-buffer (format "%s: %s" vterm-tabs-buffer-name name) t))

(defun vterm-tabs-next (&optional offset)
  "Switch to the next panel tab, skipping OFFSET tabs."
  (interactive "P")
  (vterm-tabs--cycle 'next (or offset 1)))

(defun vterm-tabs-prev (&optional offset)
  "Switch to the previous panel tab, skipping OFFSET tabs."
  (interactive "P")
  (vterm-tabs--cycle 'previous (or offset 1)))

;;; Minor mode

(defvar vterm-tabs-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<f6>"  #'vterm-tabs-toggle)
    (keymap-set map "C-M-r" #'vterm-tabs-rename-buffer)
    (keymap-set map "C-M-t" #'vterm-tabs-home)
    (keymap-set map "C-M-p" #'vterm-tabs-project)
    (keymap-set map "C-M-f" #'vterm-tabs-next)
    (keymap-set map "C-M-b" #'vterm-tabs-prev)
    map)
  "Keymap for `vterm-tabs-mode'.")

(define-minor-mode vterm-tabs-mode
  "Enable tab-line navigation in vterm panel buffers."
  :lighter nil
  :keymap vterm-tabs-mode-map
  (if vterm-tabs-mode
      (progn
        (setq-local tab-line-tabs-function    #'vterm-tabs--all-buffers
                    tab-line-tab-name-function (if (fboundp 'svg-tabs-tab-name)
                                                   #'svg-tabs-tab-name
                                                 #'tab-line-tab-name-truncated-buffer)
                    tab-line-close-button-show nil
                    tab-line-new-button-show   nil
                    mode-line-format          nil)
        (tab-line-mode 1))
    (kill-local-variable 'tab-line-tabs-function)
    (kill-local-variable 'tab-line-tab-name-function)
    (kill-local-variable 'mode-line-format)
    (tab-line-mode -1)))

(defun vterm-tabs--maybe-enable ()
  "Enable `vterm-tabs-mode' in terminal and related buffers."
  (when (derived-mode-p 'vterm-mode 'compilation-mode
                        'flymake-project-diagnostics-mode 'magit-status-mode)
    (vterm-tabs-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-vterm-tabs-mode
  vterm-tabs-mode vterm-tabs--maybe-enable
  :group 'vterm-tabs
  (if global-vterm-tabs-mode
      (progn
        (advice-add 'tab-line-select-tab-buffer :around #'vterm-tabs--select-buffer)
        (advice-add 'compilation-start          :after  #'vterm-tabs--save-compilation-buffer)
        (advice-add 'recompile                  :around #'vterm-tabs--find-prev-compilation)
        (add-hook   'compilation-filter-hook    #'vterm-tabs--colorize-compilation)
        (add-to-list 'display-buffer-alist vterm-tabs--display-rule)
        (add-to-list 'display-buffer-alist vterm-tabs--main-window-rule t))
    (advice-remove 'tab-line-select-tab-buffer #'vterm-tabs--select-buffer)
    (advice-remove 'compilation-start          #'vterm-tabs--save-compilation-buffer)
    (advice-remove 'recompile                  #'vterm-tabs--find-prev-compilation)
    (remove-hook   'compilation-filter-hook    #'vterm-tabs--colorize-compilation)
    (setq display-buffer-alist
          (delq vterm-tabs--display-rule
                (delq vterm-tabs--main-window-rule display-buffer-alist)))))

(provide 'vterm-tabs)
;;; vterm-tabs.el ends here
