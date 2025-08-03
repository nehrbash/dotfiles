;;; svg-tabs.el --- SVG-based tab line formatting -*- lexical-binding: t; -*-

;; Author: Stephen Nehrbass
;; Keywords: convenience, tabs
;; Version: 1.0
;; Package-Requires: ((emacs "30.1") (svg-tag-mode "0.3") (tab-line "1.0") (all-the-icons "5.0"))

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

;; This package provides SVG-based tab formatting for tab-line-mode,
;; with icons and special formatting for different buffer types.

;;; Code:

(require 'svg-tag-mode)
(require 'tab-line)
(require 'subr-x)
(require 'flymake)


;; Optional dependencies - only use if available
(defvar svg-tabs--svg-tag-mode-available
  (condition-case nil
	  (require 'svg-tag-mode)
	(error nil)))

(defvar svg-tabs--all-the-icons-available
  (condition-case nil
	  (require 'all-the-icons)
	(error nil)))

(defgroup svg-tabs nil
  "SVG-based tab line formatting."
  :group 'convenience
  :prefix "svg-tabs-")

(defcustom svg-tabs-icon-scale 1.0
  "Scale factor for icons in tabs."
  :type 'float
  :group 'svg-tabs)

(defcustom svg-tabs-show-compilation-stats t
  "Whether to show compilation statistics in compilation buffer tabs."
  :type 'boolean
  :group 'svg-tabs)

(defcustom svg-tabs-use-svg-rendering t
  "Whether to use SVG rendering for tabs (requires svg-tag-mode)."
  :type 'boolean
  :group 'svg-tabs)

(defun svg-tabs--get-icon (icon-type icon-name)
  "Get icon if all-the-icons is available, otherwise return empty string."
  (if svg-tabs--all-the-icons-available
	  (condition-case nil
		  (pcase icon-type
			('faicon (all-the-icons-faicon icon-name))
			('material (all-the-icons-material icon-name))
			('octicon (all-the-icons-octicon icon-name))
			(_ ""))
		(error ""))
	""))

(defun svg-tabs--get-buffer-icon-and-name (buffer)
  "Get the appropriate icon and name for BUFFER."
  (let ((buffer-name (buffer-name buffer)))
	(with-current-buffer buffer
	  (cond
	   ((derived-mode-p 'vterm-mode)
		(concat (svg-tabs--get-icon 'faicon "terminal")
				(if svg-tabs--all-the-icons-available " " "")
				buffer-name))
	   ((derived-mode-p 'magit-status-mode)
		(concat (svg-tabs--get-icon 'faicon "git")
				(if svg-tabs--all-the-icons-available " " "")
				buffer-name))
	   ((derived-mode-p 'flymake-project-diagnostics-mode)
		(concat (svg-tabs--get-icon 'faicon "stethoscope")
				(if svg-tabs--all-the-icons-available " " "")
				"Diagnostics"))
	   ((derived-mode-p 'compilation-mode)
		(svg-tabs--format-compilation-tab buffer-name))
	   (t buffer-name)))))

(defun svg-tabs--format-compilation-tab (buffer-name)
  "Format compilation buffer tab with error/warning counts."
  (let ((icon (svg-tabs--get-icon 'faicon "cogs"))
		(name (if (string-match "\\`\\*\\(.*?\\)\\*\\'" buffer-name)
				  (match-string 1 buffer-name)
				"Compile")))
	(if (and svg-tabs-show-compilation-stats
			 (boundp 'compilation-num-errors-found)
			 (boundp 'compilation-num-warnings-found)
			 (boundp 'compilation-num-infos-found))
		(format "%s%s%s E:%d W:%d I:%d"
				icon
				(if svg-tabs--all-the-icons-available " " "")
				name
				(or compilation-num-errors-found 0)
				(or compilation-num-warnings-found 0)
				(or compilation-num-infos-found 0))
	  (concat icon
			  (if svg-tabs--all-the-icons-available " " "")
			  name))))

(defun svg-tabs--svg-line-tab-name-buffer (buffer &optional _buffers)
  "Create the SVG representation of BUFFER's tab in the tab line."
  (let* ((tab-name (svg-tabs--get-buffer-icon-and-name buffer))
		 (current-p (eq (current-buffer) buffer))
		 (face (if current-p 'tab-line-tab-current 'tab-line-tab-inactive)))

	;; Try SVG rendering if enabled and available, otherwise fallback to plain text
	(if (and svg-tabs-use-svg-rendering
			 svg-tabs--svg-tag-mode-available)
		(condition-case nil
			(let ((svg-result (svg-tag-make
							  tab-name
							  :face face
							  :inverse current-p
							  :radius 5
							  :margin 0
							  :scale svg-tabs-icon-scale
							  :font-weight 'bold)))
			  (if svg-result
				  (propertize tab-name 'display svg-result)
				;; Fallback to plain text if SVG creation fails
				(propertize tab-name 'face face)))
		  (error
		   ;; If anything goes wrong, return plain text
		   (propertize tab-name 'face face)))
	  ;; Plain text fallback
	  (propertize tab-name 'face face))))

;;;###autoload
(defun svg-tabs-setup ()
  "Set up SVG tabs for 'tab-line-mode'."
  (interactive)
  (setq tab-line-tab-name-function #'svg-tabs--svg-line-tab-name-buffer
		tab-line-new-button-show nil
		tab-line-close-button-show nil))

;;;###autoload
(defun svg-tabs-reset ()
  "Reset tab-line to default settings."
  (interactive)
  (setq tab-line-tab-name-function #'tab-line-tab-name-truncated-buffer
		tab-line-new-button-show t
		tab-line-close-button-show t))

;; Auto-setup when loaded (can be disabled by user)
;;;###autoload
(svg-tabs-setup)

(provide 'svg-tabs)
;;; svg-tabs.el ends here
