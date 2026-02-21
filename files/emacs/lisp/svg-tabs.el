;;; svg-tabs.el --- SVG tab-line labels for vterm-tabs  -*- lexical-binding: t; -*-

;; Author: Stephen Nehrbass
;; Keywords: convenience, tabs
;; Version: 2.0
;; Package-Requires: ((emacs "31.1") (svg-lib "0.3"))

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

;; SVG tab-line name formatter for vterm-tabs buffers.  Renders each tab
;; as a rounded SVG tag via `svg-lib'.

;;; Code:

(require 'tab-line)
(require 'svg-lib)

;;; Faces

(defface svg-tabs-active
  '((t :foreground "#352718" :background "#e4b53f"))
  "Face for the selected tab in the vterm panel."
  :group 'vterm-tabs)

(defface svg-tabs-inactive
  '((t :foreground "#e8e4b1" :background "#59463f"))
  "Face for inactive tabs in the vterm panel."
  :group 'vterm-tabs)

;;; Per-mode labels

(defun svg-tabs--label (buffer)
  "Return a short label string for BUFFER."
  (with-current-buffer buffer
    (cond
     ((derived-mode-p 'vterm-mode)       (buffer-name))
     ((derived-mode-p 'magit-status-mode) "Magit")
     ((derived-mode-p 'flymake-project-diagnostics-mode) "Diagnostics")
     ((derived-mode-p 'compilation-mode) "Compile")
     (t (buffer-name)))))

;;; Entry point

;;;###autoload
(defun svg-tabs-tab-name (buffer &optional _buffers)
  "Return an SVG image string for BUFFER suitable for `tab-line-tab-name-function'."
  (if (not (buffer-live-p buffer))
      "[dead]"
    (let* ((current-p (eq buffer (window-buffer)))
           (face      (if current-p 'svg-tabs-active 'svg-tabs-inactive))
           (label     (svg-tabs--label buffer)))
      (propertize label 'display
                  (svg-lib-tag label face
                               :inverse current-p
                               :radius 6 :padding 1
                               :font-size 13
                               :height 1.3 :margin 1)))))

(provide 'svg-tabs)
;;; svg-tabs.el ends here
