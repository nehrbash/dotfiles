;;; sn-modeline.el --- Custom minimal modeline  -*- lexical-binding: t; -*-

;; Author: nehrbash
;; Version: 1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: mode-line

;;; Commentary:

;; Minimal, conditional modeline.  Each segment only appears when relevant.
;; Flymake counts are read directly from `flymake-diagnostics' so they work
;; with any backend, including eglot.

;;; Code:

(require 'cl-lib)

;;; Options

(defgroup sn-modeline nil
  "Custom modeline."
  :group 'mode-line)

(defcustom sn-modeline-image-height 32
  "Height of the decorative mode-line image in pixels."
  :type 'integer
  :group 'sn-modeline)

;;; Checker faces
;;
;; Defaults inherit from standard semantic faces so any theme works out of the
;; box.  When ef-themes or modus-themes is active, `sn-modeline--apply-theme'
;; is called via `modus-themes-post-load-hook' (ef-themes fires the same hook
;; via `ef-themes-take-over-modus-themes-mode') and overrides each face with
;; the exact palette color from the active theme.
;;
;; Symbols (✖ ⚠ ·) are in the basic Unicode range and render in any
;; modern terminal emulator without patched fonts.

(defface sn-modeline-checker-error
  '((t :inherit error :weight bold))
  "Face for flymake error count in the modeline."
  :group 'sn-modeline)

(defface sn-modeline-checker-warning
  '((t :inherit warning))
  "Face for flymake warning count in the modeline."
  :group 'sn-modeline)

(defface sn-modeline-checker-info
  '((t :inherit font-lock-doc-face))
  "Face for flymake note count in the modeline."
  :group 'sn-modeline)

(defface sn-modeline-checker-ok
  '((t :inherit success))
  "Face for flymake clean status in the modeline."
  :group 'sn-modeline)

(defface sn-modeline-checker-running
  '((t :inherit shadow))
  "Face for flymake running/pending status in the modeline."
  :group 'sn-modeline)

(defun sn-modeline--apply-theme ()
  "Remap checker faces to the active ef/modus theme palette.
Called from `modus-themes-post-load-hook', which ef-themes also fires
when `ef-themes-take-over-modus-themes-mode' is enabled."
  (when (fboundp 'modus-themes-with-colors)
    (modus-themes-with-colors
      (custom-set-faces
       `(sn-modeline-checker-error   ((,c :foreground ,red-warmer   :weight bold)))
       `(sn-modeline-checker-warning ((,c :foreground ,yellow-warmer)))
       `(sn-modeline-checker-info    ((,c :foreground ,blue-warmer)))
       `(sn-modeline-checker-ok      ((,c :foreground ,green-warmer)))
       `(sn-modeline-checker-running ((,c :foreground ,fg-dim)))))))

(add-hook 'modus-themes-post-load-hook #'sn-modeline--apply-theme)

;;; Private helpers

(defun sn-modeline--flymake-category (diag)
  "Return the canonical flymake category symbol for DIAG.
Flymake maps type values (keywords or custom symbols) to one of
`flymake-error', `flymake-warning', or `flymake-note' via the
`flymake-category' symbol property.  Eglot uses this indirection,
so we must normalise before comparing."
  (let ((type (flymake-diagnostic-type diag)))
    (or (get type 'flymake-category) type)))

(defun sn-modeline--flymake-counts ()
  "Return (ERRORS WARNINGS NOTES) from live flymake diagnostics."
  (let ((errors 0) (warnings 0) (notes 0))
    (dolist (d (flymake-diagnostics))
      (pcase (sn-modeline--flymake-category d)
        ('flymake-error   (cl-incf errors))
        ('flymake-warning (cl-incf warnings))
        (_                (cl-incf notes))))
    (list errors warnings notes)))

(defvar sn-modeline--flymake-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'flymake-show-project-diagnostics)
    (define-key map [mode-line mouse-2] #'flymake-show-buffer-diagnostics)
    map)
  "Keymap for clicking the flymake segment in the modeline.")

(defun sn-modeline--flymake-props (str face)
  "Return STR propertized with FACE and the flymake click keymap."
  (propertize str
              'face face
              'mouse-face 'mode-line-highlight
              'local-map sn-modeline--flymake-map
              'help-echo "mouse-1: project diagnostics\nmouse-2: buffer diagnostics"))

;;; Segments

(defun sn-modeline-sloth-image ()
  "Return sloth image segment if the file exists."
  (let ((img (expand-file-name "img/sloth-head.jpg" user-emacs-directory)))
    (when (file-exists-p img)
      (propertize " "
                  'display (create-image img nil nil
                                         :height sn-modeline-image-height
                                         :ascent 'center)))))

(defun sn-modeline-kbd-macro ()
  "Return kbd macro indicator when recording or executing."
  (when (or defining-kbd-macro executing-kbd-macro)
    (propertize (if defining-kbd-macro " REC " " MACRO ")
                'face 'mode-line-emphasis)))

(defun sn-modeline-narrow ()
  "Return narrow indicator when the buffer is narrowed."
  (when (buffer-narrowed-p)
    (propertize " Narrow " 'face 'warning)))

(defun sn-modeline-remote ()
  "Return remote host indicator when on a TRAMP connection."
  (when-let* ((host (file-remote-p default-directory 'host)))
    (propertize (format " @%s " host) 'face 'error)))

(defun sn-modeline-dedicated ()
  "Return indicator when the selected window is dedicated."
  (when (window-dedicated-p)
    (propertize " [D] " 'face 'mode-line-highlight)))

(defun sn-modeline-input-method ()
  "Return current input method title when an input method is active."
  (when current-input-method
    (propertize (format " [%s] " current-input-method-title) 'face 'success)))

(defun sn-modeline-meow ()
  "Return meow state indicator when `meow-mode' is active."
  (when (bound-and-true-p meow-mode)
    (meow--update-indicator)))

(defun sn-modeline-buffer-name ()
  "Return buffer name with modification and read-only prefixes."
  (propertize (format "%s%s%s"
                      (if (buffer-modified-p) "*" "")
                      (if buffer-read-only "%" "")
                      (buffer-name))
              'face 'mode-line-buffer-id))

(defun sn-modeline-process ()
  "Return `mode-line-process' when a process is active."
  (when mode-line-process
    (format-mode-line mode-line-process)))

(defun sn-modeline-breadcrumb ()
  "Return breadcrumb imenu path when `breadcrumb-mode' is active."
  (when (bound-and-true-p breadcrumb-mode)
    (breadcrumb-imenu-crumbs)))

(defun sn-modeline-eglot ()
  "Return eglot LSP status when `eglot--managed-mode' is active."
  (when (and (bound-and-true-p eglot--managed-mode)
             (fboundp 'eglot-current-server))
    (if-let* ((server (eglot-current-server)))
        (let* ((nick    (eglot-project-nickname server))
               (pending (hash-table-count
                         (jsonrpc--request-continuations server)))
               (err     (jsonrpc-last-error server)))
          (cond
           (err           (propertize " ⚠ LSP" 'face 'error))
           ((> pending 0) (propertize (format " ⟳%d" pending) 'face 'warning))
           (t             (propertize (format " %s" nick) 'face 'success))))
      (propertize " LSP?" 'face 'shadow))))

(defun sn-modeline-flymake ()
  "Return flymake diagnostic counts when `flymake-mode' is active.
Shows all non-zero counts simultaneously.  Reads live diagnostics
directly so eglot-provided results are always current."
  (when (bound-and-true-p flymake-mode)
    (pcase-let ((`(,e ,w ,n) (sn-modeline--flymake-counts)))
      (if (= 0 (+ e w n))
          ;; No diagnostics: show spinner while running, checkmark when idle.
          (if (flymake-running-backends)
              (sn-modeline--flymake-props " …" 'sn-modeline-checker-running)
            (sn-modeline--flymake-props " ✔" 'sn-modeline-checker-ok))
        ;; Show every non-zero count (doom style: all visible at once).
        (concat
         (when (> e 0) (sn-modeline--flymake-props (format " ✖%d" e) 'sn-modeline-checker-error))
         (when (> w 0) (sn-modeline--flymake-props (format " ⚠%d" w) 'sn-modeline-checker-warning))
         (when (> n 0) (sn-modeline--flymake-props (format " ·%d" n) 'sn-modeline-checker-info)))))))

(defun sn-modeline-vc ()
  "Return VC branch name when in a version-controlled buffer."
  (when (and vc-mode (stringp vc-mode))
    (let* ((backend (and buffer-file-name
                         (vc-backend buffer-file-name)))
           (branch  (if backend
                        (substring-no-properties
                         vc-mode (+ 2 (length (symbol-name backend))))
                      (string-trim vc-mode))))
      (propertize (format "  %s" branch)
                  'face '(:inherit success :weight bold)))))

;;; Mode-line format

(defvar-local sn-modeline-format
    '((:eval (sn-modeline-sloth-image))
      (:eval (sn-modeline-kbd-macro))
      (:eval (sn-modeline-narrow))
      (:eval (sn-modeline-remote))
      (:eval (sn-modeline-dedicated))
      (:eval (sn-modeline-input-method))
      (:eval (sn-modeline-meow))
      "  "
      (:eval (sn-modeline-buffer-name))
      "  "
      (:eval (sn-modeline-process))
      " "
      (:eval (sn-modeline-breadcrumb))
      mode-line-format-right-align
      (:eval (sn-modeline-eglot))
      " "
      (:eval (sn-modeline-flymake))
      "  "
      (:eval (sn-modeline-vc))
      "  ")
  "Custom minimal mode-line format.")

;;;###autoload
(define-minor-mode sn-modeline-mode
  "Enable the sn custom modeline."
  :global t
  :group 'sn-modeline
  (setq-default mode-line-format
                (if sn-modeline-mode
                    sn-modeline-format
                  (default-value 'mode-line-format)))
  (force-mode-line-update t))

(provide 'sn-modeline)
;;; sn-modeline.el ends here
