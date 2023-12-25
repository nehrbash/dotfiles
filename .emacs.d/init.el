;;; init.el --- Initialization file for Emacs -*- lexical-binding: nil -*-
;;; Commentary: Emacs Startup File, initialization for Emacs
;;; Code:

(defun load-all-environment-variables ()
	"Load all environment variables from the user's shell."
	(let ((shell-env (shell-command-to-string "env")))
	  (dolist (var (split-string shell-env "\n"))
		(when (string-match "\\([^=]+\\)=\\(.*\\)" var)
		  (let ((name (match-string 1 var))
				(value (match-string 2 var)))
			(setenv name value))))))
(load-all-environment-variables)

(eval-when-compile
  (require 'package)
  (require 'use-package))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq
 warning-minimum-level :emergency
 comp-async-report-warnings-errors nil
 native-comp-jit-compilation t
 native-compile-prune-cache t
 package-install-upgrade-built-in t
 package-native-compile t
 package-quickstart t
 use-package-always-ensure t
 use-package-compute-statistics t
 use-package-expand-minimally t
 async-bytecomp-package-mode t)
(use-package async
  :init (async-bytecomp-package-mode 1))
(defun sn/finish-install()
  (interactive)
  (treesit-auto-install-all)
  (all-the-icons-install-fonts)
  (yas-reload-all)
  (recentf-cleanup)
  (nerd-icons-install-fonts)
  (package-quickstart-refresh))
(add-hook 'package-upgrade-all-hook 'sn/finish-install)

(use-package ef-themes
  :hook
  (server-after-make-frame
   . (lambda ()
	   (ef-themes-with-colors
		 (custom-set-faces
		  `(scroll-bar ((,c :foreground ,bg-alt :background ,bg-dim))))
		 )))
  :custom
  (custom-safe-themes t)
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  (ef-themes-headings
   '((0 variable-pitch light 2.1)
	 (1 variable-pitch light 1.8)
	 (t variable-pitch 1.2)
	 (agenda-date 1.9)
	 (agenda-structure variable-pitch light 1.8)
	 (t variable-pitch)))
  :config
  (defun my-ef-themes-mod ()
	"Tweak the style of the ef theme."
	(ef-themes-with-colors
	  (custom-set-faces
	   `(blamer-face ((,c :foreground ,fg-alt :italic t)))
	   `(scroll-bar ((,c :foreground ,bg-alt :background ,bg-dim)))
	   `(mode-line-active ((,c :background ,bg-mode-line :foreground ,fg-main  :box (:line-width 3 :color "#281d12"))))
	   `(mode-line-inactive ((,c  :box (:line-width 3 :color ,bg-active))))
	   `(org-document-title ((,c :height 1.8)))
	   `(org-modern-todo ((,c :height 1.2)))
	   `(org-modern-done ((,c :height 1.2)))
	   `(org-modern-tag ((,c :height 1.2)))
	   `(default ((,c :font "Source Code Pro" :height 115)))
	   `(unspecified-bg ((,c :foreground ,bg-main :background ,bg-main))))))
  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-mod)
  ;; (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-melissa-dark)
  :init
  (spacious-padding-mode 1)
  (ef-themes-select 'ef-melissa-dark))

(use-package rainbow-delimiters
  :hook ((prog-mode conf-mode) . rainbow-delimiters-mode))

(use-package doom-modeline
  :defer t
  :init
  (defun sn/set-modeline ()
	(require 'doom-modeline)
	(line-number-mode -1)
	(column-number-mode -1)
	(doom-modeline-def-modeline 'simple-line
	  '(bar modals buffer-info remote-host)
	  '(compilation objed-state misc-info persp-name lsp checker process vcs))
	(doom-modeline-set-modeline 'simple-line 'default))
  :custom
  (doom-modeline-project-detection 'project)
  (doom-modeline-vcs-max-length 30)
  (doom-modeline-height 32)
  :hook (after-init . sn/set-modeline))

(set-display-table-slot standard-display-table 'truncation ?\s) ;; remove the $ on wrap lines.
(pixel-scroll-precision-mode t)
(global-prettify-symbols-mode t)

(use-package page-break-lines
  :config (global-page-break-lines-mode))

(use-package default-text-scale
		  :bind (("C-M-=". default-text-scale-increase)
				 ("C-M--" . default-text-scale-decrease)))

(use-package spacious-padding
  :custom
  (spacious-padding-widths
   '( :internal-border-width 15
	  :header-line-width 4
	  :mode-line-width 2
	  :tab-width 4
	  :right-divider-width 30
	  :scroll-bar-width 8)))

(setq-default
 fill-column 120
 blink-cursor-interval 0.4
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 tab-width 4
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 set-mark-command-repeat-pop t
 tooltip-delay .8
 ring-bell-function 'ignore)
(delete-selection-mode t)
(global-goto-address-mode t)
(add-hook 'after-init-hook 'transient-mark-mode) ;; standard highlighting
(setq browse-url-browser-function #'browse-url-firefox)
(setq use-dialog-box nil) ;; disable pop-ups
(global-set-key (kbd "C-c C-p") 'find-file-at-point)
(set-default 'truncate-lines t) ;; don't wrap lines globally

(use-package recentf
  :ensure nil
  :init
  ;; save backup and auto save to system tmp
  (setq backup-directory-alist
		`((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
		`((".*" ,temporary-file-directory t)))
  (recentf-mode)
  :custom
  (bookmark-default-file (expand-file-name "var/bookmarks.el" user-emacs-directory))
  (recentf-auto-cleanup 'never) ; Disable automatic cleanup at load time
  (recentf-max-saved-items 50)
  (recentf-exclude '("*/type-break.el$"
					 ".*![^!]*!.*"
					 "*/ssh:*")))

(use-package autorevert
  :init (global-auto-revert-mode 1))

(customize-set-variable 'tramp-default-method "ssh")
(with-eval-after-load 'tramp
  (setq tramp-verbose 0
		tramp-use-ssh-controlmaster-options nil) ;; use .ssh/config controlmaster settings
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-connection-properties
			 (list (regexp-quote "/ssh:ag-nehrbash:")
				   "remote-shell" "/usr/bin/bash"
				   "direct-async-process" t
				   "tramp-direct-async" t)))
(setq vc-handled-backends '(Git)) ;; I only use git

(use-package savehist
  :ensure nil
  :init (savehist-mode 1)
  :config
  (setq history-length 25))

(save-place-mode 1)

(use-package anzu
  :bind (([remap query-replace-regexp] . anzu-query-replace-regexp)
		 ([remap query-replace] . anzu-query-replace)
		 ("C-M-w". isearch-yank-symbol))
  :custom
  (anzu-mode-lighter "")
  :config
  (defun sanityinc/isearch-exit-other-end ()
	"Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
	(interactive)
	(isearch-exit)
	(goto-char isearch-other-end))
  (define-key isearch-mode-map [(control return)] 'sanityinc/isearch-exit-other-end)
  ;; Search back/forth for the symbol at point
  ;; See http://www.emacswiki.org/emacs/SearchAtPoint
  (defun isearch-yank-symbol ()
	"*Put symbol at current point into search string."
	(interactive)
	(let ((sym (thing-at-point 'symbol)))
	  (if sym
		  (progn
			(setq isearch-regexp t
				  isearch-string (concat "\\_<" (regexp-quote sym) "\\_>")
				  isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
				  isearch-yank-flag t))
		(ding)))
	(isearch-search-and-update)))

(use-package ibuffer-project
  :bind ("C-x C-b" . ibuffer)
  :custom ((ibuffer-show-empty-filter-groups nil)
		   (ibuffer-project-use-cache t))
  :config
  (defun ibuffer-set-up-preferred-filters ()
			   (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
			   (unless (eq ibuffer-sorting-mode 'project-file-relative)
				 (ibuffer-do-sort-by-project-file-relative)))
  :hook (ibuffer . ibuffer-set-up-preferred-filters))

(setq ad-redefinition-action 'accept)

(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-<return>") 'sanityinc/newline-at-end-of-line)

(use-package display-line-numbers
  :if (fboundp 'display-line-numbers-mode)
  :init
  (setq-default display-line-numbers-width 3)
  (setq-default display-line-numbers-type 'relative)
  :hook (prog-mode . display-line-numbers-mode))

(use-package expand-region
  :bind (("M-C e" . er/expand-region)
		 ("M-C o" . er/mark-outside-pairs)))

(use-package symbol-overlay
  :hook ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :config
  (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
  (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos
		 (point)))
	(back-to-indentation)
	(kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

(defun sanityinc/backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
	(cond ((elt ppss 3)
		   (goto-char (elt ppss 8))
		   (sanityinc/backward-up-sexp (1- arg)))
		  ((backward-up-list arg)))))
(global-set-key [remap backward-up-list] 'sanityinc/backward-up-sexp) ; C-M-u, C-M-up

(use-package which-key
  :custom (which-key-idle-delay 1)
  :config (which-key-mode 1))

(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
		 ("C->" . mc/mark-next-like-this)
		 ("C-+" . mc/mark-next-like-this)
		 ("C-c C-<" . mc/mark-all-like-this)
		 ;; From active region to multiple cursors:
		 ("C-c m r" . set-rectangular-region-anchor)
		 ("C-c m c" . mc/edit-lines)
		 ("C-c m e" . mc/edit-ends-of-lines)
		 ("C-c m a" . mc/edit-beginnings-of-lines)))

(use-package whitespace-cleanup-mode
  :commands (whitespace-cleanup)
  :hook ((prog-mode text-mode conf-mode) . sanityinc/show-trailing-whitespace)
  :config
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)
  (defun sanityinc/show-trailing-whitespace ()
	"Enable display of trailing whitespace in this buffer."
	(setq-local show-trailing-whitespace t)
	(whitespace-cleanup-mode 1)))

(electric-pair-mode t)
(use-package paren ; highight matching paren
  :ensure nil
  :hook (prog-mode . show-paren-mode))

(use-package winner
  :bind (("C-x 2" . split-window-func-with-other-buffer-vertically)
		 ("C-x 3" . split-window-func-with-other-buffer-horizontally)
		 ("C-x 1" . sanityinc/toggle-delete-other-windows)
		 ("C-x |" . split-window-horizontally-instead)
		 ("C-x _" . split-window-vertically-instead)
		 ("<f7>" . sanityinc/split-window)
		 ("C-c <down>" . sanityinc/toggle-current-window-dedication))
  :config
  (defun split-window-func-with-other-buffer-vertically ()
	"Split this window vertically and switch to the new window."
	(interactive)
	(split-window-vertically)
	(let ((target-window (next-window)))
	  (set-window-buffer target-window (other-buffer))
	  (select-window target-window)))

  (defun split-window-func-with-other-buffer-horizontally ()
	"Split this window horizontally and switch to the new window."
	(interactive)
	(split-window-horizontally)
	(let ((target-window (next-window)))
	  (set-window-buffer target-window (other-buffer))
	  (select-window target-window)))

  (defun sanityinc/toggle-delete-other-windows ()
	"Delete other windows in frame if any, or restore previous window config."
	(interactive)
	(if (and (bound-and-true-p winner-mode)
		   (equal (selected-window) (next-window)))
		(winner-undo)
	  (delete-other-windows)))

  (defun split-window-horizontally-instead ()
	"Kill any other windows and re-split such that the current window is on the top half of the frame."
	(interactive)
	(let ((other-buffer (and (next-window) (window-buffer (next-window)))))
	  (delete-other-windows)
	  (split-window-horizontally)
	  (when other-buffer
		(set-window-buffer (next-window) other-buffer))))

  (defun split-window-vertically-instead ()
	"Kill any other windows and re-split such that the current window is on the left half of the frame."
	(interactive)
	(let ((other-buffer (and (next-window) (window-buffer (next-window)))))
	  (delete-other-windows)
	  (split-window-vertically)
	  (when other-buffer
		(set-window-buffer (next-window) other-buffer))))

  (defun sanityinc/split-window()
	"Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
	(interactive)
	(if (eq last-command 'sanityinc/split-window)
		(progn
		  (jump-to-register :sanityinc/split-window)
		  (setq this-command 'sanityinc/unsplit-window))
	  (window-configuration-to-register :sanityinc/split-window)
	  (switch-to-buffer-other-window nil)))

  (defun sanityinc/toggle-current-window-dedication ()
	"Toggle whether the current window is dedicated to its current buffer."
	(interactive)
	(let* ((window (selected-window))
		   (was-dedicated (window-dedicated-p window)))
	  (set-window-dedicated-p window (not was-dedicated))
	  (message "Window %sdedicated to %s"
			   (if was-dedicated "no longer " "")
			   (buffer-name)))))

(use-package avy
  :bind ("C-:" . avy-goto-char-timer))

(setq confirm-kill-processes nil)

;; (use-package use-package-chords
;;   :config (key-chord-mode 1))
(use-package meow
  :config
  (setq meow-replace-state-name-list
   '((normal . "🟢")
	 (motion . "🟡")
	 (keypad . "🟣")
	 (insert . "🟠")
	 (beacon . "🔴")))
  (add-to-list 'meow-mode-state-list '(org-mode . insert))
  (add-to-list 'meow-mode-state-list '(eat-mode . insert))
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  (add-to-list 'meow-mode-state-list '(git-commit-mode . insert))
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh)
  (meow-motion-overwrite-define-key
   ;; Use e to move up, n to move down.
   ;; Since special modes usually use n to move down, we only overwrite e here.
   '("e" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   ;; To execute the originally e in MOTION state, use SPC e.
   '("e" . "H-e")
   '("o" . switch-window)
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))

  ;; (setq meow-smex-keymap (make-keymap))
  ;; (meow-define-state paren
  ;;	"meow state for interacting with smartparens"
  ;;	:lighter " [P]"
  ;;	:keymap meow-paren-keymap)

  ;; ;; meow-define-state creates the variable
  ;; (setq meow-cursor-type-paren 'hollow)

  ;; (meow-define-keys 'paren
  ;;	'("<escape>" . meow-normal-mode)
  ;;	'("l" . sp-forward-sexp)
  ;;	'("h" . sp-backward-sexp)
  ;;	'("j" . sp-down-sexp)
  ;;	'("k" . sp-up-sexp)
  ;;	'("n" . sp-forward-slurp-sexp)
  ;;	'("b" . sp-forward-barf-sexp)
  ;;	'("v" . sp-backward-barf-sexp)
  ;;	'("c" . sp-backward-slurp-sexp)
  ;;	'("u" . meow-undo))
  ;; (meow-define-state paren
  ;;	"meow state for interacting with smartparens"
  ;;	:lighter " [P]"
  ;;	:keymap meow-paren-keymap)

  ;; ;; meow-define-state creates the variable
  ;; (setq meow-cursor-type-paren 'hollow)

  ;; (meow-define-keys 'paren
  ;;	'("<escape>" . meow-normal-mode)
  ;;	'("l" . sp-forward-sexp)
  ;;	'("h" . sp-backward-sexp)
  ;;	'("j" . sp-down-sexp)
  ;;	'("k" . sp-up-sexp)
  ;;	'("n" . sp-forward-slurp-sexp)
  ;;	'("b" . sp-forward-barf-sexp)
  ;;	'("v" . sp-backward-barf-sexp)
  ;;	'("c" . sp-backward-slurp-sexp)
  ;;	'("u" . meow-undo))
  (meow-global-mode 1))

(use-package transient
  :defer t
  :bind
  (:map isearch-mode-map
			  ("M-t" . sn/isearch-menu))
  :config
  (transient-define-prefix sn/isearch-menu ()
	"isearch Menu"
	[["Edit Search String"
	  ("e"
	   "Edit the search string (recursive)"
	   isearch-edit-string
	   :transient nil)
	  ("w"
	   "Pull next word or character word from buffer"
	   isearch-yank-word-or-char
	   :transient nil)
	  ("s"
	   "Pull next symbol or character from buffer"
	   isearch-yank-symbol-or-char
	   :transient nil)
	  ("l"
	   "Pull rest of line from buffer"
	   isearch-yank-line
	   :transient nil)
	  ("y"
	   "Pull string from kill ring"
	   isearch-yank-kill
	   :transient nil)
	  ("t"
	   "Pull thing from buffer"
	   isearch-forward-thing-at-point
	   :transient nil)]
	 ["Replace"
	  ("q"
	   "Start ‘query-replace’"
	   anzu-isearch-query-replace
	   :if-nil buffer-read-only
	   :transient nil)
	  ("x"
	   "Start ‘query-replace-regexp’"
	   anzu-isearch-query-replace-regexp
	   :if-nil buffer-read-only
	   :transient nil)
	  ]]
	[["Toggle"
	  ("X"
	   "Toggle regexp searching"
	   isearch-toggle-regexp
	   :transient nil)
	  ("S"
	   "Toggle symbol searching"
	   isearch-toggle-symbol
	   :transient nil)
	  ("W"
	   "Toggle word searching"
	   isearch-toggle-word
	   :transient nil)
	  ("F"
	   "Toggle case fold"
	   isearch-toggle-case-fold
	   :transient nil)
	  ("L"
	   "Toggle lax whitespace"
	   isearch-toggle-lax-whitespace
	   :transient nil)]

	 ["Misc"
	  ("l"
	   "Start ‘consult-line’"
	   consult-line
	   :transient nil)
	  ("g"
	   "Start ‘consult-git-grep’"
	   consult-git-grep
	   :transient nil)
	  ("r"
	   "Start ‘consult-ripgrep’"
	   consult-ripgrep
	   :transient nil)
	  ("o"
	   "occur"
	   isearch-occur
	   :transient nil)]]))

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
	(add-to-list 'auto-mode-alist (cons pattern mode))))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
	(error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
							 (file-name-nondirectory buffer-file-name)))
	(delete-file (buffer-file-name))
	(kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
		(filename (buffer-file-name)))
	(unless filename
	  (error "Buffer '%s' is not visiting a file!" name))
	(progn
	  (when (file-exists-p filename)
		(rename-file filename new-name 1))
	  (set-visited-file-name new-name)
	  (rename-buffer new-name))))

(defun toggle-mode-line ()
  "toggles the modeline on and off"
	   (interactive)
	   (setq mode-line-format
			 (if (equal mode-line-format nil)
				 (default-value 'mode-line-format)))
	   (redraw-display))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil))

(use-package move-dup
  :bind(("M-<up>" . move-dup-move-lines-up)
		("M-<down>" . move-dup-move-lines-down)
		("C-c d" . move-dup-duplicate-down)
		("C-c u" . move-dup-duplicate-up)))

(use-package whole-line-or-region
  :config (whole-line-or-region-global-mode t))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
	(let ((line-move-visual nil))
	  (forward-line (1- arg))))

  (let ((orig-point (point)))
	(back-to-indentation)
	(when (= orig-point (point))
	  (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
				'smarter-move-beginning-of-line)

(use-package switch-window
  :init
	(meow-leader-define-key
	'("o" . switch-window))
  :config
  (setq switch-window-shortcut-style 'alphabet
		switch-window-timeout nil)
  :bind ("C-x o" . switch-window))

(use-package windswap
  :config
  (windmove-default-keybindings 'control)
  (windswap-default-keybindings 'shift 'control))

(use-package sudo-edit
  :defer t
  :commands (sudo-edit))

(use-package fullframe)

(use-package minibuffer
  :ensure nil
  :bind (:map minibuffer-local-completion-map
			  ("<backtab>" . minibuffer-force-complete))
  :custom
  (enable-recursive-minibuffers t)
  (minibuffer-eldef-shorten-default t)
  (read-minibuffer-restore-windows nil) ;; don't revert to original layout after cancel.
  (resize-mini-windows t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook
  (completion-list-mode . force-truncate-lines)
  (minibuffer-setup . (lambda ()
						(set-window-scroll-bars (minibuffer-window) nil nil)
						(cursor-intangible-mode 1)))
  :config
  (minibuffer-depth-indicate-mode)
  (minibuffer-electric-default-mode))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-multiform-mode 1))
(use-package marginalia
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
			  ("M-a" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))
(use-package all-the-icons-completion
  :after marginalia
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(use-package orderless
  :custom
  (orderless-matching-styles 'orderless-regexp)
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (completion-category-defaults nil)
  (completion-styles '(orderless flex))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package wgrep
  :commands (wgrep wgrep-change-to-wgrep-mode))

(use-package consult
  :after vertico
  :defer t
  :bind ((:map meow-normal-state-keymap
			   ("C-b" . consult-buffer-other-window)
			   ("M-b". consult-buffer);; orig. switch-to-buffer-other-window
			   ("P" . consult-yank-pop)
			   ("M-o" . consult-outline)
			   ("C-M-r" . consult-register)
			   ("C-M-s" . consult-register-store))

		 ;; Custom M-# bindings for fast register access
		 ("M-#" . consult-register-load)


		 ;; Other custom bindings
		 ("<help> a" . consult-apropos)            ;; orig. apropos-command
		 ;; M-g bindings (goto-map)
		 ("M-g e" . consult-compile-error)
		 ("M-g n" . consult-flymake)
						;; Alternative: consult-org-heading
		 ("M-g m" . consult-mark)
		 ("M-g k" . consult-global-mark)
		 ("M-g i" . consult-imenu)
		 ("M-g I" . consult-imenu-multi)

		 ("M-s f" . consult-find)
		 ("M-s L" . consult-locate)
		 ("M-s G" . consult-git-grep)
		 ("M-s r" . consult-ripgrep)
		 ("M-s m" . consult-multi-occur)
		 ("M-s k" . consult-keep-lines)
		 ("M-s u" . consult-focus-lines))
  :init
   (meow-leader-define-key
	'("b" . consult-bookmark)
	'("<f4>" . consult-kmacro)
	'("h" . consult-recent-file))
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'consult-line :before (lambda (&optional initial start)(push-mark-no-activate)) '((name . "add-mark")))
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref xref-show-definitions-function #'consult-xref)
  (setq register-preview-delay 0.5
		register-preview-function #'consult-register-format)
  :custom
  (consult-narrow-key "<")
  (consult-preview-key '("M-," :debounce 0 any))
  :config

  ;; (setq consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))
  (defvar consult--source-org
	(list :name     "Org"
		  :category 'buffer
		  :narrow   ?o
		  :face     'consult-buffer
		  :history  'buffer-name-history
		  :state    #'consult--buffer-state
		  :new
		  (lambda (name)
			(with-current-buffer (get-buffer-create name)
			  (insert "#+title: " name "\n\n")
			  (org-mode)
			  (consult--buffer-action (current-buffer))))
		  :items
		  (lambda ()
			(mapcar #'buffer-name
					(seq-filter
					 (lambda (x)
					   (eq (buffer-local-value 'major-mode x) 'org-mode))
					 (buffer-list))))))
  (defvar consult--source-vterm
	(list :name     "Term"
		  :category 'buffer
		  :narrow   ?v
		  :face     'consult-buffer
		  :history  'buffer-name-history
		  :state    #'consult--buffer-state
		  :new
		  (lambda (name)
			(vterm (concat "Term " name))
			(setq-local vterm-buffer-name-string nil))
		  :items
		  (lambda () (consult--buffer-query :sort 'visibility
											:as #'buffer-name
											:include '("Term\\ ")))))
  (defvar consult--source-star
	(list :name     "*Star-Buffers*"
		  :category 'buffer
		  :narrow   ?s
		  :face     'consult-buffer
		  :history  'buffer-name-history
		  :state    #'consult--buffer-state
		  :items
		  (lambda () (consult--buffer-query :sort 'visibility
											:as #'buffer-name
											:include '("\\*." "^magit")))))
  ;; remove org and vterm buffers from buffer list
  (setq consult--source-buffer
		(plist-put
		 consult--source-buffer :items
		 (lambda () (consult--buffer-query
					 :sort 'visibility
					 :as #'buffer-name
					 :exclude '("\\*."           ; star buffers
								"\\#."
								"^type-break.el"
								"Term\\ "        ; Term buffers
								"^magit"         ; magit buffers
								"[\\.]org$"))))) ; org files

  (setq consult--source-project-buffer
		(plist-put
		 consult--source-project-buffer :items
		 (lambda ()
		   (consult--buffer-query
			:sort 'visibility
			:as #'buffer-name
			:exclude '("\\*."           ; star buffers
					   "Term\\ "        ; Term buffers
					   "^magit"          ; magit buffers
					   "^type-break.el"
					   "\#\!*"
					   )))))

  ;; reorder, mainly to move recent-file down and org
  (setq consult-buffer-sources
		'(consult--source-hidden-buffer
		  consult--source-modified-buffer
		  consult--source-buffer
		  consult--source-org
		  consult--source-vterm
		  consult--source-bookmark
		  consult--source-recent-file
		  consult--source-file-register
		  consult--source-project-buffer-hidden
		  consult--source-project-recent-file-hidden
		  consult--source-star))
  (setq consult-project-buffer-sources
		'(consult--source-project-buffer
		  consult--source-vterm
		  consult--source-project-recent-file
		  consult--source-star)))

(use-package embark
  :bind (("M-." . embark-act)
		 ("C-;" . embark-dwin)
		 ("C-h B" . embark-bindings)
		 (:map minibuffer-mode-map
			  ("M-e" . sn/edit-search-results))
		 (:map embark-region-map
		 ("w" . google-this)
		 ("g" . gptel)))
  :init
  (defun sn/edit-search-results ()
	"Export results using `embark-export' and activate `wgrep'."
	(interactive)
	(progn
	  (run-at-time 0 nil #'embark-export)
	  (run-at-time 0 nil #'wgrep-change-to-wgrep-mode)))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				 nil
				 (window-parameters (mode-line-format . none))))
  (setq embark-action-indicator (lambda (map _target)
								  (which-key--show-keymap "Embark" map nil nil 'no-paging)
								  #'which-key--hide-popup-ignore-command)
		embark-become-indicator embark-action-indicator)
  (use-package embark-vc))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package project
  :bind-keymap ("C-c p". project-prefix-map))

(use-package protogg
  :vc (:url "https://github.com/nehrbash/protogg.git"
			   :branch "main" :rev :newest)
  :custom (protogg-minibuffer-toggle-key "M-g")
  :bind (("C-c x" . protogg-compile)
		 ([remap dired] . protogg-dired) ;; C-x d
		 ("C-c e" . protogg-eshell)
		 ("M-s d" . protogg-find-dired)
		 ([remap find-file] . protogg-find-file) ;; C-x C-f
		 ([remap list-buffers] . protogg-list-buffers) ;; type C-x C-b
		 ;; note these are not interactive so they won't toggle.
		 ([remap async-shell-command] . protogg-async-shell-command) ;; M-&
		 ([remap shell-command] . protogg-shell-command) ;; M-!
		 ([remap switch-to-buffer] . sn/consult-buffer)
		 ("M-s i" . sn/imenu))
  :config
  (protogg-define 'consult-project-buffer 'consult-buffer sn/consult-buffer)
  (protogg-define 'consult-imenu-multi 'consult-imenu sn/imenu))

(use-package corfu
  :defer 1
  :hook (((prog-mode conf-mode yaml-mode) . (lambda ()
					   (setq-local corfu-auto t
								   corfu-auto-delay 0
								   corfu-auto-prefix 1
									completion-styles '(orderless-fast basic)
								   corfu-popupinfo-delay 0.6))))
  :bind (:map corfu-map ("M-SPC" . corfu-insert-separator)
			  ("TAB" . corfu-next)
			  ([tab] . corfu-next)
			  ("S-TAB" . corfu-previous)
			  ([backtab] . corfu-previous))
  :custom
  (tab-always-indent 'complete)
  (corfu-quit-no-match 'separator)
  (corfu-auto-delay 0.8)
  (corfu-popupinfo-delay 0.2)
  (corfu-auto-prefix 1.3)
  (completion-cycle-threshold 3)
  :config
  ;; TAB cycle if there are only few candidates
  (defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
	   `(orderless-regexp . ,(concat "^" (regexp-quote word)))))
  (orderless-define-completion-style orderless-fast
	(orderless-style-dispatchers '(orderless-fast-dispatch))
	(orderless-matching-styles '(orderless-literal orderless-regexp)))
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package corfu-candidate-overlay
  :after corfu
  :vc (corfu-candidate-overlay :url "https://code.bsdgeek.org/adam/corfu-candidate-overlay.git"
							   :branch "master" :rev :newest)
  :config (corfu-candidate-overlay-mode +1))

(use-package corfu-terminal
  :after corfu
  :vc (:url "https://codeberg.org/akib/emacs-corfu-terminal.git"
			:branch "master" :rev :newest))

(use-package kind-icon
  :after corfu
  :custom ((kind-icon-default-face 'corfu-default))
  :config
  (plist-put kind-icon-default-style :height 0.9)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :bind (("M-/" . completion-at-point) ;; overwrite dabbrev-completion binding with capf
		 ("C-c / t" . complete-tag)        ;; etags
		 ("C-c / d" . cape-dabbrev)        ;; or dabbrev-completion
		 ("C-c / h" . cape-history)
		 ("C-c / f" . cape-file)
		 ("C-c / k" . cape-keyword)
		 ("C-c / s" . cape-elisp-symbol)
		 ("C-c / e" . cape-elisp-block)
		 ("C-c / a" . cape-abbrev)
		 ("C-c / l" . cape-line)
		 ("C-c / z" . cape-codeium))
  :custom (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  :init
  (defalias 'cape-codeium (cape-capf-interactive #'codeium-completion-at-point))
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package yasnippet
  :bind ("C-c s" . yas-insert-snippet)
  :custom
  (yas-verbosity 1)
  (yas-wrap-around-region t)
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "~/.emacs.d/etc/yasnippet/snippets"))
  (yas-global-mode))
(use-package yasnippet-snippets
  :after yasnippet)
(use-package yasnippet-capf
  :after (cape yasnippet)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)) ;; Prefer the name of the snippet instead)

(use-package jinx
  :bind (("M-$" . jinx-correct-word-save-to-file)
		 ("C-M-$" . #'jinx-correct-all)
		 (:map jinx-overlay-map ;; change correct to right click not
			   ("<mouse-1>" . nil)
			   ("<mouse-3>" . jinx-correct)))
  :init (global-jinx-mode)
  (add-to-list 'vertico-multiform-categories
			   '(jinx grid (vertico-grid-annotate . 30)))
  :config
  (defun jinx-correct-word-save-to-file ()
	"Correct word between START and END, and save corrected word to a file, removing duplicates."
	(interactive)
	(progn
	  (call-interactively #'jinx-correct)
	  (let ((current-word (thing-at-point 'word t)))
		(with-temp-buffer
		  (insert current-word)
		  (append-to-file (point-min) (point-max) (expand-file-name "~/.jinxcorrections") t))))))

(use-package define-word
  :commands (define-word)
  :bind ("M-^" . define-word-at-point))

(use-package dired
  :defer t
  :ensure nil
  :commands (dired dired-jump)
  :hook (dired-mode . (lambda ()
						(dired-omit-mode 1)
						(dired-hide-details-mode 1)
						(toggle-mode-line)
						(hl-line-mode 1)))
  :custom
  ((dired-mouse-drag-files t)
   (dired-omit-files "^\\.\\.?$")
   (dired-listing-switches "-agho --group-directories-first")
   (dired-omit-verbose nil)
   (dired-recursive-deletes 'top)
   (dired-dwim-target t)))
(use-package dired-single
  :after dired
  :bind (:map dired-mode-map
			  ("b" . dired-single-up-directory) ;; alternative would be ("f" . dired-find-alternate-file)
			  ("f" . dired-single-buffer)))
(use-package dired-ranger
  :after dired
  :bind (:map dired-mode-map
			  ("w" . dired-ranger-copy)
			  ("m" . dired-ranger-move)
			  ("H" . dired-omit-mode)
			  ("y" . dired-ranger-paste)))
(use-package all-the-icons
  :defer t)
(use-package all-the-icons-dired
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package dired-collapse
  :after dired
  :hook  (dired-mode . dired-collapse-mode))
(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))
(use-package dired-hide-dotfiles
  :after dired
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
			  ("." . dired-hide-dotfiles-mode)))

(use-package consult-dir
  :after consult
  :bind (("C-x C-d" . consult-dir)
		 (:map vertico-map
			   ("C-x C-d" . consult-dir)
			   ("C-x C-j" . consult-dir-jump-file)))
  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (defun consult-dir--tramp-docker-hosts ()
	"Get a list of hosts from docker."
	(when (require 'docker-tramp nil t)
	  (let ((hosts)
			(docker-tramp-use-names t))
		(dolist (cand (docker-tramp--parse-running-containers))
		  (let ((user (unless (string-empty-p (car cand))
						(concat (car cand) "@")))
				(host (car (cdr cand))))
			(push (concat "/docker:" user host ":/") hosts)))
		hosts)))
  (defvar consult-dir--source-tramp-docker
	`(:name     "Docker"
				:narrow   ?d
				:category file
				:face     consult-file
				:history  file-name-history
				:items    ,#'consult-dir--tramp-docker-hosts)
	"Docker candiadate source for `consult-dir'.")
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-docker t))

(use-package org-contrib
  :defer t) ;; install but don't require unless needed.
(use-package org
  :init
  (defun gtd () (interactive) (org-agenda 'nil "g"))
  :bind
  (("C-c a" .  gtd)
		 ("C-c c" . org-capture)
		 (:map org-mode-map
			   ( "C-M-<up>" . org-up-element)))
  :hook
  (org-export-before-processing .
								(lambda (backend)
								  (require 'ox-extra)))
  :custom
  (org-adapt-indentation t)
  (org-auto-align-tags nil)
  (org-edit-src-content-indentation 0)
  (org-edit-timestamp-down-means-later t)
  (org-ellipsis "…")
  (org-fast-tag-selection-single-key 'expert)
  (org-hide-emphasis-markers t)
  (org-image-actual-width nil)
  (org-insert-heading-respect-content t)
  (org-log-done 'time)
  (org-pretty-entities t)
  (org-return-follows-link  t)
  (org-special-ctrl-a/e t)
  (org-src-fontify-natively t)
  (org-catch-invisible-edits 'show-and-error)
  (org-src-tab-acts-natively t)
  (org-startup-folded t)
  (org-startup-with-inline-images t)
  (org-tags-column 0)
  ;; TODO(SN): https://github.com/karthink/org-auctex
  (org-startup-with-latex-preview nil)
  (org-support-shift-select t)
  (org-archive-location "%s_archive::* Archive")
  (org-latex-pdf-process '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -outdir=~/.cache/emacs %f"))
  (org-directory "~/doc")
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-agenda-files
		(cl-remove-if-not #'file-exists-p
						  '("~/doc/inbox.org"
							"~/doc/projects.org"
							"~/doc/gcal.org"
							"~/doc/repeater.org")))
  (org-capture-templates
		`(("t" "Tasks")
		  ("tt" "Todo" entry (file "~/doc/inbox.org")
		   "* TODO %?\n%U\n%a\n" :clock-keep t)
		  ("tt" "Todo" entry (file "~/doc/inbox.org")
		   "* TODO %?\n%U\n%a\n" :clock-keep t)
		  ("tn" "Next" entry (file "~/doc/inbox.org")
		   "* NEXT %?\nSCHEDULED: %t\n%U\n%a\n" :clock-keep t)
		  ("ti" "Inprogress" entry (file "~/doc/inbox.org")
		   "* NEXT %?\nSCHEDULED: %t\n%U\n%a\n" :clock-keep t :clock-in t)
		  ("p" "New Project (clock-in)" entry (file "~/doc/projects.org")
		   "* PROJECT %?\n" :clock-keep t :clock-in t)
		  ("c" "Current task" checkitem (clock))
		  ("r" "Roam")
		  ("rt" "Go to today's daily note" entry (function (lambda ()
															 (org-roam-dailies-goto-today)
															 (org-capture-finalize))))
		  ("rf" "Find or create an Org-roam node" entry (function (lambda ()
																	(org-roam-node-find)
																	(org-capture-finalize))))
		  ("rv" "Open Roam UI in browser" entry (function (lambda ()
															(org-roam-ui-open)
															(org-capture-finalize))))))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((dot . t)
	 (emacs-lisp . t)
	 (gnuplot . t)
	 (latex . t)
	 (python . t)
	 (,(if (locate-library "ob-sh") 'sh 'shell) . t)
	 (sql . t)
	 (sqlite . t))))

(use-package org
 :config
 (defun sn/org-babel-tangle-dont-ask ()
   "Tangle Org file without asking for confirmation."
   (let ((org-confirm-babel-evaluate nil))
	 (org-babel-tangle)))
 :hook
 (org-mode . (lambda ()
			   (add-hook 'after-save-hook #'sn/org-babel-tangle-dont-ask
						 'run-at-end 'only-in-org-mode))))

(use-package org-modern
  :after org
  :config
  (global-org-modern-mode))

(use-package org
  :bind ((:map org-mode-map
			   ("C-c v" . wr-mode)))
  :init
  (define-minor-mode wr-mode
	"Set up a buffer for word editing.
  This enables or modifies a number of settings so that the
  experience of word processing is a little more like that of a
  typical word processor."
	:interactive t " Writing" nil
	(if wr-mode
		(progn
		  (setq truncate-lines nil
				word-wrap t
				cursor-type 'bar)
		  (when (eq major-mode 'org)
			(kill-local-variable 'buffer-face-mode-face))
		  (buffer-face-mode 1)
		  (setq-local
		   blink-cursor-interval 0.8
		   buffer-face-mode-face '((:family "Google Sans" :weight bold ))
		   show-trailing-whitespace nil
		   line-spacing 0.2
		   electric-pair-mode nil)
		  (visual-line-mode 1)
		  (variable-pitch-mode 1))

	  (kill-local-variable 'truncate-lines)
	  (kill-local-variable 'word-wrap)
	  (kill-local-variable 'cursor-type)
	  (kill-local-variable 'blink-cursor-interval)
	  (kill-local-variable 'show-trailing-whitespace)
	  (kill-local-variable 'line-spacing)
	  (kill-local-variable 'electric-pair-mode)
	  (buffer-face-mode -1)
	  (visual-line-mode -1)
	  (variable-pitch-mode -1)))
  :hook (org-mode . wr-mode))

(use-package org-appear
  :after org
  :vc (:url "https://github.com/awth13/org-appear.git"
				  :branch "master" :rev :newest)
  :hook (org-mode . org-appear-mode))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package org-clock
  :ensure nil  ;; built in
  :config
  (org-clock-persistence-insinuate)
  (org-resolve-clocks)
  :init
  (defvar org-clock-prefix-map (make-sparse-keymap)
	"A keymap for handy global access to org helpers, particularly clocking.")
  :bind-keymap ("C-c o" . org-clock-prefix-map)
  :bind (:map org-clock-prefix-map
			  ("j" . org-clock-goto)
			  ("l" . org-clock-in-last)
			  ("i" . org-clock-in)
			  ("o" . org-clock-out))
  :custom
  (org-clock-in-resume t)
  (org-clock-persist t)
  ;; Save clock data and notes in the LOGBOOK drawer
  (org-clock-into-drawer t)
  ;; Save state changes in the LOGBOOK drawer
  (org-log-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (org-clock-out-remove-zero-time-clocks t)
  ;; dont' show clock in bar because we use system bar
  (org-clock-clocked-in-display nil)
  ;; Enable auto clock resolution for finding open clocks
  (org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  ;; Include current clocking task in clock reports
  (org-clock-report-include-clocking-task t)
  ;; use pretty things for the clocktable
  (org-pretty-entities t)
  (org-clock-persist 'history))

(use-package type-break
  :hook ((after-init . type-break-mode)
		 (org-clock-in-prepare . type-break-mode))
  :custom
  (type-break-interval (* 25 60)) ;; 25 mins
  (type-break-good-rest-interval (* 5 60)) ;; 5 mins
  (type-break-good-break-interval (* 5 60)) ;; 5 mins
  (type-break-keystroke-threshold '(nil . 3000)) ;; 500 words is 3,000
  (type-break-demo-boring-stats t)
  (type-break-query-mode t)
  (type-break-warning-repeat nil)
  (type-break-query-function 'sn/type-break-query)
  ;; This will stop the warnings before it's time to take a break
  (type-break-time-warning-intervals '())
  (type-break-keystroke-warning-intervals '())
  (type-break-terse-messages t)
  ;; (type-break-query-function '(lambda (a &rest b) t))
  (type-break-mode-line-message-mode nil)
  (type-break-demo-functions '(type-break-demo-boring))
  :init
  (defun sn/type-break-query ()
	"Ask yes or no, and restart type-break if the answer is no."
	(if (y-or-n-p "Do you want to continue type-break? ")
		t
	  (type-break-mode 1)))
  (defun org-clock-in-to-task-by-title (task-title)
	"Clock into an Org Agenda task by its title within a custom agenda command."
	(interactive "sEnter the title of the task: ")
	(save-window-excursion
	  (org-agenda nil "t")
	  (with-current-buffer "*Org Agenda(t)*"
		(goto-char (point-min))
		(if (search-forward task-title nil t)
			(progn
			  (org-agenda-goto)
			  (org-clock-in))
		  (message "Task with title \"%s\" not found in the custom agenda view." task-title)))))
  (defun format-seconds-to-mm-ss (seconds)
	"Formats time to MM:SS."
	(let* ((minutes (floor (/ seconds 60)))
		   (remaining-seconds (- seconds (* minutes 60))))
	  (format "%02d:%02d" minutes remaining-seconds)))
  (defun type-break-json-data ()
	"Prints type break data used in eww bar."
	(let* ((total-break-time (type-break-time-difference nil type-break-time-next-break))
		   (time-difference (type-break-time-difference nil type-break-time-next-break))
		   (formatted-time (format-seconds-to-mm-ss time-difference))
		   (percent (if type-break-mode
						(number-to-string (/ (* 100.0 time-difference)
											 type-break-interval))
					  "0"))
		   (json-data `(:percent ,percent
								 :time ,formatted-time
								 :task ,(if (string-empty-p org-clock-heading)
											"No Active Task"
										  org-clock-heading)
								 :summary ,(concat (if (or (not org-clock-heading) (string= org-clock-heading ""))
													   "No Active Task"
													 org-clock-heading)
												   " " formatted-time)
								 :keystroke ,(or (cdr type-break-keystroke-threshold) "none")
								 :keystroke-count ,type-break-keystroke-count)))
	  (json-encode json-data))))

(defun toggle-org-pdf-export-on-save ()
  (interactive)
  (if (memq 'org-latex-export-to-pdf after-save-hook)
	  (progn
		(remove-hook 'after-save-hook 'org-latex-export-to-pdf t)
		(message "Disabled org pdf export on save for current buffer..."))
	(add-hook 'after-save-hook 'org-latex-export-to-pdf nil t)
	(message "Enabled org export on save for current buffer...")))

(defun toggle-org-html-export-on-save ()
  (interactive)
  (if (memq 'org-html-export-to-html after-save-hook)
	  (progn
		(remove-hook 'after-save-hook 'org-html-export-to-html t)
		(message "Disabled org html export on save for current buffer..."))
	(add-hook 'after-save-hook 'org-html-export-to-html nil t)
	(message "Enabled org html export on save for current buffer...")))

(use-package org-agenda
  :ensure nil
  :hook (org-agenda-mode . hl-line-mode)
  :custom
  (org-agenda-prefix-format "  %i %?-2 t%s")
  ;; (org-agenda-prefix-format
  ;;  '((agenda . " %i %-12:c%?-12t% s")
  ;;	 (todo . " %i %-12:c")
  ;;	 (tags . " %i %-12:c")
  ;;	 (search . " %i %-12:c")))
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
	 (800 1000 1200 1400 1600 1800 2000)
	 " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")
  (org-agenda-category-icon-alist
   `(
	 ("work" "~/.dotfiles/icons/work.svg" nil nil :ascent center :mask heuristic)
	 ("music" "~/.dotfiles/icons/music.svg" nil nil :ascent center :mask heuristic)
	 ("chore" "~/.dotfiles/icons/chore.svg" nil nil :ascent center :mask heuristic)
	 ("events" "~/.dotfiles/icons/events.svg" nil nil :ascent center :mask heuristic)
	 ("inbox" "~/.dotfiles/icons/inbox.svg" nil nil :ascent center :mask heuristic)
	 ("walk" "~/.dotfiles/icons/walk.svg" nil nil :ascent center :mask heuristic)
	 ("solution" "~/.dotfiles/icons/solution.svg" nil nil :ascent center :mask heuristic)
	 ("community" "~/.dotfiles/icons/community.svg" nil nil :ascent center :mask heuristic)
	 ("idea" "~/.dotfiles/icons/idea.svg" nil nil :ascent center :mask heuristic)
	 ("personal" "~/.dotfiles/icons/man.svg" nil nil :ascent center :mask heuristic)
	 ("scheduled" "~/.dotfiles/icons/scheduled.svg" nil nil :ascent center :mask heuristic)
	 ("class" "~/.dotfiles/icons/class.svg" nil nil :ascent center :mask heuristic)
	 ("plant" "~/.dotfiles/icons/plant.svg" nil nil :ascent center :mask heuristic)
	 ("check" "~/.dotfiles/icons/check.svg" nil nil :ascent center :mask heuristic)
	 ("search" "~/.dotfiles/icons/search.svg" nil nil :ascent center :mask heuristic)
	 ("home" "~/.dotfiles/icons/home.svg" nil nil :ascent center :mask heuristic)
	 ("book" "~/.dotfiles/icons/book.svg" nil nil :ascent center :mask heuristic)
	 ("cook" "~/.dotfiles/icons/cook.svg" nil nil :ascent center :mask heuristic)
	 ("buy" "~/.dotfiles/icons/buy.svg" nil nil :ascent center :mask heuristic)
	 ("shower" "~/.dotfiles/icons/shower.svg" nil nil :ascent center :mask heuristic)
	 ("archive" "~/.dotfiles/icons/archive.svg" nil nil :ascent center :mask heuristic)
	 ))
  :config
  (setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  ;; Set active-project-match
  (let ((active-project-match "-INBOX/PROJECT"))
	(setq org-stuck-projects `(,active-project-match ("NEXT" "INPROGRESS"))
		  org-agenda-compact-blocks t
		  org-agenda-sticky t
		  org-agenda-start-on-weekday nil
		  org-agenda-span 'day
		  org-agenda-include-diary nil
		  org-agenda-use-time-grid nil
		  org-agenda-window-setup 'current-window
		  org-agenda-sorting-strategy
		  '((agenda habit-down time-up user-defined-up effort-up category-keep)
			(todo category-up effort-up)
			(tags category-up effort-up)
			(search category-up)))
	(setq org-agenda-custom-commands
		  `(("g" "GTD"
			 ((agenda "" nil)
			  (tags "INBOX"
					((org-agenda-overriding-header "Inbox")
					 (org-tags-match-list-sublevels nil)
					 (org-agenda-skip-function
					  '(lambda ()
						 (org-agenda-skip-entry-if 'nottodo '("TODO" "DONE" "CANCELLED"))))))
			  (stuck nil
					 ((org-agenda-overriding-header "Stuck Projects")
					  (org-agenda-tags-todo-honor-ignore-options t)
					  (org-tags-match-list-sublevels t)
					  (org-agenda-todo-ignore-scheduled 'future)))
			  (tags-todo "-INBOX"
						 ((org-agenda-overriding-header "Next Actions")
						  (org-agenda-tags-todo-honor-ignore-options t)
						  (org-agenda-todo-ignore-scheduled 'future)
						  (org-agenda-skip-function '(lambda ()
													   (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
														   (org-agenda-skip-entry-if 'nottodo '("NEXT" "INPROGRESS")))))
						  (org-tags-match-list-sublevels t)
						  (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
			  (tags-todo ,active-project-match
						 ((org-agenda-overriding-header "Projects")
						  (org-tags-match-list-sublevels t)
						  (org-agenda-sorting-strategy
						   '(category-keep))))
			  (tags-todo "-INBOX-NEXT-REPEATER"
						 ((org-agenda-overriding-header "Orphaned Tasks")
						  (org-agenda-tags-todo-honor-ignore-options t)
						  (org-agenda-todo-ignore-scheduled 'future)
						  (org-agenda-skip-function
						   '(lambda ()
							  (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
								  (org-agenda-skip-subtree-if 'nottodo '("TODO")))))
						  (org-tags-match-list-sublevels t)
						  (org-agenda-sorting-strategy '(category-keep))))
			  (tags-todo "/WAITING"
						 ((org-agenda-overriding-header "Waiting")
						  (org-agenda-tags-todo-honor-ignore-options t)
						  (org-agenda-todo-ignore-scheduled 'future)
						  (org-agenda-sorting-strategy
						   '(category-keep))))
			  (tags-todo "/DELEGATED"
						 ((org-agenda-overriding-header "Delegated")
						  (org-agenda-tags-todo-honor-ignore-options t)
						  (org-agenda-todo-ignore-scheduled 'future)
						  (org-agenda-sorting-strategy '(category-keep))))
			  (tags-todo "-INBOX"
						 ((org-agenda-overriding-header "On Hold")
						  (org-agenda-skip-function
						   '(lambda ()
							  (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
								  (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
						  (org-tags-match-list-sublevels nil)
						  (org-agenda-sorting-strategy '(category-keep))))))))))

(use-package org
  :hook ((org-clock-in . (lambda () (org-todo "INPROGRESS")
						   (org-save-all-org-buffers)))
		 (org-clock-out . (lambda ()
							;; (unless (string-equal (org-get-todo-state) "DONE"))
							(org-todo "NEXT")
							(setq org-clock-heading "")
							(org-save-all-org-buffers))))
  :custom
  (org-todo-keywords
   (quote ((sequence "TODO(t)" "NEXT(n/!)" "INPROGRESS(i/!)" "|" "DONE(d!/!)")
		   (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
		   (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
   org-todo-repeat-to-state "NEXT")
  (org-todo-keyword-faces
   (quote (("NEXT" :inherit warning)
		   ("PROJECT" :inherit font-lock-string-face)))))

(setq org-refile-use-cache nil)
;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))
(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))
;; Exclude DONE state tasks from refile targets
(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)
(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
	(org-refile goto default-buffer rfloc msg)))
(defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
	(org-agenda-refile goto rfloc no-update)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

(use-package org-gcal
  :defer t
  :requires json
  :config
  (defun load-gcal-credentials ()
	"Load Google Calendar credentials from a JSON file."
	(let* ((json-file "~/.gcal-emacs")
		   (json-data (json-read-file json-file)))
	  (setq plstore-cache-passphrase-for-symmetric-encryption t)
	  (setq org-gcal-client-id (cdr (assoc 'client-id json-data)))
	  (setq org-gcal-client-secret (cdr (assoc 'client-secret json-data)))
	  (setq org-gcal-fetch-file-alist `((,(cdr (assoc 'mail json-data)) .  "~/doc/gcal.org")))))
  (load-gcal-credentials)
  :bind (:map org-agenda-mode-map
			  ("M-g" . org-gcal-sync)))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package pdf-tools
  :ensure nil
  :defer 2
  :hook
  (pdf-tools-enabled . (lambda ()  (pdf-view-midnight-minor-mode 1)
						 (toggle-mode-line)))
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-view-midnight-colors '("#e8e4b1" . "#352718" ))
  :config
  (setopt pdf-continuous-suppress-introduction t)
  (pdf-loader-install))

(use-package pdf-continuous-scroll-mode
  :ensure nil
  :defer 3
  :after pdf-tools
  :vc (:url "https://github.com/dalanicolai/pdf-continuous-scroll-mode.el.git"
			:branch "master" :rev :newest))

(use-package org-roam
  :init (setq org-roam-v2-ack t)
  :config (org-roam-db-autosync-mode)
  :custom
  (org-roam-directory "~/doc/Roam/")
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-dailies-directory "Journal/")
  (setq org-roam-dailies-capture-templates
	  '(("d" "default" entry
		 "* %?"
		 :target (file+head "%<%Y-%m-%d>.org"
							"#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n f"   . org-roam-node-find)
		   ("C-c n d"   . org-roam-dailies-goto-date)
		   ("C-c n n"   . org-roam-buffer-display-dedicated)
		   ("C-c n c"   . org-roam-dailies-capture-today)
		   ("C-c n C" . org-roam-dailies-capture-tomorrow)
		   ("C-c n t"   . org-roam-dailies-goto-today)
		   ("C-c n y"   . org-roam-dailies-goto-yesterday)
		   ("C-c n r"   . org-roam-dailies-goto-tomorrow)
		   ("C-c n G"   . org-roam-graph)
		 :map org-mode-map
		 (("C-c n i" . org-roam-node-insert))))
(use-package consult-org-roam
  :bind ("C-c n g" . org-roam-node-find)
  :after org-roam)

(use-package org-roam-ui
  :vc (:url "https://github.com/org-roam/org-roam-ui.git"
			:branch "main" :rev :newest)
  :after org-roam
  :init
  (set-face-attribute 'default nil :family "Iosevka")
  (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
  :config
  (setq org-roam-ui-sync-theme t
		org-roam-ui-follow t
		org-roam-ui-update-on-save t
		org-roam-ui-open-on-start nil))

(add-hook 'prog-mode-hook 'hl-line-mode) ;; hilight line

(use-package indent-bars
  :defer t
  :hook ((prog-mode conf-mode yaml-mode) . indent-bars-mode)
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth nil)
  (indent-bars-display-on-blank-lines nil)
:vc (:url "https://github.com/jdtsmith/indent-bars.git"
		  :branch "main" :rev :newest))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package treesit-auto
  :init
  (setq treesit-font-lock-level 4)

  (setq major-mode-remap-alist
 '((js-mode . js-ts-mode)
   (sh-mode . bash-ts-mode)))
  :config (global-treesit-auto-mode))

(use-package eglot
  :defer t
  :hook
  ((go-ts-mode rust-ts-mode bash-ts-mode js-ts-mode terraform-mode) . eglot-ensure)
  (eglot-managed-mode . (lambda ()
						  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
						  (add-hook 'before-save-hook #'eglot-organize-imports nil t)
						  (list (cape-super-capf
								 #'codeium-completion-at-point
								 #'eglot-completion-at-point
								 #'yasnippet-capf
								 #'cape-file))))

  :bind (:map eglot-mode-map
			  ;; "C-h ."  eldoc-doc-buffer
			  ("C-c C-c" . project-compile)
			  ("C-c r" . eglot-rename)
			  ("C-c o" . eglot-code-action-organize-imports))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (setq-default eglot-workspace-configuration
				'(:gopls
				  (:usePlaceholders t
									:staticcheck t
									:gofumpt t
									:analyses
									(:nilness t
											  :shadow t
											  :unusedparams t
											  :unusedwrite t
											  :unusedvariable t)
									:hints
									(:assignVariableTypes t
														  :constantValues t
														  :rangeVariableTypes t))))
  (fset #'jsonrpc--log-event #'ignore)
  :init
  (defun eglot-organize-imports () (interactive)
		 (eglot-code-actions nil nil "source.organizeImports" t))
  (defun project-find-go-module (dir)
	(when-let ((root (locate-dominating-file dir "go.mod")))
	  (cons 'go-module root)))
  (cl-defmethod project-root ((project (head go-module)))
	(cdr project))
  (add-hook 'project-find-functions #'project-find-go-module))
(use-package consult-eglot
  :bind(:map eglot-mode-map ("C-c f" . consult-eglot-symbols)))

(use-package git-gutter
  :defer t
  ;; hook eglot so that not enabled in most buffers and lower priority (also don't like it in text documents)
  :hook (eglot-server-initialized . (lambda (server)
									  (run-at-time 1 nil
												   (lambda () (git-gutter-mode)))))
  :custom
  ((git-gutter:ask-p nil)
   (git-gutter:update-interval 2)))
(use-package git-gutter-fringe
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package magit
  :commands (magit-status magit-dispatch project-switch-project)
  :config
  (require 'magit-extras)
  (fullframe magit-status magit-mode-quit-window)
  (setq-default magit-diff-refine-hunk t)
  :bind (("C-x g" . magit-status)
		 ("C-x M-g" . magit-dispatch)
		 (:map magit-status-mode-map
			   ("C-M-<up>" . magit-section-up))))
(use-package forge
  :after magit)
(use-package magit-todos
  :after magit
  :hook(magit-mode . magit-todos-mode))
(use-package blamer
  :bind (("C-c C-i" . blamer-mode)
		 ("C-c i" . blamer-show-posframe-commit-info))
  :custom
  (blamer-idle-time 0.6)
  (blamer-min-offset 70)
  ;; :config  (global-blamer-mode 1) # don't actually want the clutter all the time.
  )
(use-package git-timemachine
  :commands (git-timemachine)
  :bind ("C-c C-g" . git-timemachine)
  :custom (git-timemachine-show-minibuffer-details t))

(use-package browse-at-remote
  :bind (("C-c g g" . browse-at-remote)
		 ("C-c g k" . browse-at-remote-kill)))

(use-package multi-vterm
  :defer t
  :hook
  (vterm-mode . (lambda ()
				  (toggle-mode-line)
				  (setq-local left-margin-width 3
							  right-margin-width 3
							  cursor-type 'bar)
				  (face-remap-add-relative
				   'default
				   :family "Iosevka"
				   :background "#281d12")
				  (face-remap-add-relative
				   'fringe
				   :background "#281d12")))
  :bind
  (("M-t" . toggle-vterm-buffer)
   :map vterm-mode-map
   ("M-t" . toggle-vterm-buffer)
   ("C-M-r" . (lambda ()
                (interactive)
                (setq-local vterm-buffer-name-string nil)
                (rename-buffer (concat "Term " (read-string "Term: ")))))
   ("C-M-t" . vterm-new-tab)
   ("C-M-p" . vterm-new-tab-projcet)
   ("C-M-f" . tab-line-switch-to-next-tab)
   ("C-M-b" . tab-line-switch-to-prev-tab)
   ("C-M-s" . (lambda ()
                (interactive)
                (consult-buffer '(consult--source-vterm))))
   ("M-w" . copy-region-as-kill)
   ("C-y" . vterm-yank))
  :custom
  (vterm-always-compile-module t)
  (vterm-buffer-name "Term")
  (vterm-buffer-name-string "Term %s")
  (vterm-buffer-maximum-size 800)
  (vterm-tramp-shells
   '(("ssh" "/bin/bash")
	 ("docker" "/bin/bash")
	 ("sudo" "/bin/bash")))
  :config
  (add-to-list 'display-buffer-alist `(,vterm-buffer-name
                                       (display-buffer-reuse-window display-buffer-at-bottom)
                                       (dedicated . t)
                                       (reusable-frames . visible)
                                       (window-height . 0.3)))
  :init
  (defun toggle-vterm-buffer ()
    "Toggle the visibility of the vterm buffer or switch to it if not currently selected."
    (interactive)
    (let ((vterm-buffer (seq-find (lambda (buffer)
									(string-prefix-p "Term" (buffer-name buffer)))
								  (buffer-list))))
      (if vterm-buffer
          (if (and (eq (current-buffer) vterm-buffer) ;; if in term buffer, hide buffer
                   (get-buffer-window vterm-buffer))
              (delete-window (get-buffer-window vterm-buffer))
            (if (get-buffer-window vterm-buffer) ;; if not seletected focus it.
                (select-window (get-buffer-window vterm-buffer))
              (progn ;; show veterm buffer
                (display-buffer vterm-buffer)
                (select-window (get-buffer-window vterm-buffer)))))
		  (vterm "Term")) ;; else make new term
	  )) 
   (defun vterm-new-tab ()
	"Create a new tab for the toggled vterm buffers"
	(interactive)
    (vterm "Term"))
   (defun vterm-new-tab-projcet ()
	"Create a new tab for the toggled vterm buffers"
	(interactive)
	(toggle-vterm-buffer) ;; I need to fugure out how to call vterm without createing new buffer.
    (multi-vterm-project)))

(use-package tab-line
  :after  multi-vterm
  :hook (vterm-mode . tab-line-mode)
  :custom
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil)
  (tab-line-separator nil)
  :config
  (defun my/tab-line-tab-name-buffer (buffer &optional _buffers)
	(with-current-buffer buffer
	  ;; TODO color things
	  (format "%s" (buffer-name buffer))))

  (setq tab-line-tab-name-function #'my/tab-line-tab-name-buffer)
  ;; Set face attributes for the tab-line
  (set-face-attribute 'tab-line nil ;; background behind tabs
                      :foreground "#281d12" :background "#281d12")
  (set-face-attribute 'tab-line-tab nil ;; active tab in another window
                      :inherit 'tab-line
                      :background "#a2c080" :foreground "#281d12" :box nil)
  (set-face-attribute 'tab-line-tab-current nil ;; active tab in current window
                      :background "#a2c080" :foreground "#281d12" :box nil)
  (set-face-attribute 'tab-line-tab-inactive nil ;; inactive tab
                      :background "#90918a" :foreground "#281d12" :box nil)
  (set-face-attribute 'tab-line-highlight nil ;; mouseover
                      :background "#79665f" :foreground "#281d12" :box nil)
  (setq tab-line-tabs-function 'tab-line-tabs-mode-buffers))

(setq-default compilation-scroll-output t)
(defvar sanityinc/last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(with-eval-after-load 'compile
  (defun sanityinc/save-compilation-buffer (&rest _)
	"Save the compilation buffer to find it later."
	(setq sanityinc/last-compilation-buffer next-error-last-buffer))
  (advice-add 'compilation-start :after 'sanityinc/save-compilation-buffer)

  (defun sanityinc/find-prev-compilation (orig &optional edit-command)
	"Find the previous compilation buffer, if present, and recompile there."
	(if (and (null edit-command)
			 (not (derived-mode-p 'compilation-mode))
			 sanityinc/last-compilation-buffer
			 (buffer-live-p (get-buffer sanityinc/last-compilation-buffer)))
		(with-current-buffer sanityinc/last-compilation-buffer
		  (funcall orig edit-command))
	  (funcall orig edit-command)))
  (advice-add 'recompile :around 'sanityinc/find-prev-compilation))

(defun sanityinc/shell-command-in-view-mode (start end command &optional output-buffer replace &rest other-args)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless (or output-buffer replace)
	(with-current-buffer "*Shell Command Output*"
	  (view-mode 1))))
(advice-add 'shell-command-on-region :after 'sanityinc/shell-command-in-view-mode)

(with-eval-after-load 'compile
  (defun sanityinc/colourise-compilation-buffer ()
	(when (eq major-mode 'compilation-mode)
	  (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'sanityinc/colourise-compilation-buffer))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 0.2)
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-show-diagnostics-at-end-of-line 'short)
  :config
  (setq elisp-flymake-byte-compile-load-path
		(append elisp-flymake-byte-compile-load-path load-path)))

(use-package go-ts-mode
  :mode "\\.go\\'"
  :ensure-system-package
  ((staticcheck . "go install honnef.co/go/tools/cmd/staticcheck@latest")
   (gofumpt . "go install mvdan.cc/gofumpt@latest")
   (gopls . "go install golang.org/x/tools/gopls@latest"))
  :hook (go-ts-mode . (lambda ()
						(subword-mode 1)
						(setq-local compile-command "go build -v && go test -v -cover && go vet"
									go-ts-mode-indent-offset 4))))
(use-package go-tag
  :ensure-system-package (gomodifytags . "go install github.com/fatih/gomodifytags@latest")
  :bind (:map go-ts-mode-map ("C-c C-t" . go-tag-add)))
(use-package go-fill-struct
  :ensure-system-package (fillstruct . "go install github.com/davidrjenni/reftools/cmd/fillstruct@latest")
  :bind (:map go-ts-mode-map ("C-c C-f" . go-fill-struct)))
(use-package go-impl
  :ensure-system-package (impl . "go install github.com/josharian/impl@latest")
  :bind (:map go-ts-mode-map ("C-c C-i" . go-impl)))
(use-package go-gen-test
  :ensure-system-package (gotests . "go install github.com/cweill/gotests/gotests@latest")
  :bind (:map go-ts-mode-map ("C-c C-g" . go-gen-test-dwim)))

(use-package rust-ts-mode
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook (rust-ts-mode . (lambda ()
						  (setq-local compile-command "cargo run")))
  :config
  (add-to-list 'eglot-server-programs '((rust-ts-mode rust-mode) . ("rustup" "run" "stable" "rust-analyzer"))))

(use-package bash-ts-mode
  :ensure nil
  :mode ("\\.sh\\'" . bash-ts-mode))
(use-package flymake-shellcheck
   :ensure t
   :commands flymake-shellcheck-load
   :init
   (add-hook 'bash-ts-mode-hook 'flymake-shellcheck-load))

(use-package js-ts-mode
  :ensure nil
  :mode ("\\.js\\'" . js-ts-mode))

(use-package toml-ts-mode
  :hook (toml-ts-mode . goto-address-prog-mode))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))
(use-package conda
  :after python
  :commands (conda-env-list conda-env-activate)
  :config
  ;; The location of your anaconda home will be guessed from a list of common
  ;; possibilities, starting with `conda-anaconda-home''s default value (which
  ;; will consult a ANACONDA_HOME envvar, if it exists).
  ;;
  ;; If none of these work for you, `conda-anaconda-home' must be set
  ;; explicitly. Afterwards, run M-x `conda-env-activate' to switch between
  ;; environments
  (or (cl-loop for dir in (list conda-anaconda-home
								"~/.anaconda"
								"~/.miniconda"
								"~/.miniconda3"
								"~/.miniforge3"
								"~/anaconda3"
								"~/miniconda3"
								"~/miniforge3"
								"~/opt/miniconda3"
								"/usr/bin/anaconda3"
								"/usr/local/anaconda3"
								"/usr/local/miniconda3"
								"/usr/local/Caskroom/miniconda/base"
								"~/.conda")
			   if (file-directory-p dir)
			   return (setq conda-anaconda-home (expand-file-name dir)
							conda-env-home-directory (expand-file-name dir)))
	  (message "Cannot find Anaconda installation"))

  ;; integration with term/eshell
  (conda-env-initialize-interactive-shells)

  (add-to-list 'global-mode-string
			   '(conda-env-current-name (" conda:" conda-env-current-name " "))
			   'append))
(use-package jupyter
  :defer t
  :config
  (add-to-list 'org-babel-load-languages '(jupyter . t))
  (setq code-cells-convert-ipynb-style '(
										 ("pandoc" "--to" "ipynb" "--from" "org")
										 ("pandoc" "--to" "org" "--from" "ipynb")
										 org-mode)))

(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . python-mode)
  :config
  (setq csv-separators '("," ";" "|" " " ", ")))

(use-package yaml-ts-mode
  :hook (yaml-ts-mode . goto-address-prog-mode))

(use-package docker
  :ensure-system-package
  ((docker . "paru -S docker")
   (docker-compose . "paru -S docker-compose")
   (devcontainer . "npm install -g @devcontainers/cli"))
  :bind ("C-c d" . docker)
  :config
  (fullframe docker-images tablist-quit)
  (fullframe docker-volumes tablist-quit)
  (fullframe docker-networks tablist-quit)
  (fullframe docker-containers tablist-quit))
(use-package dockerfile-mode
  :mode ("\\.dockerfile\\'" . dockerfile-mode))
(use-package docker-compose-mode
  :mode ("\docker-compose.yml\\'" . docker-compose-mode))

(defvar devcontainer-setup-done nil
  "Flag to track whether the devcontainer setup has been performed.")

(defun devcontainer-setup ()
  "Prompt user to reopen in devcontainer if the current file path doesn't contain '/docker:'."
  (unless devcontainer-setup-done
	(if (y-or-n-p "Reopen in devcontainer? (y/n) ")
		(progn
		  (setq devcontainer-setup-done t)
		  (shell-command "devcontainer up --workspace-folder .")
		  (find-file "/docker:dev-container:/workspace"))
	  (message "Devcontainer setup canceled."))))

(use-package terraform-mode
  :mode ("\\.dockerfile\\'" . dockerfile-mode))

(use-package yuck-mode
  :mode ("\\.yuck\\'" . yuck-mode)
  :hook (yuck-mode . (lambda () (setq-local lisp-indent-offset 2))))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package mu4e
  :ensure nil
  :commands (mu4e)
  ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :init
  (setq smtpmail-smtp-server "smtp.fastmail.com"
	  smtpmail-smtp-service 465
	  smtpmail-stream-type 'ssl)
  :custom
	;; This is set to 't' to avoid mail syncing issues when using mbsync
  (mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (mu4e-update-interval (* 10 60))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-maildir "~/Mail")

  (mu4e-drafts-folder "/[Gmail]/Drafts")
  (mu4e-sent-folder   "/[Gmail]/Sent Mail")
  (mu4e-refile-folder "/[Gmail]/All Mail")
  (mu4e-trash-folder  "/[Gmail]/Trash")

  :config


  (setq mu4e-maildir-shortcuts
	  '(("/Inbox"             . ?i)
		("/[Gmail]/Sent Mail" . ?s)
		("/[Gmail]/Trash"     . ?t)
		("/[Gmail]/Drafts"    . ?d)
		("/[Gmail]/All Mail"  . ?a))))

(use-package whisper
  :vc (:url "https://github.com/natrys/whisper.el"
			:branch "master" :rev :newest)
  :bind ("C-h w" . whisper-run)
  :config
  (setq whisper-install-directory "~/.cache/"
		whisper-model "base"
		whisper-language "en"
		whisper-translate nil))

(use-package gptel
  :bind (("<f5>" . gptel)
		 ("C-<f5>" . gptel-menu))
  :custom
  (gptel-model "gpt-4")
  (gptel-default-mode 'org-mode))

;; we recommend using use-package to organize your init.el
(use-package codeium
  :defer t
  :vc (:url "https://github.com/Exafunction/codeium.el.git"
			:branch "main" :rev :newest)
  ;; :hook (emacs-startup .  (lambda () (run-with-timer 0.1 nil #'codeium-init)))
  :custom
  (codeium-log-buffer nil)
  :config
  (defun my-codeium/document/text ()
	"limiting the string sent to codeium for better performance."
	(buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  (setq codeium/document/text 'my-codeium/document/text))

(use-package cus-dir
  :vc (:url "https://gitlab.com/mauroaranda/cus-dir.git"
			:branch "master" :rev :newest)
  :bind ("C-x p d" . project-customize-dirlocals);; overwrite project-find-dir
  :config
  (defun project-customize-dirlocals ()
	"Customize directory local variables for the current project.
If not in a project, prompt for the project root."
	(interactive)
	(let ((project (project-current t)))
	  (if project
		  (let ((default-directory (project-root project)))
			(customize-dirlocals))))))

(use-package ibrowse
  :bind ("<f8>" . ibrowse-tab-select))

(use-package speed-type
  :defer t
  :commands speed-type-top-x)

(use-package google-this
  :bind ("M-s w" . google-this))

(use-package devdocs
  :defer t)

(use-package gcmh
  :vc (:url "https://github.com/emacsmirror/gcmh.git"
			   :branch "master" :rev :newest)
  :hook (after-init . gcmh-mode))
