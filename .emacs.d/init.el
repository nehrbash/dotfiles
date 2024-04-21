;;; init.el --- Initialization file for Emacs -*- lexical-binding:t -*-
;;; Commentary: Emacs Startup File, initialization for Emacs. DO NOT EDIT, auto tangled from Emacs.org.
;;; Code:

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; Install use-package support
(elpaca elpaca-use-package
		;; use-package enable :ensure keyword.
		(elpaca-use-package-mode)
		(setq use-package-always-ensure t))
(setopt
 warning-minimum-level :emergency
 native-comp-jit-compilation t
 byte-compile-docstring-max-column 120
 native-compile-prune-cache t)

(defun sn/finish-install ()
  (interactive)
  (progn
	(treesit-auto-install-all)
	(all-the-icons-install-fonts t)
	(yas-reload-all)
	(recentf-cleanup)
	(nerd-icons-install-fonts t)))

(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda ()
									(progn
									  (load custom-file 'noerror)
									  (my-ef-themes-mod)
									  (kill-buffer " elpaca--read-file"))))

(use-package ef-themes
  :custom
  (custom-safe-themes t)
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  (ef-themes-headings
   '((0 variable-pitch light 2.1)
	 (1 variable-pitch light 1.6)
	 (t variable-pitch 1.1)
	 (agenda-date 1.9)
	 (agenda-structure variable-pitch light 1.8)
	 (t variable-pitch)))
  :init
  (defun my-ef-themes-mod ()
	"Tweak the style of the ef theme."
	(interactive)
	(mapc #'disable-theme custom-enabled-themes)
	(load-theme 'ef-melissa-dark t)
	(ef-themes-with-colors
	  (custom-set-faces
	   `(window-divider ((,c :background ,bg-main :foreground ,bg-main))) 
	   `(window-divider-first-pixel ((,c :background ,bg-main :foreground ,bg-main)))
       `(window-divider-last-pixel ((,c :background ,bg-main :foreground ,bg-main)))
	   `(blamer-face ((,c :foreground ,fg-alt :italic t))) 
	   `(tab-line ((,c  :foreground  "#281d12" :background "#281d12" :box (:line-width 3 :color ,bg-dim))))
	   `(tab-line-tab ((,c :inherit 'tab-line :background ,fg-alt :foreground "#281d12")))
	   `(treemacs-window-background-face ((,c :background "#281d12")))
	   `(tab-line-tab-current ((,c :background ,fg-alt :foreground "#281d12")))
	   `(tab-line-tab-inactive ((,c :background ,fg-dim :foreground "#281d12")))
	   `(tab-line-highlight ((,c :background ,bg-active :foreground "#281d12")))
	   `(line-number ((,c :inherit (ef-themes-fixed-pitch shadow default) :background "#281d12")))
	   `(tab-line-env-default ((,c :background ,green-faint )))
	   `(tab-line-env-1 ((,c :background ,red-faint )))
	   `(tab-line-env-2 ((,c :background ,yellow-faint )))
	   `(tab-line-env-3 ((,c :background ,blue-faint )))
	   `(scroll-bar ((,c :foreground ,bg-alt :background ,bg-dim)))
	   `(mode-line ((,c :font "Iosevka Aile" :background ,bg-mode-line :foreground ,fg-main  :box (:line-width 3 :color "#281d12"))))
	   `(mode-line-active ((,c :background ,bg-mode-line :foreground ,fg-main  :box (:line-width 3 :color "#281d12"))))
	   `(mode-line-inactive ((,c  :box (:line-width 3 :color ,bg-active))))
	   `(org-document-title ((,c :height 1.8)))
	   `(org-modern-todo ((,c :height 1.2)))
	   `(org-modern-done ((,c :height 1.2)))
	   `(org-modern-tag ((,c :height 1.2)))
	   `(fixed-pitch ((,c :font "Iosevka")))
	   `(variable-pitch ((,c :font "Iosevka")))
	   `(org-modern-symbol ((,c :font "Iosevka")))
	   `(default ((,c :font "Iosevka" :height 115)))
	   `(unspecified-bg ((,c :inherit 'default))))))
  (defun sn/load-my-theme (frame)
	(select-frame frame)
	(when (display-graphic-p frame)
	  (progn
		(message "Loading theme")
		(my-ef-themes-mod)	
		(remove-hook 'after-make-frame-functions 'sn/load-my-theme nil))))
  (if (daemonp)
	  (add-hook 'after-make-frame-functions 'sn/load-my-theme)))

(use-package doom-modeline
  :custom
  (doom-modeline-project-detection 'project)
  (doom-modeline-vcs-max-length 30)
  (doom-modeline-height 32)
  (doom-modeline-lsp nil)
  (doom-modeline-workspace-name nil)
  :config
  (doom-modeline-def-modeline 'simple-line
    '(bar modals buffer-info remote-host)
    '(compilation objed-state persp-name process vcs))
  (defun sn/set-modeline ()
	"Customize doom-modeline."
	(line-number-mode -1)
	(column-number-mode -1)
	(doom-modeline-set-modeline 'simple-line 'default))
  (sn/set-modeline))

(set-display-table-slot standard-display-table 'truncation ?\s) ;; remove the $ on wrap lines.
(global-prettify-symbols-mode t)
(setopt after-delete-frame-functions nil)

(use-package pixel-scroll
  :ensure nil
  :bind
  ([remap scroll-up-command]   . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up)
  :custom
  (pixel-scroll-precision-interpolate-page t)
  (pixel-scroll-precision-use-momentum t)
  :init
  (pixel-scroll-precision-mode 1))

(use-package page-break-lines
  :init (global-page-break-lines-mode))

(use-package default-text-scale
		  :bind (("C-M-=". default-text-scale-increase)
				 ("C-M--" . default-text-scale-decrease)))

(use-package rainbow-delimiters
  :hook ((prog-mode conf-mode) . rainbow-delimiters-mode))

(use-package spacious-padding
  :config (spacious-padding-mode 1)
  :custom
  (spacious-padding-widths
   '( :internal-border-width 15
	  :header-line-width 4
	  :mode-line-width 2
	  :tab-width 4
	  :right-divider-width 30
	  :scroll-bar-width 8)))

(use-package olivetti
  :bind
  ("C-x \"" . olivetti-mode)
  :custom
  (olivetti-style nil))

(setq-default fringe-indicator-alist
              (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist))

(use-package treemacs
  :commands (treemacs)
  :bind
  ("M-SPC t" . treemacs)
  :hook
  (treemacs-mode . (lambda ()
					 (toggle-mode-line)
					 (set-window-fringes (selected-window) 0 0)))
  :custom
  (treemacs-wrap-around nil)
  (treemacs-indentation 1)
  (treemacs-is-never-other-window t)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-resize-icons 18)
  (treemacs-git-mode 'deferred))
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

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
 ring-bell-function 'ignore
 truncate-lines t)
(setopt browse-url-browser-function #'browse-url-firefox
		use-dialog-box nil
		text-mode-ispell-word-completion nil)
(kill-ring-deindent-mode t)
(delete-selection-mode t)
(global-goto-address-mode t)
(transient-mark-mode t)
(global-unset-key (kbd "M-SPC")) ;; my second C-c binding

(use-package recentf
  :ensure nil
  :custom
  (recentf-auto-cleanup 'never) ; Disable automatic cleanup at load time
  (recentf-max-saved-items 50)
  (recentf-exclude '(".*![^!]*!.*"
					 "*/ssh:*"
					 "*/docker:*"
					 "*/sshfs:*"))
  (backup-directory-alist
		`((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
		`((".*" ,temporary-file-directory t)))
  :init
  (recentf-mode 1))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-use-notify nil)
  :init (global-auto-revert-mode 1))

(setopt tramp-default-method "ssh"
		tramp-verbose 0
		tramp-use-ssh-controlmaster-options nil)
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-connection-properties
			 (list (regexp-quote "/ssh:ag-nehrbash:")
				   "remote-shell" "/usr/bin/bash"
				   "direct-async-process" t
				   "tramp-direct-async" t)))

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
  :ensure nil
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
  :ensure nil
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

(setq confirm-kill-processes nil)

(use-package avy
  :commands avy-goto-char-timer
  :bind ("C-M-s" . avy-goto-char-timer))

(use-package lasgun
  :ensure (:host github :repo "aatmunbaxi/lasgun.el")
  :config
  (require 'transient)
  ;; Defines some lasgun actions
  (define-lasgun-action lasgun-action-upcase-word t upcase-word)
  (define-lasgun-action lasgun-action-downcase-word t downcase-word)
  (define-lasgun-action lasgun-action-kill-word nil kill-word)

  (transient-define-prefix lasgun-transient ()
	"Main transient for lasgun."
	[["Marks"
	  ("c" "Char timer" lasgun-mark-char-timer :transient t)
	  ("w" "Word" lasgun-mark-word-0 :transient t)
	  ("l" "Begin of line" lasgun-mark-line :transient t)
	  ("s" "Symbol" lasgun-mark-symbol-1 :transient t)
	  ("w" "Whitespace end" lasgun-mark-whitespace-end :transient t)
	  ("x" "Clear lasgun mark ring" lasgun-clear-lasgun-mark-ring :transient t)
	  ("u" "Undo lasgun mark" lasgun-pop-lasgun-mark :transient t)]
	 ["Actions"
	  ("SPC" "Make cursors" lasgun-make-multiple-cursors)
	  ("." "Embark act all" lasgun-embark-act-all)
	  ("U" "Upcase" lasgun-action-upcase-word)
	  ("l" "Downcase" lasgun-action-downcase-word)
	  ("K" "Kill word" lasgun-action-kill-word)
	  ("q" "Quit" transient-quit-one)]])
  (global-set-key (kbd "M-SPC i") 'lasgun-transient))

(use-package transient
  :defer t
  :bind
  (:map isearch-mode-map
		("C-t" . sn/isearch-menu))
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
  :custom
  (switch-window-shortcut-style 'alphabet)
  (switch-window-timeout 2)
  :bind
  ("C-x o" . switch-window))

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
  ;; :hook (minibuffer-setup . olivetti-mode) ;; fun but glichy 
  :bind
  (:map minibuffer-local-completion-map
  		("<backtab>" . minibuffer-force-complete))
  :custom
  (enable-recursive-minibuffers t)
  (minibuffer-eldef-shorten-default t)
  (read-minibuffer-restore-windows t) ;; don't revert to original layout after cancel.
  (resize-mini-windows nil)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook
  (completion-list-mode . force-truncate-lines)
  (minibuffer-setup . (lambda ()
  						(cursor-intangible-mode 1)))
  :config
  (minibuffer-depth-indicate-mode)
  (minibuffer-electric-default-mode))

(use-package vertico
  :after minibuffer
  :config
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (add-to-list 'vertico-multiform-commands
			   '(project-switch-project buffer))
  (add-to-list 'vertico-multiform-commands
			   '(consult-ripgrep buffer)))
(use-package marginalia
  :init (marginalia-mode)
  :bind
  (:map minibuffer-local-map
		("M-a" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))
(use-package all-the-icons-completion
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

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
  :bind
  ("C-c M-x" . consult-mode-command)
  ("C-c h" . consult-history)
  ("C-c k" . consult-kmacro)
  ("C-c m" . consult-man)
  ("C-c i" . consult-info)
  ([remap Info-search] . consult-info)
  ;; C-x bindings in `ctl-x-map'
  ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
  ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
  ("C-x f" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
  ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
  ;; Custom M-# bindings for fast register access
  ("M-\"" . consult-register)
  ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  ("C-M-'" . consult-register)
  ;; Other custom bindings
  ("M-y" . consult-yank-pop)                ;; orig. yank-pop
  ;; M-g bindings in `goto-map'
  ("M-SPC e" . consult-compile-error)
  ("M-g g" . consult-goto-line)             ;; orig. goto-line
  ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
  ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
  ("M-SPC m" . consult-mark)
  ("M-SPC g" . consult-global-mark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ;; M-s bindings in `search-map'
  ("M-s d" . consult-find)                  ;; Alternative: consult-fd
  ("M-s c" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ;; Isearch integration
  ("M-s e" . consult-isearch-history)
  (:map isearch-mode-map
		("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
		("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
		("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
		("M-s L" . consult-line-multi))            ;; needed by consult-line to detect isearch
  ;; Minibuffer history
  (:map minibuffer-local-map
		("M-s" . consult-history)                 ;; orig. next-matching-history-element
		("M-r" . consult-history))
  :init
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
  (defun consult-term ()
	(interactive)
	(consult-buffer '(consult--source-vterm)))
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
					   "\#\!*")))))
  ;; reorder, mainly to move recent-file down and  org
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
  :bind
  ("M-SPC SPC" . embark-act)
  ("C-;" . embark-dwim)
  ("C-h B" . embark-bindings)
  (:map minibuffer-mode-map
		("M-e" . sn/edit-search-results))
  (:map embark-region-map
		("w" . google-this)
		("g" . gptel))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (defun embark-which-key-indicator ()
	"An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
	(lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
		(which-key--show-keymap
		 (if (eq (plist-get (car targets) :type) 'embark-become)
			 "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
		 (if prefix
			 (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
		 nil nil t (lambda (binding)
					 (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
		'(embark-which-key-indicator
		  embark-highlight-indicator
		  embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
	"Hide the which-key indicator immediately when using the completing-read prompter."
	(which-key--hide-popup-ignore-command)
	(let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
   
  (defun sn/edit-search-results ()
	"Export results using `embark-export' and activate `wgrep'."
	(interactive)
	(progn
	  (run-at-time 0 nil #'embark-export)
	  (run-at-time 0 nil #'wgrep-change-to-wgrep-mode))))

(use-package embark-vc
  :after embark)
(use-package embark-consult
  :after embark
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package project
  :ensure nil
  :bind-keymap ("C-c p". project-prefix-map))

(use-package protogg 
  :ensure (:host github :repo "nehrbash/protogg")
  :custom (protogg-minibuffer-toggle-key "M-g")
  :bind (("M-SPC c" . protogg-compile)
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
  :hook (((prog-mode conf-mode yaml-mode) . sn/corfu-basic))
  :bind (:map corfu-map ("M-SPC" . corfu-insert-separator)
			  ("TAB" . corfu-next)
			  ([tab] . corfu-next)
			  ("S-TAB" . corfu-previous)
			  ([backtab] . corfu-previous))
  :custom
  (tab-always-indent 'complete)
  (corfu-quit-no-match t) ;; 'separator
  (corfu-auto-delay 0.8)
  (corfu-popupinfo-delay 0.2)
  (corfu-auto-prefix 2)
  (completion-cycle-threshold 2)
  (corfu-on-exact-match nil) ;; can't be insert so that snippets don't auto complete
  :config
  (defun sn/corfu-basic ()
	"Setup completion for programming"
	(setq-local corfu-auto t
				eldoc-idle-delay 0.1
				corfu-auto-delay 0
				completion-styles '(orderless-fast basic)
				corfu-popupinfo-delay 0.6))
  ;; TAB cycle if there are only few candidates
  (defun orderless-fast-dispatch (word index total)
	(and (= index 0) (= total 1) (length< word 4)
		 `(orderless-regexp . ,(concat "^" (regexp-quote word)))))
  (orderless-define-completion-style orderless-fast
	"A basic completion suitable for coding."
	(orderless-style-dispatchers '(orderless-fast-dispatch))
	(orderless-matching-styles '(orderless-literal orderless-regexp)))
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package corfu-candidate-overlay
  :hook (text-mode
		 . (lambda ()
			 (setq-local corfu-auto nil)
			 (corfu-candidate-overlay-mode +1))))

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
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(use-package yasnippet
  :hook ((text-mode
	prog-mode
	conf-mode
	snippet-mode) . yas-minor-mode-on)
  :bind ("C-c s" . yas-insert-snippet)
  :custom
  (yas-verbosity 1)
  (yas-snippet-dir "~/.emacs.d/snippets")
  (yas-wrap-around-region t))
(use-package yasnippet-snippets
  :after yasnippet)
(use-package yasnippet-capf
  :defer t) ;; Prefer the name of the snippet instead)

(use-package jinx
  :bind
  (:map jinx-overlay-map
		("C-M-$" . #'jinx-correct-all))
  :init
  (global-jinx-mode)
  :config
  (add-to-list 'vertico-multiform-categories
			   '(jinx grid (vertico-grid-annotate . 30)))
  (defun jinx-save-corrected-word ()
	"Save corrected word to a file."
	(interactive)
	  (let ((current-word (thing-at-point 'word t)))
		(with-temp-buffer
		  (insert current-word)
		  (insert "\n")
		  (append-to-file (point-min) (point-max) (concat user-emacs-directory "jinx_corrections")))))
  (advice-add 'jinx-correct :after #'jinx-save-corrected-word))

(use-package define-word
  :commands (define-word)
  :bind ("M-s D" . define-word-at-point))

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
  :bind
  ("C-x C-d" . consult-dir)
  (:map vertico-map
		("C-x C-d" . consult-dir)
		("C-x C-j" . consult-dir-jump-file))
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
  :ensure nil
  :bind
  ("C-c a" .  gtd)
  ("C-c c" . org-capture)
  (:map org-mode-map
		( "C-M-<up>" . org-up-element)
		("C-c v" . wr-mode))
  :hook
  (org-mode . wr-mode)
  (org-mode . (lambda ()
				(add-hook 'after-save-hook #'sn/org-babel-tangle-dont-ask
						  'run-at-end 'only-in-org-mode)))
  (org-export-before-processing .
								(lambda (backend)
								  (require 'ox-extra)))
  :custom
  (org-todo-keywords
   (quote ((sequence "TODO(t)" "NEXT(n/!)" "INPROGRESS(i/!)" "|" "DONE(d!/!)")
		   (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
		   (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
   org-todo-repeat-to-state "NEXT")
  (org-todo-keyword-faces
   (quote (("NEXT" :inherit warning)
		   ("PROJECT" :inherit font-lock-string-face))))
  (org-adapt-indentation t)
  (org-clock-resolve-expert t)
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
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-startup-folded t)
  (org-startup-with-inline-images t)
  (org-tags-column 0)
  ;; TODO(SN): https://github.com/karthink/org-auctex
  (org-startup-with-latex-preview nil);; wait for the async rendering to be merged
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
	 ("tt" "Todo" entry (file+headline "~/doc/inbox.org" "Inbox")
	  "* TODO %?\nOn %U\While Editing %a\n" :clock-keep t)
	 ("ti" "Inprogress" entry (file+headline "~/doc/inbox.org" "Inprogress")
	  "* INPROGRESS %?\nSCHEDULED: %t\nOn %U\While Editing %a\n" :clock-keep t :clock-in t)
	 ("p" "New Project")
	 ("pp" "Personal Project" entry (file+headline "~/doc/projects.org" "Things I Want Done")
	  "* PROJECT %?\n" :clock-keep t)
	 ("pP" "Personal Project (clock-in)" entry (file+headline "~/doc/projects.org" "Things I Want Done")
	  "* PROJECT %?\n" :clock-keep t :clock-in t)
	 ("pw" "Work Project" entry (file+headline "~/doc/projects.org" "Work")
	  "* PROJECT %?\n" :clock-keep t)
	 ("pW" "Work Project (clock-in)" entry (file+headline "~/doc/projects.org" "Work")
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
  (defun sn/org-babel-tangle-dont-ask ()
	"Tangle Org file without asking for confirmation."
	(let ((org-confirm-babel-evaluate nil))
	  (org-babel-tangle)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((dot . t)
	 (emacs-lisp . t)
	 (gnuplot . t)
	 (latex . t)
	 (python . t)
	 (,(if (locate-library "ob-sh") 'sh 'shell) . t)
	 (sql . t)
	 (sqlite . t)))
  (defun sn/org-clock-in-if-inprogress ()
	"Clock in when the task state is changed to INPROGRESS."
	(when (string= org-state "INPROGRESS")
	  (org-clock-in)))
  (add-hook 'org-after-todo-state-change-hook 'sn/org-clock-in-if-inprogress)
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
		   show-trailing-whitespace nil
		   line-spacing 0.2
		   electric-pair-mode nil)
		  (olivetti-mode 1)
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
	  (olivetti-mode -1)
	  (variable-pitch-mode -1))))

(use-package ob-mermaid
  :after org
  :ensure-system-package (mmdc . "paru -S --needed --noconfirm mermaid-cli")
  :config
  (add-to-list 'org-babel-load-languages '(mermaid . t)))

(defun gtd () (interactive)
		 (progn
		   (org-resolve-clocks)
		   (org-agenda 'nil "g")))
(defun sn/org-capture-frame ()
  "Run org-capture in its own frame."
  (interactive)
  (require 'cl-lib)
  (select-frame-by-name "capture")
  (delete-other-windows)
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
    (condition-case err
        (org-capture)
      ;; "q" signals (error "Abort") in `org-capture'
      ;; delete the newly created frame in this scenario.
      (user-error (when (string= (cadr err) "Abort")
                    (delete-frame))))))
(add-hook 'org-capture-mode-hook 'toggle-mode-line)

(use-package org-modern
  :after org
  :config
  (global-org-modern-mode))

(use-package org-appear
  :ensure (:host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package org-clock
  :ensure nil  ;; built in
  :bind 
  (("C-o" . org-clock-map))
  :config
  (defvar org-clock-map (make-sparse-keymap)
    "Keymap for org-clock commands.")

  (define-key org-clock-map (kbd "j") 'org-clock-goto)
  (define-key org-clock-map (kbd "l") 'org-clock-in-last)
  (define-key org-clock-map (kbd "i") 'org-clock-in)
  (define-key org-clock-map (kbd "o") 'org-clock-out)
  :hook
  (org-clock-in . type-break-mode)
  (org-clock-out . (lambda () (type-break-mode -1)))
  (org-clock-in . (lambda () (org-todo "INPROGRESS")
  					(org-save-all-org-buffers)))
  (org-clock-out . (lambda ()
  					 (unless (string-equal (org-get-todo-state) "DONE")
  					   (org-todo "NEXT")
  					   (setq org-clock-heading "")
  					   (org-save-all-org-buffers))))
  :init
  (org-clock-persistence-insinuate)
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
  :ensure nil
  :custom
  (org-clock-ask-before-exiting nil)
  (type-break-interval (* 25 60)) ;; 25 mins
  (type-break-good-rest-interval (* 5 60)) ;; 5 mins
  (type-break-good-break-interval (* 5 60)) ;; 5 mins
  (type-break-keystroke-threshold '(nil . 3000)) ;; 500 words is 3,000
  (type-break-demo-boring-stats t)
  (type-break-file-name nil) ;; don't save across sessions file is annoying
  (type-break-query-mode t)
  (type-break-warning-repeat nil)
  ;; This will stop the warnings before it's time to take a break
  (type-break-time-warning-intervals '())
  (type-break-keystroke-warning-intervals '())
  (type-break-query-function 'sn/type-break-query)
  (type-break-mode-line-message-mode nil)
  (type-break-demo-functions '(type-break-demo-boring))
  :init
  (defun sn/org-mark-current-done ()
	"Clock out of the current task and mark it as DONE."
	(interactive)
	(let ((org-clock-out-switch-to-state "DONE"))
	  (org-clock-out)
	  (setq org-clock-heading "")
	  (org-save-all-org-buffers)))
  (defun sn/type-break-toggle ()
	(interactive)
	(if type-break-mode
		(type-break-mode -1)
	  (type-break-mode 1)))
  (defun sn/type-break-query (a &rest b)
	"Auto say yes and ask to quit type break."
	(if (>= (type-break-time-difference
								 type-break-interval-start
								 type-break-time-last-break) 0)
		(y-or-n-p "Do you want to continue type-break? ")
	  t))
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
	(let* ((time-difference  (when type-break-mode (type-break-time-difference nil type-break-time-next-break)))
		   (formatted-time (if time-difference (format-seconds-to-mm-ss time-difference)
							 "00:00"))
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
								 :keystroke ,(if type-break-mode (cdr type-break-keystroke-threshold) "none")
								 :keystroke-count ,(if type-break-mode type-break-keystroke-count 0))))
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
  (org-agenda-prefix-format "%i  %?-2 t%s")
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
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
   ("archive" "~/.dotfiles/icons/archive.svg" nil nil :ascent center :mask heuristic)))
  :config
  (setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  ;; Set active-project-match
  (let ((active-project-match "-inbox/PROJECT"))
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
  			(tags "inbox"
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
  			(tags-todo "-inbox"
  					   ((org-agenda-overriding-header "Next Actions")
  						(org-agenda-tags-todo-honor-ignore-options t)
  						(org-agenda-todo-ignore-scheduled 'future)
  						(org-agenda-skip-function
  						 '(lambda ()
  							(or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
  								(org-agenda-skip-entry-if 'nottodo '("NEXT" "INPROGRESS")))))
  						(org-tags-match-list-sublevels t)
  						(org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
  			(tags-todo ,active-project-match
  					   ((org-agenda-overriding-header "Projects")
  						(org-tags-match-list-sublevels t)
  						(org-agenda-sorting-strategy
  						 '(category-keep))))
  			(tags-todo "-inbox-repeater"
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
  			(tags-todo "-inbox"
  					   ((org-agenda-overriding-header "On Hold")
  						(org-agenda-skip-function
  						 '(lambda ()
  							(or (org-agenda-skip-subtree-if 'todo '("WAITING"))
  								(org-agenda-skip-entry-if 'nottodo '("HOLD")))))
  						(org-tags-match-list-sublevels nil)
  						(org-agenda-sorting-strategy '(category-keep))))))))))

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

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package pdf-continuous-scroll-mode
  :after pdf-tools
  :ensure (:host github :repo "dalanicolai/pdf-continuous-scroll-mode.el"))

(use-package org-roam
  :defer t
  :init (setq-default org-roam-v2-ack t)
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
  :ensure (:host github :repo "org-roam/org-roam-ui")
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

(setopt comment-auto-fill-only-comments t)
(add-hook 'prog-mode-hook #'auto-fill-mode)

(use-package indent-bars
  :ensure (:host github :repo "jdtsmith/indent-bars")
  :defer t
  :hook ((prog-mode conf-mode yaml-mode) . indent-bars-mode)
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.3)
  (indent-bars-pad-frac 0.1))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  (treesit-font-lock-level 4)
  (major-mode-remap-alist
   '((js-mode . js-ts-mode)
	 (sh-mode . bash-ts-mode)))
  :config (global-treesit-auto-mode))

(use-package eglot
  :ensure ( :inherit elpaca-menu-gnu-devel-elpa)
  :hook
  ((go-ts-mode rust-ts-mode bash-ts-mode js-ts-mode terraform-mode) . eglot-ensure)
  (eglot-managed-mode . eglot-inlay-hints-mode)
  (eglot-managed-mode . sn/setup-eglot)
  (eglot-managed-mode . sn/eglot-eldoc)
  :preface
  (defun sn/eglot-eldoc ()
    (setq eldoc-documentation-strategy
            'eldoc-documentation-compose-eagerly))
  :bind (:map eglot-mode-map
			  ("C-h ." . eldoc-doc-buffer)
			  ("C-c C-c" . project-compile)
			  ("C-c r" . eglot-rename)
			  ("C-c a" . eglot-code-actions)
			  ("C-c C-o" . eglot-code-action-organize-imports))
  :custom
  (eglot-report-progress nil)
  (eglot-autoshutdown t)
  (eglot-sync-connect nil)
  (eglot-events-buffer-config '(:size 0 :format full))
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
														  :rangeVariableTypes t))
				  :js-ts
                  (:format 
				   (:convertTabsToSpaces t
										 :indentSize 1
										 :tabSize 1
										 :tabWidth 1))))

  :init
  (defun eglot-organize-imports () (interactive)
		 (eglot-code-actions nil nil "source.organizeImports" t))
  (defun sn/setup-eglot ()
	"Eglot customizations"
	(add-hook 'before-save-hook #'eglot-format-buffer -10 t)
	(add-hook 'before-save-hook 'eglot-organize-imports)
	(setq-local completion-at-point-functions
				(list (cape-capf-super
					   #'eglot-completion-at-point
					   #'yasnippet-capf
					   #'cape-file)
					  #'codeium-completion-at-point))))
(use-package consult-eglot
  :after eglot
  :bind
  (:map eglot-mode-map
		("C-c f" . consult-eglot-symbols)))

(use-package dape
  :bind ("<F7>" . dape)
  ;; To use window configuration like gud (gdb-mi)
  :init
  (setq dape-buffer-window-arrangement 'gud)
  :config
  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)
  ;; To not display info and/or buffers on startup
  (remove-hook 'dape-on-start-hooks 'dape-info)
  (remove-hook 'dape-on-start-hooks 'dape-repl)
  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
  ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)
  ;; By default dape uses gdb keybinding prefix
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")
  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-compile-hooks 'kill-buffer)
  (add-hook 'dape-on-start-hooks
            (defun dape--save-on-start ()
              (save-some-buffers t t))))

(use-package git-gutter
  :init
  (global-git-gutter-mode t)
  :custom
  (git-gutter:ask-p nil)
  (git-gutter:update-interval 0.8))
(use-package git-gutter-fringe
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))
(use-package blamer
  :bind
  ("C-c C-i" . blamer-mode)
  ("C-c i" . blamer-show-posframe-commit-info)
  :custom
  (blamer-idle-time 0.6)
  (blamer-min-offset 70))

(use-package magit
  :commands (magit-status magit-dispatch project-switch-project)
  :config
  (fullframe magit-status magit-mode-quit-window)
  (require 'magit-extras)
  :custom
  (magit-diff-refine-hunk t)
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch))
(use-package git-timemachine
  :defer t
  :bind ("C-c C-g" . git-timemachine)
  :custom (git-timemachine-show-minibuffer-details t))

(use-package browse-at-remote
  :bind
  ("C-c g g" . browse-at-remote)
  ("C-c g k" . browse-at-remote-kill))

(use-package svg-tag-mode :ensure t)
(use-package tab-line
  :ensure nil
  :hook (vterm-mode . tab-line-mode)
  :custom
  (tab-line-new-button-show nil)
  (tab-line-close-button-show 'selected)
  :config
  (defface tab-bar-svg-active
  '((t (:foreground "#a1aeb5")))
  "Tab bar face for selected tab.")

(defface tab-bar-svg-inactive
  '((t (:foreground "#a1aeb5")))
  "Tab bar face for inactive tabs.")

(defun eli/tab-bar-svg-padding (width string)
  (let* ((style svg-lib-style-default)
         (margin      (plist-get style :margin))
         (txt-char-width  (window-font-width nil 'fixed-pitch))
         (tag-width (- width (* margin txt-char-width)))
         (padding (- (/ tag-width txt-char-width) (length string))))
    padding))

(defun eli/tab-bar-tab-name-with-svg (tab i)
  (let* ((current-p (eq (car tab) 'current-tab))
         (name (concat (if tab-bar-tab-hints (format "%d " i) "")
                       (alist-get 'name tab)
                       (or (and tab-bar-close-button-show
                                (not (eq tab-bar-close-button-show
                                         (if current-p 'non-selected 'selected)))
                                tab-bar-close-button)
                           "")))
         (padding (plist-get svg-lib-style-default :padding))
         (width)
         (image-scaling-factor 1.0))
    (when tab-bar-auto-width
      (setq width (/ (frame-inner-width)
                     (length (funcall tab-bar-tabs-function))))
      (when tab-bar-auto-width-min
        (setq width (max width (if (window-system)
                                   (nth 0 tab-bar-auto-width-min)
                                 (nth 1 tab-bar-auto-width-min)))))
      (when tab-bar-auto-width-max
        (setq width (min width (if (window-system)
                                   (nth 0 tab-bar-auto-width-max)
                                 (nth 1 tab-bar-auto-width-max)))))
      (setq padding (eli/tab-bar-svg-padding width name)))
    (propertize
     name
     'display
     (svg-tag-make
      name
      :face (if (eq (car tab) 'current-tab) 'tab-bar-svg-active 'tab-bar-svg-inactive)
      :inverse (eq (car tab) 'current-tab) :margin 0 :radius 6 :padding padding
      :height 1.1))))
(setq tab-bar-tab-name-format-function #'eli/tab-bar-tab-name-with-svg)
(defun sn/tab-line-tab-name-buffer (buffer &optional _buffers)
  "how tabs should look"
  (let* ((name (buffer-name buffer))
         (padding (plist-get svg-lib-style-default :padding))
         (width 200)
         (image-scaling-factor 1.5))
    (propertize
     name
     'display
     (svg-tag-make
      name
      :face (if (eq (buffer-name) buffer) 'tab-bar-svg-active 'tab-bar-svg-inactive)
      :inverse (eq (buffer-name) buffer) :margin 0 :radius 6 :padding padding
      :height 1.1))))
(setq tab-line-tab-name-function #'sn/tab-line-tab-name-buffer)

(defun sn/tab-group (buffer)
  "Group buffers by major mode.
  Returns a single group name as a string for buffers with major modes
  flymake-project-diagnostics-mode, compilation-mode, and vterm-mode."
  (with-current-buffer buffer
    (when (or (derived-mode-p 'flymake-project-diagnostics-mode)
			  (derived-mode-p 'compilation-mode)
			  (derived-mode-p 'vterm-mode))
	  "🦥")))
(advice-add 'tab-line-select-tab-buffer :around
            (lambda (orig-fun &rest args)
              (let ((window (selected-window)))
                (progn
				  (set-window-dedicated-p window nil)
                  (apply orig-fun args)
				  (setq multi-vterm-dedicated-window (selected-window))
				  (setq multi-vterm-dedicated-buffer (current-buffer))
				  (setq multi-vterm-dedicated-buffer-name (buffer-name))
                  (set-window-dedicated-p (window) t)
				  ))))

  ;;(setq tab-line-tabs-buffer-group-function #'my-tab-line-buffer-group-by-major-mode)
  ;; (setq tab-line-tab-face-functions 'sn/line-tab-face-env)
  (setq tab-line-tabs-function 'tab-line-tabs-buffer-groups)
  (setq tab-line-tabs-buffer-group-function 'sn/tab-group)
  )

(use-package direnv
 :config
 (direnv-mode))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  (global-ligature-mode t))

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
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-wrap-around t)
  (flymake-no-changes-timeout 3)
  (flymake-fringe-indicator-position 'right-fringe)
  ;; (flymake-show-diagnostics-at-end-of-line 'short)
  :bind
  ("M-g f" . consult-flymake)          
  ("M-SPC p" . flymake-show-project-diagnostics))
(use-package flymake-shellcheck
   :commands flymake-shellcheck-load
   :hook (bash-ts-mode . flymake-shellcheck-load))

(use-package go-ts-mode
  :ensure nil
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
(use-package go-impl
  :ensure-system-package (impl . "go install github.com/josharian/impl@latest")
  :bind (:map go-ts-mode-map ("C-c C-i" . go-impl)))
(use-package go-gen-test
  :ensure-system-package (gotests . "go install github.com/cweill/gotests/gotests@latest")
  :bind (:map go-ts-mode-map ("C-c C-g" . go-gen-test-dwim)))
(use-package gotest
  :bind
  (:map go-ts-mode-map
		("C-c t t" . go-test-current-test)))

(use-package rust-ts-mode
  :ensure nil
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook (rust-ts-mode . (lambda ()
						  (setq-local compile-command "cargo run")))
  :config
  (add-to-list 'eglot-server-programs '((rust-ts-mode rust-mode) . ("rustup" "run" "stable" "rust-analyzer"))))

(use-package bash-ts-mode
  :ensure nil
  :mode ("\\.sh\\'" . bash-ts-mode))

(use-package js-ts-mode
  :ensure nil
  :mode ("\\.js\\'" . js-ts-mode))

(use-package toml-ts-mode
  :ensure nil
  :hook (toml-ts-mode . goto-address-prog-mode))

(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  :config
  (setq csv-separators '("," ";" "|" " " ", ")))

(use-package yaml-ts-mode
  :ensure nil
  :hook (yaml-ts-mode . goto-address-prog-mode))

(use-package docker
  :ensure-system-package
  (docker . "paru -S docker")
  (docker-compose . "paru -S docker-compose")
  (devcontainer . "npm install -g @devcontainers/cli")
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

(use-package terraform-mode)

(use-package yuck-mode
  :mode ("\\.yuck\\'" . yuck-mode)
  :hook (yuck-mode . (lambda () (setq-local lisp-indent-offset 2))))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package web-mode
  :mode ("\\.html$" .  web-mode)
  :hook (web-mode . sn/web-mode-hook)
  :config
  (defun sn/web-mode-hook()
	(web-mode-set-engine "go")))

(defun sn/start-ag-devcontainer ()
  "Start work."
  (interactive)
  ;; Close all buffers associated with files in ~/src/analytics-hub/
  (dolist (buffer (buffer-list))
    (when (and (buffer-file-name buffer)
               (string-prefix-p (expand-file-name "~/src/analytics-hub/") (buffer-file-name buffer)))
      (kill-buffer buffer)))
  ;; Run the command in ~/src/analytics-hub/
  (let ((default-directory (expand-file-name "~/src/analytics-hub/")))
    (let ((result (shell-command "devcontainer up --workspace-folder .")))
      (if (= result 0)
          (message "Command executed successfully.")
        (message "Error: Command failed. Container might not be up."))))
  ;; Open Dired in /docker:dev-container:/workspace/
  (dired "/docker:dev-container:/workspace/")
  (treemacs))

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
  :ensure (:host github :repo "natrys/whisper.el")
  :bind ("M-SPC w" . whisper-run)
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
  :ensure (:host github :repo "Exafunction/codeium.el")
  :custom
  (codeium-log-buffer nil)
  :config
  (advice-add 'codeium-completion-at-point :around #'cape-wrap-buster)
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  (defun my-codeium/document/text ()
	"limiting the string sent to codeium for better performance."
	(buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  (setq codeium/document/text 'my-codeium/document/text))

(use-package cus-dir
  :ensure (:host gitlab :repo "mauroaranda/cus-dir")
  :bind ("C-x p d" . customize-dirlocals-project))

(use-package speed-type
  :defer t)

(use-package google-this
  :bind ("M-s w" . google-this))

(use-package devdocs
  :defer t)

(defun ea-write-clipboard ()
  (let ((file "/tmp/.emacs_everywhere_clipboard"))
    (if (region-active-p)
        (write-region (region-beginning) (region-end) file)
      (write-region (point-min) (point-max) file))
    (kill-buffer "*Emacs Everywhere*")))

(defun ea-commit ()
  (interactive)
  (ea-write-clipboard)
  (delete-frame))

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
  (toggle-mode-line)
  (local-set-key (kbd "M-SPC SPC") 'ea-commit)
  (local-set-key (kbd "M-SPC DEL") 'ea-abort))

(use-package consult-taskfile
  :load-path "~/src/consult-taskfile"
  :bind
  ("M-SPC x" . consult-task)
  ("M-SPC c" . taskfile))

(use-package ement
  :ensure t
  :custom
  (ement-room-prism 'both)
  (ement-save-sessions t))
(use-package burly
  :ensure t)

(use-package gcmh
  :ensure (:host github :repo "emacsmirror/gcmh")
  :hook (after-init . gcmh-mode))
