;;; init.el --- Initialization file for Emacs -*- lexical-binding:t -*-
;;; Commentary: Emacs Startup File, initialization for Emacs. DO NOT EDIT, auto tangled from Emacs.org.
;;; Code:

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil :depth 1 :inherit ignore
			      :files (:defaults "elpaca-test.el" (:exclude "extensions"))
			      :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	(if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
						  ,@(when-let* ((depth (plist-get order :depth)))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(setq elpaca-order-defaults '(:type git :protocol https :inherit t :depth 1))
(cl-pushnew 'transient elpaca-ignored-dependencies)
  (elpaca elpaca-use-package
		;; use-package enable :ensure keyword.
		(elpaca-use-package-mode))
  (setopt
   use-package-always-ensure t
   warning-minimum-level :emergency
   native-comp-jit-compilation t
   native-comp-async-report-warnings-errors 'silent
   byte-compile-docstring-max-column 120
   native-compile-prune-cache t)

(defun run-commands-if-no-lock-file ()
  (let ((lock-file "~/.config/emacs/install_lock"))
    (unless (file-exists-p lock-file)
      (condition-case err
	(nerd-icons-install-fonts t)
	(error (message "Error running nerd-icons-install-fonts: %s" err)))
      (condition-case err
	(yas-reload-all)
	(error (message "Error running yas-reload-all: %s" err)))
      (condition-case err
	(recentf-cleanup)
	(error (message "Error running recentf-cleanup: %s" err)))
      (write-region "" nil lock-file))))

(setq custom-file (expand-file-name "customs.el" user-emacs-directory))

(defun kill-all-internal-buffers ()
  "Kill all buffers whose names start with a space, except *scratch*."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((name (buffer-name buf)))
      (when (and (string-prefix-p " " name)
                 (not (string-equal name "*scratch*")))
        (kill-buffer buf)))))

(defun sn/elpacha-hook ()
  "Settup after elpaca finishes"
  (progn
	(load custom-file 'noerror)
	(kill-all-internal-buffers)))
(add-hook 'elpaca-after-init-hook 'sn/elpacha-hook)

(use-package ef-themes
  :custom
  (custom-safe-themes t)
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  (ef-themes-headings
	'((agenda-date 1)
	   (agenda-structure variable-pitch light 1.8)
	   (t variable-pitch)))
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  (defun sn-rgb-to-hex (r g b)
	"Convert RGB to hex color format."
	(format "#%02x%02x%02x" r g b))

  (defun sn-darken-color (hex-color factor)
	"Darken HEX-COLOR by factor (a float between 0 and 1)."
	(let* ((r (string-to-number (substring hex-color 1 3) 16))
			(g (string-to-number (substring hex-color 3 5) 16))
			(b (string-to-number (substring hex-color 5 7) 16))
			(r-dark (max 0 (floor (* r factor))))
			(g-dark (max 0 (floor (* g factor))))
			(b-dark (max 0 (floor (* b factor)))))
      (sn-rgb-to-hex r-dark g-dark b-dark)))
  (defun sn-ef-themes-mod ()
	"Tweak the style of the ef theme."
	(interactive)
	(kill-all-internal-buffers) ;; kill minibuffer buffers so that vertico posframe color is applied
    (modus-themes-with-colors
	  (let ((darker (sn-darken-color bg-main 0.7)))
		(custom-set-faces
		  `(default ((,c :family "Iosevka" )))
		  `(org-table ((,c :family "Iosevka")))
		  `(org-modern-symbol ((,c :family "Iosevka" )))
		  `(org-block ((,c :family "Iosevka" )))
		  `(variable-pitch ((,c :family "Iosevka Aile")))
		  `(ef-themes-fixed-pitch ((,c :family "Iosevka" )))
		  `(ef-themes-key-binding ((,c :inherit (bold ef-themes-fixed-pitch) :foreground ,yellow-warmer)))
		  `(page-break-lines ((,c :inherit (bold ef-themes-fixed-pitch) :foreground ,yellow-warmer)))
		  `(window-divider ((,c :background ,bg-main :foreground ,bg-main)))
		  `(window-divider-first-pixel ((,c :background ,bg-main :foreground ,bg-main)))
		  `(window-divider-last-pixel ((,c :background ,bg-main :foreground ,bg-main)))
		  `(tab-line ((,c :family "Iosevka Aile" :background ,bg-dim :foreground ,fg-dim :height 110 :box nil)))
		  `(tab-line-tab-group ((,c :inherit 'tab-line)))
		  `(tab-line-tab ((,c :inherit 'tab-line :background unspecified :foreground ,fg-main :box nil)))
		  `(tab-line-tab-current ((,c :inherit 'tab-line-tab :background ,bg-main :box nil)))
		  `(tab-line-tab-inactive ((,c :inherit 'tab-line-tab :background ,bg-dim :foreground ,fg-dim :box nil)))
		  `(tab-line-tab-inactive-alternate ((,c :inherit 'tab-line-tab :background ,bg-dim :foreground ,fg-dim :box nil)))
		  `(tab-line-highlight ((,c :inherit unspecified :background unspecified :foreground unspecified :box nil)))
		  `(svg-tabs-active ((,c :foreground ,fg-main :background ,bg-main)))
		  `(svg-tabs-inactive ((,c :foreground ,fg-dim :background ,bg-dim)))
		  `(line-number ((,c :background ,darker)))
		  `(vertico-posframe ((,c :background ,darker))) ;; does not take affect if hidden buffers are created
		  `(vertico-posframe-border ((,c (:background ,bg-dim))))
		  `(scroll-bar ((,c :foreground ,fg-alt :background ,darker)))
		  `(mode-line ((,c :family "Iosevka Aile"  :foreground ,fg-main  :box (:line-width 3 :color ,darker))))
		  `(mode-line-active ((,c :foreground ,fg-main  :box (:line-width 3 :color ,darker ))))
		  `(mode-line-inactive ((,c :height 120 :box (:line-width 3 :color ,darker))))
		  `(eldoc-box-border ((,c :background ,fg-alt)))
		  `(eldoc-box-body ((,c :family "Iosevka Aile" :background ,darker :height 0.8)))
		  `(breadcrumb-face ((,c :foreground ,fg-alt)))
		  `(breadcrumb-imenu-leaf-face ((,c :foreground ,fg-alt)))))))
  (add-hook 'modus-themes-post-load-hook #'sn-ef-themes-mod)
  (modus-themes-load-theme 'ef-melissa-light)
  ;; this may cause flashes but fixes a lot of issues like 1. scroll bar colors, 2. terminal theme
  (defun sn-apply-theme-to-frame (frame)
	"Reapply theme modifications to newly created FRAME."
	(with-selected-frame frame
	  (modus-themes-load-theme 'ef-melissa-light)))
  (add-hook 'after-make-frame-functions #'sn-apply-theme-to-frame))

(setopt mode-line-compact nil
        mode-line-right-align-edge 'right-margin)

(use-package sn-modeline
  :ensure (sn-modeline :type file :main "~/.config/emacs/lisp/sn-modeline.el")
  :config
  (sn-modeline-mode 1))

(use-package modern-tab-bar
  :ensure (modern-tab-bar :host github :repo "aaronjensen/emacs-modern-tab-bar")
  :init
  (setq tab-bar-show t
        tab-bar-new-button nil
        tab-bar-close-button-show nil)

  (modern-tab-bar-mode))

(use-package breadcrumb
  :ensure (:host github :repo "joaotavora/breadcrumb"))

(set-display-table-slot standard-display-table 'truncation ?\s) ;; remove the $ on wrap lines.
(global-prettify-symbols-mode t)

(use-package pixel-scroll
  :ensure nil
  :bind
  ([remap scroll-up-command]   . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up)
  :custom
  (maximum-scroll-margin 0.25)
  (scroll-preserve-screen-position t)
  (pixel-scroll-precision-interpolate-page t)
  :init
  (pixel-scroll-precision-mode 1))

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package page-break-lines
  :init (global-page-break-lines-mode))

(use-package default-text-scale
		  :bind (("C-M-=". default-text-scale-increase)
				 ("C-M--" . default-text-scale-decrease)))

(use-package rainbow-delimiters
  :hook ((prog-mode conf-mode) . rainbow-delimiters-mode))

    (use-package display-fill-column-indicator
      :ensure nil
      :hook ((prog-mode conf-mode) . display-fill-column-indicator-mode))

(use-package olivetti
  :hook (markdown-mode . olivetti-mode)
  :custom
  (olivetti-minimum-body-width 120)
  (olivetti-style nil))

(setq-default fringe-indicator-alist
	      (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist))

(defun sn/change-window-divider ()
  "Change the window divider to have connected characters."
  (let* ((display-table (or buffer-display-table standard-display-table)))
    (set-display-table-slot display-table 'vertical-border ?│)
	(set-display-table-slot display-table 'box-vertical ?┃)
	(set-display-table-slot display-table 'box-horizontal ?━)
    (set-display-table-slot display-table 'box-down-right ?┏)
    (set-display-table-slot display-table 'box-down-left  ?┓)
    (set-display-table-slot display-table 'box-up-right   ?┗)
    (set-display-table-slot display-table 'box-up-left    ?┛)
    (set-window-display-table (selected-window) display-table)))
(add-hook 'window-configuration-change-hook 'sn/change-window-divider)

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
				       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
				       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
				       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  (global-ligature-mode t))

(setq-default
  fill-column 100
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
  tooltip-delay .3
  ring-bell-function 'ignore
  truncate-lines nil
  word-wrap t)
(setopt
  idle-update-delay 0.1
  use-dialog-box nil
  text-mode-ispell-word-completion nil)

(setq goto-address-url-face 'link
  goto-address-url-mouse-face 'highlight
  goto-address-mail-face 'link
  goto-address-mail-mouse-face 'highlight)
(global-goto-address-mode 1)
(defun browse-url-zen (url &optional _new-window)
  "Open URL in Zen Browser via Flatpak."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (start-process "zen-browser" nil "flatpak" "run" "app.zen_browser.zen" url))
(with-eval-after-load 'browse-url
  (setq browse-url-browser-function #'browse-url-zen))
(global-unset-key (kbd "M-SPC")) ;; my second C-c binding

(defvar my/simple-mouse-menu
  (easy-menu-create-menu
   "Edit"
   '(["Copy"   kill-ring-save :active (use-region-p)]
     ["Paste"  yank           :active t]
     ["Cut"    kill-region    :active (use-region-p)])))

(defun my/popup-simple-edit-menu (event)
  "Popup a simple Copy/Paste menu at mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (popup-menu my/simple-mouse-menu))

;; Bind right-click
(global-set-key [mouse-3] #'my/popup-simple-edit-menu)

(use-package prot-window
  :ensure (:host gitlab
			:repo "protesilaos/dotfiles"
			:files ("emacs/.emacs.d/prot-lisp/prot-window.el"
					 "emacs/.emacs.d/prot-lisp/prot-common.el")
			:main "emacs/.emacs.d/prot-lisp/prot-window.el")
  :config
  (defun hide-modeline-in-buffer (window)
	"Hide the modeline in the buffer displayed in WINDOW."
	(with-current-buffer (window-buffer window)
      (setq-local mode-line-format nil)))
  (setq display-buffer-alist
    `(("\\`\\*Async Shell Command\\*\\'"
	(display-buffer-no-window))
       ("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
	 (display-buffer-no-window)
	 (allow-no-window . t))
       ;; bottom side window
       ("\\*\\Org \\(Select\\|Note\\|Agenda\\)*"
	 (display-buffer-in-side-window)
		 (window-width . fit-window-to-buffer)
	 (side . left)
	 (slot . 0)
	 (window-parameters . ((mode-line-format . none)
								(no-other-window . t))))
	   ((or (derived-mode . dired-mode)
	  (derived-mode . eat-mode))
		 (display-buffer-same-window)
	 (body-function . hide-modeline-in-buffer))
	   ("\\*Embark Collect \\*"
		 (display-buffer-reuse-mode-window display-buffer-at-bottom)
		 (window-parameters (mode-line-format . none)))
       ("\\*Embark Actions\\*"
	 (display-buffer-reuse-mode-window display-buffer-below-selected)
	 (window-height . fit-window-to-buffer)
	 (window-parameters . ((no-other-window . t)
				(mode-line-format . none))))
       ("\\*\\(Output\\|Register Preview\\).*"
	 (display-buffer-reuse-mode-window display-buffer-at-bottom))
       ;; below current window
       ("\\(\\*Capture\\*\\|CAPTURE-.*\\)"
	 (display-buffer-reuse-mode-window display-buffer-below-selected))
       ((derived-mode . reb-mode) ; M-x re-builder
	 (display-buffer-reuse-mode-window display-buffer-below-selected)
	 (window-height . 4) ; note this is literal lines, not relative
	 (dedicated . t)
	 (preserve-size . (t . t)))
       ((or . ((derived-mode . occur-mode)
		(derived-mode . grep-mode)
		(derived-mode . Buffer-menu-mode)
		(derived-mode . log-view-mode)
		(derived-mode . help-mode) ; See the hooks for `visual-line-mode'
		"\\*\\(|Buffer List\\|Occur\\|vc-change-log\\|eldoc.*\\).*"
		"\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
		prot-window-shell-or-term-p
		))
	 (prot-window-display-buffer-below-or-pop)
	 (body-function . prot-window-select-fit-size))
       ("\\*\\(Calendar\\|Bookmark Annotation\\|ert\\).*"
	 (display-buffer-reuse-mode-window display-buffer-below-selected)
	 (dedicated . t)
	 (window-height . fit-window-to-buffer))
       ((or . ((derived-mode . Man-mode)
		(derived-mode . woman-mode)
		"\\*\\(Man\\|woman\\).*"))
	 (display-buffer-same-window)))))

  (use-package recentf
    :ensure nil
	:hook (elpaca-after-init . recentf-mode)
    :custom
    (recentf-auto-cleanup 300)
    (recentf-max-saved-items 100)
    (recentf-exclude
	'(
	   ".*!\\([^!]*!\\).*" ;; matches any string with more than one exclamation mark
	   "/\\.cache.*/.*"    ;; matches any string that includes a directory named .cache
	   "/tmp/.*"           ;; matches any string that includes directory named tmp
	   "/\\.config/emacs/.*"    ;; matches any string that includes directory .config/emacs
	   ))
	:config
	(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))))

(use-package files
  :ensure nil
  :hook (elpaca-after-init . auto-save-visited-mode)
  :custom
  (auto-save-visited-interval 30)
  (remote-file-name-inhibit-auto-save-visited t)
  (auto-save-default nil))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-remote-files t)
  (auto-revert-verbose nil)
  :init (global-auto-revert-mode 1))

(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "ssh")
  (tramp-verbose 0)
  (tramp-use-connection-share nil)
  (tramp-use-ssh-controlmaster-options nil)
  :config
  (add-to-list 'backup-directory-alist
	     (cons tramp-file-name-regexp nil))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-connection-properties
			   (list (regexp-quote "/ssh:ag-nehrbash:")
					 "direct-async-process" t
					 "tramp-direct-async" t))
  (add-to-list 'tramp-connection-properties
			   (list (regexp-quote "/docker:")
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
  (defun sn-isearch-exit-other-end ()
	"Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
	(interactive)
	(isearch-exit)
	(goto-char isearch-other-end))
  (define-key isearch-mode-map [(control return)] 'sn-isearch-exit-other-end)
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
  :custom
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-project-use-cache t)
  :config
  (defun sn/ibuffer-preferred-filters ()
	"hides stare buffers and sorts by project."
	(setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
	(unless (eq ibuffer-sorting-mode 'project-file-relative)
	  (ibuffer-do-sort-by-project-file-relative))
	(setq ibuffer-tmp-hide-regexps '("^\\*.*" "^ .*"))
	(ibuffer-update t))
  :hook (ibuffer . sn/ibuffer-preferred-filters))

(setq ad-redefinition-action 'accept)
(defun sn-newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline))

(global-set-key (kbd "C-<return>") 'sn-newline-at-end-of-line)

(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-width 3)
  :hook ((prog-mode web-mode conf-mode yaml-mode) . display-line-numbers-mode)
  (display-line-numbers-mode . (lambda ()
								 (face-remap-add-relative
								  'fringe :background "#281d12"))))

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

(defun sn-backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
	(cond ((elt ppss 3)
		   (goto-char (elt ppss 8))
		   (sn-backward-up-sexp (1- arg)))
		  ((backward-up-list arg)))))
(global-set-key [remap backward-up-list] 'sn-backward-up-sexp) ; C-M-u, C-M-up

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
  :hook (whitespace-cleanup-mode . sn/show-trailing-whitespace)
  :init
  (global-whitespace-cleanup-mode 1)
  :config
  (setq whitespace-cleanup-mode-ignore-modes
	(append '(markdown-mode org-mode vterm-mode)
	  whitespace-cleanup-mode-ignore-modes))
  (defun sn/show-trailing-whitespace ()
	"Enable display of trailing whitespace in this buffer."
	(setq-local show-trailing-whitespace t)))

(electric-indent-mode t)
(electric-pair-mode t)
(use-package paren ; highight matching paren
  :ensure nil
  :hook (prog-mode . show-paren-mode))

(use-package winner
  :ensure nil
  :init (winner-mode 1)
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

  (defun sn-toggle-delete-other-windows ()
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

  (defun sn-split-window ()
	"Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
	(interactive)
	(if (eq last-command 'sn-split-window)
		(progn
		  (jump-to-register :sn-split-window)
		  (setq this-command 'sn-unsplit-window))
	  (window-configuration-to-register :sn-split-window)
	  (switch-to-buffer-other-window nil)))

  (defun sn-toggle-current-window-dedication ()
	"Toggle whether the current window is dedicated to its current buffer."
	(interactive)
	(let* ((window (selected-window))
		   (was-dedicated (window-dedicated-p window)))
	  (set-window-dedicated-p window (not was-dedicated))
	  (message "Window %sdedicated to %s"
			   (if was-dedicated "no longer " "")
			   (buffer-name)))))

(setq confirm-kill-processes nil)

;; Leader sub-maps — defined eagerly so packages can :bind into them
(defvar my-leader-map   (make-sparse-keymap) "Top-level leader keymap (SPC).")
(defvar my-search-map   (make-sparse-keymap) "SPC s — search.")
(defvar my-file-map     (make-sparse-keymap) "SPC f — file.")
(defvar my-buffer-map   (make-sparse-keymap) "SPC b — buffer/bookmark.")
(defvar my-goto-map     (make-sparse-keymap) "SPC g — goto/navigation.")
(defvar my-project-map  (make-sparse-keymap) "SPC p — project.")
(defvar my-notes-map    (make-sparse-keymap) "SPC n — org-roam/notes.")
(defvar my-org-map      (make-sparse-keymap) "SPC o — org.")
(defvar my-window-map   (make-sparse-keymap) "SPC w — window.")
(defvar my-register-map (make-sparse-keymap) "SPC r — register/yank.")
(defvar my-toggle-map   (make-sparse-keymap) "SPC t — toggles/tools.")
(defvar my-execute-map  (make-sparse-keymap) "SPC x — execute.")
(defvar my-code-map     (make-sparse-keymap) "SPC c — code/LSP.")
(define-key my-leader-map "s" my-search-map)
(define-key my-leader-map "f" my-file-map)
(define-key my-leader-map "b" my-buffer-map)
(define-key my-leader-map "g" my-goto-map)
(define-key my-leader-map "p" my-project-map)
(define-key my-leader-map "n" my-notes-map)
(define-key my-leader-map "o" my-org-map)
(define-key my-leader-map "w" my-window-map)
(define-key my-leader-map "r" my-register-map)
(define-key my-leader-map "t" my-toggle-map)
(define-key my-leader-map "x" my-execute-map)
(define-key my-leader-map "c" my-code-map)

(use-package meow
  :demand t
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
    (add-to-list 'meow-mode-state-list '(agent-shell-mode . insert))
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh)
    (meow-motion-overwrite-define-key
	;; Use e to move up. Since special modes usually use n to move down,
	;; we only overwrite e here.
	'("e" . meow-prev)
	'("<escape>" . ignore))
    (meow-leader-define-key
	'("1" . meow-digit-argument)
	'("2" . meow-digit-argument)
	'("3" . meow-digit-argument)
	'("4" . meow-digit-argument)
	'("5" . meow-digit-argument)
	'("6" . meow-digit-argument)
	'("7" . meow-digit-argument)
	'("8" . meow-digit-argument)
	'("9" . meow-digit-argument)
	'("0" . meow-digit-argument)
	;; Motion-state e escape hatch
	'("e" . "H-e"))
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
	;; '("i" . meow-prev)
	;; '("I" . meow-prev-expand)
	'("f" . meow-find)
	'("g" . meow-cancel-selection)
	'("G" . meow-grab)
	;; '("n" . meow-left)
	;; '("N" . meow-left-expand)
	;; '("o" . meow-right)
	;; '("O" . meow-right-expand)
	'("j" . meow-join)
	'("k" . meow-kill)
	'("l" . meow-line)
	'("L" . meow-goto-line)
	'("m" . meow-mark-word)
	'("M" . meow-mark-symbol)
	    '("e" . avy-goto-char-timer)
	    '("E" . avy-resume)
	'("h" . meow-block)
	'("H" . meow-to-block)
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
  ;; ── Top-level single-key shortcuts ───────────────────────────────
  (define-key my-leader-map " "  #'consult-buffer)           ; SPC SPC — buffer switcher
  (define-key my-leader-map "/"  #'consult-ripgrep)          ; SPC /   — ripgrep
  (define-key my-leader-map "j"  #'meow-comment)             ; SPC j   — comment
  (define-key my-leader-map "a"  #'agent-shell)              ; SPC a   — AI chat
  (define-key my-leader-map "?"  #'meow-cheatsheet)          ; SPC ?   — cheatsheet
  (define-key my-leader-map "i"  #'consult-imenu)            ; SPC i   — imenu
  (define-key my-leader-map "y"  #'consult-yank-pop)         ; SPC y   — yank ring
  ;; ── SPC s — Search ───────────────────────────────────────────────
  (define-key my-search-map "s"  #'consult-line)             ; SPC s s — line search
  (define-key my-search-map "S"  #'consult-line-multi)       ; SPC s S — multi-buffer
  (define-key my-search-map "r"  #'consult-ripgrep)          ; SPC s r — ripgrep
  (define-key my-search-map "g"  #'consult-grep)             ; SPC s g — grep
  (define-key my-search-map "G"  #'consult-git-grep)         ; SPC s G — git grep
  (define-key my-search-map "d"  #'consult-find)             ; SPC s d — find by name
  (define-key my-search-map "c"  #'consult-locate)           ; SPC s c — locate
  (define-key my-search-map "k"  #'consult-keep-lines)       ; SPC s k — keep lines
  (define-key my-search-map "u"  #'consult-focus-lines)      ; SPC s u — focus lines
  (define-key my-search-map "e"  #'consult-isearch-history)  ; SPC s e — isearch history
  (define-key my-search-map "w"  #'google-this)              ; SPC s w — web search
  (define-key my-search-map "D"  #'define-word-at-point)     ; SPC s D — define word
  ;; ── SPC f — File ─────────────────────────────────────────────────
  (define-key my-file-map "f"    #'find-file)                ; SPC f f — find file
  (define-key my-file-map "."    #'find-file-at-point)       ; SPC f . — open at point
  (define-key my-file-map "r"    #'consult-recent-file)      ; SPC f r — recent files
  (define-key my-file-map "d"    #'consult-dir)              ; SPC f d — consult-dir
  (define-key my-file-map "s"    #'yas-insert-snippet)       ; SPC f s — snippet
  ;; ── SPC b — Buffer / Bookmark ────────────────────────────────────
  (define-key my-buffer-map "b"  #'consult-buffer)           ; SPC b b — buffer list
  (define-key my-buffer-map "o"  #'consult-buffer-other-window) ; SPC b o — other window
  (define-key my-buffer-map "O"  #'consult-buffer-other-frame)  ; SPC b O — other frame
  (define-key my-buffer-map "t"  #'consult-buffer-other-tab)    ; SPC b t — other tab
  (define-key my-buffer-map "m"  #'consult-bookmark)         ; SPC b m — bookmarks
  (define-key my-buffer-map "i"  #'ibuffer)                  ; SPC b i — ibuffer
  ;; ── SPC g — Goto / Navigation ────────────────────────────────────
  (define-key my-goto-map "g"    #'consult-goto-line)        ; SPC g g — goto line
  (define-key my-goto-map "o"    #'consult-outline)          ; SPC g o — outline
  (define-key my-goto-map "i"    #'consult-imenu)            ; SPC g i — imenu
  (define-key my-goto-map "I"    #'consult-imenu-multi)      ; SPC g I — imenu multi
  (define-key my-goto-map "m"    #'consult-mark)             ; SPC g m — marks
  (define-key my-goto-map "M"    #'consult-global-mark)      ; SPC g M — global marks
  (define-key my-goto-map "e"    #'consult-compile-error)    ; SPC g e — compile errors
  (define-key my-goto-map "f"    #'consult-flymake)          ; SPC g f — flymake
  (define-key my-goto-map "d"    #'consult-xref-stack-backward) ; SPC g d — xref back
  ;; ── SPC p — Project ──────────────────────────────────────────────
  (define-key my-project-map "p" project-prefix-map)         ; SPC p p — project map
  (define-key my-project-map "d" #'flymake-show-project-diagnostics) ; SPC p d — diag
  (define-key my-project-map "m" #'sn/project-menu)          ; SPC p m — project menu
  ;; ── SPC n — Notes (org-roam) ─────────────────────────────────────
  (define-key my-notes-map "f"   #'org-roam-node-find)       ; SPC n f — find node
  (define-key my-notes-map "i"   #'org-roam-node-insert)     ; SPC n i — insert node
  (define-key my-notes-map "n"   #'org-roam-buffer-display-dedicated) ; SPC n n
  (define-key my-notes-map "g"   #'org-roam-graph)           ; SPC n g — graph
  (define-key my-notes-map "d"   #'org-roam-dailies-goto-date)       ; SPC n d — date
  (define-key my-notes-map "t"   #'org-roam-dailies-goto-today)      ; SPC n t — today
  (define-key my-notes-map "y"   #'org-roam-dailies-goto-yesterday)  ; SPC n y — yesterday
  (define-key my-notes-map "r"   #'org-roam-dailies-goto-tomorrow)   ; SPC n r — tomorrow
  (define-key my-notes-map "c"   #'org-roam-dailies-capture-today)   ; SPC n c — capture
  (define-key my-notes-map "C"   #'org-roam-dailies-capture-tomorrow) ; SPC n C
  ;; ── SPC o — Org ──────────────────────────────────────────────────
  (define-key my-org-map "a"     #'gtd)                      ; SPC o a — agenda
  (define-key my-org-map "c"     #'org-capture)              ; SPC o c — capture
  (define-key my-org-map "j"     #'org-clock-goto)           ; SPC o j — clock goto
  (define-key my-org-map "i"     #'consult-clock-in)         ; SPC o i — clock in
  (define-key my-org-map "o"     #'org-clock-out)            ; SPC o o — clock out
  (define-key my-org-map "l"     #'org-clock-in-last)        ; SPC o l — clock in last
  ;; ── SPC w — Window ───────────────────────────────────────────────
  (define-key my-window-map "w"  #'ace-window)               ; SPC w w — ace-window
  (define-key my-window-map "o"  #'switch-window)            ; SPC w o — switch-window
  (define-key my-window-map "2"  #'split-window-func-with-other-buffer-vertically)
  (define-key my-window-map "3"  #'split-window-func-with-other-buffer-horizontally)
  (define-key my-window-map "1"  #'sn-toggle-delete-other-windows)
  (define-key my-window-map "|"  #'split-window-horizontally-instead)
  (define-key my-window-map "_"  #'split-window-vertically-instead)
  (define-key my-window-map "d"  #'sn-toggle-current-window-dedication)
  (define-key my-window-map "s"  #'sn-split-window)
  ;; ── SPC r — Register / Yank ──────────────────────────────────────
  (define-key my-register-map "r" #'consult-register)        ; SPC r r — registers
  (define-key my-register-map "s" #'consult-register-store)  ; SPC r s — store
  (define-key my-register-map "y" #'consult-yank-pop)        ; SPC r y — yank pop
  (define-key my-register-map "m" #'consult-kmacro)          ; SPC r m — kmacro
  ;; ── SPC t — Toggle / Tools ───────────────────────────────────────
  (define-key my-toggle-map "d"  #'docker)                   ; SPC t d — docker
  (define-key my-toggle-map "w"  #'whisper-run)              ; SPC t w — whisper
  (define-key my-toggle-map "f"  #'treesit-fold-toggle)      ; SPC t f — fold
  (define-key my-toggle-map "p"  #'flymake-show-project-diagnostics)
  (define-key my-toggle-map "P"  #'toggle-flymake-show-diagnostics-at-end-of-line)
  (define-key my-toggle-map "s"  #'gptel-toggle-sidebar)     ; SPC t s — AI sidebar
  (define-key my-toggle-map "e"  #'er/expand-region)         ; SPC t e — expand region
  (define-key my-toggle-map "m"  #'lasgun-transient)         ; SPC t m — multi-cursor
  ;; ── SPC x — Execute ──────────────────────────────────────────────
  (define-key my-execute-map "x" #'consult-taskfile)         ; SPC x x — taskfile
  (define-key my-execute-map "c" #'taskfile)                 ; SPC x c — taskfile menu
  (define-key my-execute-map "m" #'consult-mode-command)     ; SPC x m — mode command
  (define-key my-execute-map "M" #'consult-man)              ; SPC x M — man pages
  (define-key my-execute-map "i" #'consult-info)             ; SPC x i — info
  (define-key my-execute-map "h" #'consult-history)          ; SPC x h — history
  (define-key my-execute-map "e" #'gptel-menu)               ; SPC x e — gptel menu
  ;; ── SPC c — Code (LSP) ───────────────────────────────────────────
  (define-key my-code-map "f"    #'consult-eglot-symbols)    ; SPC c f — symbols
  (define-key my-code-map "r"    #'eglot-rename)             ; SPC c r — rename
  (define-key my-code-map "a"    #'eglot-code-actions)       ; SPC c a — code actions
  (define-key my-code-map "q"    #'eglot-code-action-quickfix)       ; SPC c q — quickfix
  (define-key my-code-map "i"    #'eglot-find-implementation)        ; SPC c i — impl
  (define-key my-code-map "t"    #'eglot-find-typeDefinition)        ; SPC c t — type def
  (define-key my-code-map "o"    #'eglot-code-action-organize-imports) ; SPC c o
  (meow-global-mode 1)
  ;; Bind SPC directly — meow-*-define-key rejects raw keymaps (not commandp)
  (define-key meow-normal-state-keymap (kbd "SPC") my-leader-map)
  (define-key meow-motion-state-keymap (kbd "SPC") my-leader-map))

(use-package meow-tree-sitter
  :after meow
  :config (meow-tree-sitter-register-defaults))

(use-package avy
  :custom
  (avy-timeout-seconds 0.5)
  (avy-keys '(?n ?e ?i ?o ?h ?t ?s ?r ?a ?d))
  (avy-dispatch-alist '((?b . avy-embark-act)
						 (?y . avy-action-yank)
						 (?Y . avy-action-yank-whole-line)
						 (?w . avy-action-copy)
						 (?W . avy-action-yank-whole-line)
						 (?v . avy-action-teleport)
						 (?V . avy-action-teleport-whole-line)
						 (?x . avy-action-kill-move)
						 (?X . avy-action-kill-stay)
						 (?m . avy-action-mark)
						 (?z . avy-acton-zap-to-char)))
  :config
  (defun avy-action-copy-whole-line (pt)
	(save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
	(bounds-of-thing-at-point 'line)
		(copy-region-as-kill start end)))
	(select-window
	  (cdr (ring-ref avy-ring 0))) t)
  (defun avy-action-yank-whole-line (pt)
	"Quick copy line."
	(avy-action-copy-whole-line pt)
	(save-excursion (yank)) t)
  (defun avy-action-teleport-whole-line (pt)
	"Quick copy line to current point."
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)
  (defun avy-embark-act (pt)
	"Use Embark to act on the item at PT."
	(unwind-protect
	  (save-excursion
	(goto-char pt)
	(embark-act))
      (select-window
		(cdr (ring-ref avy-ring 0))) t))
  (setf
	(alist-get ?y avy-dispatch-alist) 'avy-embark-act
	(alist-get ?y avy-dispatch-alist) 'avy-action-yank
	(alist-get ?w avy-dispatch-alist) 'avy-action-copy
	(alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
	(alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
	(alist-get ?t avy-dispatch-alist) 'avy-action-teleport
	(alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line))

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
	[["Single Marks"
	  ("c" "Char timer" lasgun-mark-char-timer :transient t)
	  ("w" "Word" lasgun-mark-word-0 :transient t)
	  ("l" "Begin of line" lasgun-mark-line :transient t)
	  ("s" "Symbol" lasgun-mark-symbol-1 :transient t)
	  ("w" "Whitespace end" lasgun-mark-whitespace-end :transient t)
	  ("x" "Clear lasgun mark ring" lasgun-clear-lasgun-mark-ring :transient t)
	  ("u" "Undo lasgun mark" lasgun-pop-lasgun-mark :transient t)]
	 ["Single Mark Actions"
	  ("SPC" "Make cursors" lasgun-make-multiple-cursors)
	  ("." "Embark act all" lasgun-embark-act-all)
	  ("U" "Upcase" lasgun-action-upcase-word)
	  ("l" "Downcase" lasgun-action-downcase-word)
	  ("K" "Kill word" lasgun-action-kill-word)
	  ("q" "Quit" transient-quit-one)]])
  (global-set-key (kbd "M-SPC i") 'lasgun-transient))

(use-package transient
  :bind
  (:map isearch-mode-map
	("C-t" . sn/isearch-menu))
  :config
  (transient-define-prefix sn/project-menu ()
	"Project Actions"
	[["Commpile"
	   ("c" "Compile" protogg-compile)
	   ("r" "Recompile" recompile)
	   ("m" "Makefile" makefile-runner)]
	  ["Git"
		("s" "Status" magit-status)
		("d" "Dispatch" magit-dispatch)
		("R" "Rebase menu" sn/smerge)
		("g" "Time Machine" git-timemachine)]]
	[["Misc"
	   ("q" "Quit" transient-quit-one)
	   ("p" "Switch Project" project-switch-project)]])
  (transient-define-prefix sn/isearch-menu ()
	"isearch Menu"
	[["Edit Search String"
	  ("e" "Edit the search string (recursive)"
	   isearch-edit-string :transient nil)
	  ("w" "Pull next word or character word from buffer"
	   isearch-yank-word-or-char :transient nil)
	  ("s" "Pull next symbol or character from buffer"
	   isearch-yank-symbol-or-char :transient nil)
	  ("l" "Pull rest of line from buffer"
	   isearch-yank-line :transient nil)
	  ("y" "Pull string from kill ring"
	   isearch-yank-kill :transient nil)
	  ("t" "Pull thing from buffer"
	   isearch-forward-thing-at-point :transient nil)]
	 ["Replace"
	  ("q" "Start ‘query-replace’"
	   anzu-isearch-query-replace :if-nil buffer-read-only :transient nil)
	  ("x" "Start ‘query-replace-regexp’"
		anzu-isearch-query-replace-regexp :if-nil buffer-read-only :transient nil)]]
	[["Toggle"
	  ("X" "Toggle regexp searching"
	   isearch-toggle-regexp :transient nil)
	  ("S" "Toggle symbol searching"
	   isearch-toggle-symbol :transient nil)
	  ("W" "Toggle word searching"
	   isearch-toggle-word :transient nil)
	  ("F" "Toggle case fold"
	   isearch-toggle-case-fold :transient nil)
	  ("L" "Toggle lax whitespace"
	   isearch-toggle-lax-whitespace :transient nil)]
	 ["Misc"
	  ("l" "Start ‘consult-line’"
	   consult-line :transient nil)
	  ("g" "Start ‘consult-git-grep’"
	   consult-git-grep :transient nil)
	  ("r" "Start ‘consult-ripgrep’"
	   consult-ripgrep :transient nil)
	  ("o" "occur"
	   isearch-occur :transient nil)]]))

(transient-mark-mode t)
(delete-selection-mode t)
(defun sn/add-mark-before (func &rest args)
  "Add a mark before calling FUNC with ARGS."
  (push-mark (point) t nil)
  (apply func args))

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

(use-package ace-window
  :custom
  (aw-keys '(?a ?r ?s ?d ?t ?n ?e ?i ?o))
  (aw-ignore-current t))

 (use-package windswap
   :config
   (windmove-default-keybindings 'control)
   (windswap-default-keybindings 'shift 'control))

 (use-package sudo-edit
   :commands (sudo-edit))

(defun revert-all-buffers-no-confirm ()
  "Revert all buffers without confirmation."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (buffer-modified-p))
	(revert-buffer t t t)))))

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))
(with-suppressed-message (save-buffer))

(use-package which-key
  :ensure nil
  :hook (elpaca-after-init . which-key-mode))

(use-package minibuffer
  :ensure nil
  :bind
  (:map minibuffer-local-map ("M-." . sn/minibuffer-fetch-symbol-at-point))
  (:map minibuffer-local-completion-map
	("<backtab>" . minibuffer-force-complete))
  :custom
  (enable-recursive-minibuffers t)
    ;; don't revert to original layout after cancel. doesn't alway work but helps
  (read-minibuffer-restore-windows nil)
  (resize-mini-windows t)
  (resize-mini-frames t)
  (minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  :hook (minibuffer-setup . cursor-intangible-mode)
  :config
  (minibuffer-electric-default-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (defun sn/minibuffer-fetch-symbol-at-point ()
	"Fetch the current or next symbol at point in the current buffer while in minibuffer."
	(interactive)
	(let ((symbol (with-minibuffer-selected-window
					(or (thing-at-point 'symbol)
		      (save-excursion
			(forward-symbol 1)
			(thing-at-point 'symbol))))))
      (when symbol
		(insert symbol)))))

(use-package vertico
  :bind
  (:map vertico-map
	("M-j" . vertico-quick-insert)
	("C-q" . vertico-quick-exit))
  :init
  (vertico-mode 1)
  (vertico-posframe-mode 1)
  (vertico-multiform-mode 1)
  :config
  (setq
	vertico-multiform-commands
    '((consult-imenu buffer indexed)
	   (corfu-move-to-minibuffer reverse indexed (:not posframe))
	   (consult-line reverse (:not posframe))
	   (consult-ripgrep reverse (:not posframe))
	   (jinx-correct-nearest posframe (vertico-posframe-poshandler . posframe-poshandler-point-top-left-corner))
	   (jinx-correct posframe (vertico-posframe-poshandler . posframe-poshandler-point-top-left-corner))
	   (project-switch-project posframe (vertico-posframe-poshandler . posframe-poshandler-frame-top-center))
       (t posframe))
	vertico-multiform-categories
    '((file grid)
       (consult-grep reverse))))

(use-package marginalia
  :bind (:map minibuffer-local-map
		  ("M-a" . marginalia-cycle))
  :custom (marginalia-align 'right)
  :hook (elpaca-after-init . marginalia-mode))

(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package vertico-posframe
  :custom
  (vertico-posframe-width 120)
  (vertico-posframe-vertico-multiform-key "M-m")
  :config
  ;; don't change colors
  (defun sn-vertico-posframe-get-border-color-advice (&rest _args)
	"Always return the color of `vertico-posframe-border`."
	(face-attribute 'vertico-posframe-border
	  :background nil t))
  (advice-add 'vertico-posframe--get-border-color :override #'sn-vertico-posframe-get-border-color-advice)
  (defun sn/posframe-poshandler-window-or-frame-center (info)
  "Position handler that centers the posframe in the window if the window width is at least 120 columns.
Otherwise, it centers the posframe in the frame."
  (let* ((window-left (plist-get info :parent-window-left))
	 (window-top (plist-get info :parent-window-top))
	 (window-width (plist-get info :parent-window-width))
	 (window-height (plist-get info :parent-window-height))
	 (posframe-width (plist-get info :posframe-width))
	 (posframe-height (plist-get info :posframe-height))
	 (frame-width (plist-get info :parent-frame-width))
	 (frame-height (plist-get info :parent-frame-height)))
    (if (>= window-width posframe-width)
	;; Center in window
	(cons (max 0 (+ window-left (/ (- window-width posframe-width) 2)))
	      (max 0 (+ window-top (/ (- window-height posframe-height) 2))))
      ;; Center in frame
      (cons (/ (- frame-width posframe-width) 2)
	    (/ (- frame-height posframe-height) 2)))))
  (setq vertico-posframe-poshandler #'sn/posframe-poshandler-window-or-frame-center))

(use-package hotfuzz
  :after orderless)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles orderless basic))
     (buffer (styles orderless basic))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (completion-lazy-highlight t)
  (orderless-matching-styles '(orderless-regexp orderless-literal))
  (orderless-component-separator #'orderless-escapable-split-on-space)

  :config
  (defun my/orderless-fuzzy-dispatcher (pattern _index _total &rest metadata)
    "Return `orderless-flex' for file and buffer completions only."
    (when (and (stringp pattern)
               (not (string-empty-p pattern))
               (memq (plist-get metadata :category) '(file buffer)))
      'orderless-flex))

  (setq orderless-style-dispatchers
        '(my/orderless-fuzzy-dispatcher)))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-enable-key "r"))

(use-package consult
  :after vertico
  :bind
  ([remap Info-search] . consult-info)
  (:map isearch-mode-map
	("M-e" . consult-isearch-history)
	("M-s e" . consult-isearch-history)
	("M-s l" . consult-line)
	("M-s L" . consult-line-multi))
  (:map minibuffer-local-map
	("M-s" . consult-history)
	("M-r" . consult-history))
  :init
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Example of advising consult-line
  (advice-add #'consult-line :around #'sn/add-mark-before)  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref xref-show-definitions-function #'consult-xref)
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)
  :custom
  (consult-narrow-key "<")
  (consult-preview-key '("M-," :debounce 0 any))
  :config
  (setq consult-preview-allowed-hooks
		(append consult-preview-allowed-hooks
				'(hl-todo-mode elide-head-mode wr-mode
				  global-org-modern-mode global-hl-todo-mode)))
  (setq consult-buffer-filter
		(append consult-buffer-filter
				'("^\\*" "^magit*" "Compile" "[\\.]org$" "shell*")))

  (defun vc-modified-files ()
	"Return list of modified files in the current VC repository."
	(when-let* ((default-directory (vc-root-dir)))
	  (let* ((git-cmd "git status --porcelain=v1 --untracked-files=no")
			  (files (split-string (shell-command-to-string git-cmd) "\n" t)))
		(mapcar (lambda (line)
				  (string-trim (substring line 3)))
		  files))))

  (defun vc-modified-file ()
	"Use completion to go to a modified file in the Git repository."
	(interactive)
	(let* ((default-directory (vc-root-dir))
			(modified-files (vc-modified-files))
			(selected-file (completing-read "Goto vc file: " modified-files nil t)))
	  (when selected-file
		(find-file selected-file))))
  (defun vc-consult-enabled-p ()
	"Check if consult VC source should be enabled."
	(vc-root-dir))

  (defun vc-consult-get-modified-items ()
  "Get modified file items for consult VC source."
  (when-let* ((root (expand-file-name (vc-root-dir))))  ; Expand the tilde!
    (let ((len (length root))
          (ht (consult--buffer-file-hash))
          (items nil))
      (dolist (relative-file (vc-modified-files))
        (let* (file-name-handler-alist
               (file (expand-file-name relative-file root)))
          (when (and (not (gethash file ht)) (string-prefix-p root file))
            (let ((part (substring file len)))
              (when (equal part "") (setq part "./"))
              (put-text-property 0 1 'multi-category `(file . ,file) part)
              (push part items)))))
      items)))

  (defvar consult-source-vc-modified-file
	(list
	  :name     "VC Modified File"
	  :narrow   ?g
	  :category 'file
	  :face     'consult-file
	  :history  'file-name-history
	  :state    #'consult--file-state
	  :enabled  #'vc-consult-enabled-p
	  :items    #'vc-consult-get-modified-items)
	"VC modified file candidate source for `consult-buffer'.")

  (defvar consult-source-org
	(list :name     "Org"
	  :category 'buffer
	  :narrow   ?o
	  :face     'consult-buffer
	  :history  'buffer-name-history
	  :state    #'consult--buffer-state
	  :new
	  (lambda (name)
		(with-current-buffer (get-buffer-create name)
		  (insert "#+title: " name "\n\n")
		  (org-mode)
		  (consult-buffer-action (current-buffer))))
	  :items
	  (lambda ()
		(mapcar #'buffer-name
		  (seq-filter
			(lambda (x)
			  (eq (buffer-local-value 'major-mode x) 'org-mode))
			(buffer-list))))))
  (defvar consult-source-vterm
	(list :name     "Term"
	  :category 'buffer
	  :narrow   ?v
	  :face     'consult-buffer
	  :history  'buffer-name-history
	  :state    #'consult--buffer-state
	  :new
	  (lambda (name)
		(vterm (concat "shell: " name))
		(setq-local vterm-buffer-name-string nil))
	  :items
	  (lambda () (consult--buffer-query
				   :sort 'visibility
				   :as #'buffer-name
				   :include '("shell\\:\\ " "shell")))))
  (defun consult-term ()
	(interactive)
	(consult-buffer '(consult--source-vterm)))
  ;; reorder, mainly to move recent-file down and  org
  (setq consult-buffer-sources
	'(consult-source-hidden-buffer
	   consult-source-modified-buffer
	   consult-source-buffer
	   consult-source-org
	   consult-source-vterm
	   consult-source-bookmark
	   consult-source-project-root
	   consult-source-recent-file
	   consult-source-file-register
	   consult-source-project-buffer-hidden
	   consult-source-project-recent-file-hidden))
  (setq consult-project-buffer-sources
	'(consult-source-hidden-buffer
	   consult-source-project-buffer
	   consult-source-vc-modified-file
	   consult-source-vterm
	   consult-source-bookmark
	   consult-source-project-recent-file)))

    (use-package consult-xref-stack
      :ensure (:host github :repo "brett-lempereur/consult-xref-stack")
      )

(use-package embark
  :bind
  ("C-h B" . embark-bindings)
  (:map embark-region-map
	("w" . google-this)
	("g" . gptel)
	("r" . gptel-rewrite))
  :custom
  (embark-mixed-indicator-delay 0.6)
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators ; the default
	'(embark-verbose-indicator
	   embark-highlight-indicator
	   embark-isearch-highlight-indicator)))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package project
  :ensure nil
  :custom (project-vc-extra-root-markers '("go.mod")))

(use-package protogg
  :ensure (:host github :repo "nehrbash/protogg")
  :demand t
  :custom (protogg-minibuffer-toggle-key "M-g")
  :bind (([remap dired] . protogg-dired)
		 ([remap find-file] . protogg-find-file)
		 ([remap list-buffers] . protogg-list-buffers)
		 ([remap async-shell-command] . protogg-async-shell-command)
		 ([remap shell-command] . protogg-shell-command)
		 ([remap switch-to-buffer] . sn/consult-buffer))
  :config
  (protogg-define 'consult-project-buffer 'consult-buffer sn/consult-buffer)
  (protogg-define 'consult-imenu-multi 'consult-imenu sn/imenu))

(use-package corfu
  :after orderless
  :hook (((prog-mode conf-mode yaml-mode) . sn/corfu-basic))
  :bind (:map corfu-map
		  ("M-SPC" . corfu-insert-separator)
		  ("M-/" . corfu-insert)
		  ("TAB" . corfu-next)
		  ([tab] . corfu-next)
		  ("S-TAB" . corfu-previous)
		  ([backtab] . corfu-previous))
  :custom
  (corfu-cycle t)
  ;; default/writting settings, see sn/corfu-basic for coding completion
  (tab-first-completion t)
  (tab-always-indent 'complete)
  (corfu-auto t)
  (corfu-auto-delay 0.8)
  ;; (corfu-popupinfo-delay (1.8 1.0))
  ;; (corfu-quit-no-match 'separator)
  (corfu-quit-no-match t)
  (corfu-auto-prefix 2)
  (completion-cycle-threshold nil)
  :init
  (global-corfu-mode t)
  :config
  (global-completion-preview-mode t)
  (setq completion-preview-minimum-symbol-length 0)
  ;; ;; Non-standard commands to that should show the preview:
  ;; ;; Org mode has a custom `self-insert-command'
  (push 'org-self-insert-command completion-preview-commands)
  ;; ;; Paredit has a custom `delete-backward-char' command
  (push 'paredit-backward-delete completion-preview-commands)
  ;; ;; Bindings that take effect when the preview is shown:
  ;; ;; Cycle the completion candidate that the preview shows
  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  ;; Convenient alternative to C-i after typing one of the above
  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert)
  ;; fast completion
  (defun orderless-fast-dispatch (word index total)
	(and (= index 0) (= total 1) (length< word 4)
	  `(orderless-regexp . ,(concat "^" (regexp-quote word)))))
  (orderless-define-completion-style orderless-fast
	"A basic completion suitable for coding."
	(orderless-style-dispatchers '(orderless-fast-dispatch))
	(orderless-matching-styles '(orderless-literal orderless-regexp)))
  (defun sn/corfu-basic ()
	"Setup completion for programming"
	(setq-local
	  corfu-auto-delay 0.06
	  completion-styles '(orderless-fast basic)))
  (corfu-popupinfo-mode t)
  (defun corfu-move-to-minibuffer ()
	"For long canadate lists view in minibuffer"
	(interactive)
	(pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
		(let ((completion-extra-properties extras)
			   completion-cycle-threshold completion-cycling)
		  (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

(use-package kind-icon
  :after corfu
  :custom 
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :bind
  ("M-/" . completion-at-point) ;; overwrite dabbrev-completion binding with capf
  ("C-M-/" . sn/codeium-capf) ;; overwirte dabbrev-expand
  ("C-M-?" . sn/cape)
  :hook
  (eglot-managed-mode . sn/code-completion)
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  (cape-dabbrev-min-length 2)
  (completion-at-point-functions
	`(,(cape-capf-super
		 #'cape-dict
		 #'cape-dabbrev
		 #'cape-file)
	   cape-abbrev))
  :config
  (defun sn-completion-preview-use-codeium (orig-fun)
	"Advice to make completion-preview only use codeium."
	(let* ((completion-at-point-functions '(codeium-completion-at-point)))
      (funcall orig-fun)))
  ;; (advice-add 'completion-preview--update :around #'sn-completion-preview-use-codeium)
  (defun sn/codeium-capf ()
	(interactive)
	(cape-interactive #'codeium-completion-at-point))
  (defun sn/code-completion ()
    (setq-local
      completion-at-point-functions
      (list
		(cape-capf-inside-code
          (cape-capf-super
			#'eglot-completion-at-point))
		(cape-capf-inside-string
		  (cape-capf-super
			#'cape-file
			#'cape-dict))
		(cape-capf-inside-comment
		  (cape-capf-super
			#'cape-dict
			#'cape-file))
		#'cape-dabbrev))) 
  (transient-define-prefix sn/cape ()
	"explicit Completion type"
	[[("d" "Dabbrev" cape-dabbrev)
	   ("s" "Spelling" cape-dict)
	   ("k" "Keyword" cape-keyword)
	   ("l" "Line" cape-line)]
	  [("c" "codeium" sn/codeium-capf)
		("e" "Elisp Symbol" cape-elisp-symbol)
		("E" "Elisp Block" cape-elisp-block)
		("t" "Tags" complete-tag)
		]
	  [("f" "File" cape-file)
		("h" "History" cape-history)
		("a" "Abbrev" cape-abbrev)
		("q" "Quit" transient-quit-one)]
	  ]))

(use-package yasnippet
  :hook ((text-mode
		  prog-mode
		  conf-mode) . yas-minor-mode-on)

  :custom
  (yas-verbosity 1)
  (yas-snippet-dir "~/.config/emacs/snippets")
  (yas-wrap-around-region t))
(use-package yasnippet-snippets
  :after yasnippet)
(use-package yasnippet-capf
  :after yasnippet)

(use-package jinx
  :ensure t
  :hook (elpaca-after-init .  global-jinx-mode)
  :bind
  ([remap ispell-word] . jinx-correct-nearest)
  (:map jinx-overlay-map
		("C-M-$" . #'jinx-correct-all))
  :config
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
  :ensure nil
  :hook (dired-mode . sn/dired-hook)
  :custom
  ((dired-mouse-drag-files t)
	(dired-omit-files "^\\.\\.?$")
	(dired-listing-switches "-agho --group-directories-first")
	(dired-omit-verbose nil)
	(dired-recursive-deletes 'top)
	(dired-dwim-target t))
  :config
  (defun sn/dired-hook ()
	(dired-omit-mode 1)
	(dired-hide-details-mode 1)
	(hl-line-mode 1)))
(use-package dired-single
  :ensure (:host github :repo "emacsattic/dired-single")
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
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
		  ("<tab>" . dired-subtree-toggle)
		  ("<backtab>" . dired-subtree-cycle)))

(use-package nerd-icons-dired
  :after dired
  :hook (dired-mode . nerd-icons-dired-mode))
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

(use-package org
  :ensure nil
  :bind
  (:map org-mode-map
    ("C-M-<up>" . org-up-element)
    ("C-c v" . wr-mode))
  :hook
  (org-mode . wr-mode)
  (org-mode . sn/org-mode-hook)
  :custom
  (org-latex-packages-alist
    '(("" "xcolor")))
  (org-latex-todo-keyword-format
    "\\colorbox{%s!30}{\\textbf{%s}}")
  (org-preview-latex-default-process 'dvisvgm)
  (org-todo-keywords
      '((sequence "TODO(t)" "ACTIVE(a)" "|" "DONE(d!)")
        (sequence "PROJECT(p)" "|" "DONE(d!)")
        (sequence "HOLD(h@/!)" "|" "DELEGATED(e@)" "CANCELLED(c@)")))
  (org-todo-repeat-to-state "TODO")
  (org-todo-keyword-faces
    (quote (("ACTIVE" :inherit warning)
     		 ("PROJECT" :inherit font-lock-string-face))))
  (org-adapt-indentation t)
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-src-preserve-indentation t) ;; mainly to ignore ‘org-edit-src-content-indentation’.
  (org-edit-timestamp-down-means-later t)
  (org-ellipsis "…")
  (org-fast-tag-selection-single-key 'expert)
  (org-hide-emphasis-markers t)
  (org-image-actual-width nil)
  (org-insert-heading-respect-content t)
  (org-return-follows-link  t)
  (org-special-ctrl-a/e t)
  (org-src-fontify-natively t)
  (org-catch-invisible-edits 'show-and-error)
  (org-src-tab-acts-natively t)
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-startup-folded t)
  (org-startup-with-inline-images t)
  (org-support-shift-select t)
  (org-archive-location "%s_archive::* Archive")
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
       ("ta" "Active" entry (file+headline "~/doc/inbox.org" "Active")
     	 "* ACTIVE %?\nSCHEDULED: %t\nOn %U\While Editing %a\n" :clock-keep t :clock-in t)
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
  (defun sn/org-mode-hook ()
    (add-hook 'after-save-hook #'sn/org-babel-tangle-dont-ask
      'run-at-end 'only-in-org-mode))
  (defun sn/org-babel-tangle-dont-ask ()
    "Tangle Org file without asking for confirmation."
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))
  (defun sn/tangle-emacs-org ()
    "Tangle Emacs.org to init.el."
    (interactive)
    (let ((org-confirm-babel-evaluate nil)
          (org-file (expand-file-name "Emacs.org" (file-name-directory (or load-file-name buffer-file-name user-init-file)))))
      (org-babel-tangle-file org-file)
      (message "Tangled %s → init.el" org-file)))
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
  (define-minor-mode wr-mode
    "Set up a buffer for word editing.
        This enables or modifies a number of settings so that the
        experience of word processing is a little more like that of a
        typical word processor."
    :interactive t " Writing" nil
    (if wr-mode
      (progn
     	(setq
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
     	(variable-pitch-mode 1)
     	(visual-line-mode 1))
      (kill-local-variable 'word-wrap)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'blink-cursor-interval)
      (kill-local-variable 'show-trailing-whitespace)
      (kill-local-variable 'line-spacing)
      (kill-local-variable 'electric-pair-mode)
      (buffer-face-mode -1)
      (variable-pitch-mode -1)
      (visual-line-mode -1)
      (olivetti-mode -1)))
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
    '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  )
(with-eval-after-load 'org (require 'ox-md nil t))

(use-package ob-mermaid
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(mermaid . t)))

(defun gtd () (interactive)
  (org-agenda 'nil "g"))
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

(use-package org-modern
  :config (global-org-modern-mode t))

   (use-package org-appear
     :ensure (:host github :repo "awth13/org-appear")
     :hook (org-mode . org-appear-mode))

 (use-package org-fragtog
   :hook (org-mode . org-fragtog-mode))

(use-package org-clock
  :ensure nil
  :after (org consult)
  :demand t
  :custom
  (org-clock-in-resume t)
  (org-clock-persist t)
  (org-clock-into-drawer t)
  (org-log-into-drawer "LOGBOOK")
  (org-log-done 'time)
  (org-log-states-order-reversed t)
  (org-pretty-entities t)
  (org-clock-clocked-in-display nil)
  (org-clock-idle-time 25)
  (org-clock-auto-clockout-timer )
  (org-clock-auto-clock-resolution 'always)
  (org-clock-report-include-clocking-task t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-pretty-entities t)
  (org-clock-resolve-expert t)
  :hook (org-after-todo-state-change . sn/org-clock-in-if-inprogress)
  :init
  (defvar org-clock-map (make-sparse-keymap)
    "Keymap for org-clock commands.")
  (defun sn/org-clock-in-if-active ()
	"Handle clocking and archiving based on the task state.
Clock in if state is ACTIVE, clock out and archive if DONE."
	(pcase org-state
      ("ACTIVE"
		(unless (and (org-clock-is-active)
                  (equal org-clock-marker (point-marker)))
		  (org-clock-in))
		(type-break-mode 1))
      ((or "DONE" "DELEGATED" "CANCELLED")
		(when (and (org-clock-is-active)
                (equal org-clock-marker (point-marker)))
		  (org-clock-out))
		(org-archive-subtree)))
	(org-save-all-org-buffers))

  (defun sn/org-demote-other-active ()
  "Demote every other ACTIVE task back to TODO.
Only the currently clocked task should carry the ACTIVE state."
  (let* ((clocked (and (org-clocking-p) org-clock-hd-marker))
          (clocked-buf (and clocked (marker-buffer clocked)))
          (clocked-pos (and clocked (marker-position clocked))))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (let ((heading-pos (save-excursion (org-back-to-heading t) (point))))
                (when (and (string= (org-get-todo-state) "ACTIVE")
                        (not (and (eq clocked-buf (current-buffer))
                               (= heading-pos clocked-pos))))
                  (org-todo "TODO"))))))))))

  (defun sn/org-clock-in-set-state ()
  "Switch task to ACTIVE when clocking in, demoting any others."
  (unless (string= (org-get-todo-state) "ACTIVE")
    (org-todo "ACTIVE"))
  (sn/org-demote-other-active))

(defun sn/org-clock-out-set-state ()
  "Switch task to TODO when clocking out, unless the task is DONE."
  (unless (string= (org-get-todo-state) "DONE")
    (org-todo "TODO")))

(add-hook 'org-clock-in-hook  #'sn/org-clock-in-set-state)
(add-hook 'org-clock-out-hook #'sn/org-clock-out-set-state)


  :config
  (defun consult-clock-in ()
    "Clock into an Org agenda heading."
    (interactive)
    (save-window-excursion
      (consult-org-agenda)
      (org-clock-in)))
  
  (consult-customize consult-clock-in
    :prompt "Clock in: "
    :preview-key "M-.")
  
  (org-clock-persistence-insinuate)
  (org-clock-auto-clockout-insinuate))

(use-package type-break
  :ensure nil
  :custom
  (type-break-interval (* 25 60)) ;; 25 mins
  (type-break-good-rest-interval (* 5 60)) ;; 5 mins
  (type-break-good-break-interval (* 5 60)) ;; 5 mins
  (type-break-keystroke-threshold '(nil . 3000)) ;; 500 words is 3,000
  (type-break-warning-repeat nil)
  (type-break-time-warning-intervals '())
  (type-break-keystroke-warning-intervals '())
  (type-break-mode-line-message-mode nil)
  (type-break-query-mode t)
  (type-break-query-function 'sn/type-break-query)
  (type-break-demo-functions '(type-break-demo-boring))
  (type-break-demo-boring-stats t)
  :init
  (defun sn/type-break-query (a &rest b)
	"Auto say yes and ask to quit type break."
	(if (>= (type-break-time-difference
			  type-break-interval-start
			  type-break-time-last-break) 0)
	  (y-or-n-p "Do you want to continue type-break? ")
	  t)))

(use-package sn-tasks
  :ensure (sn-tasks :type file :main "~/.config/emacs/lisp/sn-tasks.el"))

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
  (defcustom my/org-scheduled-tag "scheduled"
  "Tag to add or remove based on the presence of a SCHEDULED timestamp."
  :type 'string
  :group 'org)

(defun my/org-update-scheduled-tag ()
  "Add or remove the scheduled tag based on the presence of a SCHEDULED timestamp."
  (org-map-entries
   (lambda ()
     (let ((scheduled (org-get-scheduled-time (point)))
	   (tags (org-get-tags)))
       (if scheduled
	   (unless (member my/org-scheduled-tag tags)
	     (org-set-tags (cons my/org-scheduled-tag tags)))
	 (when (member my/org-scheduled-tag tags)
	   (org-set-tags (remove my/org-scheduled-tag tags))))))))

(defun my/org-agenda-update-scheduled-tag ()
  "Hook to update the scheduled tag before building the agenda."
  (save-excursion
    (my/org-update-scheduled-tag)))

  (add-hook 'org-agenda-before-todo-list-hook #'my/org-agenda-update-scheduled-tag)
  (setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  ;; Set active-project-match
  (let ((active-project-match "-INBOX/PROJECT"))
	(setq org-stuck-projects `(,active-project-match ("TODO" "ACTIVE"))
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
			  (tags-todo ,active-project-match
						 ((org-agenda-overriding-header "Projects")
						  (org-tags-match-list-sublevels t)
						  (org-agenda-sorting-strategy
						   '(category-keep))))
			  (tags-todo "-INBOX"
						 ((org-agenda-overriding-header "Next Actions")
						  (org-agenda-tags-todo-honor-ignore-options t)
						  (org-agenda-todo-ignore-scheduled 'future)
						  (org-agenda-skip-function
						   '(lambda ()
							  (or (org-agenda-skip-subtree-if 'todo '("HOLD"))
								  (org-agenda-skip-entry-if 'nottodo '("TODO" "ACTIVE")))))
						  (org-tags-match-list-sublevels t)
						  (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
			  (stuck nil
					 ((org-agenda-overriding-header "Stuck Projects")
					  (org-agenda-tags-todo-honor-ignore-options t)
					  (org-tags-match-list-sublevels t)
					  (org-agenda-todo-ignore-scheduled 'future)))
			  (tags-todo "-INBOX-repeater"
						 ((org-agenda-overriding-header "Orphaned Tasks")
						  (org-agenda-tags-todo-honor-ignore-options t)
						  (org-agenda-todo-ignore-scheduled 'future)
						  (org-agenda-skip-function
						   '(lambda ()
							  (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "DELEGATED"))
								  (org-agenda-skip-subtree-if 'nottodo '("TODO")))))
						  (org-tags-match-list-sublevels t)
						  (org-agenda-sorting-strategy '(category-keep))))
			  (tags-todo "/DELEGATED"
						 ((org-agenda-overriding-header "Delegated")
						  (org-agenda-tags-todo-honor-ignore-options t)
						  (org-agenda-todo-ignore-scheduled 'future)
						  (org-agenda-sorting-strategy '(category-keep))))
			  (tags-todo "-INBOX"
						 ((org-agenda-overriding-header "On Hold")
						  (org-agenda-skip-function
						   '(lambda ()
							  (org-agenda-skip-entry-if 'nottodo '("HOLD"))))
						  (org-tags-match-list-sublevels nil)
						  (org-agenda-sorting-strategy '(category-keep))))))))))

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))
(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))
;; Exclude DONE state tasks from refile targets
(defun sn-verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sn-verify-refile-target)
(defun sn-org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
	(org-refile goto default-buffer rfloc msg)))
(defun sn-org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
	(org-agenda-refile goto rfloc no-update)))

;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

(use-package toc-org
  :config
  (add-to-list 'org-tag-alist '("TOC" . ?T))
  :init
   (add-hook 'org-mode-hook 'toc-org-mode)
  (add-hook 'markdown-mode-hook 'toc-org-mode))

(use-package pdf-tools
  :ensure (:host github :repo "vedang/pdf-tools")
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook ((pdf-tools-enabled . pdf-view-midnight-minor-mode)
         (pdf-view-mode . (lambda ()
                            (setq-local cursor-type nil
							  mode-line-format nil)
                            (blink-cursor-mode -1)
                            (when (bound-and-true-p hl-line-mode)
                              (hl-line-mode -1)))))
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-view-midnight-colors '("#e8e4b1" . "#352718" ))
  (pdf-annot-activate-created-annotations t)
  :config
  (pdf-tools-install :no-query)
  ;; Required for continuous-scroll branch
  (require 'pdf-roll nil t)
  )

(use-package org-roam
  :after gptel
  :init
  (setq-default org-roam-v2-ack t)
  (add-hook 'org-roam-dailies-find-file-hook #'gptel-mode)
  :config
  (org-roam-db-autosync-mode)
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
  )
(use-package consult-org-roam
  :after org-roam)

(use-package org-roam-ui
  :ensure (:host github :repo "org-roam/org-roam-ui")
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

(use-package hl-line
  :ensure nil ;; built-in
  :hook (prog-mode . hl-line-mode))

(use-package simple
  :ensure nil ;; built-in
  :custom
  (comment-auto-fill-only-comments t)
  :hook (prog-mode . auto-fill-mode))

(use-package indent-bars
  :ensure (:host github :repo "jdtsmith/indent-bars")
  :hook ((prog-mode conf-mode yaml-mode) . indent-bars-mode)
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.2))
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.3)
  (indent-bars-pad-frac 0.1))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package treesit
  :ensure nil
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :preface
  (dolist (mapping
	   '((python-mode . python-ts-mode)
	     (css-mode . css-ts-mode)
	     (typescript-mode . typescript-ts-mode)
	     (js2-mode . js-ts-mode)
	     (bash-mode . bash-ts-mode)
	     (conf-toml-mode . toml-ts-mode)
	     (go-mode . go-ts-mode)
			 (go-mod-mode . go-mod-ts-mode)
	     (css-mode . css-ts-mode)
	     (json-mode . json-ts-mode)
	     (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :custom
  (treesit-font-lock-level 4))

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate" :branch "development")
  :custom (combobulate-key-prefix "C-c j")
  :hook (prog-mode . combobulate-mode))

(use-package inheritenv
  :demand t)

(use-package mise
  :ensure (:host github :repo "eki3z/mise.el")
  :after inheritenv
  :demand t
  :hook (after-init . global-mise-mode)
  :custom
  ;; Automatically refresh environment when navigating in eshell
  (mise-update-on-eshell-directory-change t)
  :config
  ;; Fix for vterm not inheriting mise PATH (github.com/eki3z/mise.el/issues/20)
  (add-to-list 'mise-auto-propagate-commands 'vterm)
  (add-to-list 'mise-auto-propagate-commands 'claude-code-ide--detect-cli)
  (add-to-list 'mise-auto-propagate-commands 'claude-code-ide-check-status))

(use-package eglot
  :ensure nil
  :hook
  ((go-ts-mode rust-ts-mode bash-ts-mode js-ts-mode terraform-mode) . eglot-ensure)
  (eglot-managed-mode . eglot-inlay-hints-mode)
  (eglot-managed-mode . sn/setup-eglot)
  :bind
  (:map eglot-mode-map
	("C-h ." . eldoc-doc-buffer)
	("C-c r" . eglot-rename)
	("C-c t" . eglot-find-typeDefinition)
	("C-c i" . eglot-find-implementation)
	("C-c a" . eglot-code-actions)
	("C-c q" . eglot-code-action-quickfix)
	("C-c o" . eglot-code-action-organize-imports))
  :custom
  (eglot-mode-line-format '(eglot-mode-line-action-suggestion))
  (eglot-code-action-indications '(margin eldoc-hint))
  (eglot-report-progress nil)
  (eglot-autoshutdown t)
  (eglot-advertise-cancellation t)
  (eglot-sync-connect nil)
  (eglot-events-buffer-config '(:size 0 :format full))
  :config
  (setq-default eglot-workspace-configuration
	'(:gopls
	   (:usePlaceholders t
		 :staticcheck t
		 :gofumpt t
		 :analyses (:nilness t
					 :unusedparams t
					 :unusedwrite t
					 :unusedvariable t)
		 :hints (:assignVariableTypes t
				  :constantValues t
				  :rangeVariableTypes t))
	   :js-ts
	   (:format
		 (:convertTabsToSpaces t
		   :indentSize 1
		   :tabSize 1
		   :tabWidth 1))))
  (defun eglot-rename (newname)
	"Rename the current symbol to NEWNAME."
	(interactive
	  (list (read-from-minibuffer
			  (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
										   "unknown symbol"))
			  (thing-at-point 'symbol t) nil nil nil
			  (symbol-name (symbol-at-point)))))
	(eglot-server-capable-or-lose :renameProvider)
	(eglot--apply-workspace-edit
	  (eglot--request (eglot--current-server-or-lose)
		:textDocument/rename `(,@(eglot--TextDocumentPositionParams)
								:newName ,newname))
	  this-command))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (defun eglot-organize-imports ()
	(interactive)
	(eglot-code-actions nil nil "source.organizeImports" t))
  (defun sn/setup-eglot ()
	"Eglot setup customizations"
	(add-hook 'before-save-hook #'eglot-format-buffer -10 'local)
	(add-hook 'before-save-hook #'eglot-organize-imports nil 'local)
	(setq-local eldoc-documentation-functions
	  (remove #'flymake-eldoc-function eldoc-documentation-functions))))
(use-package consult-eglot
  :after eglot
  :bind
  (:map eglot-mode-map
	("C-c f" . consult-eglot-symbols)))

(use-package eldoc-box
  :custom
  (eldoc-idle-delay 1.2)
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-cleanup-interval 0.8)
  (eldoc-box-only-multi-line nil)
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-box-offset '(32 32 32))
  (eldoc-box-max-pixel-width 600)
  (eldoc-box-max-pixel-height 200)
  :hook (eldoc-mode . eldoc-box-hover-mode))

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
  (remove-hook 'dape-on-start-hooks 'dape-repl)o (add-hook 'dape-on-start-hooks
	    (defun dape--save-on-start ()
	      (save-some-buffers t t))))

(use-package git-gutter
  :hook (elpaca-after-init .  global-git-gutter-mode)
  :custom
  (git-gutter:ask-p nil)
  (git-gutter:update-interval 0.4))
(use-package git-gutter-fringe
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package magit
  :after (transient git-timemachine)
  :config
  (require 'smerge-mode)
  (transient-define-prefix sn/smerge ()
	"Rebase Menu"
	[["Smerge nav/show"
	   ("n" "Next" smerge-next :transient t)
	   ("p" "Prev" smerge-prev :transient t)
	   ("h" "Highlight diff" smerge-refine :transient t)]
	  ["Quick Resolve"
		("a" "Keep All" smerge-keep-all :transient t)
		("u" "Keep Upper" smerge-keep-upper :transient t)
		("l" "Keep Lower" smerge-keep-lower :transient t)]]
	[["Resolve"
	   ("r" "Resolve Intelligently" smerge-resolve :transient t)
	   ("R" "Auto Resolve All" smerge-resolve-all :transient t)
	   ("e" "Resolve With Ediff" smerge-ediff)]
	  ["Actions"
		("q" "Quit" transient-quit-one)
		("s" "Magit Status" magit-status)
		("S" "git Logs" magit-pg-repo)]])
  (defun sn/git-squash-then-rebase (branch)
	"Squash merge and then rebase the current branch onto BRANCH."
	(interactive (list (magit-read-branch "Branch to rebase onto")))
	;; Fetch the latest changes from the remote
	(magit-fetch-from-upstream branch)
	;; Reset to the last common commit
	(magit-run-git "reset" "--soft" (magit-git-string "merge-base" "HEAD" branch))
	;; Create a single commit from all staged changes
	(magit-commit)
	;; Rebase onto the updated branch
	(magit-run-git "rebase" branch))
  :custom
  (magit-log-margin-show-committer-date t)
  (magit-diff-refine-hunk t))
(use-package git-timemachine
  :custom (git-timemachine-show-minibuffer-details t))

(use-package hl-todo
  :ensure (hl-todo :depth nil)
  :init (global-hl-todo-mode t)
  :after ef-themes
  :config
  (defun sn-ef-themes-hl-todo-faces ()
	"Configure `hl-todo-keyword-faces' with Ef themes colors.
The exact color values are taken from the active Ef theme."
	(modus-themes-with-colors
      (setq hl-todo-keyword-faces
			`(("HOLD" . ,yellow-warmer)
	      ("TODO" . ,red)
	      ("ACTIVE" . ,blue)
	      ("THEM" . ,magenta)
	      ("PROG" . ,cyan-warmer)
	      ("OKAY" . ,green-warmer)
	      ("DONT" . ,yellow-warmer)
	      ("FAIL" . ,red-warmer)
	      ("BUG" . ,red-warmer)
	      ("DONE" . ,green)
	      ("NOTE" . ,blue-warmer)
	      ("KLUDGE" . ,cyan)
	      ("HACK" . ,cyan)
	      ("TEMP" . ,red)
	      ("FIXME" . ,red-warmer)
	      ("XXX+" . ,red-warmer)
	      ("REVIEW" . ,red)
	      ("DEPRECATED" . ,yellow-warmer)))))
  (add-hook 'modus-themes-post-load  #'sn-ef-themes-hl-todo-faces))
(use-package magit-todos
  :init (magit-todos-mode 1))

      (use-package magit-pretty-graph
	:ensure (:host github :repo "georgek/magit-pretty-graph")
	:bind
	)

(use-package browse-at-remote
  :bind
  ("C-c g g" . browse-at-remote)
  ("C-c g k" . browse-at-remote-kill))

  (use-package svg-tag-mode)
  (use-package vterm
    :hook (vterm-mode . sn/setup-vterm)
    :init
    (defun sn/vterm-apply-theme ()
  	"Apply current theme colors to vterm buffer."
  	(modus-themes-with-colors
  	  (let ((darker (sn-darken-color bg-main 0.7)))
  		(face-remap-add-relative
  		  'default
  		  :background darker)
  		(face-remap-set-base
  		  'default
  		  :background darker)
  		(face-remap-add-relative
  		  'fringe
  		  :background darker))))
    (defun sn/setup-vterm ()
  	"Setup vterm buffer with custom font and theme colors."
  	(set (make-local-variable 'buffer-face-mode-face) '(:family "IosevkaTerm Nerd Font"))
  	(buffer-face-mode t)
  	(setq-local left-margin-width 3
  	  right-margin-width 3
  	  cursor-type 'bar)
  	(sn/vterm-apply-theme))
    (defun sn/vterm-update-all-themes ()
  	"Update theme colors in all vterm buffers."
  	(dolist (buffer (buffer-list))
  	  (when (eq (buffer-local-value 'major-mode buffer) 'vterm-mode)
  		(with-current-buffer buffer
  		  (sn/vterm-apply-theme)))))
    :config
    (add-hook 'modus-themes-post-load-hook #'sn/vterm-update-all-themes)
    (defun old-version-of-vterm--get-color (index &rest args)
  	"This is the old version before it was broken by commit
  https://github.com/akermu/emacs-libvterm/commit/e96c53f5035c841b20937b65142498bd8e161a40.
  Re-introducing the old version fixes auto-dim-other-buffers for vterm buffers."
  	(cond
        ((and (>= index 0) (< index 16))
  		(face-foreground
  		  (elt vterm-color-palette index)
  		  nil 'default))
        ((= index -11)
  		(face-foreground 'vterm-color-underline nil 'default))
        ((= index -12)
  		(face-background 'vterm-color-inverse-video nil 'default))
        (t
  		nil)))
    (advice-add 'vterm--get-color :override #'old-version-of-vterm--get-color)
    (defun my/vterm-standalone ()
  	"Create a standalone vterm frame without modeline and minibuffer."
  	(interactive)
  	(let ((frame (make-frame '((name . "vterm-standalone")
  								(minibuffer . nil)))))
        (select-frame frame)
        (let ((display-buffer-alist nil))
  		(vterm))
  	  (setq mode-line-format nil)))
    )
  (use-package svg-lib :ensure t)
  (use-package svg-tabs
    :ensure (svg-tabs :type file :main "~/.config/emacs/lisp/svg-tabs.el")
    :after svg-lib)
  (use-package vterm-tabs
    :ensure (vterm-tabs :type file :main "~/.config/emacs/lisp/vterm-tabs.el")
    :bind
    (("<f6>" . vterm-tabs-toggle)
  	:map vterm-mode-map
  	("C-M-s" . consult-term)
  	("M-w" . copy-region-as-kill)
  	("C-y" . vterm-yank))
    :custom
    (vterm-buffer-maximum-size 800)
    (vterm-tramp-shells
  	'(("ssh" "/bin/bash")
  	   ("docker" "/bin/bash")
  	   ("sudo" "/bin/bash")))
    (vterm-always-compile-module t)
    :config
    (global-vterm-tabs-mode 1))

(use-package makefile-runner
  :ensure (:host github :repo "danamlund/emacs-makefile-runner"))
(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output t)
  (compile-command "task "))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-wrap-around t)
  (flymake-no-changes-timeout 2)
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-show-diagnostics-at-end-of-line 'fancy)

  :config
  (defun toggle-flymake-show-diagnostics-at-end-of-line ()
  "Toggle 'flymake-show-diagnostics-at-end-of-line' between 'fancy and nil."
  (interactive)
  (setq flymake-show-diagnostics-at-end-of-line
        (if (eq flymake-show-diagnostics-at-end-of-line 'fancy)
            nil
          'fancy))
	;; hacky turn off on
  (flymake-mode 0)
  (flymake-mode 1)
  (message "flymake-show-diagnostics-at-end-of-line is now %s"
           flymake-show-diagnostics-at-end-of-line)))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :custom (flymake-shellcheck-args '("--exclude=SC1090"))
  :hook (bash-ts-mode . flymake-shellcheck-load))

(use-package treesit-fold
  :ensure (:host github :repo "emacs-tree-sitter/treesit-fold")

  :custom (treesit-fold-summary-max-length 80)
  :init
  (global-treesit-fold-mode))

(use-package go-mode
  :hook (go-ts-mode . (lambda ()
						(subword-mode 1)
						(setq-local go-ts-mode-indent-offset 4))))
(use-package go-tag
  :bind (:map go-ts-mode-map ("C-c C-t" . go-tag-add)))
(use-package go-impl
  :bind (:map go-ts-mode-map ("C-c C-i" . go-impl)))
(use-package go-gen-test
  :bind (:map go-ts-mode-map ("C-c t g" . go-gen-test-dwim)))
(use-package gotest
  :bind
  (:map go-ts-mode-map
		("C-c t f" . go-test-current-file)
		("C-c t t" . go-test-current-test)))

(use-package rust-ts-mode
  :ensure nil
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook (rust-ts-mode . (lambda ()
						  (setq-local compile-command "cargo run"))))
(use-package cargo-jump-xref
  :after toml-mode
  :ensure (:host github :repo "eval-exec/cargo-jump-xref.el")
  :config
  (add-to-list 'xref-backend-functions #'cargo-jump-xref-backend))

(use-package geiser
  :config
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile))))
(use-package geiser-guile
  :after geiser)

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
  )
(use-package docker-compose-mode
  :mode ("\docker-compose.yml\\'" . docker-compose-mode))

(use-package sqlformat
  :hook (sql-mode . sqlformat-on-save-mode)
  :custom
  (sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g")))

(use-package systemd)

(use-package terraform-mode)

(use-package lisp-mode
  :ensure nil
  :custom (lisp-indent-offset 2)
  (elisp-fontify-semantically t))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
		 ("\\.css\\'" . web-mode)
		 ("\\.jsx?\\'" . web-mode)
		 ("\\.tsx?\\'" . web-mode)
		 ("\\.json\\'" . web-mode))
  :hook (web-mode . sn/web-mode-hook)
  :custom (web-mode-markup-indent-offset 2)
  :config
  (defun sn/web-mode-hook()
	"Set web-mode engine to 'go' if in web-mode."
	(when (equal major-mode 'web-mode)
      (web-mode-set-engine "go"))))

(defun sn/start-ag-devcontainer ()
  "Close analytics-hub buffers, start the dev container, and open Dired and Magit."
  (interactive)
  ;; Close all buffers in ~/src/analytics-hub/
  (dolist (buffer (buffer-list))
    (when (string-prefix-p (expand-file-name "~/src/gateway/") (or (buffer-file-name buffer) ""))
      (kill-buffer buffer)))
  ;; Set default directory and run the dev container command
  (let* ((default-directory (expand-file-name "~/src/gateway/gateway/"))
	  (output-buffer "*Start Dev-Container Output*")
	  (result (call-process-shell-command "devcontainer up --workspace-folder ." nil output-buffer t)))
    (if (= result 0)
	  (dired "/docker:mise-container:/workspace/gateway")
      (message "Error: Command failed. Check %s for details." output-buffer))))

(defun sn/ssh-pub-key ()
  "Select a .pub key from ~/.ssh/ and copy its contents to the kill ring."
  (interactive)
  (let* ((ssh-dir (expand-file-name "~/.ssh/"))
	 (keys (when (file-exists-p ssh-dir)
		 (directory-files ssh-dir t "\\.pub$")))
	 (key-name (completing-read "Select SSH public key: " keys nil t)))
    (when key-name
      (with-temp-buffer
	(insert-file-contents key-name)
	(kill-new (buffer-string)))
      (message "Copied %s to kill ring" key-name))))

(defun sn/copy-path ()
  "Copy the buffer's file path and line number to the kill ring.
If the buffer is part of a project, copy the relative path from the project root.
Otherwise, copy the absolute file path. Appends the line number at the end."
  (interactive)
  (let* ((file-name (buffer-file-name))
	 (line-number (line-number-at-pos))
	 (project (project-current nil))
	 (project-root (and project (project-root project)))
	 (relative-file-name (and project-root
				  file-name
				  (file-relative-name file-name project-root))))
    (if (and project-root relative-file-name)
	(progn
	  (kill-new (format "%s:%d" relative-file-name line-number))
	  (message "Copied relative file path: %s:%d" relative-file-name line-number))
      (if file-name
	  (progn
	    (kill-new (format "%s:%d" file-name line-number))
	    (message "Copied file path: %s:%d" file-name line-number))
	(message "Buffer is not visiting a file")))))

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

(use-package gptel
  :hook
  (org-mode . gptel-activate-if-model-exists)
  :custom
  (gptel-model 'gemma-4-e4b-it)
  (gptel-display-buffer-action
    '((display-buffer-reuse-window display-buffer-in-side-window)
       (side . right)
       (window-width . fit-window-to-buffer)
       (slot . 0)))
  (gptel-default-mode 'org-mode)
  (gptel-use-tools t)
  :init
  (defun gptel-activate-if-model-exists ()
	"Activate gptel mode if the GPTEL_MODEL property exists in any part of the Org document."
	(org-with-wide-buffer
      (goto-char (point-min))
      (let ((found nil))
		(while (and (not found) (re-search-forward "^\\*+" nil t))
	  (when (org-entry-get (point) "GPTEL_MODEL")
			(setq found t)))
		(when found
	  (gptel-mode 1)))))
  :config
  ;; Bind f5 in vterm to toggle the gptel sidebar.  Deferred because
  ;; vterm-mode-map doesn't exist until vterm is loaded, and we no
  ;; longer gate the whole gptel config behind `:after vterm'.
  (with-eval-after-load 'vterm
    (define-key vterm-mode-map (kbd "<f5>") #'gptel-toggle-sidebar))

  (add-hook 'gptel-post-stream #'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (add-to-list 'gptel-tools
	     (gptel-make-tool
	      :name "read_url"
	      :function (lambda (url)
			 ;; function implementation
			 )
	      :description "Fetch and read the contents of a URL"
	      :args (list '(:name "url"
			    :type string
			    :description "The URL to read"))
	      :category "web"))

  (defun gptel-toggle-sidebar ()
  "Toggle a custom sidebar for today's daily note, initializing with 'gptel-mode' if new."
  (interactive)
  (let* ((buffer (or (get-buffer (format-time-string "%Y-%m-%d.org"))
		     (save-window-excursion
		       (org-roam-dailies-goto-today)
		       (current-buffer))))
	 (width (+ (or fill-column 80) 2)))  ;; Default fallback to 82 if fill-column is not set.
    (if-let* ((window (get-buffer-window buffer)))
	;; If the sidebar is already open, close it.
	(delete-window window)
      ;; Otherwise, set up the sidebar buffer and open window.
      (progn
	(let* ((window (display-buffer
			buffer
			`((display-buffer-in-side-window
			   display-buffer-same-window)
			  (side . right)
			  (window-width . ,width)
			  (slot . 0)))))
	  (with-current-buffer buffer
	    (progn
	      (goto-char (point-min
))
	      (if (org-find-exact-headline-in-buffer "Chuck Chats")
		  (goto-char (org-find-exact-headline-in-buffer "Chuck Chats"))
		(progn
		  (goto-char (1- (point-max)))
		  (insert "\n* Chuck Chats\n")
		  (gptel-mode)
		  (gptel-org-set-properties (point))))))
	  (when window
	    (set-window-dedicated-p window t)
	    (set-window-parameter window 'no-other-window t)
	    (with-selected-window window
	      (setq mode-line-format nil))
	    (select-window window)))))))
  (defun gptel-close-headers (from to)
	"Fold all org headers in the current buffer, except for the last response."
	(when (eq major-mode 'org-mode)
	  (progn
		(condition-case nil
      (progn
	(org-cycle-global 0)
	(goto-char from)
	(while (outline-invisible-p (line-end-position))
		  (progn
			(outline-show-subtree)
			(outline-next-visible-heading 1))))
      (error (message "gptel-close-headers error: %s" (error-message-string err))))
		(outline-show-subtree) ; not sure why but sometimes needs this
		(org-end-of-line))))
  (add-to-list 'gptel-post-response-functions #'gptel-close-headers)
  (defun gptel-save-if-file (to from)
	"Save the current buffer if it is associated with a file."
	(when (buffer-file-name)
      (save-buffer)))
  (add-to-list 'gptel-post-response-functions #'gptel-save-if-file)

  ;; Only two backends: local Gemma via llama.cpp and Claude.  Drop the
  ;; built-in ChatGPT entry so the menu doesn't surface OpenAI models.
  (setf (alist-get "ChatGPT" gptel--known-backends nil 'remove #'equal) nil)

  ;; Local llama.cpp server (redfish only) — Gemma 4 E4B served by
  ;; home-llama-server-service-type.  Default backend when available.
  (when (executable-find "llama-server")
	(setq gptel-backend
		  (gptel-make-openai "llama-cpp-local"
			:protocol "http"
			:host "localhost:8080"
			:stream t
			:key "none"
			:models '(gemma-4-e4b-it)))
	(setq gptel-model 'gemma-4-e4b-it))

  ;; Anthropic Claude via Claude Max OAuth.  Reads the access token
  ;; from ~/.claude/.credentials.json (maintained by the `claude' CLI)
  ;; and sends it as a Bearer token with the `oauth-2025-04-20'
  ;; anthropic-beta header.  The Max API additionally requires the
  ;; system prompt to begin with the Claude Code identity — injected by
  ;; an around-advice on `gptel--request-data' below.  Run `claude'
  ;; in a terminal to refresh the token when it expires.
  (defconst my/claude-code-system-identity
	"You are Claude Code, Anthropic's official CLI for Claude."
	"Required system prompt prefix for Claude Max OAuth API calls.")

  (defun my/claude-oauth-token ()
	"Return the current Claude Code OAuth access token, or nil."
	(when (file-readable-p "~/.claude/.credentials.json")
      (let* ((json-object-type 'alist)
			 (creds (json-read-file "~/.claude/.credentials.json"))
			 (oauth (alist-get 'claudeAiOauth creds)))
		(alist-get 'accessToken oauth))))

  (gptel-make-anthropic "Claude"
    :stream t
    :key #'my/claude-oauth-token
    :header
    (lambda ()
      (when-let* ((token (my/claude-oauth-token)))
		`(("Authorization"    . ,(concat "Bearer " token))
		  ("anthropic-version" . "2023-06-01")
		  ("anthropic-beta"    . "oauth-2025-04-20")))))

  (defun my/gptel-claude-oauth-inject-identity (orig-fun backend prompts)
	"Prepend the Claude Code identity to the system prompt for the Claude backend.
Anthropic's Max OAuth gateway rejects requests whose system prompt
doesn't start with that identity."
	(if (and (gptel-anthropic-p backend)
			 (equal (gptel-backend-name backend) "Claude"))
		(let ((gptel--system-message
			   (cond
				((null gptel--system-message)
				 (list my/claude-code-system-identity))
				((stringp gptel--system-message)
				 (list my/claude-code-system-identity gptel--system-message))
				((listp gptel--system-message)
				 (cons my/claude-code-system-identity gptel--system-message)))))
		  (funcall orig-fun backend prompts))
      (funcall orig-fun backend prompts)))
  (advice-add 'gptel--request-data :around
			  #'my/gptel-claude-oauth-inject-identity))

(use-package whisper
  :ensure (:host github :repo "natrys/whisper.el")
  :custom
  (whisper-install-whispercpp nil)  ; we install it via Guix Home
  (whisper-model "large-v3-turbo")
  (whisper-language "en")
  (whisper-translate nil)
  (whisper-use-threads (num-processors))
  :config
  ;; Point whisper.el at the Guix-provided binary and model.
  (setq whisper--ffmpeg-input-format "pulse"
		whisper--ffmpeg-input-device "default")
  (let ((model (expand-file-name
				"~/.guix-home/profile/share/ai-models/ggml-large-v3-turbo-q5_0.bin"))
		(bin   (executable-find "whisper-cli")))
	(when (file-exists-p model)
      (setq whisper--ggml-model-file model))
	(when bin
      (setq whisper--install-path (file-name-directory bin)))))

(use-package gptel-agent
  :after gptel)

(use-package agent-shell
  :ensure (:host github :repo "xenodium/agent-shell")
  :custom
  (agent-shell-show-welcome-message nil)
  (agent-shell-preferred-agent-config 'claude-code)
  ;; claude-agent-acp is a Bun single-file executable.  When it tries to
  ;; spawn the Claude Code CLI it re-execs itself via process.execPath,
  ;; but the child process can't read $bunfs paths, so session/new hangs
  ;; forever.  Point CLAUDE_CODE_EXECUTABLE at the real `claude' binary
  ;; so the SDK uses that instead of re-exec'ing itself.
  (agent-shell-anthropic-claude-environment
   (list (concat "CLAUDE_CODE_EXECUTABLE="
                 (or (executable-find "claude") "claude")))))

(use-package agent-shell-dispatch
  :ensure (:host github :repo "cassandracomar/agent-shell-dispatch")
  :after agent-shell
  :config
  (agent-shell-dispatch-global-mode 1))

(use-package mcp-server
  :ensure (:host github :repo "rhblind/emacs-mcp-server"
           :files ("*.el" "tools/*.el" "mcp-wrapper.py" "mcp-wrapper.sh"))
  :hook (emacs-startup . mcp-server-start-unix))

 (use-package codeium
   :ensure (:host github :repo "Exafunction/codeium.el")
   :custom
   (codeium-log-buffer nil)
   :config
   ;; modeline
   (setq codeium-mode-line-enable
     (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
   (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t))

(use-package google-this)

(use-package ea :ensure (ea :type file :main "~/.config/emacs/lisp/ea.el"))

(use-package consult-taskfile :ensure (consult-taskfile :type file :main "~/.config/emacs/lisp/consult-taskfile/consult-taskfile.el"))
