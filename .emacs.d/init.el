;;; init.el --- Initialization file for Emacs -*- lexical-binding:t -*-
;;; Commentary: Emacs Startup File, initialization for Emacs. DO NOT EDIT, auto tangled from Emacs.org.
;;; Code:

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca elpaca-use-package
		;; use-package enable :ensure keyword.
		(elpaca-use-package-mode))
(setopt
 use-package-always-ensure t
 warning-minimum-level :emergency
 native-comp-jit-compilation t
 byte-compile-docstring-max-column 120
 native-compile-prune-cache t)

(defun run-commands-if-no-lock-file ()
  (let ((lock-file "~/.emacs.d/install_lock"))
    (unless (file-exists-p lock-file)
      (condition-case err
        (all-the-icons-install-fonts)
        (error (message "Error running all-the-icons-install-fonts: %s" err)))
      (condition-case err
        (yas-reload-all)
        (error (message "Error running yas-reload-all: %s" err)))
      (condition-case err
        (recentf-cleanup)
        (error (message "Error running recentf-cleanup: %s" err)))
      (condition-case err
        (nerd-icons-install-fonts)
        (error (message "Error running nerd-icons-install-fonts: %s" err))) ;; commented as 'nerd-icons-install-fonts' function doesn't exist.
      (write-region "" nil lock-file))))

(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(defun sn/elpacha-hook ()
  "Settup after elpaca finishes"
  (progn
	(let ((buffer (get-buffer " elpaca--read-file")))
	  (when buffer
		(kill-buffer buffer)))
	(load custom-file 'noerror)))
(add-hook 'elpaca-after-init-hook 'sn/elpacha-hook)

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
  :preface
  (defun my-rgb-to-hex (r g b)
	"Convert RGB to hex color format."
	(format "#%02x%02x%02x" r g b))

  (defun my-darken-color (hex-color factor)
	"Darken HEX-COLOR by factor (a float between 0 and 1)."
	(let* ((r (string-to-number (substring hex-color 1 3) 16))
			(g (string-to-number (substring hex-color 3 5) 16))
			(b (string-to-number (substring hex-color 5 7) 16))
			(r-dark (max 0 (floor (* r factor))))
			(g-dark (max 0 (floor (* g factor))))
			(b-dark (max 0 (floor (* b factor)))))
      (my-rgb-to-hex r-dark g-dark b-dark)))
  (defun my-ef-themes-mod ()
	"Tweak the style of the ef theme."
	(interactive)
    (ef-themes-with-colors
	  (let ((darker (my-darken-color bg-main 0.7)))
		(custom-set-faces
		  `(default ((,c :family "Iosevka" )))
		  `(variable-pitch ((,c :family "Iosevka Aile")))
		  `(ef-themes-key-binding ((,c :inherit (bold ef-themes-fixed-pitch) :foreground ,yellow-warmer :height 0.95)))
		  
		  `(window-divider ((,c :background ,bg-main :foreground ,bg-main)))
		  `(window-divider-first-pixel ((,c :background ,bg-main :foreground ,bg-main)))
		  `(window-divider-last-pixel ((,c :background ,bg-main :foreground ,bg-main)))
		  `(blamer-face ((,c :foreground ,fg-alt :italic t)))
		  `(tab-line              ((,c :background ,bg-dim :foreground ,bg-dim :height 1.1 :box nil)))
		  `(tab-line-tab-group    ((,c :inherit 'tab-line)))
		  `(tab-line-tab          ((,c :inherit 'tab-line :background nil  :forground nil :box nil)))
		  `(tab-line-tab-current  ((,c :height 1.1 :inherit 'tab-line-tab :background nil :box nil)))
		  `(tab-line-tab-inactive ((,c :height 1.1 :inherit 'tab-line-tab :background ,bg-dim :forground ,bg-dim :box nil)))
		  `(tab-line-tab-inactive-alternate ((,c :height 1.1 :inherit 'tab-line-tab :background ,bg-dim :forground ,bg-dim :box nil)))
		  `(tab-line-highlight ((,c :inherit nil :background nil :foreground nil :box nil)))
		  `(line-number ((,c :background ,darker)))
		  `(vertico-posframe ((,c :inherit default :background ,darker)))
		  `(vertico-posframe-border ((,c (:background ,bg-dim))))
		  `(scroll-bar ((,c :foreground ,fg-alt :background ,darker)))
		  `(mode-line ((,c :family "Iosevka Aile" :background ,bg-mode-line :foreground ,fg-main  :box (:line-width 3 :color ,darker))))
		  `(mode-line-active ((,c :background ,bg-mode-line :foreground ,fg-main  :box (:line-width 3 :color ,darker ))))
		  `(mode-line-inactive ((,c :height 120 :box (:line-width 3 :color ,darker))))
		  `(eldoc-box-border ((,c :background ,fg-alt)))
		  `(eldoc-box-body ((,c :family "Iosevka Aile" :background ,darker)))
		  `(breadcrumb-imenu-leaf-face ((,c :inherit mode-line :foreground ,fg-alt)))
		  ;; `(org-document-title ((,c :height 1.4)))
		  ;; `(org-modern-todo ((,c :height 1.2)))
		  ;; `(org-modern-done ((,c :height 1.2)))
		  ;; `(org-modern-tag ((,c :height 1.2)))
		  ;; `(org-modern-symbol ((,c :font "Iosevka")))
		  ))))
  ;; load theme
  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-mod)
  (ef-themes-select 'ef-melissa-dark))

(use-package prot-modeline
  :ensure (:host gitlab
			:repo "protesilaos/dotfiles"
			:files ("emacs/.emacs.d/prot-lisp/prot-modeline.el" 
					 "emacs/.emacs.d/prot-lisp/prot-common.el")
			:main "emacs/.emacs.d/prot-lisp/prot-modeline.el")
  :config
  (setq mode-line-compact nil) ; Emacs 28
  (setq mode-line-right-align-edge 'right-margin) ; Emacs 30
  (defvar custom-mode-line-height 32
	"Height of the mode line image.")
  (defun custom-sloth-image-segment ()
	"Return an image segment with a specified height."
	(let ((img-file (expand-file-name "img/sloth-head.jpg" user-emacs-directory))
           (img-height custom-mode-line-height))
      (when (file-exists-p img-file)
		(propertize " "
          'display (create-image img-file nil nil :height img-height :ascent 'center)))))
  (setq-default mode-line-format
    '("%e"
	   (:eval (custom-sloth-image-segment))
       prot-modeline-kbd-macro
       prot-modeline-narrow
       prot-modeline-buffer-status
       prot-modeline-window-dedicated-status
       prot-modeline-input-method
	   (:eval (meow--update-indicator))
       "  "
       prot-modeline-buffer-identification
       "  "
       prot-modeline-process
	   " "
	   (:eval (breadcrumb-imenu-crumbs))
       mode-line-format-right-align ; Emacs 30
	   "  "
       prot-modeline-eglot
       "  "
       prot-modeline-flymake
	   "  "
	   prot-modeline-vc-branch
       "  ")))

;; (line-number-mode -1)
;; (column-number-mode -1)

(use-package tab-bar
  :ensure nil
  :after breadcrumb
  :custom
  (tab-bar-show t)
  (tab-bar-format-tabs nil)
  (tab-bar-close-button-show nil)
  (tab-bar-tab-name-function #'sn/tab-bar-tab-name-function)
  (tab-bar-format '(tab-bar-format-tabs
					 tab-bar-format-align-right
					 breadcrumb-project-crumbs
					 (lambda() "    ")))
  (project-switch-commands #'sn/project-find-dir)
  :config
  (defun sn/tab-bar-tab-name-function ()
	(let ((project (project-current)))
      (if project
		(project-root project)
		(tab-bar-tab-name-current))))
  (defun sn/project-find-dir ()
  "Start Dired in a directory inside the current project root."
  (interactive)
  (tab-bar-new-tab)
	(let* ((project (project-current t))
			(default-directory (project-root project))
			(dir "./"))
      (dired dir)
	  (delete-other-windows))))

(use-package breadcrumb
  :ensure (:host github :repo "joaotavora/breadcrumb"))

(set-display-table-slot standard-display-table 'truncation ?\s) ;; remove the $ on wrap lines.
(global-prettify-symbols-mode t)
(setopt after-delete-frame-functions nil)

(use-package pixel-scroll
  :ensure nil
  :bind
  ([remap scroll-up-command]   . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up)
  :hook
  (pixel-scroll-precision-mode . sn/smooth-scroll-hook)
  (elpaca-after-init . pixel-scroll-precision-mode)
  :custom
  (pixel-scroll-precision-interpolate-page t)
  (pixel-scroll-precision-use-momentum t)
  (pixel-scroll-precision-interpolate-page t)
  (pixel-scroll-precision-interpolation-total-time 0.25)
  (pixel-scroll-precision-large-scroll-height 10) ;; default 40
  (scroll-conservatively 5)
  (scroll-margin 5)
  :init
  (defun kb/pixel-recenter (&optional arg redisplay)
	"Similar to `recenter' but with pixel scrolling.
ARG and REDISPLAY are identical to the original function."
	;; See the links in line 6676 in window.c for
	(when-let* ((current-pixel (pixel-posn-y-at-point))
				 (target-pixel (if (numberp arg)
                                 (* (line-pixel-height) arg)
								 (* 0.5 (window-body-height nil t))))
				 (distance-in-pixels 0)
				 (pixel-scroll-precision-interpolation-total-time
				   (/ pixel-scroll-precision-interpolation-total-time 2.0)))
      (setq target-pixel
		(if (<= 0 target-pixel)
		  target-pixel
          (- (window-body-height nil t) (abs target-pixel))))
      (setq distance-in-pixels (- target-pixel current-pixel))
      (condition-case err
        (pixel-scroll-precision-interpolate distance-in-pixels nil 1)
		(error nil ;; (message "[kb/pixel-recenter] %s" (error-message-string err))
		  ))
      (when redisplay (redisplay t))))

  (defun kb/pixel-scroll-up (&optional arg)
	"(Nearly) drop-in replacement for `scroll-up'."
	(cond
	  ((eq this-command 'scroll-up-line)
		(funcall (ad-get-orig-definition 'scroll-up) (or arg 1)))
	  (t
		(unless (eobp) ; Jittery window if trying to go down when already at bottom
		  (pixel-scroll-precision-interpolate
			(- (* (line-pixel-height)
				 (or arg (- (window-text-height) next-screen-context-lines))))
			nil 1)))))

  (defun kb/pixel-scroll-down (&optional arg)
	"(Nearly) drop-in replacement for `scroll-down'."
	(cond
	  ((eq this-command 'scroll-down-line)
		(funcall (ad-get-orig-definition 'scroll-down) (or arg 1)))
	  (t
		(pixel-scroll-precision-interpolate
		  (* (line-pixel-height)
			(or arg (- (window-text-height) next-screen-context-lines)))
		  nil 1))))

	(defun sn/smooth-scroll-hook ()
	  (cond
		(pixel-scroll-precision-mode
		  (advice-add 'scroll-up :override 'kb/pixel-scroll-up)
		  (advice-add 'scroll-down :override 'kb/pixel-scroll-down)
		  (advice-add 'recenter :override 'kb/pixel-recenter))
		(t
		  (advice-remove 'scroll-up 'kb/pixel-scroll-up)
		  (advice-remove 'scroll-down 'kb/pixel-scroll-down)
		  (advice-remove 'recenter 'kb/pixel-recenter)))))

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

(setq-default
 fill-column 80
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
(global-goto-address-mode t)
(setq browse-url-firefox-program "zen-browser")
(defun browse-url-zen (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
           (concat "zen-browser " url) nil
           browse-url-firefox-program
           (append
            browse-url-firefox-arguments
            (if (browse-url-maybe-new-window new-window)
		(if browse-url-firefox-new-window-is-tab
		    '("-new-tab")
		  '("-new-window")))
            (list url)))))
(with-eval-after-load 'browse-url
  (setq browse-url-browser-function #'browse-url-zen))
(global-unset-key (kbd "M-SPC")) ;; my second C-c binding

(use-package prot-window
  :ensure (:host gitlab
			:repo "protesilaos/dotfiles"
			:files ("emacs/.emacs.d/prot-lisp/prot-window.el"
					 "emacs/.emacs.d/prot-lisp/prot-common.el")
			:main "emacs/.emacs.d/prot-lisp/prot-window.el")
  :config
  (setq display-buffer-alist
    `(;; no window
       ("\\`\\*Async Shell Command\\*\\'"
         (display-buffer-no-window))
       ("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
         (display-buffer-no-window)
         (allow-no-window . t))
       ;; bottom side window
       ("\\*\\(Org \\(Select\\|Note\\)\\|Agenda Commands\\)\\*" ; the `org-capture' key selection and `org-add-log-note'
         (display-buffer-in-side-window)
         (dedicated . t)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((mode-line-format . none))))
       ;; bottom buffer (NOT side window)
       ((or . ((derived-mode . flymake-diagnostics-buffer-mode)
                (derived-mode . flymake-project-diagnostics-mode)
                (derived-mode . messages-buffer-mode)
                (derived-mode . backtrace-mode)))
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . 0.3)
         (dedicated . t)
         (preserve-size . (t . t)))
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
                ;; ,world-clock-buffer-name
                ))
         (prot-window-display-buffer-below-or-pop)
         (body-function . prot-window-select-fit-size))
       ("\\*\\(Calendar\\|Bookmark Annotation\\|ert\\).*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (dedicated . t)
         (window-height . fit-window-to-buffer))
       ;; NOTE 2022-09-10: The following is for `ispell-word', though
       ;; it only works because I override `ispell-display-buffer'
       ;; with `prot-spell-ispell-display-buffer' and change the
       ;; value of `ispell-choices-buffer'.
       ("\\*ispell-top-choices\\*.*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . fit-window-to-buffer))
       ;; same window

       ;; NOTE 2023-02-17: `man' does not fully obey the
       ;; `display-buffer-alist'.  It works for new frames and for
       ;; `display-buffer-below-selected', but otherwise is
       ;; unpredictable.  See `Man-notify-method'.
       ((or . ((derived-mode . Man-mode)
                (derived-mode . woman-mode)
                "\\*\\(Man\\|woman\\).*"))
         (display-buffer-same-window)))))

(use-package recentf
  :ensure nil
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-saved-items 100)
  (backup-directory-alist
	`((".*" . ,temporary-file-directory)))
  (recentf-exclude
	'(
	   ".*!\\([^!]*!\\).*" ;; matches any string with more than one exclamation mark
	   "/\\.cache.*/.*"    ;; matches any string that includes a directory named .cache
	   "/tmp/.*"           ;; matches any string that includes directory named tmp
	   "/.emacs\\.d/.*"    ;; matches any string that includes directory .emacs.d
	   ))
  :hook ((kill-emacs save-buffer) . (lambda ()
									  (recentf-save-list)))
  (elpaca-after-init . recentf-mode)
  :config
  )

(use-package files
  :ensure nil
  :hook (elpaca-after-init . auto-save-visited-mode)
  :config
  (auto-save-visited-mode +1)
  :custom
  (auto-save-default nil))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-use-notify nil)
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
  (setq tramp-verbose 0)
  (add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-connection-properties
			   (list (regexp-quote "/ssh:ag-nehrbash:")
					 "remote-shell" "/usr/bin/bash"
					 "direct-async-process" t
					 "tramp-direct-async" t))
  (add-to-list 'tramp-connection-properties
			   (list (regexp-quote "/docker:")
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
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-<return>") 'sanityinc/newline-at-end-of-line)

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

(defun sanityinc/backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
	(cond ((elt ppss 3)
		   (goto-char (elt ppss 8))
		   (sanityinc/backward-up-sexp (1- arg)))
		  ((backward-up-list arg)))))
(global-set-key [remap backward-up-list] 'sanityinc/backward-up-sexp) ; C-M-u, C-M-up

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
  :hook ((prog-mode text-mode conf-mode web-mode sql-mode) . sanityinc/show-trailing-whitespace)
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
 	'("0" . meow-digit-argument)
 	'("f ." . find-file-at-point))
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
 	'("i" . meow-prev)
 	'("I" . meow-prev-expand)
 	'("f" . meow-find)
 	'("g" . meow-cancel-selection)
 	'("G" . meow-grab)
 	'("n" . meow-left)
 	'("N" . meow-left-expand)
 	'("o" . meow-right)
 	'("O" . meow-right-expand)
 	'("j" . meow-join)
 	'("k" . meow-kill)
 	'("l" . meow-line)
 	'("L" . meow-goto-line)
 	'("m" . meow-mark-word)
 	'("M" . meow-mark-symbol)
 	'("e" . meow-next)
 	'("E" . meow-next-expand)
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
(meow-global-mode 1))

(use-package avy
  :commands avy-goto-char-timer
  :custom (avy-timeout-seconds 0.3)
  :bind ("M-j" . avy-goto-char-timer)
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
  ("C-x g" . sn/project-menu)
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

(transient-mark-mode t)
(delete-selection-mode t)
;; (kill-ring-deindent-mode t) ; emacs 30
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
  (aw-ignore-current t)
  :bind ("M-o" . ace-window))

(use-package windswap
  :config
  (windmove-default-keybindings 'control)
  (windswap-default-keybindings 'shift 'control))

(use-package sudo-edit
  :commands (sudo-edit))

(use-package fullframe)

(defun revert-all-buffers-no-confirm ()
  "Revert all buffers without confirmation."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (buffer-modified-p))
        (revert-buffer t t t)))))

(use-package desktop
  :ensure nil
  :if (daemonp)
  :custom
  (desktop-save t)
  :init
  (desktop-save-mode 1)
  (desktop-read)
  )

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))
(with-suppressed-message (save-buffer))

(use-package minibuffer
  :ensure nil
  :bind
  (:map minibuffer-local-map ("M-." . sn/minibuffer-fetch-symbol-at-point))
  (:map minibuffer-local-completion-map
  	("<backtab>" . minibuffer-force-complete))
  :custom
  (enable-recursive-minibuffers t)
  (minibuffer-eldef-shorten-default t)
  (read-minibuffer-restore-windows t) ;; don't revert to original layout after cancel.
  (resize-mini-windows t)
  (minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  :hook
  (completion-list-mode . force-truncate-lines)
  (minibuffer-setup . (lambda ()
  						(cursor-intangible-mode 1)))
  :config
  (defun sn/minibuffer-fetch-symbol-at-point ()
	"Fetch the current or next symbol at point in the current buffer while in minibuffer."
	(interactive)
	(let ((symbol (with-minibuffer-selected-window
					(or (thing-at-point 'symbol) 
                      (save-excursion
                        (forward-symbol 1)
                        (thing-at-point 'symbol))))))
      (when symbol
		(insert symbol))))
  (minibuffer-depth-indicate-mode)
  (minibuffer-electric-default-mode))

(use-package vertico
  :hook (elpaca-after-init .  vertico-mode)
  :bind
  (:map vertico-map
	("M-j" . vertico-quick-insert)
	("C-q" . vertico-quick-exit))
  :config
  (setq vertico-multiform-commands
    '((consult-imenu buffer indexed)
	   (consult-line reverse)
	   (project-switch-project posframe
         (vertico-posframe-poshandler . posframe-poshandler-frame-top-center))
       (t posframe)))
  (setq vertico-multiform-categories
    '((file grid)
	   (jinx grid (vertico-grid-annotate . 30))
       (consult-grep reverse)))
  (vertico-multiform-mode 1))
(use-package marginalia
  :init
  (marginalia-mode)
  :custom
  (marginalia-align 'right)
  :bind
  (:map minibuffer-local-map
	("M-a" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))
(use-package all-the-icons-completion
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

(use-package vertico-posframe
  :after vertico
  :custom
  (vertico-posframe-width 180)
  (vertico-posframe-vertico-multiform-key "M-m")
  :config
  ;; don't change colors
  (defun my-vertico-posframe-get-border-color-advice (&rest _args)
  "Always return the color of `vertico-posframe-border`."
	(face-attribute 'vertico-posframe-border
	  :background nil t))
  (advice-add 'vertico-posframe--get-border-color :override #'my-vertico-posframe-get-border-color-advice)
  (vertico-posframe-mode 1)
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
  (orderless-matching-styles 'orderless-regexp)
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (completion-lazy-hilit t)
  (completion-flex-nospace t)
  (completion-category-defaults nil)
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-enable-key "r"))

(use-package consult
  :after vertico
  :bind
  ("C-s" . consult-line)
  ("C-r" . consult-ripgrep)
  ("M-S" . consult-line-multi)
  ("C-c M-x" . consult-mode-command)
  ("C-c h" . consult-history)
  ("C-c k" . consult-kmacro)
  ("C-c m" . consult-man)
  ("C-c i" . consult-info)
  ([remap Info-search] . consult-info)
  ("C-x M-:" . consult-complex-command)
  ("C-x b" . consult-buffer)
  ("C-x f" . consult-buffer-other-window)
  ("C-x 5 b" . consult-buffer-other-frame)
  ("C-x t b" . consult-buffer-other-tab)
  ("C-x r b" . consult-bookmark)
  ("M-\"" . consult-register)
  ("M-'" . consult-register-store)
  ("C-M-'" . consult-register)
  ("M-y" . consult-yank-pop)
  ("M-SPC e" . consult-compile-error)
  ("M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("M-g o" . consult-outline)
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
  (setq consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))
  (defun vc-modified-file ()
	"Use completion to go to a modified file in the Git repository."
	(interactive)
	(let* ((default-directory (vc-root-dir))  ;; Ensures we're in the root of the project
			(git-cmd "git status --porcelain=v1 --untracked-files=no")  ;; Git command to get modified files
			(files (split-string (shell-command-to-string git-cmd) "\n" t))
			(modified-files (mapcar (lambda (line)
									  (string-trim (substring line 3))) files))
			;; Use completing-read to select the file
			(selected-file (completing-read "Goto vc file: " modified-files nil t)))
	  (when selected-file
		(find-file selected-file))))
  (defvar consult--source-vc-modified-file
	`(:name     "VC Modified File"
	   :narrow   ?g
	   :category file
	   :face     consult-file
	   :history  file-name-history
	   :state    ,#'consult--file-state
	   :new
	   ,(lambda (file)
		  (consult--file-action
			(expand-file-name file (vc-root-dir))))
	   :enabled
	   ,(lambda ()
		  (vc-root-dir))
	   :items
	   ,(lambda ()
		  (when-let (root (vc-root-dir))
			(let ((len (length root))
				   (ht (consult--buffer-file-hash))
				   items)
			  (dolist (file (vc-modified-files) (nreverse items))
				(unless (eq (aref file 0) ?/)
				  (let (file-name-handler-alist) ;; No Tramp slowdown please.
					(setq file (expand-file-name file))))
				(when (and (not (gethash file ht)) (string-prefix-p root file))
				  (let ((part (substring file len)))
					(when (equal part "") (setq part "./"))
					(put-text-property 0 1 'multi-category `(file . ,file) part)
					(push part items))))))))
	"VC modified file candidate source for `consult-buffer'.")
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
				   :exclude '("\\*."
							   "\\#."
							   "shell"
							   "shell\\:\\ "
							   "^magit"
							   "[\\.]org$")))))

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
	   consult--source-vc-modified-file
	   consult--source-vterm
	   consult--source-project-recent-file
	   consult--source-star)))

(use-package consult-xref-stack
  :ensure (:host github :repo "brett-lempereur/consult-xref-stack")
  :bind
  ("C-," . consult-xref-stack-backward))

(use-package embark
  :bind
  ("M-SPC SPC" . embark-act)
  ("C-;" . embark-dwim)
  ("C-h B" . embark-bindings)
  (:map minibuffer-mode-map
		("M-SPC" . embark-act))
  (:map embark-region-map
		("w" . google-this)
		("g" . gptel))
  :custom
  (embark-mixed-indicator-delay 0.6)
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators ; the default 
   '(embark-mixed-indicator
	 embark-highlight-indicator
	 embark-isearch-highlight-indicator)))
(use-package embark-consult
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
  :after orderless
  :hook (((prog-mode conf-mode yaml-mode) . sn/corfu-basic))
  :bind (:map corfu-map ("M-SPC" . corfu-insert-separator)
		  ("TAB" . corfu-next)
		  ([tab] . corfu-next)
		  ("S-TAB" . corfu-previous)
		  ([backtab] . corfu-previous))
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt) ;; Always preselect the prompt
  ;; default/writting settings, see sn/corfu-basic for coding completion
  (tab-first-completion t)
  (tab-always-indent 'complete)
  (corfu-auto-delay 0.8)
  (corfu-popupinfo-delay 0.2)
  (corfu-quit-no-match 'separator)
  (corfu-auto-prefix 2)
  (completion-cycle-threshold nil)
  :init
  (global-corfu-mode t)
  (global-completion-preview-mode t)
  :config
  (setq completion-preview-minimum-symbol-length 2)
  ;; Non-standard commands to that should show the preview:
  ;; Org mode has a custom `self-insert-command'
  (push 'org-self-insert-command completion-preview-commands)
  ;; Paredit has a custom `delete-backward-char' command
  (push 'paredit-backward-delete completion-preview-commands)
  ;; Bindings that take effect when the preview is shown:
  ;; Cycle the completion candidate that the preview shows
  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate)
  ;; Convenient alternative to C-i after typing one of the above
  (keymap-set completion-preview-active-mode-map "M-i" #'completion-preview-insert)
  (defun my-custom-completion-at-point-functions ()
 	"Define your custom list of completion at point functions here."
 	(list #'codeium-completion-at-point))
  (defun my-custom-completion-preview--update-advice (orig-fun &rest args)
 	(let ((completion-at-point-functions (my-custom-completion-at-point-functions)))
      (apply orig-fun args)))
  ;; (advice-add 'completion-preview--update :around #'my-custom-completion-preview--update-advice)
  
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
	  corfu-auto t
	  corfu-auto-delay 0
	  corfu-quit-no-match t
	  completion-styles '(orderless-fast basic)
	  corfu-popupinfo-delay .8))
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
  :custom ((kind-icon-default-face 'corfu-default))
  :config
  (plist-put kind-icon-default-style :height 0.9)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :demand t ; for eglot
  :bind
  ("M-/" . completion-at-point) ;; overwrite dabbrev-completion binding with capf
  ("C-c /" . sn/cape)
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  (cape-dabbrev-min-length 2)
  :config
  (transient-define-prefix sn/cape ()
	"explicit Completion type"
	[[
	  ("d" "Dabbrev" cape-dabbrev)
	  ("s" "Spelling" cape-dict)
	  ("k" "Keyword" cape-keyword)
	  ("l" "Line" cape-line)]
	 [
	  ("f" "File" cape-file)
	  ("h" "History" cape-history)
	  ("a" "Abbrev" cape-abbrev)
	  ("q" "Quit" transient-quit-one)]
	 [
	  ("e" "Elisp Symbol" cape-elisp-symbol)
	  ("E" "Elisp Block" cape-elisp-block)
	  ("t" "Tags" complete-tag)
	  ]])
  :config
  (setq completion-at-point-functions
	'(#'cape-dict
	   #'cape-dabbrev
	   #'cape-file
	   #'cape-abbrev))
  (defun sn/cape-in-string ()
	(cape-wrap-inside-string
	  (cape-capf-super
		#'cape-file
		#'cape-dabbrev
		#'cape-dict)))
  (defun sn/cape-in-comment ()
	(cape-wrap-inside-comment
	  (cape-capf-super
		#'cape-dabbrev
		#'cape-dict
		#'cape-file)))
  (defun sn/cape-in-code ()
	(cape-wrap-nonexclusive
	  (cape-capf-inside-code
		(cape-capf-super
		  #'eglot-completion-at-point
		  #'yasnippet-capf
		  #'cape-dabbrev)))))

(use-package yasnippet
  :hook ((text-mode
		  prog-mode
		  conf-mode) . yas-minor-mode-on)
  :bind ("C-c s" . yas-insert-snippet)
  :custom
  (yas-verbosity 1)
  (yas-snippet-dir "~/.emacs.d/snippets")
  (yas-wrap-around-region t))
(use-package yasnippet-snippets
  :after yasnippet)
(use-package yasnippet-capf
  :after yasnippet)

(use-package abbrev
  :ensure nil
  :init
  (setq-default abbrev-mode t)
  :config
  ;; common auto correction like abbrevs
  (define-abbrev-table 'global-abbrev-table '(
											   ("abbout" "about" nil 0)
											   ("abotu" "about" nil 0)
											   ("abouta" "about a" nil 0)
											   ("aboutit" "about it" nil 0)
											   ("aboutthe" "about the" nil 0)
											   ("abscence" "absence" nil 0)
											   ("accesories" "accessories" nil 0)
											   ("accidant" "accident" nil 0)
											   ("accomodate" "accommodate" nil 0)
											   ("accordingto" "according to" nil 0)
											   ("accross" "across" nil 0)
											   ("acheive" "achieve" nil 0)
											   ("acheived" "achieved" nil 0)
											   ("acheiving" "achieving" nil 0)
											   ("acn" "can" nil 0)
											   ("acommodate" "accommodate" nil 0)
											   ("acomodate" "accommodate" nil 0)
											   ("acomplished" "accomplished" nil 0)
											   ("actualyl" "actually" nil 0)
											   ("acurate" "accurate" nil 0)
											   ("addictional" "additional" nil 0)
											   ("additinal" "additional" nil 0)
											   ("addtional" "additional" nil 0)
											   ("addtions" "additions" nil 0)
											   ("adequit" "adequate" nil 0)
											   ("adequite" "adequate" nil 0)
											   ("adn" "and" nil 0)
											   ("advanage" "advantage" nil 0)
											   ("affraid" "afraid" nil 0)
											   ("afterthe" "after the" nil 0)
											   ("aganist" "against" nil 0)
											   ("aggresive" "aggressive" nil 0)
											   ("agian" "again" nil 0)
											   ("agreemeent" "agreement" nil 0)
											   ("agreemeents" "agreements" nil 0)
											   ("agreemnet" "agreement" nil 0)
											   ("agreemnets" "agreements" nil 0)
											   ("agressive" "aggressive" nil 0)
											   ("ahppen" "happen" nil 0)
											   ("ahve" "have" nil 0)
											   ("allwasy" "always" nil 0)
											   ("allwyas" "always" nil 0)
											   ("almots" "almost" nil 0)
											   ("almsot" "almost" nil 0)
											   ("alomst" "almost" nil 0)
											   ("alot" "a lot" nil 0)
											   ("alraedy" "already" nil 0)
											   ("alreayd" "already" nil 0)
											   ("alreday" "already" nil 0)
											   ("alwasy" "always" nil 0)
											   ("alwats" "always" nil 0)
											   ("alway" "always" nil 0)
											   ("alwyas" "always" nil 0)
											   ("amde" "made" nil 0)
											   ("ameria" "America" nil 0)
											   ("amke" "make" nil 0)
											   ("amkes" "makes" nil 0)
											   ("anbd" "and" nil 0)
											   ("andone" "and one" nil 0)
											   ("andt he" "and the" nil 0)
											   ("andteh" "and the" nil 0)
											   ("andthe" "and the" nil 0)
											   ("anothe" "another" nil 0)
											   ("anual" "annual" nil 0)
											   ("apache" "Apache" nil 0)
											   ("apparant" "apparent" nil 0)
											   ("apparrent" "apparent" nil 0)
											   ("appearence" "appearance" nil 0)
											   ("appeares" "appears" nil 0)
											   ("applicaiton" "application" nil 0)
											   ("applicaitons" "applications" nil 0)
											   ("applyed" "applied" nil 0)
											   ("appointiment" "appointment" nil 0)
											   ("approrpiate" "appropriate" nil 0)
											   ("approrpriate" "appropriate" nil 0)
											   ("aquisition" "acquisition" nil 0)
											   ("aquisitions" "acquisitions" nil 0)
											   ("arent" "aren't" nil 0)
											   ("arguement" "argument" nil 0)
											   ("arguements" "arguments" nil 0)
											   ("arnt" "aren't" nil 0)
											   ("arond" "around" nil 0)
											   ("artical" "article" nil 0)
											   ("articel" "article" nil 0)
											   ("asdvertising" "advertising" nil 0)
											   ("assistent" "assistant" nil 0)
											   ("asthe" "as the" nil 0)
											   ("atention" "attention" nil 0)
											   ("atmospher" "atmosphere" nil 0)
											   ("attentioin" "attention" nil 0)
											   ("atthe" "at the" nil 0)
											   ("audeince" "audience" nil 0)
											   ("audiance" "audience" nil 0)
											   ("authetication" "authentication" nil 0)
											   ("availalbe" "available" nil 0)
											   ("awya" "away" nil 0)
											   ("aywa" "away" nil 0)
											   ("bakc" "back" nil 0)
											   ("balence" "balance" nil 0)
											   ("ballance" "balance" nil 0)
											   ("baout" "about" nil 0)
											   ("bcak" "back" nil 0)
											   ("beacause" "because" nil 0)
											   ("beacuse" "because" nil 0)
											   ("becasue" "because" nil 0)
											   ("becaus" "because" nil 0)
											   ("bc" "because" nil 0)
											   ("becausea" "because a" nil 0)
											   ("becauseof" "because of" nil 0)
											   ("becausethe" "because the" nil 0)
											   ("becauseyou" "because you" nil 0)
											   ("becomeing" "becoming" nil 0)
											   ("becomming" "becoming" nil 0)
											   ("becuase" "because" nil 0)
											   ("becuse" "because" nil 0)
											   ("befoer" "before" nil 0)
											   ("beggining" "beginning" nil 0)
											   ("begining" "beginning" nil 0)
											   ("beginining" "beginning" nil 0)
											   ("behabviour" "behaviour" nil 0)
											   ("behaivior" "behaviour" nil 0)
											   ("behavour" "behaviour" nil 0)
											   ("beleiev" "believe" nil 0)
											   ("beleieve" "believe" nil 0)
											   ("beleif" "belief" nil 0)
											   ("beleive" "believe" nil 0)
											   ("beleived" "believed" nil 0)
											   ("beleives" "believes" nil 0)
											   ("beliefe" "belief" nil 0)
											   ("beliveve" "believe" nil 0)
											   ("benifit" "benefit" nil 0)
											   ("benifits" "benefits" nil 0)
											   ("betwen" "between" nil 0)
											   ("beutiful" "beautiful" nil 0)
											   ("blase" "blase" nil 0)
											   ("boxs" "boxes" nil 0)
											   ("brodcast" "broadcast" nil 0)
											   ("butthe" "but the" nil 0)
											   ("bve" "be" nil 0)
											   ("cafe" "cafe" nil 0)
											   ("caharcter" "character" nil 0)
											   ("calcullated" "calculated" nil 0)
											   ("calulated" "calculated" nil 0)
											   ("candidtae" "candidate" nil 0)
											   ("candidtaes" "candidates" nil 0)
											   ("caontains" "contains" nil 0)
											   ("capabilites" "capabilities" nil 0)
											   ("catagory" "category" nil 0)
											   ("categiory" "category" nil 0)
											   ("certian" "certain" nil 0)
											   ("challange" "challenge" nil 0)
											   ("challanges" "challenges" nil 0)
											   ("chaneg" "change" nil 0)
											   ("chanegs" "changes" nil 0)
											   ("changable" "changeable" nil 0)
											   ("changeing" "changing" nil 0)
											   ("changng" "changing" nil 0)
											   ("charachter" "character" nil 0)
											   ("charachters" "characters" nil 0)
											   ("charactor" "character" nil 0)
											   ("charecter" "character" nil 0)
											   ("charector" "character" nil 0)
											   ("cheif" "chief" nil 0)
											   ("chekc" "check" nil 0)
											   ("chnage" "change" nil 0)
											   ("cieling" "ceiling" nil 0)
											   ("circut" "circuit" nil 0)
											   ("claer" "clear" nil 0)
											   ("claered" "cleared" nil 0)
											   ("claerly" "clearly" nil 0)
											   ("cliant" "client" nil 0)
											   ("cliche" "cliche" nil 0)
											   ("cna" "can" nil 0)
											   ("colection" "collection" nil 0)
											   ("comanies" "companies" nil 0)
											   ("comany" "company" nil 0)
											   ("comapnies" "companies" nil 0)
											   ("comapny" "company" nil 0)
											   ("combintation" "combination" nil 0)
											   ("comited" "committed" nil 0)
											   ("comittee" "committee" nil 0)
											   ("commadn" "command" nil 0)
											   ("comming" "coming" nil 0)
											   ("commitee" "committee" nil 0)
											   ("committe" "committee" nil 0)
											   ("committment" "commitment" nil 0)
											   ("committments" "commitments" nil 0)
											   ("committy" "committee" nil 0)
											   ("comntain" "contain" nil 0)
											   ("comntains" "contains" nil 0)
											   ("compair" "compare" nil 0)
											   ("compatable" "compatible" nil 0)
											   ("compleated" "completed" nil 0)
											   ("compleatly" "completely" nil 0)
											   ("compleatness" "completeness" nil 0)
											   ("completly" "completely" nil 0)
											   ("completness" "completeness" nil 0)
											   ("complteted" "completed" nil 0)
											   ("composate" "composite" nil 0)
											   ("compteted" "completed" nil 0)
											   ("comtain" "contain" nil 0)
											   ("comtains" "contains" nil 0)
											   ("comunicate" "communicate" nil 0)
											   ("comunity" "community" nil 0)
											   ("condersider" "consider" nil 0)
											   ("condolances" "condolences" nil 0)
											   ("conected" "connected" nil 0)
											   ("conferance" "conference" nil 0)
											   ("configration" "configuration" nil 0)
											   ("confirmmation" "confirmation" nil 0)
											   ("conjuntion" "conjunction" nil 0)
											   ("considerit" "considerate" nil 0)
											   ("considerite" "considerate" nil 0)
											   ("consistant" "consistent" nil 0)
											   ("consonent" "consonant" nil 0)
											   ("conspiricy" "conspiracy" nil 0)
											   ("constuction" "construction" nil 0)
											   ("consultent" "consultant" nil 0)
											   ("consumeable" "consumable" nil 0)
											   ("contitions" "conditions" nil 0)
											   ("controlable" "controllable" nil 0)
											   ("convertable" "convertible" nil 0)
											   ("cooparate" "cooperate" nil 0)
											   ("cooporate" "cooperate" nil 0)
											   ("corproation" "corporation" nil 0)
											   ("corproations" "corporations" nil 0)
											   ("corrospond" "correspond" nil 0)
											   ("corruptable" "corruptible" nil 0)
											   ("cotten" "cotton" nil 0)
											   ("coudl" "could" nil 0)
											   ("coudln" "couldn" nil 0)
											   ("coudn" "couldn" nil 0)
											   ("couldnt" "couldn't" nil 0)
											   ("couldthe" "could the" nil 0)
											   ("cpoy" "copy" nil 0)
											   ("creme" "creme" nil 0)
											   ("ctaegory" "category" nil 0)
											   ("cusotmer" "customer" nil 0)
											   ("cusotmers" "customers" nil 0)
											   ("cutsomer" "customer" nil 0)
											   ("cutsomers" "customers" nil 0)
											   ("cxan" "can" nil 0)
											   ("danceing" "dancing" nil 0)
											   ("dcument" "document" nil 0)
											   ("deatils" "details" nil 0)
											   ("decison" "decision" nil 0)
											   ("decisons" "decisions" nil 0)
											   ("decor" "decor" nil 0)
											   ("defendent" "defendant" nil 0)
											   ("definately" "definitely" nil 0)
											   ("definded" "defined" nil 0)
											   ("dependances" "dependencies" nil 0)
											   ("deptartment" "department" nil 0)
											   ("desicion" "decision" nil 0)
											   ("desicions" "decisions" nil 0)
											   ("desision" "decision" nil 0)
											   ("desisions" "decisions" nil 0)
											   ("detente" "detente" nil 0)
											   ("determin" "determine" nil 0)
											   ("determins" "determine" nil 0)
											   ("develeoprs" "developers" nil 0)
											   ("devellop" "develop" nil 0)
											   ("develloped" "developed" nil 0)
											   ("develloper" "developer" nil 0)
											   ("devellopers" "developers" nil 0)
											   ("develloping" "developing" nil 0)
											   ("devellopment" "development" nil 0)
											   ("devellopments" "developments" nil 0)
											   ("devellops" "develop" nil 0)
											   ("develope" "develop" nil 0)
											   ("developement" "development" nil 0)
											   ("developements" "developments" nil 0)
											   ("developor" "developer" nil 0)
											   ("developors" "developers" nil 0)
											   ("develpment" "development" nil 0)
											   ("devloped" "developed" nil 0)
											   ("diaplay" "display" nil 0)
											   ("didint" "didn't" nil 0)
											   ("didnot" "did not" nil 0)
											   ("didnt" "didn't" nil 0)
											   ("difefrent" "different" nil 0)
											   ("diferences" "differences" nil 0)
											   ("differance" "difference" nil 0)
											   ("differances" "differences" nil 0)
											   ("differant" "different" nil 0)
											   ("differemt" "different" nil 0)
											   ("differnt" "different" nil 0)
											   ("diffrent" "different" nil 0)
											   ("directer" "director" nil 0)
											   ("directers" "directors" nil 0)
											   ("directiosn" "direction" nil 0)
											   ("disatisfied" "dissatisfied" nil 0)
											   ("discoverd" "discovered" nil 0)
											   ("disign" "design" nil 0)
											   ("dispaly" "display" nil 0)
											   ("dissonent" "dissonant" nil 0)
											   ("distribusion" "distribution" nil 0)
											   ("distrubution" "distribution" nil 0)
											   ("divsion" "division" nil 0)
											   ("docuement" "documents" nil 0)
											   ("docuemnt" "document" nil 0)
											   ("documetn" "document" nil 0)
											   ("documnet" "document" nil 0)
											   ("documnets" "documents" nil 0)
											   ("doese" "does" nil 0)
											   ("doesnt" "doesn't" nil 0)
											   ("doign" "doing" nil 0)
											   ("doimg" "doing" nil 0)
											   ("doind" "doing" nil 0)
											   ("dollers" "dollars" nil 0)
											   ("donig" "doing" nil 0)
											   ("dont" "don't" nil 0)
											   ("dont" "don't" nil 0)
											   ("dosnt" "doesn't" nil 0)
											   ("driveing" "driving" nil 0)
											   ("drnik" "drink" nil 0)
											   ("eclair" "eclair" nil 0)
											   ("efel" "feel" nil 0)
											   ("effecient" "efficient" nil 0)
											   ("efort" "effort" nil 0)
											   ("eforts" "efforts" nil 0)
											   ("ehr" "her" nil 0)
											   ("eligable" "eligible" nil 0)
											   ("emacs" "Emacs" nil 0)
											   ("embarass" "embarrass" nil 0)
											   ("emigre" "emigre" nil 0)
											   ("enahancements" "enhancements" nil 0)
											   ("english" "English" nil 0)
											   ("enought" "enough" nil 0)
											   ("entree" "entree" nil 0)
											   ("equippment" "equipment" nil 0)
											   ("equivalant" "equivalent" nil 0)
											   ("esle" "else" nil 0)
											   ("especally" "especially" nil 0)
											   ("especialyl" "especially" nil 0)
											   ("espesially" "especially" nil 0)
											   ("excellant" "excellent" nil 0)
											   ("excercise" "exercise" nil 0)
											   ("exchagne" "exchange" nil 0)
											   ("exchagnes" "exchanges" nil 0)
											   ("excitment" "excitement" nil 0)
											   ("exhcange" "exchange" nil 0)
											   ("exhcanges" "exchanges" nil 0)
											   ("experiance" "experience" nil 0)
											   ("experienc" "experience" nil 0)
											   ("exprience" "experience" nil 0)
											   ("exprienced" "experienced" nil 0)
											   ("eyt" "yet" nil 0)
											   ("facade" "facade" nil 0)
											   ("faeture" "feature" nil 0)
											   ("faetures" "features" nil 0)
											   ("familair" "familiar" nil 0)
											   ("familar" "familiar" nil 0)
											   ("familliar" "familiar" nil 0)
											   ("fammiliar" "familiar" nil 0)
											   ("feild" "field" nil 0)
											   ("feilds" "fields" nil 0)
											   ("fianlly" "finally" nil 0)
											   ("fidn" "find" nil 0)
											   ("fifith" "fifth" nil 0)
											   ("finalyl" "finally" nil 0)
											   ("finnally" "finally" nil 0)
											   ("finnish" "finish" nil 0)
											   ("firends" "friends" nil 0)
											   ("firts" "first" nil 0)
											   ("fixit" "fix it" nil 0)
											   ("follwo" "follow" nil 0)
											   ("follwoing" "following" nil 0)
											   ("foloowing" "following" nil 0)
											   ("fora" "for a" nil 0)
											   ("foriegn" "foreign" nil 0)
											   ("forthe" "for the" nil 0)
											   ("forwrd" "forward" nil 0)
											   ("forwrds" "forwards" nil 0)
											   ("foudn" "found" nil 0)
											   ("foward" "forward" nil 0)
											   ("fowards" "forwards" nil 0)
											   ("freind" "friend" nil 0)
											   ("freindly" "friendly" nil 0)
											   ("freinds" "friends" nil 0)
											   ("frmo" "from" nil 0)
											   ("fromt he" "from the" nil 0)
											   ("fromthe" "from the" nil 0)
											   ("furneral" "funeral" nil 0)
											   ("fwe" "few" nil 0)
											   ("garantee" "guarantee" nil 0)
											   ("gaurd" "guard" nil 0)
											   ("gemeral" "general" nil 0)
											   ("gerat" "great" nil 0)
											   ("gerneral" "general" nil 0)
											   ("geting" "getting" nil 0)
											   ("gettin" "getting" nil 0)
											   ("gievn" "given" nil 0)
											   ("giveing" "giving" nil 0)
											   ("gloabl" "global" nil 0)
											   ("goign" "going" nil 0)
											   ("gonig" "going" nil 0)
											   ("govenment" "government" nil 0)
											   ("goverment" "government" nil 0)
											   ("gruop" "group" nil 0)
											   ("gruops" "groups" nil 0)
											   ("grwo" "grow" nil 0)
											   ("guidlines" "guidelines" nil 0)
											   ("hadbeen" "had been" nil 0)
											   ("hadnt" "hadn't" nil 0)
											   ("haev" "have" nil 0)
											   ("hapen" "happen" nil 0)
											   ("hapened" "happened" nil 0)
											   ("hapening" "happening" nil 0)
											   ("hapens" "happens" nil 0)
											   ("happend" "happened" nil 0)
											   ("hasbeen" "has been" nil 0)
											   ("hasnt" "hasn't" nil 0)
											   ("havebeen" "have been" nil 0)
											   ("haveing" "having" nil 0)
											   ("haven;t" "haven't" nil 0)
											   ("hda" "had" nil 0)
											   ("hearign" "hearing" nil 0)
											   ("heire" "he-ire" nil 0)
											   ("helpdesk" "help-desk" nil 0)
											   ("helpfull" "helpful" nil 0)
											   ("herat" "heart" nil 0)
											   ("hesaid" "he said" nil 0)
											   ("hewas" "he was" nil 0)
											   ("hge" "he" nil 0)
											   ("hier" "heir" nil 0)
											   ("hismelf" "himself" nil 0)
											   ("hiten" "hitting" nil 0)
											   ("hitten" "hitting" nil 0)
											   ("hlep" "help" nil 0)
											   ("howerver" "however" nil 0)
											   ("hsa" "has" nil 0)
											   ("hsi" "his" nil 0)
											   ("hte" "the" nil 0)
											   ("htere" "there" nil 0)
											   ("htese" "these" nil 0)
											   ("htey" "they" nil 0)
											   ("hting" "thing" nil 0)
											   ("htink" "think" nil 0)
											   ("htis" "this" nil 0)
											   ("htp:" "http:" nil 0)
											   ("http:\\\\" "http://" nil 0)
											   ("httpL" "http:" nil 0)
											   ("hvae" "have" nil 0)
											   ("hvaing" "having" nil 0)
											   ("hwich" "which" nil 0)
											   ("i" "I" nil 0)
											   ("idae" "idea" nil 0)
											   ("idaes" "ideas" nil 0)
											   ("identifiy" "identify" nil 0)
											   ("identofy" "identify" nil 0)
											   ("ihs" "his" nil 0)
											   ("imediate" "immediate" nil 0)
											   ("imediatly" "immediately" nil 0)
											   ("immediatly" "immediately" nil 0)
											   ("impilies" "implies" nil 0)
											   ("implemenation" "implementation" nil 0)
											   ("importent" "important" nil 0)
											   ("importnat" "important" nil 0)
											   ("impossable" "impossible" nil 0)
											   ("improvemnt" "improvement" nil 0)
											   ("improvment" "improvement" nil 0)
											   ("includ" "include" nil 0)
											   ("indecate" "indicate" nil 0)
											   ("indenpendence" "independence" nil 0)
											   ("indenpendent" "independent" nil 0)
											   ("indepedent" "independent" nil 0)
											   ("independance" "independence" nil 0)
											   ("independant" "independent" nil 0)
											   ("influance" "influence" nil 0)
											   ("infomation" "information" nil 0)
											   ("informatoin" "information" nil 0)
											   ("inital" "initial" nil 0)
											   ("initalization" "initialization" nil 0)
											   ("instaleld" "installed" nil 0)
											   ("insted" "instead" nil 0)
											   ("insurence" "insurance" nil 0)
											   ("inteh" "in the" nil 0)
											   ("interum" "interim" nil 0)
											   ("inthe" "in the" nil 0)
											   ("inturn" "in turn" nil 0)
											   ("invitaion" "invitation" nil 0)
											   ("invstigated" "investigated" nil 0)
											   ("inwhich" "in which" nil 0)
											   ("isnt" "isn't" nil 0)
											   ("isthe" "is the" nil 0)
											   ("itis" "it is" nil 0)
											   ("ititial" "initial" nil 0)
											   ("itll" "it'll" nil 0)
											   ("itnerest" "interest" nil 0)
											   ("itnerested" "interested" nil 0)
											   ("itneresting" "interesting" nil 0)
											   ("itnerests" "interests" nil 0)
											   ("itwas" "it was" nil 0)
											   ("ivestigative" "investigative" nil 0)
											   ("iwll" "will" nil 0)
											   ("iwth" "with" nil 0)
											   ("jsut" "just" nil 0)
											   ("jugment" "judgment" nil 0)
											   ("knowldge" "knowledge" nil 0)
											   ("knowlege" "knowledge" nil 0)
											   ("knwo" "know" nil 0)
											   ("knwon" "known" nil 0)
											   ("knwos" "knows" nil 0)
											   ("konw" "know" nil 0)
											   ("konwn" "known" nil 0)
											   ("konws" "knows" nil 0)
											   ("labratory" "laboratory" nil 0)
											   ("languange" "language" nil 0)
											   ("lastyear" "last year" nil 0)
											   ("learnign" "learning" nil 0)
											   ("lenght" "length" nil 0)
											   ("levle" "level" nil 0)
											   ("libary" "library" nil 0)
											   ("librarry" "library" nil 0)
											   ("librery" "library" nil 0)
											   ("liek" "like" nil 0)
											   ("liekd" "liked" nil 0)
											   ("lieutenent" "lieutenant" nil 0)
											   ("liev" "live" nil 0)
											   ("likly" "likely" nil 0)
											   ("lisense" "license" nil 0)
											   ("littel" "little" nil 0)
											   ("litttle" "little" nil 0)
											   ("liuke" "like" nil 0)
											   ("liveing" "living" nil 0)
											   ("loev" "love" nil 0)
											   ("lonly" "lonely" nil 0)
											   ("lookign" "looking" nil 0)
											   ("lookup" "look up" nil 0)
											   ("maintenence" "maintenance" nil 0)
											   ("makeing" "making" nil 0)
											   ("managment" "management" nil 0)
											   ("mantain" "maintain" nil 0)
											   ("marraige" "marriage" nil 0)
											   ("memeber" "member" nil 0)
											   ("merchent" "merchant" nil 0)
											   ("mesage" "message" nil 0)
											   ("mesages" "messages" nil 0)
											   ("mispell" "misspell" nil 0)
											   ("mispelling" "misspelling" nil 0)
											   ("mispellings" "misspellings" nil 0)
											   ("mkae" "make" nil 0)
											   ("mkaes" "makes" nil 0)
											   ("mkaing" "making" nil 0)
											   ("moeny" "money" nil 0)
											   ("morgage" "mortgage" nil 0)
											   ("mroe" "more" nil 0)
											   ("mysefl" "myself" nil 0)
											   ("myu" "my" nil 0)
											   ("naive" "naive" nil 0)
											   ("necassarily" "necessarily" nil 0)
											   ("necassary" "necessary" nil 0)
											   ("neccessarily" "necessarily" nil 0)
											   ("neccessary" "necessary" nil 0)
											   ("necesarily" "necessarily" nil 0)
											   ("necesary" "necessary" nil 0)
											   ("negotiaing" "negotiating" nil 0)
											   ("nkow" "know" nil 0)
											   ("nothign" "nothing" nil 0)
											   ("nver" "never" nil 0)
											   ("nwe" "new" nil 0)
											   ("nwo" "now" nil 0)
											   ("obediant" "obedient" nil 0)
											   ("ocasion" "occasion" nil 0)
											   ("occassion" "occasion" nil 0)
											   ("occured" "occurred" nil 0)
											   ("occurence" "occurrence" nil 0)
											   ("occurences" "occurrences" nil 0)
											   ("occurrance" "occurrence" nil 0)
											   ("ocur" "occur" nil 0)
											   ("odbc" "ODBC" nil 0)
											   ("oeprator" "operator" nil 0)
											   ("ofits" "of its" nil 0)
											   ("ofthe" "of the" nil 0)
											   ("oging" "going" nil 0)
											   ("ohter" "other" nil 0)
											   ("omre" "more" nil 0)
											   ("oneof" "one of" nil 0)
											   ("onepoint" "one point" nil 0)
											   ("online" "on-line" nil 0)
											   ("ont he" "on the" nil 0)
											   ("onthe" "on the" nil 0)
											   ("onyl" "only" nil 0)
											   ("opcode" "op-code" nil 0)
											   ("operaror" "operator" nil 0)
											   ("oppasite" "opposite" nil 0)
											   ("opperation" "operation" nil 0)
											   ("oppertunity" "opportunity" nil 0)
											   ("opposate" "opposite" nil 0)
											   ("opposible" "opposable" nil 0)
											   ("opposit" "opposite" nil 0)
											   ("oppotunities" "opportunities" nil 0)
											   ("oppotunity" "opportunity" nil 0)
											   ("orginization" "organization" nil 0)
											   ("orginized" "organized" nil 0)
											   ("originial" "original" nil 0)
											   ("orignal" "original" nil 0)
											   ("otehr" "other" nil 0)
											   ("otu" "out" nil 0)
											   ("outof" "out of" nil 0)
											   ("overthe" "over the" nil 0)
											   ("ovverides" "overrides" nil 0)
											   ("owrk" "work" nil 0)
											   ("owuld" "would" nil 0)
											   ("oxident" "oxidant" nil 0)
											   ("papaer" "paper" nil 0)
											   ("parliment" "parliament" nil 0)
											   ("partof" "part of" nil 0)
											   ("paticular" "particular" nil 0)
											   ("paymetn" "payment" nil 0)
											   ("paymetns" "payments" nil 0)
											   ("pciture" "picture" nil 0)
											   ("peice" "piece" nil 0)
											   ("peices" "pieces" nil 0)
											   ("peolpe" "people" nil 0)
											   ("peopel" "people" nil 0)
											   ("percentof" "percent of" nil 0)
											   ("percentto" "percent to" nil 0)
											   ("performence" "performance" nil 0)
											   ("perhasp" "perhaps" nil 0)
											   ("perhpas" "perhaps" nil 0)
											   ("permanant" "permanent" nil 0)
											   ("perminent" "permanent" nil 0)
											   ("perscriptions" "prescriptions" nil 0)
											   ("personalyl" "personally" nil 0)
											   ("pleasent" "pleasant" nil 0)
											   ("plugin" "plug-in" nil 0)
											   ("poeple" "people" nil 0)
											   ("porblem" "problem" nil 0)
											   ("porblems" "problems" nil 0)
											   ("porvide" "provide" nil 0)
											   ("possable" "possible" nil 0)
											   ("postition" "position" nil 0)
											   ("potentialy" "potentially" nil 0)
											   ("prefure" "prefer" nil 0)
											   ("pregnent" "pregnant" nil 0)
											   ("prelease" "release" nil 0)
											   ("prerelease" "pre-release" nil 0)
											   ("presance" "presence" nil 0)
											   ("privleged" "privileged" nil 0)
											   ("probelm" "problem" nil 0)
											   ("probelms" "problems" nil 0)
											   ("problesm" "problems" nil 0)
											   ("proceding" "proceeding" nil 0)
											   ("proctetion" "protection" nil 0)
											   ("prominant" "prominent" nil 0)
											   ("protction" "protection" nil 0)
											   ("protoge" "protege" nil 0)
											   ("psoition" "position" nil 0)
											   ("ptogress" "progress" nil 0)
											   ("puting" "putting" nil 0)
											   ("pwoer" "power" nil 0)
											   ("quater" "quarter" nil 0)
											   ("quaters" "quarters" nil 0)
											   ("quesion" "question" nil 0)
											   ("quesions" "questions" nil 0)
											   ("questioms" "questions" nil 0)
											   ("questiosn" "questions" nil 0)
											   ("questoin" "question" nil 0)
											   ("quetion" "question" nil 0)
											   ("quetions" "questions" nil 0)
											   ("raidus" "RADIUS" nil 0)
											   ("realyl" "really" nil 0)
											   ("reccomend" "recommend" nil 0)
											   ("reccommend" "recommend" nil 0)
											   ("receieve" "receive" nil 0)
											   ("recieve" "receive" nil 0)
											   ("recieved" "received" nil 0)
											   ("recieving" "receiving" nil 0)
											   ("recomend" "recommend" nil 0)
											   ("recomendation" "recommendation" nil 0)
											   ("recomendations" "recommendations" nil 0)
											   ("recomended" "recommended" nil 0)
											   ("recomending" "recommending" nil 0)
											   ("reconize" "recognize" nil 0)
											   ("recrod" "record" nil 0)
											   ("redirector" "re-director" nil 0)
											   ("regardsless" "regardless" nil 0)
											   ("regession" "regression" nil 0)
											   ("regresion" "regression" nil 0)
											   ("releated" "related" nil 0)
											   ("religous" "religious" nil 0)
											   ("relize" "realize" nil 0)
											   ("reloacted" "relocated" nil 0)
											   ("reltaed" "related" nil 0)
											   ("reluctent" "reluctant" nil 0)
											   ("remeber" "remember" nil 0)
											   ("reommend" "recommend" nil 0)
											   ("representativs" "representatives" nil 0)
											   ("representives" "representatives" nil 0)
											   ("represetned" "represented" nil 0)
											   ("represnt" "represent" nil 0)
											   ("requirment" "requirement" nil 0)
											   ("requirments" "requirements" nil 0)
											   ("reserach" "research" nil 0)
											   ("resollution" "resolution" nil 0)
											   ("resorces" "resources" nil 0)
											   ("respomd" "respond" nil 0)
											   ("respomse" "response" nil 0)
											   ("responce" "response" nil 0)
											   ("responsability" "responsibility" nil 0)
											   ("responsable" "responsible" nil 0)
											   ("responsibile" "responsible" nil 0)
											   ("responsiblity" "responsibility" nil 0)
											   ("restaraunt" "restaurant" nil 0)
											   ("restuarant" "restaurant" nil 0)
											   ("reult" "result" nil 0)
											   ("reveiw" "review" nil 0)
											   ("reveiwing" "reviewing" nil 0)
											   ("rumers" "rumors" nil 0)
											   ("runnning" "running" nil 0)
											   ("rwite" "write" nil 0)
											   ("rythm" "rhythm" nil 0)
											   ("saidhe" "said he" nil 0)
											   ("saidit" "said it" nil 0)
											   ("saidthat" "said that" nil 0)
											   ("saidthe" "said the" nil 0)
											   ("scedule" "schedule" nil 0)
											   ("sceduled" "scheduled" nil 0)
											   ("scen" "scene" nil 0)
											   ("scol" "scowl" nil 0)
											   ("scoli" "scowl" nil 0)
											   ("seance" "seance" nil 0)
											   ("secratary" "secretary" nil 0)
											   ("sectino" "section" nil 0)
											   ("securtiy" "security" nil 0)
											   ("seh" "she" nil 0)
											   ("selectoin" "selection" nil 0)
											   ("sentance" "sentence" nil 0)
											   ("separeate" "separate" nil 0)
											   ("seperate" "separate" nil 0)
											   ("seperated" "separated" nil 0)
											   ("sercumstances" "circumstances" nil 0)
											   ("serveral" "several" nil 0)
											   ("sez" "says" nil 0)
											   ("shcool" "school" nil 0)
											   ("shesaid" "she said" nil 0)
											   ("shineing" "shining" nil 0)
											   ("shiped" "shipped" nil 0)
											   ("shoudl" "should" nil 0)
											   ("shouldent" "shouldn't" nil 0)
											   ("shouldnt" "shouldn't" nil 0)
											   ("showinf" "showing" nil 0)
											   ("signifacnt" "significant" nil 0)
											   ("simalar" "similar" nil 0)
											   ("similiar" "similar" nil 0)
											   ("simpilified" "simplified" nil 0)
											   ("simpyl" "simply" nil 0)
											   ("sincerly" "sincerely" nil 0)
											   ("sitll" "still" nil 0)
											   ("smae" "same" nil 0)
											   ("smoe" "some" nil 0)
											   ("soem" "some" nil 0)
											   ("sohw" "show" nil 0)
											   ("soical" "social" nil 0)
											   ("somethign" "something" nil 0)
											   ("someting" "something" nil 0)
											   ("somewaht" "somewhat" nil 0)
											   ("somthing" "something" nil 0)
											   ("somtimes" "sometimes" nil 0)
											   ("soudn" "sound" nil 0)
											   ("soudns" "sounds" nil 0)
											   ("speach" "speech" nil 0)
											   ("specificaly" "specifically" nil 0)
											   ("specificalyl" "specifically" nil 0)
											   ("statment" "statement" nil 0)
											   ("statments" "statements" nil 0)
											   ("stnad" "stand" nil 0)
											   ("stopry" "story" nil 0)
											   ("stoyr" "story" nil 0)
											   ("stpo" "stop" nil 0)
											   ("strentgh" "strength" nil 0)
											   ("stroy" "story" nil 0)
											   ("struggel" "struggle" nil 0)
											   ("strugle" "struggle" nil 0)
											   ("studnet" "student" nil 0)
											   ("sublanguange" "sublanguage" nil 0)
											   ("substitued" "substituted" nil 0)
											   ("successfull" "successful" nil 0)
											   ("successfuly" "successfully" nil 0)
											   ("successfulyl" "successfully" nil 0)
											   ("sucess" "success" nil 0)
											   ("sucessfull" "successful" nil 0)
											   ("sufficiant" "sufficient" nil 0)
											   ("suposed" "supposed" nil 0)
											   ("suppossed" "supposed" nil 0)
											   ("suprise" "surprise" nil 0)
											   ("suprised" "surprised" nil 0)
											   ("suprisingly" "surprisingly" nil 0)
											   ("swiming" "swimming" nil 0)
											   ("tahn" "than" nil 0)
											   ("taht" "that" nil 0)
											   ("talekd" "talked" nil 0)
											   ("talior" "tailor" nil 0)
											   ("talkign" "talking" nil 0)
											   ("tath" "that" nil 0)
											   ("tecnical" "technical" nil 0)
											   ("teh" "the" nil 0)
											   ("tehy" "they" nil 0)
											   ("terminiated" "terminated" nil 0)
											   ("termoil" "turmoil" nil 0)
											   ("tghe" "the" nil 0)
											   ("tghis" "this" nil 0)
											   ("thansk" "thanks" nil 0)
											   ("thatthe" "that the" nil 0)
											   ("thecompany" "the company" nil 0)
											   ("thefirst" "the first" nil 0)
											   ("thegovernment" "the government" nil 0)
											   ("themself" "themselves" nil 0)
											   ("themselfs" "themselves" nil 0)
											   ("thenew" "the new" nil 0)
											   ("theri" "their" nil 0)
											   ("thesame" "the same" nil 0)
											   ("thetwo" "the two" nil 0)
											   ("theyll" "they'll" nil 0)
											   ("theyve" "they've" nil 0)
											   ("thgat" "that" nil 0)
											   ("thge" "the" nil 0)
											   ("thier" "their" nil 0)
											   ("thier" "their" nil 0)
											   ("thigsn" "things" nil 0)
											   ("thisyear" "this year" nil 0)
											   ("thna" "than" nil 0)
											   ("thne" "then" nil 0)
											   ("thnig" "thing" nil 0)
											   ("thnigs" "things" nil 0)
											   ("threatend" "threatened" nil 0)
											   ("thsi" "this" nil 0)
											   ("thsoe" "those" nil 0)
											   ("thta" "that" nil 0)
											   ("tihs" "this" nil 0)
											   ("timne" "time" nil 0)
											   ("tiogether" "together" nil 0)
											   ("tje" "the" nil 0)
											   ("tjhe" "the" nil 0)
											   ("tkae" "take" nil 0)
											   ("tkaes" "takes" nil 0)
											   ("tkaing" "taking" nil 0)
											   ("tlaking" "talking" nil 0)
											   ("todya" "today" nil 0)
											   ("togehter" "together" nil 0)
											   ("tomorow" "tomorrow" nil 0)
											   ("tongiht" "tonight" nil 0)
											   ("tonihgt" "tonight" nil 0)
											   ("totaly" "totally" nil 0)
											   ("totalyl" "totally" nil 0)
											   ("tothe" "to the" nil 0)
											   ("towrad" "toward" nil 0)
											   ("traditionalyl" "traditionally" nil 0)
											   ("transfered" "transferred" nil 0)
											   ("truely" "truly" nil 0)
											   ("truley" "truly" nil 0)
											   ("tryed" "tried" nil 0)
											   ("tthe" "the" nil 0)
											   ("tyhat" "that" nil 0)
											   ("tyhe" "the" nil 0)
											   ("udnerstand" "understand" nil 0)
											   ("understnad" "understand" nil 0)
											   ("undert he" "under the" nil 0)
											   ("unicode" "Unicode" nil 0)
											   ("unicode" "Unicode" nil 0)
											   ("unitedstates" "United States" nil 0)
											   ("unliek" "unlike" nil 0)
											   ("unpleasently" "unpleasantly" nil 0)
											   ("unregistared" "unregistered" nil 0)
											   ("untill" "until" nil 0)
											   ("untilll" "until" nil 0)
											   ("useing" "using" nil 0)
											   ("usualyl" "usually" nil 0)
											   ("veyr" "very" nil 0)
											   ("virtualyl" "virtually" nil 0)
											   ("vrey" "very" nil 0)
											   ("vulnerible" "vulnerable" nil 0)
											   ("waht" "what" nil 0)
											   ("warrent" "warrant" nil 0)
											   ("wasnt" "wasn't" nil 0)
											   ("watn" "want" nil 0)
											   ("wehn" "when" nil 0)
											   ("wernt" "weren't" nil 0)
											   ("werre" "were" nil 0)
											   ("wethee" "whether" nil 0)
											   ("whcih" "which" nil 0)
											   ("wherre" "where" nil 0)
											   ("whic" "which" nil 0)
											   ("whihc" "which" nil 0)
											   ("whos" "who's" nil 0)
											   ("whove" "who've" nil 0)
											   ("whta" "what" nil 0)
											   ("wief" "wife" nil 0)
											   ("wierd" "weird" nil 0)
											   ("wihch" "which" nil 0)
											   ("wiht" "with" nil 0)
											   ("willbe" "will be" nil 0)
											   ("windoes" "windows" nil 0)
											   ("witha" "with a" nil 0)
											   ("withe" "with" nil 0)
											   ("withthe" "with the" nil 0)
											   ("wiull" "will" nil 0)
											   ("wnat" "want" nil 0)
											   ("wnated" "wanted" nil 0)
											   ("wnats" "wants" nil 0)
											   ("woh" "who" nil 0)
											   ("wohle" "whole" nil 0)
											   ("wokr" "work" nil 0)
											   ("woudl" "would" nil 0)
											   ("woudln" "wouldn" nil 0)
											   ("wouldbe" "would be" nil 0)
											   ("wouldnt" "wouldn't" nil 0)
											   ("wriet" "write" nil 0)
											   ("writting" "writing" nil 0)
											   ("wrod" "word" nil 0)
											   ("wroet" "wrote" nil 0)
											   ("wroking" "working" nil 0)
											   ("wtih" "with" nil 0)
											   ("wuould" "would" nil 0)
											   ("wya" "way" nil 0)
											   ("yera" "year" nil 0)
											   ("yeras" "years" nil 0)
											   ("yersa" "years" nil 0)
											   ("yoiu" "you" nil 0)
											   ("youare" "you are" nil 0)
											   ("youd" "you'd" nil 0)
											   ("youre" "you're" nil 0)
											   ("youve" "you've" nil 0)
											   ("ytou" "you" nil 0)
											   ("yuo" "you" nil 0)
											   ("yuor" "your" nil 0)
											   ))
  (define-abbrev global-abbrev-table "cdate" ""
    #'(lambda ()
        (insert (format-time-string "%Y-%m-%d"))))
  (defun insert-current-time ()
	"Insert the current time in the format HH:MM:SS."
	(interactive)
	(insert (format-time-string "%H:%M:%S")))
  (define-abbrev global-abbrev-table "ctime" "" 'insert-current-time)
  :custom
  (save-abbrevs nil))

(use-package jinx
  :hook (elpaca-after-init . global-jinx-mode)
  :bind
  (:map jinx-overlay-map
		("C-M-$" . #'jinx-correct-all))
  (([remap ispell] . jinx-correct-nearest))
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
(use-package all-the-icons)
(use-package dired-subtree 
  :after dired
  :bind (:map dired-mode-map
		  ("<tab>" . dired-subtree-toggle)
		  ("<backtab>" . dired-subtree-cycle)))

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
  :after org)
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
  :custom
  (org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n/!)" "INPROGRESS(i/!)" "|" "DONE(d!/!)")
			 (sequence "PROJECT(p)" "|" "DONE(d!/!)")
			 (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
	org-todo-repeat-to-state "NEXT")
  (org-todo-keyword-faces
	(quote (("NEXT" :inherit warning)
			 ("PROJECT" :inherit font-lock-string-face))))
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
  (org-src-ask-before-returning-to-edit-buffer nil)
  (org-startup-folded t)
  (org-startup-with-inline-images t)
  (org-tags-column 0)
  ;; TODO(SN): https://github.com/karthink/org-auctex
  (org-startup-with-latex-preview nil);; wait for the async rendering to be merged
  (org-support-shift-select t)
  (org-archive-location "%s_archive::* Archive")
  ;; (org-latex-pdf-process '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -outdir=~/.cache/emacs %f"))
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
		(visual-line-mode 1)
		(variable-pitch-mode 1))
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
  :init
  (global-org-modern-mode t))

(use-package org-appear
  :ensure (:host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package org-clock
  :ensure nil  ;; built in
  :after consult
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
  :init
  (defvar org-clock-map
    (let ((map (make-sparse-keymap))) 
      (define-key map (kbd "j") 'org-clock-goto)
      (define-key map (kbd "l") 'org-clock-in-last)
      (define-key map (kbd "i") 'consult-clock-in)
      (define-key map (kbd "o") 'org-clock-out)
      map)
    "Keymap for org-clock commands.")
  (global-set-key (kbd "C-o") org-clock-map)
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
  :custom
  (org-clock-in-resume t)
  (org-clock-persist 'history)
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
  (org-clock-resolve-expert t)
  (org-clock-persistence-insinuate))

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
  							  (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
  								  (org-agenda-skip-entry-if 'nottodo '("NEXT" "INPROGRESS")))))
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
  :config
  (add-to-list 'org-tag-alist '("TOC" . ?T))
  :init
   (add-hook 'org-mode-hook 'toc-org-mode)
  (add-hook 'markdown-mode-hook 'toc-org-mode))

(use-package pdf-tools
  :ensure (:host github :repo "aikrahguzar/pdf-tools"
			:branch "continuous-scroll")
  :hook (pdf-tools-enabled . (lambda ()
							   (pdf-view-midnight-minor-mode 1)
							   (toggle-mode-line)))
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-view-midnight-colors '("#e8e4b1" . "#352718" ))
  (pdf-annot-activate-created-annotations t)
  :magic ("%PDF" . pdf-view-mode)
  :init (setq-default pdf-view-roll-minor-mode t)
  :config
  (require 'pdf-tools)
  (require 'pdf-view)
  (require 'pdf-misc)
  (require 'pdf-occur)
  (require 'pdf-util)
  (require 'pdf-annot)
  (require 'pdf-info)
  (require 'pdf-isearch)
  (require 'pdf-history)
  (require 'pdf-links)
  (require 'pdf-roll)
  (pdf-tools-install :no-query)
  )

(use-package org-roam
  :demand t
  :init
  (setq-default org-roam-v2-ack t)
  :config
  (org-roam-db-autosync-mode)
  (defun sn/org-roam-dailies-goto-today ()
	"Open today's daily note non-interactively and return the buffer name as a string."
	(interactive)
	(org-roam-dailies-goto-today)
	(get-buffer (format-time-string "%Y-%m-%d.org")))
  (setq initial-buffer-choice #'sn/org-roam-dailies-goto-today)
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
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((css . ("https://github.com/tree-sitter/tree-sitter-css"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go"))
			   (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
			   ))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional. Combobulate works in both xxxx-ts-modes and
  ;; non-ts-modes.

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
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
  :config
  (mp-setup-install-grammars)
  :custom
  (treesit-font-lock-level 4))

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate" :branch "development")
  :custom (combobulate-key-prefix "C-c j")
  :hook (prog-mode . combobulate-mode))

(use-package eglot
  :ensure nil
  :after cape
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
  (eglot-code-action-indications '(nearby mode-line margin))
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
	(setq-local
	  completion-at-point-functions (list
									  #'sn/cape-in-code
									  #'sn/cape-in-string
									  #'sn/cape-in-comment
									  #'codeium-completion-at-point
									  )))) 
(use-package consult-eglot
  :after eglot
  :bind
  (:map eglot-mode-map
	("C-c f" . consult-eglot-symbols)))

(use-package eldoc-box
  :custom
  (eldoc-idle-delay 0)
  (eldoc-box-cleanup-interval 3)
  (eldoc-box-only-multi-line t)
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-box-offset '(32 32 32))
  (eldoc-box-max-pixel-width 600)
  (eldoc-box-max-pixel-height 200)
  :hook (eglot-managed-mode . eldoc-box-hover-mode))

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
(use-package blamer
  :bind
  ("C-c i" . blamer-show-posframe-commit-info)
  :custom
  (blamer-idle-time 0.7)
  (blamer-min-offset 10)
  (blamer-max-commit-message-length 50)
  (blamer-type 'visual)
  :init (global-blamer-mode t))

(use-package magit
  :ensure nil
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
  :custom
  (magit-diff-refine-hunk t))
(use-package git-timemachine
  :custom (git-timemachine-show-minibuffer-details t))

(use-package hl-todo
  :ensure (hl-todo :depth nil)
  :init (global-hl-todo-mode t)
  :hook (ef-themes-post-load . my-ef-themes-hl-todo-faces)
  :config
  (defun my-ef-themes-hl-todo-faces ()
	"Configure `hl-todo-keyword-faces' with Ef themes colors.
The exact color values are taken from the active Ef theme."
	(ef-themes-with-colors
      (setq hl-todo-keyword-faces
			`(("HOLD" . ,yellow)
              ("TODO" . ,red)
              ("NEXT" . ,blue)
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
              ("DEPRECATED" . ,yellow)))))
  (my-ef-themes-hl-todo-faces))
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
  :hook
  (vterm-mode . (lambda ()
				  (toggle-mode-line)
				  (set (make-local-variable 'buffer-face-mode-face) '(:family "IosevkaTerm Nerd Font"))
				  (buffer-face-mode t)
				  (setq-local left-margin-width 3
							  right-margin-width 3
							  cursor-type 'bar)
				  (face-remap-add-relative
				   'default
				   :background "#281d12")
				  (face-remap-set-base
				   'default
				   :background "#281d12")
				  (face-remap-add-relative
				   'fringe
				   :background "#281d12"))))

(use-package vterm-tabs
  ;; :ensure (:host github :repo "nehrbash/vterm-tabs")
  :load-path "~/src/vterm-tabs"
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
  (require 'svg-tabs)
  (global-vterm-tabs-mode 1))

(use-package direnv
  :config (direnv-mode))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  (global-ligature-mode t))

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
  (flymake-no-changes-timeout 3)
  (flymake-fringe-indicator-position 'right-fringe)
  ;; (flymake-show-diagnostics-at-end-of-line 'short)
  :bind
  ("M-g f" . consult-flymake)
  ("M-SPC p" . flymake-show-project-diagnostics))
(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :config
  (setq sideline-flymake-display-mode 'point) ; 'point to show errors only on point
										; 'line to show errors on the current line
  (setq sideline-backends-right '(sideline-flymake)))
(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook (bash-ts-mode . flymake-shellcheck-load))

(use-package treesit-fold
  :ensure (:host github :repo "emacs-tree-sitter/treesit-fold")
  :bind ("M-SPC f" . treesit-fold-toggle)
  :custom (treesit-fold-summary-max-length 80)
  :init
  (global-treesit-fold-mode))

(use-package go-mode
  :ensure-system-package
  ((staticcheck . "go install honnef.co/go/tools/cmd/staticcheck@latest")
   (gofumpt . "go install mvdan.cc/gofumpt@latest")
   (gopls . "go install golang.org/x/tools/gopls@latest"))
  :hook (go-ts-mode . (lambda ()
						(subword-mode 1)
						(setq-local go-ts-mode-indent-offset 4))))
(use-package go-tag
  :ensure-system-package (gomodifytags . "go install github.com/fatih/gomodifytags@latest")
  :bind (:map go-ts-mode-map ("C-c C-t" . go-tag-add)))
(use-package go-impl
  :ensure-system-package (impl . "go install github.com/josharian/impl@latest")
  :bind (:map go-ts-mode-map ("C-c C-i" . go-impl)))
(use-package go-gen-test
  :ensure-system-package (gotests . "go install github.com/cweill/gotests/gotests@latest")
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
						  (setq-local compile-command "cargo run")))
  :config
  (add-to-list 'eglot-server-programs '((rust-ts-mode rust-mode) . ("rustup" "run" "stable" "rust-analyzer"))))
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
  :ensure-system-package
  (docker . "paru -S docker")
  (docker-compose . "paru -S docker-compose")
  (devcontainer . "npm install -g @devcontainers/cli")
  :bind ("M-SPC d" . docker)
  :config
  (fullframe docker-images tablist-quit)
  (fullframe docker-volumes tablist-quit)
  (fullframe docker-networks tablist-quit)
  (fullframe docker-containers tablist-quit))
(use-package docker-compose-mode
  :mode ("\docker-compose.yml\\'" . docker-compose-mode))

(use-package sqlformat
  :ensure-system-package (pgformatter)
  :hook (sql-mode . sqlformat-on-save-mode)
  :custom
  (sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g")))

(use-package terraform-mode)

(use-package lisp-mode
  :ensure nil
  :custom (lisp-indent-offset 2))

(use-package yuck-mode
  :mode ("\\.yuck\\'" . yuck-mode)
  :hook (yuck-mode . (lambda () (setq-local lisp-indent-offset 2))))

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
    (when (string-prefix-p (expand-file-name "~/src/analytics-hub/") (or (buffer-file-name buffer) ""))
      (kill-buffer buffer)))
  ;; Set default directory and run the dev container command
  (let* ((default-directory (expand-file-name "~/src/analytics-hub/"))
         (output-buffer "*Start Dev-Container Output*")
         (result (call-process-shell-command "devcontainer up --workspace-folder ." nil output-buffer t)))
    (if (= result 0)
        (progn
          (message "Dev container started successfully.")
          ;; Open Dired and Magit only if the container starts successfully
          (dired "/docker:dev-container:/workspace/")
          (magit-status))
      (message "Error: Command failed. Check %s for details." output-buffer))))

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
  :bind
  ("<f5>" . gptel-toggle-sidebar)
  (:map vterm-mode-map
	 ("<f5>" . gptel-toggle-sidebar))
  ("C-<f5>" . gptel-menu)
  :hook (org-mode . gptel-activate-if-model-exists)
  :custom
  (gptel-model 'gpt-4o)
  (gptel-display-buffer-action
    '((display-buffer-reuse-window display-buffer-in-side-window)
       (side . right)
       (window-width . 80)
       (slot . 0)))
  (gptel-default-mode 'org-mode)
  :config
  (defun gptel-toggle-sidebar ()
	"Toggle a custom sidebar for the buffer '*My Sidebar*'."
	(interactive)
	(let ((buffer-name "AI Chat"))
	  (if-let* ((window (get-buffer-window buffer-name)))
		;; If the sidebar is already open, close it.
		(delete-window window)
		;; Else, create the sidebar using
		(let ((chat-buffer (gptel buffer-name)))
		  (display-buffer-in-side-window
			chat-buffer gptel-display-buffer-action)
		  (let ((window (get-buffer-window chat-buffer)))
			(when window
			  (set-window-dedicated-p window t)
			  (set-window-parameter window 'no-other-window t)
			  (select-window window)
			  (setq mode-line-format nil)))))))
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
  (defun gptel-activate-if-model-exists ()
	"Activate gptel mode if the GPTEL_MODEL property exists in any part of the Org document."
	(org-with-wide-buffer
      (goto-char (point-min))
      (let ((found nil))
		(while (and (not found) (re-search-forward "^\\*+" nil t))
          (when (org-entry-get (point) "GPTEL_MODEL")
			(setq found t)))
		(when found
          (gptel-mode 1))))))

(use-package codeium
  :ensure (:host github :repo "Exafunction/codeium.el")
  :after cape
  :custom
  (codeium-log-buffer nil)
  :config
  (advice-add 'codeium-completion-at-point :around #'cape-wrap-buster)
  
  ;; You can overwrite all the codeium configs!
  ;; for example, we recommend limiting the string sent to codeium for better performance
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
      (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)
  )

(use-package aidermacs
  :ensure (:host github :repo "MatthewZMD/aidermacs")
  :bind (("M-SPC a" . aidermacs-transient-menu))
  :config
  ; Enable minor mode for Aider files
  (aidermacs-setup-minor-mode)
  :custom
  ; See the Configuration section below
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet"))

(use-package cus-dir
  :ensure (:host gitlab :repo "mauroaranda/cus-dir")
  :bind ("C-x p d" . customize-dirlocals-project))

(use-package google-this
  :bind ("M-s w" . google-this))

(use-package ea
  :load-path "~/.emacs.d/lisp")

(defun setup-min-frame ()
  "Set up frames for minibuffer and editor."
  (setq default-minibuffer-frame
		(make-frame
		 '((name . "Emacs Minibuffer")
		   (width . 80)
		   (height . 1)
		   (minibuffer . only)
		   (top . 0) ;; Ensure it's at the top
		   (left . 0)))))
(defun setup-no-min-frame ()
  "Set up frames for minibuffer and editor."
  (setq new-frame
		(make-frame
		 '((name . "Emacs Frame")
		   (width . 80)
		   (height . 30)
		   (minibuffer . nil)
		   (top . 50) ;; Adjust as needed
		   (left . 0)))))

;; (add-hook 'window-setup-hook 'setup-frames)

(use-package consult-taskfile
  :load-path "~/src/consult-taskfile"
  :bind
  ("M-SPC x" . consult-task)
  ("M-SPC c" . taskfile))

(use-package ement
  :custom
  (ement-room-prism 'both)
  (ement-save-sessions t))
(use-package burly)

(use-package zalgo-mode
  ;; :load-path "~/src/zalgo-mode"
  :ensure (:host github :repo "nehrbash/zalgo-mode")
  :bind
  ("C-c z z" . zalgo-mode) ;; minor mode for typeing zalgo
  ("C-c z r" . zalgo-transform-region)
  ("C-c z w" . zalgo-transform-word)
:custom
(zalgo-max-up 5)
(zalgo-max-mid 3)
(zalgo-max-down 5))

(use-package eww
   :ensure nil
   :custom
   (eww-auto-rename-buffer 'title)
   :config
   (define-advice eww (:around (oldfun &rest args) always-new-buffer)
     "Always open EWW in a new buffer."
     (let ((current-prefix-arg '(4)))
       (apply oldfun args)))
   ;; :bind
   ;; (:map eww-mode-map
   ;;           [mouse-8] #'eww-back-url
   ;;           [mouse-9] #'eww-forward-url)
)
