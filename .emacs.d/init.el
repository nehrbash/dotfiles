(add-to-list 'load-path "~/.emacs.d/lisp/")
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
(setq package-native-compile t
      async-bytecomp-package-mode t
      package-quickstart t
      package-install-upgrade-built-in t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq use-package-always-ensure t
      use-package-expand-minimally t
      use-package-compute-statistics t) ;; don't worry this takes like zero time
;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file 'noerror 'nomessage)
;; set this after no-littering
(add-hook 'package-upgrade-all-hook
          (lambda ()
            (package-quickstart-refresh)))

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

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
(autoload 'mwheel-install "mwheel")
(defun sanityinc/console-frame-setup ()
  (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
  (mouse-wheel-mode 1))
(add-hook 'after-make-console-frame-hooks 'sanityinc/console-frame-setup)

(use-package move-dup
  :bind(("M-<up>" . move-dup-move-lines-up)
        ("M-<down>" . move-dup-move-lines-down)
        ("C-c d" . move-dup-duplicate-down)
        ("C-c u" . move-dup-duplicate-up)))

(use-package whole-line-or-region
  :ensure t
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
  :config
  (setq switch-window-shortcut-style 'alphabet
        switch-window-timeout nil)
  :bind
  ("C-x o" . switch-window))

(use-package windswap
  :defer 4
  :config
  (windmove-default-keybindings 'control)
  (windswap-default-keybindings 'shift 'control))

(use-package sudo-edit
  :commands (sudo-edit))

(use-package fullframe)

(setq-default
 fill-column 120
 blink-cursor-interval 0.4
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 tab-width 4
 make-backup-files nil
 auto-save-default nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 set-mark-command-repeat-pop t
 tooltip-delay .8
 truncate-lines nil
 truncate-partial-width-windows nil
 ring-bell-function 'ignore)
(delete-selection-mode t)
(global-goto-address-mode t)
(add-hook 'after-init-hook 'transient-mark-mode) ;; standard highlighting
(setq browse-url-browser-function #'browse-url-firefox)
(setq use-dialog-box nil)
(global-set-key (kbd "C-c C-p") 'find-file-at-point)

(use-package recentf
  :ensure nil
  :hook ((package-upgrade-all . recentf-cleanup))
  :custom
  (bookmark-save-flag 1)
  (bookmark-default-file (expand-file-name "var/bookmarks.el" user-emacs-directory))
  (recentf-auto-cleanup 'never) ; Disable automatic cleanup at load time
  (recentf-max-saved-items 25))

(use-package autorevert
  :defer 15
  :config (global-auto-revert-mode 1)
  :delight auto-revert-mode)

(setq tramp-verbose 1)
(setq tramp-encoding-shell "/bin/bash")
(setq tramp-default-method "ssh")
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq vc-handled-backends '(Git))
(use-package tramp
  :defer t
  :config (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package savehist
  :ensure nil
  :config
  (setq history-length 25)
  (savehist-mode 1))

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
  :delight symbol-overlay-mode
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
  :delight which-key-mode
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
  :delight
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
  (require 'winner)
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

(setq default-frame-alist '((alpha-background . 90) (font . "Source Code Pro-10") (left-fringe . 10) (right-fringe . 10) (vertical-scroll-bars . nil)))
(add-hook 'after-init-hook
  (lambda ()
    (pixel-scroll-precision-mode t) 
    (set-face-attribute 'header-line nil :height 100)))

(use-package page-break-lines
  :defer 3
  :config (page-break-lines-mode))

(use-package doom-themes
  :hook (after-init . (lambda ()
                        (load-theme 'doom-gruvbox t)
                        (doom-themes-treemacs-config)
                        (doom-themes-org-config)))
  :custom ((doom-themes-enable-bold t)
           (doom-gruvbox-padded-modeline t)
           (doom-themes-enable-italic t)
           (custom-safe-themes t)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package global-prettify-symbols-mode
  :ensure nil
  :hook ((prog-mode text-mode) . global-prettify-symbols-mode))

(use-package doom-modeline
  :after doom-themes
  :hook ((after-init . doom-modeline-mode))
  :init
  (line-number-mode -1)
  (column-number-mode -1)
  (setq mode-line-position nil)
  :custom ((doom-modeline-project-detection 'project)
		   (doom-modeline-vcs-max-length 30)
		   (doom-modeline-hud t)
		   (doom-modeline-unicode-fallback t)
		   (doom-modeline-env-version t)
		   (doom-modeline-buffer-encoding nil)
		   (doom-modeline-workspace-name t)
		   (doom-modeline-buffer-file-name-style 'auto)
		   (doom-modeline-height 27)
		   (doom-modeline-buffer-state-icon t)
		   (doom-modeline-icon t)))

(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)

(use-package default-text-scale
  :bind (("C-M-=". default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)))

(defun stealthily (fn &rest args)
    "Apply FN to ARGS while inhibiting modification hooks."
    (let ((inhibit-modification-hooks t))
      (apply fn args)))
(use-package minibuffer
  :ensure nil
  :bind
  (:map minibuffer-local-completion-map
        ("<backtab>" . minibuffer-force-complete))
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (minibuffer-eldef-shorten-default t)
  (resize-mini-windows t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :init
  (minibuffer-depth-indicate-mode)
  (minibuffer-electric-default-mode)
  :hook
  (completion-list-mode . force-truncate-lines)
  (minibuffer-setup . cursor-intangible-mode)
  :config
  (advice-add 'minibuf-eldef-setup-minibuffer :around #'stealthily))

(use-package vertico
  :hook (after-init . vertico-mode))
(use-package marginalia
  :hook (vertico-mode . marginalia-mode)
  :bind (:map minibuffer-local-map
			  ("M-a" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))
(use-package all-the-icons-completion
  :hook (marginalia-mode .marginall-the-icons-completion-marginalia-setup))

(use-package orderless
  :after (minibuffer vertico)
  :custom
  ;; (orderless-matching-styles 'orderless-regexp)
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-category-defaults nil)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package wgrep
  :commands (wgrep wgrep-change-to-wgrep-mode))

(use-package consult
  :after vertico
  :bind (("C-r" . consult-ripgrep-symbol-at-point)
         ;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c C-m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x f" . consult-recent-file)
         ("C-c C-f" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ("M-s ." . consult-line-thing-at-point)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)) 
  :init
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref xref-show-definitions-function #'consult-xref)
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  :custom
  (consult-narrow-key "<")
  (consult-preview-key '("M-," :debounce 0 any))
  :config
  (recentf-mode 1)
  (setq consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))
  (defalias 'consult-line-thing-at-point 'consult-line)
  (defalias 'consult-ripgrep-symbol-at-point 'consult-ripgrep)
  (consult-customize consult-ripgrep-symbol-at-point :initial (thing-at-point 'symbol))
  (consult-customize consult-line-thing-at-point :initial (thing-at-point 'symbol))
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
                       "^magit"         ; magit buffers
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

(use-package consult-flycheck
  :commands consult-flycheck
  :after (consult flycheck))

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
        embark-become-indicator embark-action-indicator))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))
(use-package embark-vc
  :after embark)

(use-package protogg
  :vc (:url "https://github.com/nehrbash/protogg.git"
               :branch "main" :rev :newest)
  :custom (protogg-minibuffer-toggle-key "M-g")
  :bind (([remap async-shell-command] . protogg-async-shell-command) ;; M-&
         ("C-c x" . protogg-compile)
         ([remap dired] . protogg-dired) ;; C-x d
         ("C-c e" . protogg-eshell)
         ("M-s d" . protogg-find-dired)
         ([remap find-file] . protogg-find-file) ;; C-x C-f
         ([remap list-buffers] . protogg-list-buffers) ;; type C-x C-b
         ([remap shell-command] . protogg-shell-command) ;; M-!
         ("C-c s" . protogg-shell)
         ([remap switch-to-buffer] . sn/consult-buffer)
         ("M-s i" . sn/imenu)
         ("M-t" . sn/multi-vterm)) ;; C-x b
  :config
  (protogg-define 'consult-project-buffer 'consult-buffer sn/consult-buffer)
  (protogg-define 'consult-imenu-multi 'consult-imenu sn/imenu))

(use-package corfu
  :after orderless
  :hook ((corfu-mode . corfu-popupinfo-mode)
		 ((prog-mode conf-mode yaml-mode) . (lambda ()
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
  (corfu-auto-prefix 2)
  :init
  (global-corfu-mode)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  :config
  (defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
       `(orderless-regexp . ,(concat "^" (regexp-quote word)))))
(orderless-define-completion-style orderless-fast
  (orderless-style-dispatchers '(orderless-fast-dispatch))
  (orderless-matching-styles '(orderless-literal orderless-regexp))))

(use-package corfu-candidate-overlay
  :ensure nil
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
  :custom ((kind-icon-default-face 'corfu-default)
		   (kind-icon-blend-background t)
		   (kind-icon-blend-frac 0.2))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :after (corfu orderless)
  :bind (("M-/" . completion-at-point) ;; overwrite dabbrev-completion binding with capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line))
  :config
  (keymap-global-set "C-c i" (cape-interactive-capf #'codeium-completion-at-point))
  :custom (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package yasnippet
  :hook (emacs-startup . yas-global-mode)
  :bind (:map yas-minor-mode-map ("C-c s" . yas-insert-snippet))
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "~/.emacs.d/etc/yasnippet/snippets"))
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t))
(use-package yasnippet-snippets
  :after yasnippet
  :hook (package-upgrade-all . (lambda () (yas-reload-all))))
;; (use-package yasnippet-capf
;;   :after cape
;;   :config
;;   (add-to-list 'completion-at-point-functions #'yasnippet-capf)) ;; Prefer the name of the snippet instead)

(use-package ispell
  :defer 5
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--run-together")))
;; (use-package flyspell
;;   :hook ((org-mode markdown-mode TeX-mode git-commit-mode
;;            yaml-mode conf-mode prog-mode) . flyspell-mode)
;;   :bind (:map flyspell-mode-map
;;               ("C-." . nil)) ;; Unbind the key
;;   :config
;;   (setq flyspell-issue-welcome-flag nil
;;         ;; Significantly speeds up flyspell, which would otherwise print
;;         ;; messages for every word when checking the entire buffer
;;         flyspell-issue-message-flag nil))

(use-package define-word
  :commands define-word)
(use-package flyspell-correct
  :after flyspell
    :bind (:map flyspell-mode-map ("M-$" . flyspell-correct-wrapper)))

(use-package flycheck
  :commands flycheck-list-errors flycheck-buffer
  :hook (prog-mode . global-flycheck-mode)
  :custom (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  :config
  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically))

(use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode))

(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-omit-mode)
  :hook (dired-mode . my-dired-mode-hook)
  :delight dired-omit-mode
  :init
  (defun my-dired-mode-hook ()
    (dired-omit-mode 1)
    (auto-revert-mode 1)
    (setq mode-line-format nil)
    (hl-line-mode 1))
  :config
  (setq dired-omit-files "^\\.\\.?$")
  (setq-default dired-dwim-target t)
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-verbose nil)
  (setq dired-recursive-deletes 'top))
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
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package dired-collapse
  :hook  (dired-mode . dired-collapse-mode))
(use-package diredfl
  :hook (dired-mode . diredfl-mode))
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("." . dired-hide-dotfiles-mode)))

(use-package consult-dir
  :after (consult)
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

(use-package org
  :bind (("C-c a" .  gtd)
         (:map org-mode-map
               ( "C-M-<up>" . org-up-element)))
  :config
  (custom-set-faces
   '(org-document-title ((t (:height 3.2))))
   '(header-line ((t (:height 3 :weight bold))))
   '(org-level-1 ((t (:foreground "#98be65" :height 1.6))))
   '(org-level-2 ((t (:foreground "#da8548" :height 1.2))))
   '(org-level-3 ((t (:foreground "#a9a1e1" :height 1.1))))
   '(header-line ((t (:height 2)))))
  (defun gtd () (interactive) (org-agenda 'nil "g"))
  (setq org-adapt-indentation t
        org-auto-align-tags nil
        org-edit-src-content-indentation 0
        org-edit-timestamp-down-means-later t
        org-fast-tag-selection-single-key 'expert
        org-hide-emphasis-markers t
        org-image-actual-width nil
        org-insert-heading-respect-content t
        org-log-done 'time
        org-pretty-entities t
        org-return-follows-link  t
        org-special-ctrl-a/e t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-startup-folded t
        org-startup-with-inline-images t
        org-archive-location "%s_archive::* Archive"))

(use-package org-contrib
  :defer t
  :hook (org-mode . (lambda ()
					  (require 'ox-extra)
					  (setq org-latex-pdf-process '("latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -outdir=~/.cache/emacs %f")))))

(use-package org-appear
  :vc (:url "https://github.com/awth13/org-appear.git"
                  :branch "master" :rev :newest)
  :hook (org-mode . org-appear-mode))

(setq org-directory "~/doc")
(setq org-default-notes-file (concat org-directory "/notes.org"))
;; (require 'cl-lib)
(setq org-agenda-files
      (cl-remove-if-not #'file-exists-p
                        '("~/doc/inbox.org"
                          "~/doc/projects.org"
                          "~/doc/gcal.org"
                          "~/doc/repeater.org")))

(use-package org
  :bind
  (("C-c c" . org-capture))
  :config
  (setq org-capture-templates
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
                                                            (org-capture-finalize)))))))

(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda ()   (setq mode-line-format nil)
              (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t)))
)
(with-eval-after-load 'org-mode
  (add-hook 'before-save-hook
            (lambda ()  (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  (org-ellipsis " ⮟"))

(use-package org
  :hook (org-mode . (lambda ()
                      (set-face-attribute 'org-table nil :inherit 'fixed-pitch :font "Source Code Pro-10" :height 1.0)
                      (set-face-attribute 'org-block nil :inherit 'fixed-pitch :font "Source Code Pro-10" :height 1.0)
                      (setq-local prettify-symbols-alist
                            '(("[ ]" .  "☐")
                              ("[X]" . "☑" )
                              ("#+TITLE:" . "")
                              ("#+title: " . "")
                              ("#+begin_src" . "⮓")
                              ("#+end_src" . "⮒")))
                      (prettify-symbols-mode 1)))
  :config
  ;; This is needed as of Org 9.2
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (latex . t)
     (octave . t)
     (python . t)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . t)
     (sqlite . t))))

(defvar org-clock-prefix-map (make-sparse-keymap)
  "A keymap for handy global access to org helpers, particularly clocking.")
(use-package org-clock
  :ensure nil
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
  :init
  (with-eval-after-load 'org
	(org-clock-persistence-insinuate)))

(use-package type-break
  :hook ((org-clock-in-prepare . type-break-mode)
		 (after-init . type-break-mode))
  ;; Setting interval of that of a pomodoro session
  :custom
  (type-break-interval (* 25 60)) ;; 25 mins
  (type-break-good-rest-interval (* 5 60)) ;; 5 mins
  (type-break-good-break-interval (* 5 60)) ;; 5 mins
  (type-break-keystroke-threshold '(nil . 3000)) ;; 500 words is 3,000
  (type-break-demo-boring-stats t)
  (type-break-query-mode t)
  (type-break-query-function 'y-or-n-p)
  ;; (type-break-query-function '(lambda (a &rest b) t))
  (type-break-demo-functions '(type-break-demo-boring))
  :config
  (defun org-clock-in-to-task-by-title (task-title)
	"Clock into an Org Agenda task by its title within a custom agenda command."
	(interactive "sEnter the title of the task: ")
    (org-agenda nil "t")
    (with-current-buffer "*Org Agenda(t)*"
      (goto-char (point-min))
      (if (search-forward task-title nil t)
          (progn
            (org-agenda-goto)
            (org-clock-in))
        (message "Task with title \"%s\" not found in the custom agenda view." task-title))))
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

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode)
  :config
  (setq org-startup-with-latex-preview t)
  (setq org-support-shift-select t))

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
          (setq-local blink-cursor-interval 0.8)
          (setq-local show-trailing-whitespace nil)
          (setq-local line-spacing 0.2)
          (setq-local electric-pair-mode nil)
          (ignore-errors (flyspell-mode 1))
          (visual-line-mode 1))
      (kill-local-variable 'truncate-lines)
      (kill-local-variable 'word-wrap)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'blink-cursor-interval)
      (kill-local-variable 'show-trailing-whitespace)
      (kill-local-variable 'line-spacing)
      (kill-local-variable 'electric-pair-mode)
      (buffer-face-mode -1)
      (flyspell-mode -1)
      (visual-line-mode -1)))
(use-package org-pretty-table
  :vc (:url "https://github.com/Fuco1/org-pretty-table.git"
                        :branch "master" :rev :newest)
  :hook (org-mode . org-pretty-table-mode))
(use-package org
  :bind ((:map org-mode-map
               ("C-c v" . wr-mode)))
  :hook ((org-mode . wr-mode)
         (org-mode . (lambda ()
            (setq-local buffer-face-mode-face '((:family "Google Sans" :weight bold )))
            (setq-local corfu-auto-delay 0.8)
            (buffer-face-mode)))))

(use-package visual-fill-column
  :hook (org-mode . dw/org-mode-visual-fill)
  :init
  (defun dw/org-mode-visual-fill ()
    (setq visual-fill-column-width 120
          visual-fill-column-center-text t)
  (visual-fill-column-mode 1)))

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

(use-package org
  :hook ((org-clock-in . (lambda () (org-todo "INPROGRESS")
						   (org-save-all-org-buffers)))
		 (org-clock-out . (lambda () (org-todo "NEXT")
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

(use-package org-agenda
  :ensure nil
  :hook (org-agenda-mode . hl-line-mode)
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

(use-package toc-org
  :hook (org-mode . toc-org-mode))

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

(use-package org-attach-screenshot
  :commands (org-attach-screenshot)
  :config
  (setq org-attach-screenshot-command-line "/usr/share/sway/scripts/grimshot copy area"))

(use-package pdf-tools
  :mode ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (setq-default pdf-view-display-size 'fit-width)
  (pdf-view-midnight-minor-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :delight(org-roam-mode)
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
  :bind (("C-c n f"   . org-roam-node-find)
           ("C-c n d"   . org-roam-dailies-goto-date)
           ("C-c n n"   . org-roam-buffer-display-dedicated)
           ("C-c n c"   . org-roam-dailies-capture-today)
           ("C-c n C" . org-roam-dailies-capture-tomorrow)
           ("C-c n t"   . org-roam-dailies-goto-today)
           ("C-c n y"   . org-roam-dailies-goto-yesterday)
           ("C-c n r"   . org-roam-dailies-goto-tomorrow)
           ("C-c n g"   . org-roam-graph)
         :map org-mode-map
         (("C-c n i" . org-roam-node-insert))))

(use-package org-roam-ui
  :vc (:url "https://github.com/org-roam/org-roam-ui.git"
            :branch "main" :rev :newest)
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(use-package org-gcal
  :after (org-agenda)
  :requires json
  :init
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

(add-hook 'prog-mode-hook 'hl-line-mode) ;; hilight line

(use-package indent-bars
  :hook ((python-mode conf-mode yaml-mode) . indent-bars-mode)
  :vc (:url "https://github.com/jdtsmith/indent-bars.git"
            :branch "main" :rev :newest))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package treesit-auto
  :init
  (setq treesit-font-lock-level 4)
  :hook ((package-upgrade-all . treesit-auto-install-all))
  :config (global-treesit-auto-mode))

(use-package eglot
  :hook (((go-ts-mode) . eglot-ensure)
         ((go-ts-mode) . eglot-format-buffer-on-save)
		 (eglot-managed-mode . (lambda ()
								 (eglot-inlay-hints-mode 1)
								 (setq-local completion-at-point-functions
											 (list (cape-capf-super #'eglot-completion-at-point  #'yasnippet-capf)))
								 
								 )))
  :custom (eglot-autoshutdown t)
  :init
  (defun eglot-format-buffer-on-save ()
	(add-hook 'before-save-hook #'eglot-format-buffer -10 t))
  (defun project-find-go-module (dir)
	(when-let ((root (locate-dominating-file dir "go.mod")))
	  (cons 'go-module root)))
  (cl-defmethod project-root ((project (head go-module)))
	(cdr project))
  (add-hook 'project-find-functions #'project-find-go-module))
(use-package consult-eglot
  :after (consult eglot))

(use-package magit
  :commands (magit-status magit-dispatch)
  :config
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

(use-package browse-at-remote
  :bind (("C-c g g" . browse-at-remote)
		 ("C-c g k" . browse-at-remote-kill)))

(use-package multi-vterm
  :hook ((vterm-mode . (lambda ()
                         (toggle-mode-line)
                         (setq left-margin-width 1
                               right-margin-width 1
                               cursor-type 'bar))))
  :bind (( "M-t" . multi-vterm-project)
         :map vterm-mode-map
         ("M-t" . toggle-vterm-buffer)
         ("C-M-r" . (lambda ()
                     (interactive)
                     (setq-local vterm-buffer-name-string nil)
                     (rename-buffer (concat "Term " (read-string "Term: ")))))
         ("C-M-t" . multi-vterm-project)
         ("C-M-f" . tab-line-switch-to-next-tab)
         ("C-M-b" . tab-line-switch-to-prev-tab)
         ("C-M-s" . (lambda ()
                      (interactive)
                      (consult-buffer '(consult--source-vterm))))
         ("M-w" . copy-region-as-kill)
         ("C-y" . vterm-yank))
  :custom
  (vterm-buffer-name-string "Term %s")
  (vterm-buffer-maximum-size 1000)
  :config

  (defun toggle-vterm-buffer ()
    "Toggle the visibility of the vterm buffer or switch to it if not currently selected."
    (interactive)
    (let ((vterm-buffer (seq-find (lambda (buffer)
                                    (string-prefix-p "Term" (buffer-name buffer)))
                                  (buffer-list))))
      (if vterm-buffer
          (if (and (eq (current-buffer) vterm-buffer)
                   (get-buffer-window vterm-buffer))
              (delete-window (get-buffer-window vterm-buffer))
            (if (get-buffer-window vterm-buffer)
                (select-window (get-buffer-window vterm-buffer))
              (progn
                (display-buffer vterm-buffer)
                (select-window (get-buffer-window vterm-buffer)))))
        (vterm))))
  (add-to-list 'display-buffer-alist `(,vterm-buffer-name
                                       (display-buffer-reuse-window display-buffer-at-bottom)
                                       (dedicated . t)
                                       (reusable-frames . visible)
                                       (window-height . 0.3))))

(use-package tab-line
  :hook (vterm-mode . tab-line-mode)
  :custom
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil)
  (tab-line-separator "")
  :config
  (use-package powerline)
  (defvar my/tab-height 28)
  (defvar my/tab-left (powerline-wave-right 'tab-line nil my/tab-height))
  (defvar my/tab-right (powerline-wave-left nil 'tab-line my/tab-height))
  (defun my/tab-line-tab-name-buffer (buffer &optional _buffers)
    (powerline-render (list my/tab-left
                            (format "%s" (buffer-name buffer))
                            my/tab-right)))
  (setq tab-line-tab-name-function #'my/tab-line-tab-name-buffer)
  ;; Set face attributes for the tab-line
  (set-face-attribute 'tab-line nil ;; background behind tabs
                      :background "#1d2021")
  (set-face-attribute 'tab-line-tab nil ;; active tab in another window
                      :inherit 'tab-line
                      :background "#8ec07c" :foreground "#0d1011" :box nil)
  (set-face-attribute 'tab-line-tab-current nil ;; active tab in current window
                      :background "#8ec07c" :foreground "#0d1011" :box nil)
  (set-face-attribute 'tab-line-tab-inactive nil ;; inactive tab
                      :background "#689d6a" :foreground "#0d1011" :box nil)
  (set-face-attribute 'tab-line-highlight nil ;; mouseover
                      :background "#928374" :foreground "#0d1011")
  (setq tab-line-tabs-function 'tab-line-tabs-mode-buffers))

(setq confirm-kill-processes nil)

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

(global-set-key [f6] 'recompile)

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

(use-package go-ts-mode
  :mode "\\.go\\'"
  :ensure-system-package ((gopls . "go get golang.org/x/tools/gopls@latest")
                          (staticcheck . "go install honnef.co/go/tools/cmd/staticcheck@latest"))
  :hook (go-ts-mode . (lambda ()
						(setq compile-command "go build -v && go test -v -cover && go vet"
							  go-ts-mode-indent-offset 4)))
  :custom (eglot-workspace-configuration
   '((:gopls .
             ((staticcheck . t)
              (matcher . "CaseSensitive"))))))
(use-package flycheck-golangci-lint
  :hook (go-ts-mode . flycheck-golangci-lint-setup))
(use-package go-tag
  :ensure-system-package (gomodifytags . "go install github.com/fatih/gomodifytags@latest")
  :bind (:map go-ts-mode-map ("C-c t" . go-tag-add)))
(use-package go-fill-struct
  :ensure-system-package (fillstruct . "go install github.com/davidrjenni/reftools/cmd/fillstruct@latest")
  :bind (:map go-ts-mode-map ("C-c f" . go-fill-struct)))
(use-package go-gen-test
  :ensure-system-package (gotests . "go install github.com/cweill/gotests/...@latest")
  :bind (:map go-ts-mode-map ("C-c g" . go-gen-test-dwim)))

(use-package rust-ts-mode
  :mode ("\\.rs\\'" . python-mode)
  )
(use-package flycheck-rust
  :after rust-ts-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package toml-ts-mode
  :hook (toml-ts-mode . goto-address-prog-mode))

(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . python-mode)
  :config
  (setq csv-separators '("," ";" "|" " " ", ")))

(use-package yaml-ts-mode
  :hook (yaml-ts-mode . goto-address-prog-mode))

(use-package docker
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

(use-package terraform-mode
  :mode ("\\.dockerfile\\'" . dockerfile-mode))

(use-package yuck-mode
  :mode ("\\.yuck\\'" . yuck-mode)
  :hook (yuck-mode . (lambda () (setq-local lisp-indent-offset 2))))

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
  (gptel-model "gpt-3.5-turbo")
  (gptel-default-mode 'org-mode))

;; we recommend using use-package to organize your init.el
(use-package codeium
    ;; if you use straight
    ;; otherwise, make sure that the codeium.el file is on load-path
    :vc (:url "https://github.com/Exafunction/codeium.el.git"
               :branch "main" :rev :newest)
	:hook (emacs-startup .  (lambda () (run-with-timer 0.1 nil #'codeium-init)))
 
    ;; :defer t ;; lazy loading, if you want
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

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
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

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

(use-package speed-type :commands speed-type-top-x
  :defer t)

(use-package google-this
  :bind ("M-s w" . google-this))

(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time 1 nil (lambda () (setq gc-cons-threshold 16777216)))) ; 16mb
(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)
