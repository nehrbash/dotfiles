(defvar doom--file-name-handler-alist file-name-handler-alist) ;; temp restore later
(setq file-name-handler-alist nil)

(setq straight-cache-autoloads t)
(setq straight-check-for-modifications nil)
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)

;; now built-in
(require 'use-package)
;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
;; (setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
;;       url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)
;; Load path 
(push "~/.emacs.d/lisp" load-path)

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

(use-package delight)
(use-package htmlize)
(use-package dsvn)
(use-package daemons)

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

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
(autoload 'mwheel-install "mwheel")
(defun sanityinc/console-frame-setup ()
  (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
  (mwheel-install))
(add-hook 'after-make-console-frame-hooks 'sanityinc/console-frame-setup)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package move-dup
  :config(global-move-dup-mode)
  :bind( ("M-<up>" . move-dup-move-lines-up)
         ("M-<down>" . move-dup-move-lines-down)
         ("C-c d" . move-dup-duplicate-down)
         ("C-c u" . move-dup-duplicate-up)))

(use-package whole-line-or-region
  :defer t
  :config (whole-line-or-region-global-mode t)
  :bind ("M-j". comment-dwim))

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
  :commands (windmove-default-keybindings windswap-default-keybindings)
  :config
  (add-hook 'after-init-hook
            (lambda ()
              (windmove-default-keybindings 'control)
              (windswap-default-keybindings 'shift 'control))))

(use-package unmodified-buffer
  :straight (:host github :repo "arthurcgusmao/unmodified-buffer")
  :hook (after-init . unmodified-buffer-global-mode)) ;; Optional

(use-package sudo-edit)

(setq-default
 bookmark-save-flag 1
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name "var/bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 ring-bell-function 'ignore)
(delete-selection-mode t)
(global-goto-address-mode t)
(add-hook 'after-init-hook 'transient-mark-mode) ;; standard highlighting
(setq browse-url-browser-function #'browse-url-firefox)

(use-package recentf
  :init
  (setq recentf-auto-cleanup 'never) ; Disable automatic cleanup at load time
  :config
  (recentf-mode 1)
  (add-hook 'find-file-hook 'recentf-save-list)
  (setq-default
   recentf-max-saved-items 300
   recentf-exclude '("/tmp/" "/ssh:" "/scp:" "/docker:" "/bookmarks.el")))

(global-auto-revert-mode 1)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))

(customize-set-variable
 'tramp-ssh-controlmaster-options
 (concat
  "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
  "-o ControlMaster=auto -o ControlPersist=yes"))

(use-package exec-path-from-shell
  :config
  (dolist (var '("LSP_USE_PLISTS"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(add-hook 'emacs-startup-hook
  (lambda ()
    (pixel-scroll-precision-mode t) ;; enable pixel scrolling
    (fringe-mode '(10 . 10))
    (set-face-attribute 'header-line nil :height 100)
    ))

(use-package doom-themes
  :straight t
  :custom ((doom-themes-enable-bold t)
           (doom-themes-enable-italic t)
           (doom-modeline-bar-inactive 'unspecified) ; suppress warning
          )
  :config
  (load-theme 'doom-gruvbox t)
  (doom-themes-org-config))
(setq custom-safe-themes t)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (
           (doom-modeline-percent-position nil)
           (doom-modeline-buffer-file-name-style 'auto)
           (doom-modeline-vcs-max-length 18)
           (doom-modeline-height 40)
           (doom-modeline-buffer-state-icon t)
           (doom-modeline-buffer-encoding nil)
           (all-the-icons-scale-factor 1)))

(defun sanityinc/maybe-suspend-frame ()
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z M-z") 'sanityinc/maybe-suspend-frame)
(global-set-key (kbd "C-z") 'undo)

;; Change global font size easily
(use-package default-text-scale)
(add-hook 'after-init-hook 'default-text-scale-mode)
(setq-default tab-width 4)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(use-package rainbow-mode
  :defer t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package dired
  :straight (:type built-in)
  :defer 1
  :commands (dired dired-jump)
  :config

  (setq-default dired-dwim-target t)
  (use-package diredfl
    :config
    (require 'dired-x)
    :hook (dired-mode . diredfl-mode)
    )
  ;; Prefer g-prefixed coreutils version of standard utilities when available
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls)))

  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-verbose nil)
  (setq dired-recursive-deletes 'top)
  (autoload 'dired-omit-mode "dired-x")

  (use-package dired-single
    :commands (dired dired-jump))

  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (dired-omit-mode 1)
              (setq mode-line-format nil)
              (hl-line-mode 1)))

  (use-package dired-ranger
    :defer t
    :config
    (put 'dired-find-alternate-file 'disabled nil)
    (define-key dired-mode-map "b" 'dired-single-up-directory)
    (define-key dired-mode-map "f" 'dired-find-alternate-file)
    (define-key dired-mode-map "l" 'dired-single-buffer)
    (define-key dired-mode-map "y" 'dired-ranger-copy)
    (define-key dired-mode-map "X" 'dired-ranger-move)
    (define-key dired-mode-map "H" 'dired-omit-mode)
    (define-key dired-mode-map "p" 'dired-ranger-paste))

  (use-package dired-collapse
    :defer t)

  (use-package all-the-icons-dired
    :defer t
    :hook (dired-mode . all-the-icons-dired-mode)))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (define-key dired-mode-map "." #'dired-hide-dotfiles-mode)
  (setq dired-omit-files "^\\(?:\\..*\\|.*~\\)$"))

(use-package vertico
  :config
  :init (vertico-mode))
(use-package embark
  :after vertico
  :bind (("M-a" . embark-act)
         :map vertico-map
             ("C-c C-o" . embark-export)
             ("C-c C-c" . embark-act)
             ("C-h B" . embark-bindings))
  :config
  (setq embark-action-indicator
      (lambda (map _target)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator))

(use-package orderless
  :after vertico
  :init
  (setq completion-styles '(orderless  basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))
(use-package consult-flycheck)
(use-package savehist :init (savehist-mode))
(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))
(defun consult-ripgrep-symbol-at-point ()
  "Run `consult-ripgrep' with the symbol at point as the initial input."
  (interactive)
  (let ((initial (when-let ((symbol (symbol-at-point)))
                   (concat "" (regexp-quote (symbol-name symbol)) ""))))
    (minibuffer-with-setup-hook
        (lambda ()
          (when initial
            (insert initial)))
      (consult-ripgrep))))
(global-set-key (kbd "C-r") #'consult-ripgrep-symbol-at-point)

(use-package consult
  :bind (
         ;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c C-m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x f" . consult-recent-file)
         ("C-x M-b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
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
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
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
         ("M-s s" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)) 
  :init
  (setq-default consult-project-root-function 'projectile-project-root)
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (defun sn/consult-ripgrep ()
    "Run `consult-ripgrep` from the project root directory if available, or the current directory otherwise."
    (interactive)
    (let ((default-directory (if (projectile-project-p)
                                 (projectile-project-root)
                               default-directory)))
      (consult-ripgrep)))


  (setq consult-narrow-key "<")
  ;; (setq consult-preview-key (kbd "M-."))
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.8 any)
   consult-ripgrep consult-git-grep consult-grep consult-find
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   sanityinc/affe-grep-at-point affe-grep))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (setq consult-dir-project-list-function nil)
  (setq consult-dir-project-list-function #'consult-dir-projectile-dirs)
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

(use-package corfu
  :init (global-corfu-mode)
  :bind (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :custom
  (tab-always-indent 'complete)
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  (corfu-auto-delay 0.1) ;; fine at 0.0 but a little annoying
  (corfu-auto-prefix 2)
  :config 
  (when (featurep 'corfu-popupinfo)
    (with-eval-after-load 'corfu
      (corfu-popupinfo-mode)))
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

  (setq completion-styles '(orderless-fast basic)))

(use-package corfu-terminal
  :when (not (display-graphic-p))
  :straight (:type git
                   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))

(use-package kind-icon
  :commands kind-icon-margin-formatter
  :init
  (add-hook 'corfu-margin-formatters #'kind-icon-margin-formatter)
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (setq kind-icon-default-face 'corfu-default
        kind-icon-blend-background t
        kind-icon-blend-frac 0.2)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package dabbrev
  :after corfu
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (read-extended-command-predicate
      #'command-completion-default-include-p)
  (add-to-list 'completion-at-point-functions #'hippie-expand)
  (dabbrev-ignored-buffer-modes '(archive-mode image-mode pdf-view-mode)))

(use-package cape
  :after corfu
  :commands (cape-dabbrev
             cape-file
             cape-history
             cape-keyword
             cape-tex
             cape-sgml
             cape-rfc1345
             cape-abbrev
             cape-dict
             cape-symbol
             cape-line)
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (defalias 'corfu--ispell-in-comments-and-strings
      (cape-super-capf (cape-capf-inside-comment #'cape-dict)
                       (cape-capf-inside-string #'cape-dict))))

(use-package flyspell ; built-in
  :hook ((org-mode . flyspell-mode)
         (markdown-mode . flyspell-mode)
         (TeX-mode . flyspell-mode)
         (git-commit-mode . flyspell-mode)
         (yaml-mode . flyspell-mode)
         (conf-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (use-package ispell
    :config
    (setq ispell-program-name "aspell"
          ispell-extra-args '("--sug-mode=ultra"
                              "--run-together")))
  (setq flyspell-issue-welcome-flag nil
        ;; Significantly speeds up flyspell, which would otherwise print
        ;; messages for every word when checking the entire buffer
        flyspell-issue-message-flag nil)
  
  (use-package flyspell-correct
    :after flyspell
    :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

  (use-package flyspell-lazy
    :after flyspell
    :config
    (setq flyspell-lazy-idle-seconds 1
          flyspell-lazy-window-idle-seconds 3)))

(use-package flycheck
  :commands flycheck-list-errors flycheck-buffer
  :hook (emacs-startup-hook . global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)
  ;; And don't recheck on idle as often
  (setq flycheck-idle-change-delay 1.0)
  (setq flycheck-display-errors-delay 0.25)
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  
  ;; (use-package flycheck-popup-tip
  ;; :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
  ;; :hook (flycheck-mode .flycheck-popup-tip-mode)
  ;; :config
  ;; ;; (setq flycheck-popup-tip-error-prefix "X ") ; if default symbol is not in font
  ;; )
 ;; TODO does this work with corfu
(use-package flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults)
))

;; Show number of matches while searching
(use-package anzu
  :config
  (add-hook 'after-init-hook 'global-anzu-mode)
  (setq anzu-mode-lighter "")
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

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
  (isearch-search-and-update))

(define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol)
(defun sanityinc/isearch-exit-other-end ()
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))
(define-key isearch-mode-map [(control return)] 'sanityinc/isearch-exit-other-end)

(setq-default grep-highlight-matches t
              grep-scroll-output t)
(use-package wgrep
  :config
   (dolist (key (list (kbd "C-c C-q") (kbd "w")))
    (define-key grep-mode-map key 'wgrep-change-to-wgrep-mode)))
(when (executable-find "ag")
           (use-package ag))
(when (executable-find "rg")
  (use-package rg))

(use-package ibuffer-vc
  :bind ("C-x C-b" . ibuffer)
  :custom (ibuffer-show-empty-filter-groups nil)
  :config
  (defun ibuffer-set-up-preferred-filters ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))
  :hook (ibuffer . ibuffer-set-up-preferred-filters))

(use-package emacs
  :config
  (setq ad-redefinition-action 'accept)
  (defun sanityinc/newline-at-end-of-line ()
    "Move to end of line, enter a newline, and reindent."
    (interactive)
    (move-end-of-line 1)
    (newline-and-indent))
  :bind
  (("RET" . newline-and-indent)
   ("C-<return>" . sanityinc/newline-at-end-of-line)))

(use-package display-line-numbers
  :if (fboundp 'display-line-numbers-mode)
  :init
  (setq-default display-line-numbers-width 3)
  (setq-default display-line-numbers-type 'relative)
  :hook (prog-mode . display-line-numbers-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :init
  (when (fboundp 'global-prettify-symbols-mode)
    (add-hook 'after-init-hook 'global-prettify-symbols-mode)))

(use-package expand-region
  :bind (("M-[" . er/expand-region)
         ("C-(" . er/mark-outside-pairs)))

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

(use-package whitespace
  :config
  (setq-default show-trailing-whitespace nil))

(use-package emacs
  :hook ((prog-mode text-mode conf-mode) . sanityinc/show-trailing-whitespace)
  :config
  (defun sanityinc/show-trailing-whitespace ()
    "Enable display of trailing whitespace in this buffer."
    (setq-local show-trailing-whitespace t)))

(use-package whitespace-cleanup-mode
  :delight
  :hook (after-init . global-whitespace-cleanup-mode))

(use-package emacs
  :bind ([remap just-one-space] . cycle-spacing))

(use-package diff-hl
  :defer t
  :config
  :hook ((dired-mode . diff-hl-dired-mode)
         (after-init . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (:map diff-hl-mode-map
         ([left-fringe mouse-2] . diff-hl-diff-goto-hunk)))
(use-package browse-at-remote) ;; open in web

(use-package git-blamed)
;;  (use-package gitignore-mode)
;;  (use-package gitconfig-mode)
  (use-package git-time-machine
    :config
    (global-set-key (kbd "C-x v t") 'git-timemachine-toggle))

  (use-package magit
    :defer t
    :config
    (setq-default magit-diff-refine-hunk t)
    ;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
    ;; quickly open magit on any one of your projects.
    (global-set-key [(meta f12)] 'magit-status)
    (global-set-key (kbd "C-x g") 'magit-status)
    (global-set-key (kbd "C-x M-g") 'magit-dispatch)

    (defun sanityinc/magit-or-vc-log-file (&optional prompt)
      (interactive "P")
      (if (and (buffer-file-name)
               (eq 'Git (vc-backend (buffer-file-name))))
          (if prompt
              (magit-log-buffer-file-popup)
            (magit-log-buffer-file t))
        (vc-print-log)))
    (with-eval-after-load 'vc
      (define-key vc-prefix-map (kbd "l") 'sanityinc/magit-or-vc-log-file)))
(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up))
(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))
;; (use-package forge
;;   :after magit)
(use-package fullframe)
(with-eval-after-load 'magit
  (fullframe magit-status magit-mode-quit-window))
(use-package git-commit
  :config
  (add-hook 'git-commit-mode-hook 'goto-address-mode))

;; Convenient binding for vc-git-grep
(with-eval-after-load 'vc
  (define-key vc-prefix-map (kbd "f") 'vc-git-grep))

(setq-default compilation-scroll-output t)
(use-package alert)
;; Customize `alert-default-style' to get messages after compilation
(defun sanityinc/alert-after-compilation-finish (buf result)
  "Use `alert' to report compilation RESULT if BUF is hidden."
  (when (buffer-live-p buf)
    (unless (catch 'is-visible
              (walk-windows (lambda (w)
                              (when (eq (window-buffer w) buf)
                                (throw 'is-visible t))))
              nil)
      (alert (concat "Compilation " result)
             :buffer buf
             :category 'compilation))))

(with-eval-after-load 'compile
  (add-hook 'compilation-finish-functions
            'sanityinc/alert-after-compilation-finish))

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
  (require 'ansi-color)
  (defun sanityinc/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'sanityinc/colourise-compilation-buffer))

(use-package yasnippet
  :straight t
  :config
  (setq yas-verbosity 1)                      ; No need to be so verbose
  (setq yas-wrap-around-region t)
  (use-package yasnippet-snippets
    :straight t)
  (with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs '(yasnippet-snippets-dir)))

  (yas-reload-all)
  (yas-global-mode t)
  (define-key yas-minor-mode-map (kbd "C-c s") #'yas-insert-snippet)

  (defun company-yasnippet-or-completion ()
    "Solve company yasnippet conflicts."
    (interactive)
    (let ((yas-fallback-behavior
           (apply 'company-complete-common nil)))
      (yas-expand)))

  (add-hook 'company-mode-hook
            (lambda ()
              (substitute-key-definition
               'company-complete-common
               'company-yasnippet-or-completion
               company-active-map))))

(use-package paredit
  :delight paredit-mode " Par"
  :hook (paredit-mode-hook . maybe-map-paredit-newline)
  :init
  (defun maybe-map-paredit-newline ()
    (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
                (minibufferp))
      (local-set-key (kbd "RET") 'paredit-newline)))
  :config
;; Suppress certain paredit keybindings to avoid clashes
(define-key paredit-mode-map (kbd "DEL") 'delete-backward-char)
(dolist (binding '("C-<left>" "C-<right>" "C-M-<left>" "C-M-<right>" "M-s" "M-?"))
  (define-key paredit-mode-map (read-kbd-macro binding) nil)))

(use-package projectile
  :bind(:map projectile-mode-map ("C-c p" . projectile-command-map))
  :config
  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden"))
  (setq-default projectile-mode-line-prefix " Proj")   ;; Shorter modeline
  (projectile-mode))
(use-package ibuffer-projectile)

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

(use-package multi-vterm
  :hook (vterm-mode . (lambda ()
                        (setq vterm-buffer-maximum-size 1000
                              multi-vterm-dedicated-window-height-percent 30
                              left-margin-width 1
                              right-margin-width 1
                              cursor-type 'bar)))
  :bind (
         ( "C-c t" . multi-vterm-dedicated-toggle)
         ( "M-t" . multi-vterm)
         :map vterm-mode-map
         ("C-c t" . multi-vterm-dedicated-toggle)
         ("M-w" . copy-region-as-kill)
         ( "C-y" . vterm-yank)))

(setq confirm-kill-processes nil)

(use-package speed-type)

(use-package org
  :straight org-contrib
  :bind (("C-c a" .  gtd)
         (:map org-mode-map
               ( "C-M-<up>" . org-up-element)))
  :config
  (defun gtd () (interactive) (org-agenda 'nil "g"))
  (require 'ox-extra)
  (require 'org-indent)
  (setq org-latex-pdf-process (list "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -outdir=~/.cache/emacs %f")
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-log-done t
        org-return-follows-link  t
        org-edit-timestamp-down-means-later t
        org-hide-emphasis-markers t
        org-catch-invisible-edits 'show-and-error
        org-export-coding-system 'utf-8
        org-fast-tag-selection-single-key 'expert
        org-html-validation-link nil
        org-image-actual-width nil
        org-adapt-indentation t
        org-edit-src-content-indentation 0
        org-auto-align-tags nil
        org-tags-column 0
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-startup-folded t
        org-startup-with-inline-images t
        org-pretty-entities t
        org-archive-location "%s_archive::* Archive")
  ;; Agenda styling
  org-agenda-tags-column 0)
(use-package org-cliplink
  :bind (("C-c l" . org-store-link)))

(straight-use-package '(org-appear :type git :host github :repo "awth13/org-appear"))
(add-hook 'org-mode-hook 'org-appear-mode)

(setq header-line-format " ")
(custom-set-faces
   '(org-document-title ((t (:height 3.2))))
   '(header-line ((t (:height 3 :weight bold))))
   '(org-level-1 ((t (:foreground "#98be65" :height 1.6))))
  '(org-level-2 ((t (:foreground "#da8548" :height 1.2))))
  '(org-level-3 ((t (:foreground "#a9a1e1" :height 1.1))))
  '(header-line ((t (:height 2)))))

(setq org-directory "~/doc")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list "~/doc/inbox.org"
                          "~/doc/projects.org"
                          "~/doc/gcal.org"
                          "~/doc/repeater.org"))

(use-package org
  :ensure t
  :bind
  (("C-c c" . org-capture))
  :config
  (setq org-capture-templates
        `(("t" "Tasks")
          ("tt" "Todo" entry (file "~/doc/inbox.org") 
           "* TODO %?\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t)
          ("tn" "Next" entry (file "~/doc/inbox.org")
           "* NEXT %?\nSCHEDULED: %t\n%U\n%a\n" :clock-resume t)
          ("n" "Notes")
          ("nn" "General Note" entry (file "")
           "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
          ("nm" "Meeting Note" entry (file "")
           "* %? :MEETING:\n%U\n%a\n" :clock-resume t)))
  
  ;; add roam nav to org capture
  (setq org-capture-templates
      (append org-capture-templates
              '(("r" "Roam")
                ("rt" "Go to today's daily note" entry (function (lambda ()
                                                                    (org-roam-dailies-goto-today)
                                                                    (org-capture-finalize))))
                ("rf" "Find or create an Org-roam node" entry (function (lambda ()
                                                                          (org-roam-node-find)
                                                                          (org-capture-finalize))))
                ("rv" "Open Roam UI in browser" entry (function (lambda ()
                                                                          (org-roam-ui-open)
                                                                          (org-capture-finalize))))))))

(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda ()   (setq mode-line-format nil)
              (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t)))
)
(with-eval-after-load 'org-mode
  (add-hook 'before-save-hook
            (lambda ()  (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  (org-ellipsis " ⮟"))

(use-package org
  :hook (org-mode . (lambda ()
                      (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
                      (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
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

(use-package org-pomodoro
  :commands (org-pomodoro)
  :bind ((:map org-agenda-mode-map
              ("P" . org-pomodoro)))
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (setq
   alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))
  (setq org-pomodoro-finished-sound "~/Music/bell.wav"
        org-pomodoro-long-break-sound "~/Music/bell.wav"
        org-pomodoro-short-break-sound "~/Music/bell.wav"
        org-pomodoro-start-sound "~/Music/bell.wav"
        org-pomodoro-killed-sound "~/Music/bell.wav"))

(defun snehrbass/org-pomodoro-time ()
  "Return the remaining pomodoro time in sec"
  (if (org-pomodoro-active-p)
      (format "%d" (org-pomodoro-remaining-seconds))
    "0"))

(defun snehrbass/org-pomodoro-task ()
  "Return the current task"
  (if (org-pomodoro-active-p)
      (cl-case org-pomodoro-state
        (:pomodoro
           (format "%s" org-clock-heading))
        (:short-break
         (format "Short Break" ))
        (:long-break
         (format "Long Break" ))
        (:overtime
         (format "Overtime!" )))
    "No Active Pomodoro"))

(defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
  "A keymap for handy global access to org helpers, particularly clocking.")
(define-key sanityinc/org-global-prefix-map (kbd "j") 'org-clock-goto)
(define-key sanityinc/org-global-prefix-map (kbd "l") 'org-clock-in-last)
(define-key sanityinc/org-global-prefix-map (kbd "i") 'org-clock-in)
(define-key sanityinc/org-global-prefix-map (kbd "o") 'org-clock-out)
(define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(org-clock-persistence-insinuate)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

               ;;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(with-eval-after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))

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

(defun toggle-mode-line ()
  "toggles the modeline on and off"
       (interactive)
       (setq mode-line-format
             (if (equal mode-line-format nil)
                 (default-value 'mode-line-format)))
       (redraw-display))

(use-package org-pretty-table
  :straight (:host github :repo "Fuco1/org-pretty-table"
                   :branch "master")
  :hook (org-mode . org-pretty-table-mode))
(use-package org
  :bind ((:map org-mode-map
               ("C-c v" . wr-mode)))
  :hook (org-mode . wr-mode)
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
      (visual-line-mode -1))))

(use-package visual-fill-column
  :defer t
  :init
  (defun dw/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

  :hook (org-mode . dw/org-mode-visual-fill))

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

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n/!)" "INPROGRESS(i/!)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")
(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))

(use-package org
  :ensure t
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
            (tags-todo "INBOX|PROJECT"
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
            (tags-todo "-INBOX/-NEXT"
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
 :ensure t
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
  :config
  (setq org-attach-screenshot-command-line "/usr/share/sway/scripts/grimshot copy area") )

(use-package pdf-tools
  :config
  (add-hook 'doc-view-mode-hook #'pdf-tools-install)
  (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12"))
  (setq-default pdf-view-display-size 'fit-width))

(use-package markdown-mode
  :config
  (add-auto-mode 'markdown-mode "\\.md\\.html\\'")
  (with-eval-after-load 'whitespace-cleanup-mode
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)))

(use-package org-roam
  :straight t
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
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil))

(when (require 'init-gcal nil 'noerror)
  (message "init-gcal loaded"))

(use-package tree-sitter-langs
  :defer t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package consult-lsp)
(use-package lsp-mode
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  :commands (lsp lsp-deferered)
  :custom
  (read-process-output-max (* 3 1024 1024)) ;; 3mb
  (lsp-completion-provider :none)           ;; corfu instaed
  (lsp-idle-delay 0.8)
  :config (lsp-enable-which-key-integration t)
  :bind-keymap ("C-." . lsp-command-map)
  :bind ((:map lsp-command-map
               ("s" . lsp-ui-doc-show)
               ("i" . lsp-find-implementation)
               ("r" . lsp-find-references)
               ("R" . lsp-rename)
               ("e" . consult-lsp-diagnostics)))
  :hook ((lsp-completion-mode . my/lsp-mode-setup-completion)
         (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay .2 )
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-eldoc-enable-hover t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-enable nil)
  :commands lsp-ui-mode)

(use-package dap-mode
  :config
  (dap-mode 1)
  (require 'dap-dlv-go)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :bind (:map go-mode-map
              ("C-," . go-goto-map)
              ("C-c C-c" . compile))
  :custom(lsp-register-custom-settings
          '(("gopls.completeUnimported" t t)
            ("gopls.staticcheck" t t)))
  :config
  (use-package flycheck-golangci-lint
    :config
    :hook (lsp-diagnostics-mode . (lambda ()
                     (flycheck-add-next-checker 'lsp 'golangci-lint)
                     (flycheck-add-next-checker 'lsp 'go-gofmt)
                     )))
  
  (use-package gorepl-mode
    :commands gorepl-run-load-current-file)
  
  (setq compile-command "go build -v && go test -v -cover && go vet") 
  ;; remote go
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     (lambda ()
                                       (cons "gopls" lsp-gopls-server-args)))
                    :major-modes '(go-mode)
                    :priority 10
                    :server-id 'gopls-remote
                    :remote? t
                    )))

(use-package ccls
  :straight t
  :config
  (setq ccls-executable "ccls")
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))
(defun lsp-cpp-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'cc-mode-hook #'lsp-cpp-install-save-hooks)

(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :custom
    (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (setq rustic-format-on-save t))

(use-package python
  :config
  (use-package conda
    :after python
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
  (use-package lsp-python-ms
    :after lsp-mode))

(use-package csv-mode)
(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")
(setq csv-separators '("," ";" "|" " " ", "))

(use-package yaml-mode
  :config (add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")
  :hook (yaml-mode-hook .goto-address-prog-mode))

(use-package docker
  :config
  (fullframe docker-images tablist-quit)
  (fullframe docker-machines tablist-quit)
  (fullframe docker-volumes tablist-quit)
  (fullframe docker-networks tablist-quit)
  (fullframe docker-containers tablist-quit))
(use-package dockerfile-mode)
(use-package docker-compose-mode)

(use-package terraform-mode)

(use-package yuck-mode)

(use-package gptel
  :straight t
  :bind (("<f5>" . gptel)
         ("C-<f5>" . gptel-menu))
  :config
  (defun gpt/read-openai-key ()
    (with-temp-buffer
      (insert-file-contents "~/.gpt-key.txt")
      (string-trim (buffer-string))))
  (setq gptel-model "gpt-3.5-turbo"
                gptel-playback t
                gptel-default-mode 'org-mode
                gptel-api-key #'gpt/read-openai-key))

(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216 ; 16mb
                          ))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1
          file-name-handler-alist doom--file-name-handler-alist)))