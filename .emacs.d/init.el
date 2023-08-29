(add-to-list 'load-path "~/.emacs.d/lisp/")
(eval-when-compile
  (require 'package)
  (require 'use-package))
(setq package-native-compile t
      package-quickstart nil
      package-install-upgrade-built-in t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq use-package-always-ensure t
      use-package-expand-minimally t
      use-package-compute-statistics t)
;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
;; set this after no-littering
(add-hook 'package-upgrade-all-hook
          (lambda ()
            (package-quickstart-refresh)))
(use-package use-package-ensure-system-package
  :ensure t)

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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package move-dup
  :config (global-move-dup-mode)
  :bind(("M-<up>" . move-dup-move-lines-up)
         ("M-<down>" . move-dup-move-lines-down)
         ("C-c d" . move-dup-duplicate-down)
         ("C-c u" . move-dup-duplicate-up)))

(use-package whole-line-or-region
  :hook (after-init . whole-line-or-region-global-mode)
  :bind (:map whole-line-or-region-local-mode ("M-j". comment-dwim))
  )

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
 indent-tabs-mode nil
 tab-width 4
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 ring-bell-function 'ignore)
(delete-selection-mode t)
(global-goto-address-mode t)
(add-hook 'after-init-hook 'transient-mark-mode) ;; standard highlighting
(setq browse-url-browser-function #'browse-url-firefox)

(global-set-key (kbd "C-c C-p") 'find-file-at-point)

(use-package recentf
  :ensure nil
  :hook ((after-init . recentf-mode)
          (package-upgrade-all . recentf-cleanup))   
  :custom
  (bookmark-save-flag 1)
  (bookmark-default-file (expand-file-name "var/bookmarks.el" user-emacs-directory))
  (recentf-auto-cleanup 'never) ; Disable automatic cleanup at load time
  (recentf-max-saved-items 80)
  (recentf-exclude '("/tmp/"))
  :config
  (add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-var-directory)))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :delight auto-revert-mode)

(use-package tramp
  :ensure nil
  :commands tramp-mode
  :custom
  ;; (tramp-default-method "ssh")
  (tramp-verbose 0)
  (tramp-encoding-shell "/bin/bash")
  (tramp-ssh-controlmaster-options (concat
    "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
    "-o ControlMaster=auto -o ControlPersist=yes"))
  (tramp-connection-properties (quote ((file-remote-p (tramp-file-name nil host port)) " ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p ")))
  :config
  (setq vc-handled-backends '(Git))
   ;; use remote path
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(defun load-all-environment-variables ()
  "Load all environment variables from the user's shell."
  (let ((shell-env (shell-command-to-string "env")))
    (dolist (var (split-string shell-env "\n"))
      (when (string-match "\\([^=]+\\)=\\(.*\\)" var)
        (let ((name (match-string 1 var))
              (value (match-string 2 var)))
          (setenv name value))))))
(add-hook 'after-init-hook 'load-all-environment-variables)

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))

;; Show number of matches while searching
(use-package anzu
  :hook (after-init . global-anzu-mode)
  :custom
  (anzu-mode-lighter "")
  :config
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace)
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
    (isearch-search-and-update))
  (define-key isearch-mode-map "\C-\M-w" 'isearch-yank-symbol))

(use-package ibuffer-vc
  :bind ("C-x C-b" . ibuffer)
  :custom (ibuffer-show-empty-filter-groups nil)
  :config
  (defun ibuffer-set-up-preferred-filters ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))
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

(setq-default show-trailing-whitespace nil)
(use-package whitespace-cleanup-mode
  :hook ((prog-mode text-mode conf-mode) . sanityinc/show-trailing-whitespace)
  :delight
  :hook (after-init . global-whitespace-cleanup-mode)
  :config
  (push 'markdown-mode whitespace-cleanup-mode-ignore-modes)
  (defun sanityinc/show-trailing-whitespace ()
    "Enable display of trailing whitespace in this buffer."
    (setq-local show-trailing-whitespace t)))

(use-package electric-pair-mode ; Easily insert matching delimiter
  :ensure nil
  :hook (after-init . electric-pair-mode))
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

(setq default-frame-alist '((alpha-background . 90) (font . "Source Code Pro-11") (left-fringe . 10) (right-fringe . 10) (vertical-scroll-bars . nil)))
(add-hook 'after-init-hook
  (lambda ()
    (pixel-scroll-precision-mode t) 
    (set-face-attribute 'header-line nil :height 100)))

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
  :hook (after-init . global-prettify-symbols-mode))

(use-package doom-modeline
  :hook ((after-init . doom-modeline-mode))
  :init
  (line-number-mode -1)
  (column-number-mode -1)
  (setq mode-line-position nil)
  :custom ((doom-modeline-project-detection 'project)
           (doom-modeline-vcs-max-length 30)
           (doom-modeline-hud t)
           (doom-modeline-env-version t)
           (doom-modeline-buffer-encoding nil)
           (doom-modeline-workspace-name t)
           (doom-modeline-buffer-file-name-style 'auto)
           (doom-modeline-height 32)
           (doom-modeline-buffer-state-icon t)
           (doom-modeline-icon t)))

(use-package treemacs
  :commands (treemacs)
  :bind ("C-c C-t" . treemacs)
  :hook (treemacs-mode . (lambda () (setq truncate-lines t))))
(use-package treemacs-magit
  :after (treemacs magit))
(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

(defun sanityinc/maybe-suspend-frame ()
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z M-z") 'sanityinc/maybe-suspend-frame)
(global-set-key (kbd "C-z") 'undo)

(use-package default-text-scale
  :hook (after-init . default-text-scale-mode))

(use-package vertico
  :hook (after-init . vertico-mode))
(use-package marginalia
  :after vertico
  :hook (vertico-mode . marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))
(use-package all-the-icons-completion
  :hook ((marginalia-mode . all-the-icons-completion-marginalia-setup)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package wgrep
  :commands (wgrep wgrep-change-to-wgrep-mode))

(use-package consult
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
  (setq consult-ripgrep-args (concat consult-ripgrep-args " --hidden"))
  ;; commands that I don'want to preview while scrolling
  ;; (consult-customize
  ;;   consult-grep consult-bookmark consult-recent-file consult-xref
  ;;   consult--source-bookmark consult--source-file-register
  ;;   consult--source-recent-file consult--source-project-recent-file 
  ;;   :preview-key '( :debounce 1 any ))
  (consult-customize
   consult-line 
   :add-history (seq-some #'thing-at-point '(region symbol)))
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
         :map vertico-map
         ("M-o" . embark-export)
         ("C-h B" . embark-bindings))
  :config
  (setq embark-action-indicator (lambda (map _target)
                                  (which-key--show-keymap "Embark" map nil nil 'no-paging)
                                  #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(add-to-list 'load-path "~/src/protogg")
(use-package protogg
  :ensure nil
  :load-path "~/src/protogg/protogg.el"
  :hook (after-init . protogg-mode)
  :custom (protogg-minibuffer-toggle-key "M-g")
   :bind (:map protogg-mode-map
         ([remap async-shell-command] . protogg-async-shell-command) ;; M-&
         ("C-c x" . protogg-compile)
         ([remap dired] . protogg-dired) ;; C-x d
         ("C-c e" . protogg-eshell)
         ("M-s d" . protogg-find-dired)
         ([remap find-file] . protogg-find-file) ;; C-x C-f
         ([remap list-buffers] . protogg-list-buffers) ;; type C-x C-b
         ([remap shell-command] . protogg-shell-command) ;; M-!
         ("C-c s" . protogg-shell)
         ([remap switch-to-buffer] . sn/consult-buffer)
         ("M-s i" . sn/imenu)) ;; C-x b
  :config
  (protogg-create 'consult-project-buffer 'consult-buffer sn/consult-buffer)
  (protogg-create 'consult-imenu-multi 'consult-imenu sn/imenu))

(use-package corfu
  :hook ((after-init . global-corfu-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :bind (:map corfu-map ("M-SPC" . corfu-insert-separator)
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :custom
  (tab-always-indent 'complete)
  (corfu-quit-no-match 'separator)
  (corfu-auto-prefix 2))

(use-package corfu-terminal
  :when (not (display-graphic-p))
  :vc (corfu-terminal :url "https://codeberg.org/akib/emacs-corfu-terminal.git"
                      :branch "master"))

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

(use-package cape
  :after corfu
   :bind (("M-/" . completion-at-point) ;; overwrite dabbrev-completion binding with capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (defalias 'corfu--ispell-in-comments-and-strings
    (cape-super-capf (cape-capf-inside-comment #'cape-dict)
                     (cape-capf-inside-string #'cape-dict))))

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

(use-package ispell
  :defer t
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--run-together")))
(use-package flyspell
  :hook ((org-mode markdown-mode TeX-mode git-commit-mode
           yaml-mode conf-mode prog-mode) . flyspell-mode)
  :bind (:map flyspell-mode-map
              ("C-." . nil)) ;; Unbind the key
  :config
  (setq flyspell-issue-welcome-flag nil
        ;; Significantly speeds up flyspell, which would otherwise print
        ;; messages for every word when checking the entire buffer
        flyspell-issue-message-flag nil))

(use-package define-word
  :commands define-word)
(use-package flyspell-correct
  :after flyspell
    :bind (:map flyspell-mode-map ("M-$" . flyspell-correct-wrapper)))

(use-package flycheck
  :commands flycheck-list-errors flycheck-buffer
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)
  ;; And don't recheck on idle as often
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))
;; TODO: use this when in terminal 
  (use-package flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode)
  ;; (setq flycheck-popup-tip-error-prefix "X ") ; if default symbol is not in font
  )
;; (use-package flycheck-posframe
;;   :hook (flycheck-mode . flycheck-posframe-mode)
;;   :config
;;   (flycheck-posframe-configure-pretty-defaults))

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
         :map vertico-map
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
  ;; ox-extra 
  :after org
  :defer t
  :config)
(use-package ox-extra ;; ignore header tagged wit ignore
  :after org-contrib
  :ensure nil)
(use-package org
  :bind (("C-c a" .  gtd)
         (:map org-mode-map
               ( "C-M-<up>" . org-up-element)))
  :config
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
(use-package ox-latex
  :ensure nil
  :after org-contrib
  :custom org-latex-pdf-process (list "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -outdir=~/.cache/emacs %f"))

(use-package org-cliplink
  :bind (("C-c l" . org-store-link)))
(custom-set-faces
   '(org-document-title ((t (:height 3.2))))
   '(header-line ((t (:height 3 :weight bold))))
   '(org-level-1 ((t (:foreground "#98be65" :height 1.6))))
  '(org-level-2 ((t (:foreground "#da8548" :height 1.2))))
  '(org-level-3 ((t (:foreground "#a9a1e1" :height 1.1))))
  '(header-line ((t (:height 2)))))

(use-package org-appear
  :vc (org-appear :url "https://github.com/awth13/org-appear"
                  :branch "master")
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
  :after org-clock
  :commands (org-pomodoro snehrbass/org-pomodoro-time snehrbass/org-pomodoro-task)
  :bind ((:map org-agenda-mode-map
               ("P" . org-pomodoro)))
  :hook ((org-pomodoro-started . gopar/load-window-config-and-close-work-agenda)
         (org-pomodoro-finished . gopar/save-window-config-and-show-work-agenda))
  :custom ((org-pomodoro-clock-break t)
           (org-pomodoro-manual-break t))
  :config
  (defun snehrbass/org-pomodoro-time ()
    "Return the remaining pomodoro time in sec"
    (if (org-pomodoro-remaining-seconds)
        (format "%d" (org-pomodoro-remaining-seconds))
      "0"))

  (defun snehrbass/org-pomodoro-task ()
    "Return the current task"
    (if org-pomodoro-state
        (cl-case org-pomodoro-state
          (:pomodoro
           (format "%s" org-clock-heading))
          (:short-break
           (format "Short Break" ))
          (:long-break
           (format "Long Break" ))
          (:overtime
           (format "Overtime!" ))
          (:none
           (format "No Active Pomodoro" )))
      "No Active Pomodoro"))
  
   (defun gopar/save-window-config-and-show-work-agenda ()
    (interactive)
    (window-configuration-to-register ?`)
    (delete-other-windows)
    (org-save-all-org-buffers)
    (org-agenda nil "w"))
   
  (defun gopar/load-window-config-and-close-work-agenda ()
    (interactive)
    (org-save-all-org-buffers)
    (jump-to-register ?`)))

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

(with-eval-after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))

(use-package type-break
  :after org-pomodoro
  :hook ((org-pomodoro-killed . type-break-mode)
         (org-pomodoro-break-finished . type-break-mode)
         (org-pomodoro-started . (lambda () (type-break-mode -1)))
         (after-init . type-break-mode))
  :init
  (defun type-break-demo-agenda ()
    "Display the Org Agenda in read-only mode. Cease the demo as soon as a key is pressed."
    (let ((buffer-name "*Typing Break Org Agenda*")
          lines)
      (condition-case ()
          (progn
            (org-agenda-list)
            (setq buffer-name (buffer-name))
            ;; Set the buffer to read-only
            (with-current-buffer buffer-name
              (read-only-mode 1))
            ;; Message to be displayed at the bottom
            (let ((msg (if type-break-terse-messages
                           ""
                         "Press any key to resume from typing break")))
              ;; Loop until key is pressed
              (while (not (input-pending-p))
                (sit-for 60))
              ;; Clean up after key is pressed
              (read-event)
              (type-break-catch-up-event)
              (kill-buffer buffer-name)))
        (quit
         (and (get-buffer buffer-name)
              (kill-buffer buffer-name))))))

  :custom
  ;; Setting interval of that of a pomodoro session
  (type-break-interval (* 25 60)) ;; 25 mins
  (type-break-good-rest-interval (* 9 60)) ;; 9 mins
  (type-break-good-break-interval (* 5 60)) ;; 5 mins
  (type-break-query-mode t)
  (type-break-keystroke-threshold '(nil . 2625))
  (type-break-demo-boring-stats t)
  (type-break-demo-functions '(type-break-demo-agenda)))

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

(use-package org-pretty-table
  :vc (org-pretty-table :url "https://github.com/Fuco1/org-pretty-table"
                        :branch "master")
  :hook (org-mode . org-pretty-table-mode))
(use-package org
  :bind ((:map org-mode-map
               ("C-c v" . wr-mode)))
  :hook ((org-mode . wr-mode)
         (org-mode . (lambda ()
            (setq-local buffer-face-mode-face '((:family "Product Sans")))
            (setq-local corfu-auto-delay 0.8)
            (buffer-face-mode))))
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
  :custom
  (org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n/!)" "INPROGRESS(i/!)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")
  (org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face)))))

(use-package org
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
  :vc (org-roam-ui :url "https://github.com/org-roam/org-roam-ui"
                   :branch "main")
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

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package treesit-auto
  :init
  (setq treesit-font-lock-level 4)
  :hook ((package-upgrade-all . treesit-auto-install-all))
  :config (global-treesit-auto-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (read-process-output-max (* 3 1024 1024)) ;; 3mb
  (lsp-completion-provider :none)           ;; corfu instead
  (lsp-idle-delay 0.3)
  (lsp-enable-which-key-integration t)
  :init
 (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
 :hook ((lsp-completion-mode . my/lsp-mode-setup-completion)
        (go-ts-mode . lsp-deferred))
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     (lambda ()
                                       (setq-local lsp-enable-file-watchers nil)
                                       (cons "gopls" '("-remote=auto"))))
                    :major-modes '(go-ts-mode)
                    :priority 10
                    :server-id 'gopls-remote
                    :remote? t
                    ))
  :bind-keymap ("C-." . lsp-command-map)
  :bind ((:map lsp-command-map
               ("C-r" . lsp-workspace-restart)
               ("C-q" . lsp-workspace-shutdown)
               ("s" . lsp-ui-doc-show)
               ("i" . lsp-find-implementation)
               ("?" . lsp-find-references)
               ("r" . lsp-rename)
               ("j" . lsp-ui-imenu)
               ("c" . compile)
               ("C" . recompile)
               ("d" . dap-hydra)
               )))
(use-package consult-lsp
  :after (consult lsp-mode)
  :bind ((:map lsp-command-map
               ("." . consult-lsp-diagnostics)))
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay .2 )
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-eldoc-enable-hover t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-enable nil))

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode)
         (prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :bind (:map diff-hl-mode-map
         ([left-fringe mouse-2] . diff-hl-diff-goto-hunk))
  :config
  (defun my/disable-diff-hl-for-remote-buffers ()
    (when (and buffer-file-name (file-remote-p buffer-file-name))
      (diff-hl-dired-mode -1)
      (diff-hl-mode -1)))
  (add-hook 'find-file-hook #'my/disable-diff-hl-for-remote-buffers))
(use-package browse-at-remote
  :commands (browse-at-remote browse-at-remote-kill))

(use-package magit
  :commands (magit-status magit-dispatch)
  :config
  (fullframe magit-status magit-mode-quit-window)
  (setq-default magit-diff-refine-hunk t)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         (:map magit-status-mode-map
               ("C-M-<up>" . magit-section-up))))
(use-package git-blamed
  :after magit)
(use-package forge
  :after magit)
(use-package magit-todos
  :after magit
  :hook(magit-mode . magit-todos-mode))

(use-package vterm
  :hook ((vterm-mode . (lambda ()
                         (toggle-mode-line)
                         (setq left-margin-width 1
                               right-margin-width 1
                               cursor-type 'bar))))
  :bind (( "M-t" . toggle-vterm-buffer)
         :map vterm-mode-map
         ("M-t" . toggle-vterm-buffer)
         ("C-M-r" . (lambda ()
                     (interactive)
                     (setq-local vterm-buffer-name-string nil)
                     (rename-buffer (concat "Term " (read-string "Term: ")))))
         ("C-M-t" .(lambda ()
                     (interactive)
                     (vterm "Term")))
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
  :hook ((go-ts-mode . (lambda ()
                         (add-hook 'before-save-hook 'lsp-format-buffer nil t)
                         (add-hook 'before-save-hook 'lsp-organize-imports nil t))))
  :custom
  (compile-command "go build -v && go test -v -cover && go vet")
  (lsp-register-custom-settings
   '(("gopls.staticcheck" t t))))
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

(use-package ccls
  :after
  (:any c-mode c++-mode objc-mode)
  :config
  (setq ccls-executable "ccls")
  :hook ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp))))

(use-package rustic
  :bind (:map rustic-mode-map
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :custom
    (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (setq rustic-format-on-save t))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))
(use-package lsp-python-ms
  :commands (lsp lsp-deferered)
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))
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

(use-package csv-mode
  :mode ("\\.[Cc][Ss][Vv]\\'" . python-mode)
  :config
  (setq csv-separators '("," ";" "|" " " ", ")))

(use-package yaml-mode
  :config (add-auto-mode 'yaml-mode "\\.yml\\.erb\\'")
  :hook (yaml-mode-hook .goto-address-prog-mode))

(use-package docker
  :bind ("C-c d" . docker)
  :config
  (fullframe docker-images tablist-quit)
  (fullframe docker-machines tablist-quit)
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
  :config
  (setq lisp-indent-offset 2))

(use-package whisper
  :vc (whisper :url "https://github.com/natrys/whisper.el"
               :branch "master")
  :bind ("C-h w" . whisper-run)
  :config
  (setq whisper-install-directory "~/.cache/"
        whisper-model "base"
        whisper-language "en"
        whisper-translate nil))

(use-package gptel
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

(use-package speed-type :commands speed-type-top-x)

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
