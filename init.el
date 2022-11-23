;; Adjust this font size
(defvar goblemacs/default-font-size 120)

(setq inhibit-startup-message t)

(setq scroll-step 1)
(setq scroll-margin 5)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

(setq dired-dwim-target t)

(setq delete-by-moving-to-trash t)

;; DiredOmitMode
(setq my-dired-ls-switches "-alh --ignore=.* --ignore=\\#* --ignore=*~")

(setq my-dired-switch 1)

(add-hook 'dired-mode-hook
	(lambda ()
	    "Set the right mode for new dired buffers."
	    (when (= my-dired-switch 1)
	    (dired-sort-other my-dired-ls-switches))))

(add-hook 'dired-mode-hook
	(lambda ()
	    (define-key dired-mode-map (kbd "M-o")
	    (lambda ()
		"Toggle between hide and show."
		(interactive)
		(setq my-dired-switch (- my-dired-switch))
		(if (= my-dired-switch 1)
		    (dired-sort-other my-dired-ls-switches)
		(dired-sort-other "-alh"))))))

(global-hl-line-mode t)		; Highlight current line

; (scroll-bar-mode -1)	; Disable visible scrollbar
(tool-bar-mode -1)	; Disable the toolbar
(tooltip-mode -1)	; Disable tooltips
(set-fringe-mode 10)	; Give some breathing room

(menu-bar-mode -1)	; Disable the menubar

;; Setup the visible bell
(setq visible-bell nil
      ring-bell-function #'ignore)

;; Set font size
(set-face-attribute 'default nil
		    :font "monospace"
		    :height goblemacs/default-font-size)

;; Make ESC quit prompts
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Display line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(menu-bar--display-line-numbers-mode-relative)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (disaplay-line-numbers-mode 0))))

(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-switch-buffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))	;; Don't start searches with ^

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts

(use-package all-the-icons)

;; use doom-modeline
(use-package doom-modeline
  :ensure
  :init (doom-modeline-mode 1))

(use-package doom-themes)

;; Set theme
(load-theme 'doom-gruvbox t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-cammand] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(defconst goblemacs/leader "C-M-;")

(use-package org
  :config
  (setq org-ellipsis " ▼"
	org-hide-emphasis-markers t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(require 'org-mouse)

(use-package general
  :config
  (general-create-definer goblemacs/leader-keys
    :keymaps '(normal insert visual emacs)
    :global-prefix goblemacs/leader)

  (goblemacs/leader-keys
   "t"	'(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")))

(general-define-key
 "C-M-j" 'counsel-switch-buffer
 "C-s" 'counsel-grep-or-swiper
 "C-SPC" nil	;; Preserve for input method
 "C-=" 'yank
 "M-h" nil
 "M-j" nil
 "M-k" nil
 "M-l" nil
 "M-;" nil
 "M-o" nil)

(general-create-definer goblemacs/leader-def
  :prefix goblemacs/leader)

(goblemacs/leader-def
 "a" 'org-agenda
 "b" 'counsel-bookmark
 "c" 'org-capture)

(use-package undo-tree)

(global-undo-tree-mode)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h")
    'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  ;(evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ;(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(general-define-key
 "M-h" 'evil-backward-char
 "M-j" 'evil-next-visual-line
 "M-k" 'evil-previous-visual-line
 "M-l" 'evil-forward-char
 "M-w" 'evil-forward-word-begin
 "M-b" 'evil-backward-word-begin
 "M-;" 'evil-normal-state
 "M-o" 'evil-open-below
 "C-M-o" 'evil-open-above
 "M-a" 'evil-append-line
 "M-d" 'evil-insert-line
 "M-p" 'evil-paste-after
 "M-P" 'evil-paste-before
 "M-u" 'evil-undo)

;; Replace C-w with SPC
(general-def :states '(normal motion emacs) "SPC" 'evil-window-map)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 5)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(goblemacs/leader-keys
 "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents/Projects")
    (setq projectile-project-search-path '(("~/Documents/Projects" . 5))))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package rg)

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; forge
; (use-package forge)

;; This part is stolen from:
;; https://hiepph.github.io/post/2018-11-07-use-package/
(use-package neotree
  :ensure t
  :bind ("<f8>" . 'neotree-toggle)
  :init
  ;; slow rendering
  (setq inhibit-compacting-font-caches t)

  ;; set icons theme
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

  ;; Every time when the neotree window is opened, let it find current file and
  ;; jump
  (setq neo-smart-open t)

  ;; show hidden files
  ; (setq-default neo-show-hidden-files t)

  ;; When running 'projectile-switch-project' (C-c p p), 'neotree' will change
  ;; root automatically
  (setq projectile-switch-project-action 'neotree-projectile-action))

;; Set org babel
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
			     (python . t)
			     (ruby . t)))

(setq org-confirm-babel-evaluate nil)

(electric-pair-mode)

;; This part is stolen from:
;; https://www.topbug.net/blog/2016/09/29/emacs-disable-certain-pairs-for-electric-pair-mode/
;; electric pair disable '<'
(setq electric-pair-inhibit-predicate
      `(lambda (c)
	 (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))

;; Structure templates
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("sho" . "src shell :results output"))
(add-to-list 'org-structure-template-alist '("shv" . "src shell :results value"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("elo" . "src emacs-lisp :results output"))
(add-to-list 'org-structure-template-alist '("elv" . "src emacs-lisp :results value"))
(add-to-list 'org-structure-template-alist '("rb" . "src ruby"))
(add-to-list 'org-structure-template-alist '("rbo" . "src ruby :results output"))
(add-to-list 'org-structure-template-alist '("rbv" . "src ruby : results value"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("pyo" . "src python :results output"))
(add-to-list 'org-structure-template-alist '("pyv" . "src python :results value"))

;; lsp-mode
(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui :commands lsp-ui-mode)

;; You need install ccls first
(use-package ccls
  :hook ((c-mode c++-mode cuda-mode) .
	 (lambda () (require 'clss) (lsp))))

(setq ccls-executable "/usr/bin/ccls")
;; (setq ccls-args '("--log-file=/tmp/ccls.log"))

;; treemacs
;; https://github.com/Alexander-Miller/treemacs#installation
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-t") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-t"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; vterm
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 1000))

;; eshell
(use-package eshell-git-prompt)

(use-package eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))
