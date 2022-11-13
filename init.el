;; Adjust this font size
(defvar goblemacs/default-font-size 100)

(setq inhibit-startup-message t)

(setq scroll-step 1)
(setq scroll-margin 5)

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

(use-package general
  :config
  (general-create-definer goblemacs/leader-keys
    :keymaps '(normal insert visual emacs)
    :global-prefix goblemacs/leader)

  (goblemacs/leader-keys
   "t"	'(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")))

(general-define-key
 "C-;" 'counsel-switch-buffer
 "C-s" 'counsel-grep-or-swiper
 "C-SPC" nil	;; Preserve for input method
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
 "M-u" 'evil-undo)

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
