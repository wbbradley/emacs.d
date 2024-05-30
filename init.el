;;; package --- Summar of My config
;;;; Commentary:
;;;; Code:
(package-initialize)

(require 'package)
(require 'compile)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa-nongnu" . "https://elpa.nongnu.org/nongnu/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; (load "package")
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; export EDITOR='emacsclient -nw -c -a ""'

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq my-theme 'gruvbox-dark-hard)
(use-package gruvbox-theme :ensure t)
(load-theme my-theme t)

(use-package powerline
             :ensure t
             :config (powerline-center-evil-theme))

(menu-bar-mode -1)
;; (scroll-bar-mode 0)
;; (tool-bar-mode 0)

(defalias 'yes-or-no-p 'y-or-n-p)


;; (global-linum-mode 1)
(setq linum-format " %4d ")


(line-number-mode 1)
(column-number-mode 1)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)


(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only signal; pass the rest to the default handler."
  (when (not (eq (car data) 'text-read-only))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)

;; *** Enable usage of xclip
;; (use-package xclip :config (xclip-mode 1))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(setq create-lockfiles nil)

(defun add-kbd (key) (kbd key))
(defvar keybindings-to-unset '("M-k" "M-j"))
(dolist (key (mapcar 'add-kbd keybindings-to-unset))
  (global-unset-key key))

(electric-pair-mode 0)

(global-visual-line-mode t)

(setq standard-indent 2)
(setq-default indent-tabs-mode nil)

(use-package drag-stuff
             :ensure t)
(drag-stuff-global-mode 1)
;; (global-set-key (kbd "C-k") 'drag-stuff-up)
;; (global-set-key (kbd "C-j") 'drag-stuff-down)

(setq show-paren-delay 0)
(show-paren-mode 1)

;; Evil mode
(use-package evil
             :ensure t
             :init
             ;; (global-set-key (kbd "C-i") 'evil-jump-forward)
             (setq evil-want-C-i-jump t)
             (setq evil-want-C-u-scroll t)
             (setq evil-vsplit-window-right t)
             (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
             (setq evil-want-keybinding nil)
             (setq evil-undo-system 'undo-redo)
             :config
             (evil-mode 1)
             (use-package evil-leader
                          :ensure t
                          :config
                          (global-evil-leader-mode))

             (use-package evil-surround
                          :ensure t
                          :config
                          (global-evil-surround-mode))
             (use-package evil-commentary
                          :ensure t
                          :config
                          (evil-commentary-mode)))
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-escape
  :config
  (setq-default evil-escape-delay 0.3)
  (setq-default evil-escape-key-sequence "jk")
  :init
  (evil-escape-mode)
  :ensure t)

;; ** Make escape quit most things
(defun minibuffer-keyboard-quit ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; (use-package ivy :ensure t)
;; (use-package counsel :ensure t)
;; (use-package swiper :ensure t)
;; (ivy-mode)
;; (setq ivy-use-virtual-buffers t)
;; (setq enable-recursive-minibuffers t)
;; ;; enable this if you want `swiper' to use it
;; ;; (setq search-default-mode #'char-fold-to-regexp)
;; (global-set-key "\C-s" 'swiper)
;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
;; (global-set-key (kbd "<f6>") 'ivy-resume)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-p") 'counsel-find-file)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "<f3>") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; (use-package navigate :ensure t)
;; [[https://github.com/keith/evil-tmux-navigator][This package]] enables seamless C-[hjkl] movement through tmux panes _and_ Emacs windows. The following commands are required to be present in your tmux config:
;; bind -n C-h run "(tmux display-message -p '#{pane_current_command}' | grep -iqE '(^|\/)n?vim(diff)?$|emacs.*$' && tmux send-keys C-h) || tmux select-pane -L"
;; bind -n C-j run "(tmux display-message -p '#{pane_current_command}' | grep -iqE '(^|\/)n?vim(diff)?$|emacs.*$' && tmux send-keys C-j) || tmux select-pane -D"
;; bind -n C-k run "(tmux display-message -p '#{pane_current_command}' | grep -iqE '(^|\/)n?vim(diff)?$|emacs.*$' && tmux send-keys C-k) || tmux select-pane -U"
;; bind -n C-l run "(tmux display-message -p '#{pane_current_command}' | grep -iqE '(^|\/)n?vim(diff)?$|emacs.*$' && tmux send-keys C-l) || tmux select-pane -R"

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
;; (define-key evil-normal-state-map [bktab] 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd "C-p") 'fzf-git-files) ;; helm-ls-git)
(define-key evil-normal-state-map (kbd "F") 'grep)
(define-key evil-normal-state-map (kbd "H") 'previous-error)
(define-key evil-normal-state-map (kbd "L") 'next-error)
(define-key evil-normal-state-map (kbd "<f9>") 'previous-error)
(define-key evil-normal-state-map (kbd "<f10>") 'next-error)

;; (global-set-key (kbd "<f3>") 'helm-git-grep)
(define-key evil-normal-state-map (kbd "<f3>") 'grep-word-under-cursor)
(defun grep-word-under-cursor ()
  "setting up grep-command using current word under cursor as a search string"
  (interactive)
  (let* ((cur-word (symbol-at-point))
         (cmd (concat "git grep -nH -e '\\<" (symbol-name cur-word) "\\>'")))
    (grep-apply-setting 'grep-command cmd)
    (grep cmd)))

;; (define-key evil-motion-state-map (kbd "jk") (kbd [escape]))
;; map :e
;; (define-key evil-ex-map "e" 'find-file)
(use-package fzf
  :bind
  (("C-p" . fzf-git-files)
   ("F" . fzf-git-grep))
    ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

(define-key evil-motion-state-map (kbd "-") 'evil-first-non-blank)
;; (define-key evil-motion-state-map (kbd "^") 'evil-beginning-of-line)

;; Evil Leader keybindings
(evil-leader/set-leader "\\")
(evil-leader/set-key
  "f" 'helm-browse-project
  "d" 'edebug-defun
  "F" 'helm-git-grep
  "0" '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  ;; "F" 'helm-projectile-ag
  "q" 'evil-quit
  "w" 'save-buffer
  "g" 'magit)

(use-package helm :ensure t :config (helm-mode t))
(use-package helm-ls-git :ensure t)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(use-package helm-git-grep :ensure t)
;; (global-unset-key (kbd "<f3>"))
(global-set-key (kbd "C-p") 'helm-ls-git)
(setq helm-ls-git-sources '(helm-source-ls-git)) 

;; (use-package projectile :ensure projectile :config (setq projectile-indexing-method 'git))
;; (use-package helm-projectile :ensure t)
;; (use-package helm-ag :ensure t)
;; (use-package company :ensure t :config (global-company-mode t) (setq company-global-modes '(not org-mode)))
;; (define-key company-mode-map (kbd "TAB") 'company-complete)
(use-package rainbow-delimiters
             :init
             (add-hook 'web-mode-hook #'rainbow-delimiters-mode)
             (add-hook 'rust-mode-hook #'rainbow-delimiters-mode))
(use-package magit
             :ensure t
             :config (setq magit-diff-refine-hunk 'all))
(use-package diff-hl
             :ensure t
             :init
             (setq diff-hl-side 'right))
(global-diff-hl-mode 1)
(diff-hl-margin-mode 1)
(diff-hl-flydiff-mode 1)
(use-package web-mode
             :ensure t
             :init
             (setq web-mode-content-types-alist '(("jsx" . "\\.tsx\\'")))
             (setq web-mode-content-types-alist '(("jsx" . "\\.js\\'")))
             :config
             (add-to-list 'auto-mode-alist '("\\.erb?\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
             (add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . web-mode)))
(use-package add-node-modules-path
             :ensure t)
(eval-after-load 'web-mode
                 '(progn
                    (add-hook 'web-mode-hook #'add-node-modules-path)
                    (add-hook 'web-mode-hook #'prettier-js-mode)))
(use-package yaml-mode :ensure t)
(use-package haml-mode :ensure t)
(use-package scss-mode
             :mode (("\.scss\'" . scss-mode)))
(use-package tide
             :ensure t)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(setq tide-tsserver-executable "node_modules/.bin/tsserver")

(add-hook 'web-mode-hook #'setup-tide-mode)
(use-package graphql-mode
             :ensure t)
(use-package rust-mode
             :ensure t)
(use-package markdown-mode
             :ensure t
             :mode (("README\\.md\\'" . gfm-mode)
                    ("\\.md\\'" . markdown-mode)
                    ("\\.markdown\\'" . markdown-mode))
             :init (setq markdown-command "multimarkdown"))
(add-to-list 'auto-mode-alist '("\\.tex.tera\\'" . latex-mode))
(setq ruby-insert-encoding-magic-comment nil)
(use-package editorconfig
             :ensure t
             :config
             (editorconfig-mode 1))
(use-package flycheck
             :ensure t
             :init
             (setq flycheck-indication-mode nil)
             (setq flycheck-display-errors-delay nil)
             (setq flycheck-idle-change-delay 2)
             (global-flycheck-mode))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(with-eval-after-load 'flycheck
                      (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                 (or (buffer-file-name) default-directory)
                 "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
;; ** Use <leader>[jk] to navigate to the next and previous error
(evil-leader/set-key
  "j" 'flycheck-next-error
  "k" 'flycheck-previous-error)
(defun reevaluate-eyecandy ()
  (load-theme my-theme t))
(if (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (reevaluate-eyecandy))))
