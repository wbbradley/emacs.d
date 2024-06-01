;;; package --- Summar of My config
;;;; Commentary:
;;;; Code:
(setq use-package-compute-statistics t)
(package-initialize)
(require 'package)

(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)
(add-to-list 'package-archives
             '("elpa-nongnu" . "https://elpa.nongnu.org/nongnu/")
             t)
(add-to-list
 'package-archives '("elpa" . "https://elpa.gnu.org/packages/")
 t)
(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode))

(setq elisp-autofmt-on-save-p 'always)
(setq elisp-autofmt-python-bin
      (string-trim (shell-command-to-string "command -v python3")))
(require 'pp)
;; (use-package rg)

;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; (load "package")
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/")
             t)

;; export EDITOR='emacsclient -nw -c -a ""'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(use-package gruvbox-theme)

(if (daemonp)
    (add-hook
     'after-make-frame-functions
     (defun my/theme-init-daemon (frame)
       (with-selected-frame frame
         (load-theme 'atom-one-dark))
       ;; Run this hook only once.
       (remove-hook
        'after-make-frame-functions #'my/theme-init-daemon)
       (fmakunbound 'my/theme-init-daemon))))
;;(setq my-theme 'gruvbox-dark-hard)
(setq my-theme 'gruvbox-dark-hard)
(load-theme my-theme t)

(use-package powerline :config (powerline-center-evil-theme))

(menu-bar-mode -1)
;; (scroll-bar-mode 0)
;; (tool-bar-mode 0)

(defalias 'yes-or-no-p 'y-or-n-p)


;; (global-linum-mode 1)
;; (setq linum-format " %4d ")
(global-display-line-numbers-mode)
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

(defconst emacs-tmp-dir
  (expand-file-name (format "emacs%d" (user-uid))
                    temporary-file-directory))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

(setq create-lockfiles nil)

(defun add-kbd (key)
  (kbd key))
(defvar keybindings-to-unset '("M-k" "M-j"))
(dolist (key (mapcar 'add-kbd keybindings-to-unset))
  (global-unset-key key))

(electric-pair-mode 0)

(global-visual-line-mode t)

(setq standard-indent 2)
(setq-default indent-tabs-mode nil)

(use-package drag-stuff)
(drag-stuff-global-mode 1)
;; (global-set-key (kbd "C-k") 'drag-stuff-up)
;; (global-set-key (kbd "C-j") 'drag-stuff-down)

(setq show-paren-delay 0)
(show-paren-mode 1)

;; Evil mode
(use-package
 evil
 :init
 ;; (global-set-key (kbd "C-i") 'evil-jump-forward)
 (setq evil-want-C-i-jump t)
 (setq evil-want-C-u-scroll t)
 (setq evil-vsplit-window-right t)
 (setq evil-split-window-below t)
 (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
 (setq evil-want-keybinding nil)
 (setq evil-undo-system 'undo-redo)
 :config
 (evil-mode 1)
 (use-package evil-leader :config (global-evil-leader-mode))

 (use-package evil-surround :config (global-evil-surround-mode))
 (use-package evil-commentary :config (evil-commentary-mode)))
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol)
  ;; make evil-search-word look for symbol rather than word boundaries
  (setq-default evil-symbol-word-search t))

(use-package
 evil-collection
 :after evil
 :config (evil-collection-init))

(use-package
 evil-escape
 :config
 (setq-default evil-escape-delay 0.3)
 (setq-default evil-escape-key-sequence "jk")
 :init (evil-escape-mode))

;; ** Make escape quit most things
(defun minibuffer-keyboard-quit ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*")
      (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key
 minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key
 minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key
 minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key
 minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; (use-package ivy)
;; (use-package counsel)
;; (use-package swiper)
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

;; (use-package navigate)
;; [[https://github.com/keith/evil-tmux-navigator][This package]] enables seamless C-[hjkl] movement through tmux panes _and_ Emacs windows. The following commands are required to be present in your tmux config:
;; bind -n C-h run "(tmux display-message -p '#{pane_current_command}' | grep -iqE '(^|\/)n?vim(diff)?$|emacs.*$' && tmux send-keys C-h) || tmux select-pane -L"
;; bind -n C-j run "(tmux display-message -p '#{pane_current_command}' | grep -iqE '(^|\/)n?vim(diff)?$|emacs.*$' && tmux send-keys C-j) || tmux select-pane -D"
;; bind -n C-k run "(tmux display-message -p '#{pane_current_command}' | grep -iqE '(^|\/)n?vim(diff)?$|emacs.*$' && tmux send-keys C-k) || tmux select-pane -U"
;; bind -n C-l run "(tmux display-message -p '#{pane_current_command}' | grep -iqE '(^|\/)n?vim(diff)?$|emacs.*$' && tmux send-keys C-l) || tmux select-pane -R"

(define-key evil-normal-state-map (kbd "C-j") (kbd "C-w j"))
(define-key evil-normal-state-map (kbd "C-k") (kbd "C-w k"))
;; (define-key evil-normal-state-map (kbd "C-h") (kbd "C-w h"))
(define-key evil-normal-state-map (kbd "C-l") (kbd "C-w l"))
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key
 evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
;; (define-key evil-normal-state-map [bktab] 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd "C-p") 'fzf-git-files) ;; helm-ls-git)
(define-key evil-normal-state-map (kbd "F") 'grep)
(define-key evil-normal-state-map (kbd "T") 'helm-etags-select)
(define-key evil-normal-state-map (kbd "H") 'previous-error)
(define-key evil-normal-state-map (kbd "L") 'next-error)
(define-key evil-normal-state-map (kbd "<f9>") 'previous-error)
(define-key evil-normal-state-map (kbd "<f10>") 'next-error)
(define-key evil-visual-state-map (kbd "!") 'eval-region)

(require 'grep)
(grep-apply-setting 'grep-command "git grep -nH ")
(setq grep-use-null-device nil)

(define-key
 evil-normal-state-map (kbd "<f3>") 'grep-word-under-cursor)
(defun grep-word-under-cursor ()
  "setting up grep-command using current word under cursor as a search string"
  (interactive)
  (let* ((cur-word (symbol-at-point))
         (cmd
          (concat
           "git grep -nH -e '\\<"
           (symbol-name cur-word)
           "\\>' -- :/")))
    ;; (grep-apply-setting 'grep-command cmd)
    (grep cmd)))

(define-key
 evil-normal-state-map (kbd "<f1>") 'help-word-under-cursor)
(defun help-word-under-cursor ()
  "get help for the word under the cursor"
  (interactive)
  (let (cur-word
        (symbol-at-point))
    (print (symbol-at-point))
    (if cur-word
        (help (symbol-name cur-word))
      nil)))

;; (define-key evil-motion-state-map (kbd "jk") (kbd [escape]))
;; map :e
;; (define-key evil-ex-map "e" 'find-file)
(use-package
 fzf
 :bind (("C-p" . fzf-git-files) ("F" . fzf-git-grep))
 ;; Don't forget to set keybinds!
 :config
 (setq
  fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
  fzf/executable "fzf"
  fzf/git-grep-args "-i --line-number %s"
  ;; command used for `fzf-grep-*` functions
  ;; example usage for ripgrep:
  ;; fzf/grep-command "rg --no-heading -nH"
  fzf/grep-command "grep -nrH"
  ;; If nil, the fzf buffer will appear at the top of the window
  fzf/position-bottom t
  fzf/window-height 15))

(use-package which-key)
(which-key-mode)
(define-key evil-motion-state-map (kbd "-") 'evil-first-non-blank)
;; (define-key evil-motion-state-map (kbd "^") 'evil-beginning-of-line)

;; Evil Leader keybindings
(evil-leader/set-leader "\\")
(evil-leader/set-key
 "F"
 'helm-browse-project
 "t"
 'treesit-inspect-node-at-point
 "d"
 'edebug-defun
 "f"
 'helm-git-grep
 "0"
 '(lambda ()
    (interactive)
    (find-file "~/.emacs.d/init.el"))
 ;; "F" 'helm-projectile-ag
 "q"
 'evil-quit-all
 "w"
 'save-buffer
 ;; "g" 'magit)
 )

(use-package helm :config (helm-mode t))
(use-package helm-ls-git)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
(use-package helm-git-grep)
;; (global-unset-key (kbd "<f3>"))
(global-set-key (kbd "C-p") 'helm-ls-git)
(setq helm-ls-git-sources '(helm-source-ls-git))

;; (use-package projectile :ensure projectile :config (setq projectile-indexing-method 'git))
;; (use-package helm-projectile)
;; (use-package helm-ag)
;; (use-package company :config (global-company-mode t) (setq company-global-modes '(not org-mode)))
;; (define-key company-mode-map (kbd "TAB") 'company-complete)
(use-package
 rainbow-delimiters
 :init
 (add-hook 'web-mode-hook #'rainbow-delimiters-mode)
 (add-hook 'rust-mode-hook #'rainbow-delimiters-mode))
;; (use-package magit :config (setq magit-diff-refine-hunk 'all))
(use-package diff-hl :init (setq diff-hl-side 'right))
(global-diff-hl-mode 1)
(diff-hl-margin-mode 1)
(diff-hl-flydiff-mode 1)
(use-package yaml-mode)
;; (use-package haml-mode)

(use-package rust-mode :init (setq rust-mode-treesitter-derive t))

(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
(setq rust-format-on-save t)
(define-key evil-normal-state-map (kbd "<f7>") 'rust-check)
(define-key evil-normal-state-map (kbd "<f8>") 'rust-run-clippy)

(use-package
 markdown-mode

 :mode
 (("README\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode))
 :init (setq markdown-command "multimarkdown"))
(add-to-list 'auto-mode-alist '("\\.tex.tera\\'" . latex-mode))
(setq ruby-insert-encoding-magic-comment nil)
(use-package
 editorconfig

 :config (editorconfig-mode 1))
(use-package
 flycheck
 :init
 (setq flycheck-indication-mode nil)
 (setq flycheck-display-errors-delay nil)
 (setq flycheck-idle-change-delay 2)
 (global-flycheck-mode))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(with-eval-after-load 'flycheck
  (advice-add
   'flycheck-eslint-config-exists-p
   :override (lambda () t)))
(defun my/use-eslint-from-node-modules ()
  (let* ((root
          (locate-dominating-file
           (or (buffer-file-name) default-directory) "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/eslint/bin/eslint.js"
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
;; ** Use <leader>[jk] to navigate to the next and previous error
(evil-leader/set-key
 "j" 'flycheck-next-error "k" 'flycheck-previous-error)
