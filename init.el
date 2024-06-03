;;; package --- Summar of My config
;;;; Commentary:
;;;; Code:
(setq use-package-compute-statistics t)
(package-initialize)
(require 'package)
(require 'flymake)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (setq flymake-ruff-program "/Users/willbradley/bin/ruff")

;;;;;;;;;;;; Setup Packages Index & use-package ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)
(add-to-list 'package-archives
             '("elpa-nongnu" . "https://elpa.nongnu.org/nongnu/")
             t)
(add-to-list
 'package-archives '("elpa" . "https://elpa.gnu.org/packages/")
 t)

(require 'use-package)
(setq use-package-always-ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 reformatter
 :hook
 (python-mode . ruff-format-on-save-mode)
 (python-ts-mode . ruff-format-on-save-mode)
 :config
 (reformatter-define
  ruff-format
  :program "ruff"
  :args
  `("format" "--stdin-filename" ,buffer-file-name "-")))

(remove-hook
 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
(use-package flymake-mypy :load-path "~/.emacs.d/lisp")
(use-package rust-mode)
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
(setq rust-format-on-save t)
(use-package cargo)
(use-package
 ;; https://github.com/mgmarlow/flymake-clippy/issues/1
 flymake-clippy
 :hook (rust-mode . flymake-clippy-setup-backend))

;; (use-package flymake-rust :load-path "~/.emacs.d/lisp" :hook (rust-mode-hook . lymake-rust-load))

;; from https://github.com/akash-akya/emacs-flymake-cursor
(use-package
 flymake-cursor
 :load-path "~/.emacs.d/lisp/emacs-flymake-cursor" ;; vendored repo path
 :config (flymake-cursor-mode))

(define-key flymake-mode-map (kbd "L") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "H") 'flymake-goto-prev-error)
(add-hook
 'prog-mode-hook
 (lambda ()
   (flymake-mode 1)
   (local-set-key [f2] 'flymake-goto-prev-error)
   (local-set-key [f3] 'flymake-goto-next-error)))


(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode))

(setq elisp-autofmt-on-save-p 'always)
(setq elisp-autofmt-python-bin
      (string-trim (shell-command-to-string "command -v python3")))
(with-eval-after-load 'python
  (define-key
   python-mode-map (kbd "<tab>") 'python-indent-shift-right)
  (define-key
   python-mode-map (kbd "<backtab>") 'python-indent-dedent-line)
  (define-key
   python-ts-mode-map (kbd "<tab>") 'python-indent-shift-right)
  (define-key
   python-ts-mode-map (kbd "<backtab>") 'python-indent-dedent-line)
  (remove-hook 'flymake-diagnostic-functions 'python-flymake))

(setq tab-always-indent 'complete)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/")
             t)

(use-package flymake-ruff :hook (python-mode . flymake-ruff-load))

;; export EDITOR='emacsclient -nw -c -a ""'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package gruvbox-theme)
(setq my-theme 'gruvbox-dark-hard)
(load-theme my-theme t)

(use-package powerline :config (powerline-center-evil-theme))

;; (menu-bar-mode -1)
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
(xterm-mouse-mode 1)

(setq compilation-scroll-output 'first-error)

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
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'parentheses)

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
(define-key evil-normal-state-map (kbd "E") 'grep)
(define-key evil-normal-state-map (kbd "T") 'helm-etags-select)
(define-key evil-normal-state-map (kbd "H") 'flymake-goto-prev-error)
(define-key evil-normal-state-map (kbd "L") 'flymake-goto-next-error)
(define-key evil-normal-state-map (kbd "<f9>") 'previous-error)
(define-key evil-normal-state-map (kbd "<f10>") 'next-error)
(define-key evil-visual-state-map (kbd "!") 'eval-region)
;; (setq tags-table-list '("~/.emacs.d" "~/lx/core"))

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
  "Get help for the word under the cursor."
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
 :bind (("C-p" . fzf-git-files))
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
(define-key
 evil-normal-state-map (kbd "F")
 (lambda ()
   (interactive)
   (fzf-git-grep)))

(use-package which-key)
(which-key-setup-side-window-right)
(setq which-key-show-early-on-C-h t)
(setq which-key-idle-delay 0.05)
(setq which-key-idle-secondary-delay 0.05)
(define-key
 evil-normal-state-map (kbd " ") 'which-key-show-major-mode)
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
 "0"
 '(lambda ()
    (interactive)
    (find-file "~/.emacs.d/init.el"))
 "q"
 'evil-quit-all
 "w"
 'save-buffer
 "SPC"
 'redraw-display)

(use-package helm :config (helm-mode t))
;; (use-package helm-ls-git)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
;; (use-package helm-git-grep)
(setq helm-ls-git-sources '(helm-source-ls-git))

(global-diff-hl-mode 1)
(diff-hl-margin-mode 1)

(use-package yaml-mode)


(define-key evil-normal-state-map (kbd "<f7>") 'rust-check)
(define-key evil-normal-state-map (kbd "<f8>") 'rust-run-clippy)
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(global-set-key [C-f1] 'show-file-name) ; Or any other key you want

(define-key evil-normal-state-map (kbd "C-g") #'show-file-name)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(reformatter
     yaml-mode
     which-key
     web-mode
     tide
     scss-mode
     rust-mode
     rg
     rainbow-delimiters
     powerline
     markdown-mode
     magit
     helm-projectile
     helm-ls-git
     helm-git-grep
     helm-ag
     haml-mode
     gruvbox-theme
     graphql-mode
     fzf
     flymake-ruff
     flymake-cursor
     evil-surround
     evil-leader
     evil-escape
     evil-commentary
     evil-collection
     elisp-autofmt
     editorconfig
     dumb-jump
     drag-stuff
     diff-hl
     counsel
     company
     add-node-modules-path)))
(set-face-attribute 'error nil :underline t) ;;"#b72727")
(set-face-attribute 'warning nil :underline t) ;;"#fabd2f")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
