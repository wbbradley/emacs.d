;;; init --- Summary of My config
;;;; Commentary:
;;;; Code:
(setq sav-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024))
(setq use-package-compute-statistics t)
(setq use-package-always-defer t)
(require 'package)

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
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq fill-column 100)

(use-package
 reformatter
 :hook
 (python-mode . autoimport-on-save-mode)
 (python-mode . ruff-format-on-save-mode)
 (python-ts-mode . autoimport-on-save-mode)
 (python-ts-mode . ruff-format-on-save-mode)
 :config
 (reformatter-define autoimport :program "autoimport" :args '("-"))
 (reformatter-define
  ruff-format
  :program "ruff"
  :args
  `("format" "--stdin-filename" ,buffer-file-name "-")))

(use-package
 python-isort
 :load-path "~/.emacs.d/lisp"
 :hook
 (python-mode . python-isort-on-save-mode)
 (python-ts-mode . python-isort-on-save-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package rust-mode)
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
(setq rust-format-on-save t)

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
   python-ts-mode-map (kbd "<backtab>") 'python-indent-dedent-line))

(setq tab-always-indent 'complete)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/")
             t)

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
(use-package
 flycheck
 :init
 (global-flycheck-mode)
 (setq flycheck-display-errors-delay 0.015))


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
 :demand t
 :config (evil-mode 1))
(use-package
 evil-leader
 :demand t
 :config (global-evil-leader-mode) (evil-leader/set-leader "\\")
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
  'redraw-display))

(use-package
 helm
 :config
 (helm-mode t)
 (define-key helm-map (kbd "C-j") 'helm-next-line)
 (define-key helm-map (kbd "C-k") 'helm-previous-line))

;; (use-package evil-surround :config (global-evil-surround-mode))
;; (use-package evil-commentary :config (evil-commentary-mode)))
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
(define-key evil-normal-state-map (kbd "<f9>") 'previous-error)
(define-key evil-normal-state-map (kbd "<f10>") 'next-error)
(define-key evil-normal-state-map (kbd "L") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "H") 'flycheck-previous-error)
(define-key evil-visual-state-map (kbd "!") 'eval-region)
;; (setq tags-table-list '("~/.emacs.d" "~/lx/core"))

(require 'grep)
(grep-apply-setting 'grep-command "git grep -nH ")
(setq grep-use-null-device nil)

(define-key
 evil-normal-state-map (kbd "<f3>") 'grep-word-under-cursor)
(defun grep-word-under-cursor ()
  "Setting up grep-command using current word under cursor as a search string."
  (interactive)
  (let* ((cur-word (symbol-at-point))
         (cmd
          (concat
           "git grep -nH -e '\\<"
           (symbol-name cur-word)
           "\\>' -- :/")))
    ;; (grep-apply-setting 'grep-command cmd)
    (grep cmd)))

(use-package
 fzf
 :bind (("C-p" . fzf-git-files))
 :config
 (setq
  fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
  fzf/executable "fzf"
  fzf/git-grep-args "-i --line-number %s"
  fzf/grep-command "grep -nrH"
  fzf/position-bottom t
  fzf/window-height 25))
(define-key
 evil-normal-state-map (kbd "F")
 (lambda ()
   (interactive)
   (fzf-git-grep)))

(use-package which-key)
(which-key-setup-side-window-right)
(setq which-key-show-early-on-C-h t)
(setq which-key-idle-delay 0.5)
(setq which-key-idle-secondary-delay 0.5)
(define-key
 evil-normal-state-map (kbd " ") 'which-key-show-major-mode)
(which-key-mode)
(define-key evil-motion-state-map (kbd "-") 'evil-first-non-blank)
;; (define-key evil-motion-state-map (kbd "^") 'evil-beginning-of-line)

;; (use-package helm-ls-git)
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
 '(custom-safe-themes
   '("7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98"
     "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8"
     default))
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(flycheck
     reformatter
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
(set-face-attribute 'error nil :underline "#b72727")
(set-face-attribute 'warning nil :underline "#fabd2f")
(set-face-attribute 'flycheck-error nil
                    ;; :underline "#b72727"
                    :underline nil
                    :foreground "white"
                    :background "#b72727")

(set-face-attribute 'flycheck-warning nil
                    ;; :underline "#fabd2f"
                    :underline nil
                    :foreground "black"
                    :background "#fabd2f")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq gc-cons-threshold sav-gc-cons-threshold)
