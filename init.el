;; Make garbage collection after init
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 8000000)))

;;; prettify
(setq initial-scratch-message "")

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(setq backup-inhibited t)
(setq auto-save-default nil)

(setq-default truncate-lines t)

(setq custom-enabled-themes '(leuven))
(load-theme 'leuven)

;;; Key binds
(global-set-key (kbd "C-<backspace>") 'kill-word)
(windmove-default-keybindings)

;;; packages

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

(use-package dired
  :straight nil
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-auto-revert-buffer t)
  (setq dired-recursive-copies 'always))

(use-package superword
  :straight nil)

(use-package dash)

(use-package f)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package exec-path-from-shell)

(use-package which-key)

(use-package ace-jump-mode
  :config (setq current-predic-arg '(4))
  :bind ("M-SPC" . 'ace-jump-mode))

(use-package hydra)

(use-package auth-source
  :straight nil
  :config (setq auth-sources '(password-store)))

;;; utils
(use-package magit
  :config (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package forge
  :after magit)

(use-package lisp-mode
  :straight nil
  :config
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode))

;;; IDE
(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region)
  ("C--" . 'er/contract-region))
(use-package smartparens
  :config (require 'smartparens-config))
(use-package projectile)		;Not sure if I like this package
(use-package whitespace-cleanup-mode)

(use-package flycheck
  :config
  (setq checkdoc-force-docstrings-flag nil))

(use-package diff-hl
  :hook ((lsp-mode . diff-hl-mode)
	 (diff-hl-mode . diff-hl-flydiff-mode))
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
(use-package lsp-mode
  :config
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-eldoc-render-all nil))


(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-imenu-enable nil)
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-sideline-enable nil))
(use-package company
  :config
  (setq company-minimum-prefix-length 0))
(use-package company-quickhelp)
(use-package company-lsp)

;;; Haskell
(use-package lsp-haskell)

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'lsp)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'company-quickhelp-mode)
  (exec-path-from-shell-copy-env "PATH"))

;;; FSharp
(use-package fsharp-mode
  :config
  (add-hook 'fsharp-mode-hook 'lsp)
  (add-hook 'fsharp-mode-hook 'company-quickhelp-mode)
  (add-hook 'fsharp-mode-hook 'subword-mode)
  (add-hook 'fsharp-mode-hook 'whitespace-cleanup-mode)
  (setq lsp-fsharp-server-install-dir
	"~/.fsharp/FsAutoComplete/bin/release_netcore/")
  (setq inferior-fsharp-program
	"dotnet fsi")
  (require 'sharper))

(use-package sharper
  :config (setq sharper-run-only-one t)
  :bind (:map fsharp-mode-map
	      ("C-c C-c" . 'sharper-main-transient)))

;;; CSharp
(use-package csharp-mode)

;;; XML ;; should probably tidy up config
(use-package nxml-mode
  :straight nil
  :init
  (add-to-list 'auto-mode-alist '("\\.xa?ml\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.fsproj\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
  :bind (:map nxml-mode-map ("C-c C-c" . 'sharper-main-transient)))

;;; LaTeX
(use-package tex
  :straight auctex
  :config (require 'lsp-latex)
  :hook ((TeX-latex-mode . lsp-deferred)
	 (TeX-mode . lsp-deferred)
	 (TeX-latex-mode . flyspell-mode)
	 (TeX-mode . flyspell-mode)
	 (TeX-latex-mode . auto-fill-mode)
	 (TeX-mode . auto-fill-mode)))
(use-package lsp-latex
  :config
  (add-to-list 'lsp-latex-build-args "-pvc"))

;;; Input
(use-package pyim)


;;; Misc


;;; Temporary packages
(add-to-list 'load-path "~/workspace/projects/emacs/gutenmacs")
(require 'gutenmacs)


(put 'downcase-region 'disabled nil)
