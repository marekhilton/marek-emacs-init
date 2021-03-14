;; Make garbage collection after init
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

;;; prettify
(setq initial-scratch-message "")

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(setq backup-inhibited t)
(setq auto-save-default nil)

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

(require 'bind-key)

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

;;; utils
(use-package magit
  :config (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

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
  (setq lsp-fsharp-server-install-dir
	"~/.fsharp/FsAutoComplete/bin/release_netcore/")
  (setq inferior-fsharp-program
	"dotnet fsi")
  (require 'sharper))

(use-package sharper
  :config (setq sharper-run-only-one t)
  :bind (:map fsharp-mode-map
	      ("C-c C-c" . 'sharper-main-transient)))

;;; XML ;; should probably tidy up config
(use-package nxml-mode
  :straight nil
  :config
  (add-to-list 'auto-mode-alist '("\\.xa?ml\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.fsproj\\'" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode)))

;;; LaTeX
(use-package auctex)


;;; Input
(use-package pyim)

;;; Temporary packages
(add-to-list 'load-path "~/workspace/projects/emacs/gutenmacs")
(require 'gutenmacs)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(leuven))
 '(package-selected-packages
   '(auctex diff-hl pyim smartparens expand-region exec-path-from-shell dash fzf promise direx hydra request fuzzy-finder ace-jump-mode projectile curl-to-elisp sed-mode sharper use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
