;; Make garbage collection after init
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

;;; prettify
;;(setq inhibit-startup-buffer-menu t)
;;(setq inhibit-startup-screen t)
;;(setq inhibit-startup-echo-area-message "locutus")
;;(setq initial-buffer-choice t)
(setq initial-scratch-message "")

(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

(setq backup-inhibited t)
(setq auto-save-default nil)

;;; packages

(require 'package)
(setq package-native-compile t)

(add-to-list 'package-archives
             (cons "melpa" "https://melpa.org/packages/")
             t)
(add-to-list 'package-archives
             (cons "org" "https://orgmode.org/elpa/")
             t)

(package-initialize)

(unless (package-installed-p 'use-package)
;  (delete-directory package-user-dir t)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(require 'bind-key)
;(require 'diminish)


(use-package dash
  :ensure t)

(use-package f
  :demand)

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
  :ensure nil
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

(use-package flycheck)
(use-package diff-hl
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
(use-package lsp-mode
  :config
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-eldoc-render-all t))
(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-imenu-enable nil)
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-sideline-enable nil))
(use-package company)
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
   '(smartparens expand-region exec-path-from-shell dash fzf promise direx hydra request fuzzy-finder ace-jump-mode projectile curl-to-elisp sed-mode sharper use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
