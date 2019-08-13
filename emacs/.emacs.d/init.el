;; disable splash screen
(setq inhibit-startup-message t)

;; Set path in dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; Set load paths
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

(require 'better-defaults)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
)
(eval-when-compile
  (require 'package)
  (defvar use-package-verbose t)
  (require 'use-package))

;; By default install dependencies if they are missing from the filesystem
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Require the appearance early
(require 'appearance)

(use-package css-eldoc)
(use-package flycheck)
(use-package flycheck-pos-tip)
(use-package gist)
(use-package js2-mode)
(use-package js2-refactor)
(use-package less-css-mode)
(use-package lorem-ipsum)
(use-package visual-regexp)
(use-package dockerfile-mode)
(use-package editorconfig)
(use-package helm)
(use-package projectile)
(use-package org)
(use-package find-file-in-project)
(use-package rainbow-delimiters)
(use-package web-mode)
(use-package go-mode)

(require 'init-ivy)
(require 'init-smartparens)
(require 'init-neotree)
(require 'init-which-key)
(require 'init-projectile)
(require 'init-company)
(require 'init-undo-tree)
(require 'init-hydra)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm editorconfig dockerfile-mode visual-regexp undo-tree lorem-ipsum js2-refactor js2-mode gist flycheck-pos-tup flycheck css-eldoc use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
