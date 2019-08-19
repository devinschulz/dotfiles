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

;; Write backup ffiles to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(require 'package)

; Some combination of GNU TLS and Emacs fail to retrieve archive
; contents over https.
; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/etw48ux
; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(if (and (version< emacs-version "26.3") (>= libgnutls-version 30600))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
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

(use-package gist :defer t)
(use-package lorem-ipsum :defer t)
(use-package visual-regexp :defer t)

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package editorconfig
  :defer t
  :config
  (editorconfig-mode 1))

(use-package helm)
(use-package find-file-in-project)
(use-package rainbow-delimiters)
(use-package embrace)

(use-package deadgrep
  :config
  (global-set-key (kbd "<f5>") #'deadgrep))

(use-package ws-butler
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package yasnippet
  :init
  (use-package yasnippet-snippets)
  :config
  (yas-global-mode 1))

(require 'init-ivy)
(require 'init-smartparens)
(require 'init-neotree)
(require 'init-which-key)
(require 'init-projectile)
(require 'init-company)
(require 'init-undo-tree)
(require 'init-hydra)
;; ;; (require 'init-tide)
(require 'init-avy)
(require 'init-crux)
(require 'init-move-text)
(require 'init-magit)
(require 'init-git-gutter)
(require 'init-multiple-cursors)
(require 'init-rainbow-delimiters)
(require 'init-html-mode)
(require 'init-lsp-mode)
(require 'init-web-mode)
(require 'init-flycheck)
(require 'init-go-mode)
(require 'init-org)

(require 'global-keys)
(require 'mode-mappings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (spacemacs-theme company-web magit move-text typescript-mode helm editorconfig dockerfile-mode visual-regexp undo-tree lorem-ipsum js2-refactor js2-mode gist flycheck-pos-tup flycheck css-eldoc use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
