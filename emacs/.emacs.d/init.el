;;; disable splash screen
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

(use-package async)

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

;; make use of standard directories
(use-package no-littering)

;; variables to remove compile-log warnings
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)

;; packages in etc
(async-bytecomp-package-mode 1)
(let ((loaded (mapcar #'file-name-sans-extension (delq nil (mapcar #'car load-history)))))
  (dolist (file (directory-files "~/.emacs.d/etc" t ".+\\.elc?$"))
    (let ((library (file-name-sans-extension file)))
      (unless (member library loaded)
        (load library nil t)
        (push library loaded)))))

;;(use-package helm)

(require 'global-keys)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-dynamic-exhibit-delay-ms 200)
 '(ivy-height 10)
 '(ivy-initial-inputs-alist nil)
 '(ivy-magic-tilde nil)
 '(ivy-re-builders-alist (quote ((t . ivy-prescient-re-builder))) t)
 '(ivy-use-virtual-buffers t)
 '(ivy-wrap t)
 '(package-selected-packages
   (quote
    (easy-kill-extras pbcopy spacemacs-theme company-web magit move-text typescript-mode helm editorconfig dockerfile-mode visual-regexp undo-tree lorem-ipsum js2-refactor js2-mode gist flycheck-pos-tup flycheck css-eldoc use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
