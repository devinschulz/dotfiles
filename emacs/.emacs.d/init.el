;;; package --- The main entrypoint to my emacs configuration

;; Install straight.el
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

;; Required for backwards compatibility with use-package
(straight-use-package 'use-package)

;; Install a package with the same name unless told otherwise with
;; :straight
(setq straight-use-package-by-default t)

;; Always load lazily unless told otherwise with :demand
(setq use-package-always-defer t)

;; Always load lazily unless told otherwise.
(setq use-package-always-defer t)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; If there's an error, let me see where it is.
(setq debug-on-error nil)

;; Up the memory so improve the performance due to the fact that the
;; client/server communication generates a lot of memory/garbage.
;; <https://github.com/emacs-lsp/lsp-mode#performance>.
(setq gc-cons-threshold (* 100 1024 1024))

;;; Load built-in utility libraries
(require 'map)

;; make use of standard directories
(use-package no-littering
  :demand t)

(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin mepla-stable",
  :pin melpa
  :commands (esup))

(use-package dash)
(use-package f)
(use-package popup)
(use-package async)
(use-package better-defaults)
(use-package direnv
  :config
  (direnv-mode))

;; Make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;load all packages in etc
(let ((loaded (mapcar #'file-name-sans-extension (delq nil (mapcar #'car load-history)))))
  (dolist (file (directory-files "~/.emacs.d/etc" t ".+\\.elc?$"))
    (let ((library (file-name-sans-extension file)))
      (unless (member library loaded)
        (load library nil t)
        (push library loaded)))))
