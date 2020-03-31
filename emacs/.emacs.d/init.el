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

;;; Load built-in utility libraries
(require 'map)

;; make use of standard directories
(use-package no-littering
  :demand t)


(use-package dash)
(use-package f)
(use-package popup)
(use-package async)
(use-package better-defaults)

;; Make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
  :straight t
  :demand t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;load all packages in etc
(let ((loaded (mapcar #'file-name-sans-extension (delq nil (mapcar #'car load-history)))))
  (dolist (file (directory-files "~/.emacs.d/etc" t ".+\\.elc?$"))
    (let ((library (file-name-sans-extension file)))
      (unless (member library loaded)
        (load library nil t)
        (push library loaded)))))
