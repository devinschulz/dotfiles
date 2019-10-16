;;; disable splash screen
(setq inhibit-startup-message t)

;; Set path in dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set load paths
(add-to-list 'load-path site-lisp-dir)

(require 'better-defaults)

;; Write backup ffiles to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Disable lockfiles
(setq create-lockfiles nil)

(require 'package)

; Some combination of GNU TLS and Emacs fail to retrieve archive
; contents over https.
; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/etw48ux
; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
;; (if (and (version< emacs-version "26.3") (>= libgnutls-version 30600))
;;     (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

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

;; Make Emacs use the $PATH set up by the user's shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :demand t
  :config (exec-path-from-shell-initialize))

;; packages in etc
(async-bytecomp-package-mode 1)
(let ((loaded (mapcar #'file-name-sans-extension (delq nil (mapcar #'car load-history)))))
  (dolist (file (directory-files "~/.emacs.d/etc" t ".+\\.elc?$"))
    (let ((library (file-name-sans-extension file)))
      (unless (member library loaded)
        (load library nil t)
        (push library loaded)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#303030" "#ff4b4b" "#d7ff5f" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#c6c6c6"])
 '(ansi-term-color-vector
   [unspecified "#272822" "#f92672" "#a6e22e" "#f4bf75" "#66d9ef" "#ae81ff" "#66d9ef" "#f8f8f2"] t)
 '(beacon-color "#cc6666")
 '(company-lsp-async t)
 '(company-lsp-cache-candidates t)
 '(company-lsp-enable-recompletion t)
 '(company-lsp-enable-snippet t)
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" default)))
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(gofmt-command "goimports")
 '(ivy-dynamic-exhibit-delay-ms 200)
 '(ivy-height 10)
 '(ivy-initial-inputs-alist nil)
 '(ivy-magic-tilde nil)
 '(ivy-re-builders-alist (quote ((t . ivy-prescient-re-builder))) t)
 '(ivy-use-virtual-buffers t)
 '(ivy-wrap t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(lsp-clients-go-server-args
   (quote
    ("--cache-style=always" "--diagnostics-style=onsave" "--format-style=goimports")))
 '(lsp-document-sync-method (quote incremental))
 '(lsp-guess-root t t)
 '(lsp-log-io nil)
 '(lsp-print-io nil)
 '(lsp-print-performance nil)
 '(lsp-reponse-timeout 10 t)
 '(lsp-trace nil t)
 '(ns-auto-hide-menu-bar nil)
 '(objed-cursor-color "#ff6c6b")
 '(package-selected-packages
   (quote
    (simpleclip magit-todos icicles powerline snazzy-theme monokai-theme zenburn-theme color-theme-sanityinc-tomorrow afternoon-theme lush-theme moe-theme blackboard-theme flyspell-correct-ivy magithub org-bullets goto-line-preview easy-kill-extras pbcopy spacemacs-theme company-web magit move-text typescript-mode helm editorconfig dockerfile-mode visual-regexp undo-tree lorem-ipsum js2-refactor js2-mode gist flycheck-pos-tup flycheck css-eldoc use-package)))
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
