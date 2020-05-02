(use-package lsp-mode
  :commands lsp
  :hook (prog-mode . lsp)
  :init
  ;; enable log only for debug
  (setq lsp-log-io nil)

  ;; Disable eldoc hover
  (setq lsp-eldoc-enable-hover nil)

  ;; Instead of prompting for the project root, guess it with projectile
  (setq lsp-auto-guess-root t)

  ;; No realtime syntax check
  (setq lsp-diagnostic-package :none)

  ;; Handle yasnippet by myself
  (setq lsp-enable-snippet nil)

  ;; Auto restart lsp
  (setq lsp-restart 'auto-restart)

  ;; Disable LSP's expensive/unnecessary features.
  (setq lsp-enable-folding nil)
  (setq lsp-enable-links nil)

  ;; Potentially slow
  (setq lsp-enable-file-watchers nil)
  (setq lsp-enable-text-document-color nil)
  (setq lsp-enable-semantic-highlighting nil)
  (setq lsp-enable-indentation nil)

  ;; Disable formatting since Apheleia is used for formatting
  (setq lsp-enable-on-type-formatting nil)

  ;; Auto-kill LSP server after last workspace buffer is killed.
  (setq lsp-keep-workspace-alive nil)

  ;; Let `flycheck-check-syntax-automatically' determine this.
  (setq lsp-flycheck-live-reporting nil)

  ;; For 'lsp-clients'
 (setq lsp-session-file (concat user-emacs-directory "var/lsp-session"))
 (setq lsp-server-install-dir (concat user-emacs-directory "var/lsp"))

  :init
  (require 'lsp-clients))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp . lsp-ui)
  :config
  (setq lsp-ui-doc-max-height 8)
  (setq lsp-ui-doc-max-width 35)
  (setq lsp-ui-sideline-ignore-duplicate t))

(use-package company-lsp
  :after (company lsp-mode)
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-async t)
  (setq lsp-prefer-capf t))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package lsp-origami :after lsp-mode)
