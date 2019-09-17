;; Show line numbers all the time
(global-linum-mode 1)

(use-package doom-modeline
  :init
  (setq doom-modeline-bar-width                 3
        doom-modeline-buffer-encoding           t
        doom-modeline-enable-word-count         nil
        doom-modeline-height                    25
        doom-modeline-icon                      t
        doom-modeline-indent-info               nil
        doom-modeline-lsp                       nil
        doom-modeline-major-mode-color-icon     t
        doom-modeline-major-mode-icon           t
        doom-modeline-minor-modes               nil)
  :config (doom-modeline-mode))

(use-package base16-theme
  :init
  (setq base16-theme-256-color-source "base16-shell")
  :config
  (load-theme 'base16-oceanicnext t))

(use-package whitespace
  :diminish
  :hook (prog-mode . whitespace-mode)
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face lines-tail
                                tabs
                                trailing
                                empty
                                space-before-tab::tab
                                space-before-tab::space)))
