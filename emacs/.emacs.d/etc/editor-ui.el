;; Show line numbers all the time
;; (global-linum-mode 1)

(use-package doom-themes
  :init (load-theme 'doom-one t)
  :config (doom-themes-org-config))

(use-package doom-modeline
  :after doom-themes
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

(use-package whitespace
  :ensure nil
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
(use-package nlinum-relative
  :ensure t
  :config
  (nlinum-relative-on)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (setq nlinum-relative-redisplay-delay 0.5) ;; delay
  (setq nlinum-relative-current-symbol "->") ;; or "" for current line
  (setq nlinum-relative-offset 1))
