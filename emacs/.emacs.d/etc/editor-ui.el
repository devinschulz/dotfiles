;; Display line numbers
(global-display-line-numbers-mode t)

;; Set the font family
(set-frame-font "Dank Mono" nil t)

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

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package monokai-theme)

(add-hook 'after-init-hook
          (lambda() (if (display-graphic-p)
                        (load-theme 'doom-vibrant t)
                      (load-theme 'doom-vibrant t))))

(use-package solaire-mode
  :after doom-themes
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

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
