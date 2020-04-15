;; Display line numbers
(global-display-line-numbers-mode t)

(setq primary-font "Operator Mono")

;; Set the font family
(when (member primary-font (font-family-list))
  ;; Fonts are appearing smaller in macOS
  (if (equal system-type 'darwin)
      (set-frame-font (format "%s-11" primary-font) nil t)
    (set-frame-font (format "%s-9" primary-font) nil t)))

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
  :hook (after-init . doom-modeline-mode))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  :hook (after-init . (lambda() (if (display-graphic-p)
                                    (load-theme 'doom-molokai 'no-confirm)
                                  (load-theme 'monokai 'no-confirm)))))

(use-package monokai-theme)

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
