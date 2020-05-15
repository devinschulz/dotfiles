;; Display line numbers
(global-display-line-numbers-mode t)

;; Set the font family
(let ((font "Operator Mono"))
  (when (member font (font-family-list))
    ;; Fonts are appearing smaller in macOS 10.15.4+
    (let ((frame-font (if (equal system-type 'darwin) (format "%s-12" font) (format "%s-9" font))))
      (set-frame-font frame-font nil t))))

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
  (doom-modeline-mode 1))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  :hook (after-init . (lambda() (if (display-graphic-p)
                                    (load-theme 'doom-one 'no-confirm)
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
