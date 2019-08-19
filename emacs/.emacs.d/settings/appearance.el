;; Show line numbers all the time
(global-linum-mode 1)

;; Set the tab width to 2 spaces
(setq-default tab-width 2)

;; Highlight current line
;; (global-hl-line-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Highlight trailing whitespace
;;(show-trailing-whitespace t)

(use-package doom-themes
  :init (load-theme 'doom-one t)
  :config
  ; (doom-themes-neotree-config)
  (doom-themes-org-config))

; (use-package material-theme
;   :init (load-theme 'material t))
; (use-package oceanic-theme
;   :config
;   (load-theme 'oceanic t))

(use-package doom-modeline :hook (after-init . doom-modeline-mode))

(provide 'appearance)
