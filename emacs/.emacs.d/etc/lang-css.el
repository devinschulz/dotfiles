(use-package css-mode
  :mode "//.css'"
  :after prettier-js
  :init (setq css-indent-offset 2))

;; SCSS mode
(use-package scss-mode
  :mode ("\\.scss'" "\\.sass'")
  :init
  (setq scss-compile-at-save nil))

;; New `less-css-mode' in Emacs 26
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

;; CSS eldoc
(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))

(use-package stylus-mode
  :straight (:host github :repo "vladh/stylus-mode"
                   :files ("stylus-mode.el")))
